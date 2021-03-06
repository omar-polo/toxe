/*
 * Copyright (c) 2020-2021 Omar Polo <op@omarpolo.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

#include <ctype.h>
#include <err.h>
#include <inttypes.h>
#include <poll.h>
#include <signal.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <tox/tox.h>
#include <unistd.h>

#include "toxe.h"

#define NODES_LEN	255
struct dht_node {
	char	*host;
	uint16_t port;
	char	*pkey;
} nodes [NODES_LEN];

size_t nodes_len = 0;

char *savepath, *savepath_tmp;

volatile sig_atomic_t intr = 0;

void
sigint_handler(int signo)
{
	intr = 1;
}


/* plist impl. */

struct atom *
make_string_check(const char *s, size_t len)
{
	if (toxe_is_valid_utf8(s, len))
		return make_strkey(s, len, ASTR);
	return MAKE_SYMBOL("invalid-utf8");
}

struct atom *
make_string(const char *s, size_t len)
{
	return make_strkey(s, len, ASTR);
}

struct atom *
make_strkey(const char *s, size_t len, int type)
{
	struct atom *a;
	size_t i;

	if ((a = calloc(1, sizeof(struct atom))) == NULL)
		err(1, "calloc");

	a->type = type;
	if ((a->str = calloc(1, len+1)) == NULL)
		err(1, "calloc");
	if (type == AKEY)
		memcpy(a->str, s, len);
	else {
		for (i = 0; i < len; ++i) {
			if (s[i] == '\\' && i < len-1) {
				switch (s[++i]) {
				case 'n':
					a->str[i] = '\n';
					break;
				default:
					a->str[i] = s[i];
					break;
				}
			} else
				a->str[i] = s[i];
		}
	}

	return a;
}

struct atom *
make_integer(int64_t i)
{
	struct atom *a;

	if ((a = calloc(1, sizeof(struct atom))) == NULL)
		err(1, "calloc");
	a->type = AINT;
	a->integer = i;
	return a;
}

struct cons *
cons(struct atom *car, struct cons *cdr)
{
	struct cons *c;

	if ((c = calloc(1, sizeof(struct cons))) == NULL)
                err(1, "calloc");
	c->car = car;
	c->cdr = cdr;
	return c;
}

void
list_free(struct cons *l)
{
	struct cons *t;

	while (l != NULL) {
		t = l->cdr;

		if (l->car != NULL) {
			if (l->car->type != AINT)
				free(l->car->str);
			free(l->car);
		}
		free(l);

		l = t;
	}
}

struct cons *
append(struct cons *a, struct cons *b)
{
	struct cons *s;

	if (a == NULL)
		return b;
	s = a;
	while (a->cdr != NULL)
		a = a->cdr;
	a->cdr = b;
	return s;
}

struct cons *
push_back(struct cons *l, struct atom *a)
{
	return append(l, cons(a, NULL));
}

struct atom *
car(struct cons *c)
{
	if (c == NULL)
		return NULL;
	return c->car;
}

struct cons *
cdr(struct cons *c)
{
	if (c == NULL)
		return NULL;
	return c->cdr;
}

int
eq(const struct atom *restrict a, const struct atom *restrict b)
{
	if (a == b)
		return 1;
	if (a == NULL || b == NULL)
		return 0;
	if (a->type != b->type)
		return 0;
	return !strcmp(a->str, b->str);
}

struct atom *
plist_get(struct cons *restrict plist, const struct atom *restrict key)
{
	while (plist != NULL) {
		if (eq(car(plist), key))
			return CADR(plist);
		plist = CDDR(plist);
	}
	return NULL;
}

struct cons *
plist(struct cons *head, ...)
{
	va_list a;
	struct atom *i;

	va_start(a, head);

	for (;;) {
		if ((i = va_arg(a, struct atom*)) == NULL)
			break;
		head = append(head,
		    cons(i,
			cons(va_arg(a, struct atom*),
			    NULL)));
	}

	va_end(a);

	return head;
}

void
pp_atom(struct atom *a)
{
	char *i;

	if (a == NULL) {
		printf("nil");
		return;
	}

	switch (a->type) {
	case AKEY:
		printf(":%s", a->str);
		break;

	case ASYM:
		printf("%s", a->str);
		break;

	case ASTR:
		printf("\"");
		for (i = a->str; *i; ++i)
			switch (*i) {
			case '\n':
			case '\"':
				printf("\\%c", *i);
				break;
			default:
				printf("%c", *i);
			}
		printf("\"");
		break;

	case AINT:
		printf("%" PRId64, a->integer);
		break;

	default:
		/* unreachable */
		abort();
	}
}

void
pp(struct cons *list)
{
	printf("(");

        for (; list != NULL; list = cdr(list)) {
		pp_atom(car(list));
		if (cdr(list) != NULL)
			printf(" ");
	}

	printf(")\n");
}


/* parsing */

const char *
skip_blanks(const char *c)
{
	while (isspace(*c))
		c++;
	return c;
}

const char *
read_atom(const char *line, struct cons **ret)
{
	int type, esc = 0;
	int64_t n;
	const char *s;

	switch (*line) {
	case ')': return line;
	case ':': type = AKEY; break;
	case '"': type = ASTR; break;
	default:
		if (isdigit(*line)) {
			type = AINT;
			n = *line - '0';
		} else
			type = ASYM;
	}

	s = line + 1;
	if (type == ASYM)
		s = line;

	for (line++; *line; line++) {
		switch (type) {
		case AKEY:
		case ASYM:
			if (isspace(*line) || *line == ')') {
				*ret = push_back(*ret, make_strkey(s, line-s, type));
				return line;
			}
			break;

		case AINT:
			if (!isdigit(*line)) {
				*ret = push_back(*ret, make_integer(n));
				return line;
			}
			n = n*10 + (*line - '0');
			break;

		case ASTR:
			if (esc) {
				esc = 0;
				continue;
			}
			if (*line == '\\') {
				esc = 1;
				continue;
			}
			if (*line == '"') {
				*ret = push_back(*ret, make_strkey(s, line-s, type));
				return line+1;
			}
		}
	}

	return NULL;
}

struct cons *
read_list(const char *line)
{
	struct cons *ret = NULL;

	line = skip_blanks(line);
	if (*line != '(')
		return 0;

        for (line++; *line != ')' && *line != '\0'; line = skip_blanks(line)) {
		if ((line = read_atom(line, &ret)) == NULL) {
			list_free(ret);
			return NULL;
		}
	}

	if (*line == '\0') {
		list_free(ret);
		return NULL;
	}

	return ret;
}



/* assume out is 2*len (plus a NUL byte) */
void
bin2hex(const uint8_t *b, size_t len, char *out)
{
	size_t i;

	for (i = 0; i < len; i++, out += 2)
		sprintf(out, "%02X", b[i]);
}

/* assume b is large len/2 */
void
hex2bin(const char *h, size_t len, uint8_t *b)
{
	size_t i;

	for (i = 0; i < len; ++i, h += 2)
		sscanf(h, "%2hhx", &b[i]);
}



struct atom *
convert_user_status(TOX_USER_STATUS s)
{
	switch (s) {
	case TOX_USER_STATUS_NONE:
		return MAKE_SYMBOL("none");
	case TOX_USER_STATUS_AWAY:
		return MAKE_SYMBOL("away");
	case TOX_USER_STATUS_BUSY:
		return MAKE_SYMBOL("busy");
	default:
		return MAKE_SYMBOL("unknown");
	}
}

struct atom *
convert_connection(TOX_CONNECTION c)
{
	switch (c) {
	case TOX_CONNECTION_NONE:
		return MAKE_SYMBOL("none");
	case TOX_CONNECTION_TCP:
		return MAKE_SYMBOL("tcp");
	case TOX_CONNECTION_UDP:
		return MAKE_SYMBOL("udp");
	default:
		return MAKE_SYMBOL("unknown");
	}
}

struct atom *
convert_message_type(TOX_MESSAGE_TYPE t)
{
	switch (t) {
	case TOX_MESSAGE_TYPE_NORMAL:
		return MAKE_SYMBOL("normal");
	case TOX_MESSAGE_TYPE_ACTION:
		return MAKE_SYMBOL("action");
	default:
		return MAKE_SYMBOL("unknown");
	}
}



void
handle_friend_request(Tox *tox, const uint8_t *pk, const uint8_t *msg,
    size_t len, void *udata)
{
	char key[2*TOX_PUBLIC_KEY_SIZE + 1];

	bzero(key, sizeof(key));
	bin2hex(pk, TOX_PUBLIC_KEY_SIZE, key);

	PPP(MAKE_KEYWORD("@type"),	MAKE_SYMBOL("friend-request"),
	    MAKE_KEYWORD("public-key"),	make_string(key, sizeof(key)),
	    MAKE_KEYWORD("message"),	make_string_check(msg, len));
}

void
handle_friend_message(Tox *tox, uint32_t fnum, TOX_MESSAGE_TYPE t,
    const uint8_t *msg, size_t len, void *udata)
{
	PPP(MAKE_KEYWORD("@type"),		MAKE_SYMBOL("friend-message"),
	    MAKE_KEYWORD("friend-number"),	MAKE_INTEGER(fnum),
	    MAKE_KEYWORD("message-type"),	convert_message_type(t),
	    MAKE_KEYWORD("message"),		make_string_check(msg, len));
}

void
handle_conn_status(Tox *tox, TOX_CONNECTION status, void *udata)
{
	PPP(MAKE_KEYWORD("@type"),		MAKE_SYMBOL("connection-status"),
	    MAKE_KEYWORD("connection-status"),	convert_connection(status));
}

void
handle_friend_name(Tox *tox, uint32_t fnum, const uint8_t *name, size_t len,
    void *udata)
{
	PPP(MAKE_KEYWORD("@type"),		MAKE_SYMBOL("friend-name"),
	    MAKE_KEYWORD("friend-number"),	MAKE_INTEGER(fnum),
	    MAKE_KEYWORD("name"),		make_string_check(name, len));
}

void
handle_friend_status_message(Tox *tox, uint32_t fnum, const uint8_t *msg,
    size_t len, void *udata)
{
	PPP(MAKE_KEYWORD("@type"),		MAKE_SYMBOL("friend-status-message"),
	    MAKE_KEYWORD("friend-number"),	MAKE_INTEGER(fnum),
	    MAKE_KEYWORD("message"),		make_string_check(msg, len));
}

void
handle_friend_status(Tox *tox, uint32_t fnum, TOX_USER_STATUS s, void *udata)
{
	PPP(MAKE_KEYWORD("@type"),		MAKE_SYMBOL("friend-status"),
	    MAKE_KEYWORD("friend-number"),	MAKE_INTEGER(fnum),
	    MAKE_KEYWORD("status"),		convert_user_status(s));
}

void
handle_friend_connection_status(Tox *tox, uint32_t fnum, TOX_CONNECTION c,
    void *udata)
{
	PPP(MAKE_KEYWORD("@type"),		MAKE_SYMBOL("friend-connection-status"),
	    MAKE_KEYWORD("friend-number"),	MAKE_INTEGER(fnum),
	    MAKE_KEYWORD("connection-status"),	convert_connection(c));
}

void
handle_friend_read_receipt(Tox *tox, uint32_t fnum, uint32_t msgid,
    void *udata)
{
	PPP(MAKE_KEYWORD("@type"),		MAKE_SYMBOL("friend-read-receipt"),
	    MAKE_KEYWORD("friend-number"),	MAKE_INTEGER(fnum),
	    MAKE_KEYWORD("message-id"),		MAKE_INTEGER(msgid));
}



int
extract_friend_number(struct cons *plist, uint32_t *ret)
{
	struct atom *i;

	*ret = 0;

	if ((i = plist_get(plist, &INIT_KEYWORD("friend-number"))) == NULL)
		return 0;
	if (i->type != AINT)
		return 0;

	*ret = i->integer;
	return 1;
}

int
extract_message(struct cons *plist, char **ret)
{
	struct atom *i;

	*ret = NULL;

	if ((i = plist_get(plist, &INIT_KEYWORD("message"))) == NULL)
		return 0;
	if (i->type != ASTR)
		return 0;

	*ret = i->str;
	return 1;
}

int
extract_message_type(struct cons *plist, TOX_MESSAGE_TYPE *ret)
{
	struct atom *i;

	*ret = TOX_MESSAGE_TYPE_NORMAL;

	if ((i = plist_get(plist, &INIT_KEYWORD("message-type"))) == NULL)
		return 0;
	if (i->type != ASYM)
		return 0;

	if (eq(i, &INIT_SYMBOL("normal")))
		*ret = TOX_MESSAGE_TYPE_NORMAL;
	else if (eq(i, &INIT_SYMBOL("action")))
		*ret = TOX_MESSAGE_TYPE_ACTION;
	else
		return 0;
	return 1;
}

int
extract_pk(struct cons *plist, uint8_t *pk)
{
	struct atom *i;

	if ((i = plist_get(plist, &INIT_KEYWORD("public-key"))) == NULL)
		return 0;
	if (i->type != ASTR)
		return 0;
	if (strlen(i->str) != 2*TOX_PUBLIC_KEY_SIZE)
		return 0;

	hex2bin(i->str, 2*TOX_PUBLIC_KEY_SIZE, pk);
	return 1;
}

int
extract_name(struct cons *plist, char **ret)
{
	struct atom *i;

	*ret = NULL;

	if ((i = plist_get(plist, &INIT_KEYWORD("name"))) == NULL)
		return 0;
	if (i->type != ASTR)
		return 0;

	*ret = i->str;
	return 1;
}



int
hself_set_name(Tox *tox, struct cons *opts)
{
	char *name;
	char *errstr = "name: wrong type or missing";

	if (!extract_name(opts, &name))
		goto err;

	tox_self_set_name(tox, name, strlen(name), NULL);
	PPP(MAKE_KEYWORD("@type"),	MAKE_SYMBOL("self-set-name"),
	    MAKE_KEYWORD("@status"),	MAKE_SYMBOL("t"));
	return 1;

err:
	PPP(MAKE_KEYWORD("@type"),	MAKE_SYMBOL("self-set-name"),
	    MAKE_KEYWORD("@status"),	NULL,
	    MAKE_KEYWORD("@err"),	MAKE_STRING(errstr));
	return 1;
}

int
hself_set_status_msg(Tox *tox, struct cons *opts)
{
	char *msg;
	char *errstr = "message: wrong type or missing";

	if (!extract_message(opts, &msg))
		goto err;

	tox_self_set_status_message(tox, msg, strlen(msg), NULL);
	PPP(MAKE_KEYWORD("@type"),	MAKE_SYMBOL("self-set-status-message"),
	    MAKE_KEYWORD("@status"),	MAKE_SYMBOL("t"));
	return 1;

err:
	PPP(MAKE_KEYWORD("@type"),	MAKE_SYMBOL("self-set-status-message"),
	    MAKE_KEYWORD("@status"),	NULL,
	    MAKE_KEYWORD("@err"),	MAKE_STRING(errstr));
	return 1;
}

int
hself_get_addr(Tox *tox, struct cons *opts)
{
	uint8_t self[TOX_ADDRESS_SIZE];
	char hex[2*TOX_ADDRESS_SIZE+1];

	tox_self_get_address(tox, self);
	bin2hex(self, sizeof(self), hex);

	PPP(MAKE_KEYWORD("@type"),	MAKE_SYMBOL("self-get-address"),
	    MAKE_KEYWORD("@status"),	MAKE_SYMBOL("t"),
	    MAKE_KEYWORD("address"),	MAKE_STRING(hex));
	return 1;
}

int
hfriend_add(Tox *tox, struct cons *opts)
{
	char *msg, *errstr;
	uint8_t pk[TOX_PUBLIC_KEY_SIZE];
	TOX_ERR_FRIEND_ADD err;

	if (!extract_pk(opts, pk)) {
		errstr = "public-key: missing or wrong type";
		goto err;
	}

	if (!extract_message(opts, &msg))
		tox_friend_add_norequest(tox, pk, &err);
	else
		tox_friend_add(tox, pk, msg, strlen(msg), &err);

	if (err != TOX_ERR_FRIEND_ADD_OK) {
		errstr = "failure to send request";
		goto err;
	}

	PPP(MAKE_KEYWORD("@type"),	MAKE_SYMBOL("friend-add"),
	    MAKE_KEYWORD("@status"),	MAKE_SYMBOL("t"));
	return 1;

err:
	PPP(MAKE_KEYWORD("@type"),	MAKE_SYMBOL("friend-add"),
	    MAKE_KEYWORD("@status"),	NULL,
	    MAKE_KEYWORD("@err"),	MAKE_STRING(errstr));
	return 1;
}

int
hfriend_send_msg(Tox *tox, struct cons *opts)
{
	uint32_t fnum, msgid;
	char *msg, *errstr;
	TOX_MESSAGE_TYPE t;
	TOX_ERR_FRIEND_SEND_MESSAGE err;

	if (!extract_friend_number(opts, &fnum)) {
		errstr = "friend-number: missing or wrong type";
		goto err;
	}

	if (!extract_message(opts, &msg)) {
		errstr = "message: missing or wrong type";
		goto err;
	}

	if (!extract_message_type(opts, &t)) {
		errstr = "message-type: missing or wrong type";
		goto err;
	}

	msgid = tox_friend_send_message(tox, fnum, t, msg, strlen(msg), &err);
	if (err != TOX_ERR_FRIEND_SEND_MESSAGE_OK) {
		errstr = "failure to send the message";
		goto err;
	}

	PPP(MAKE_KEYWORD("@type"),		MAKE_SYMBOL("friend-send-message"),
	    MAKE_KEYWORD("friend-number"),	MAKE_INTEGER(fnum),
	    MAKE_KEYWORD("message-id"),		MAKE_INTEGER(msgid),
	    MAKE_KEYWORD("@status"),		MAKE_SYMBOL("t"));
	return 1;

err:
	PPP(MAKE_KEYWORD("@type"),	MAKE_SYMBOL("friend-send-message"),
	    MAKE_KEYWORD("@status"),	NULL,
	    MAKE_KEYWORD("@err"),	MAKE_STRING(errstr));
	return 1;
}

int
hget_chatlist(Tox *tox, struct cons *opts)
{
	size_t len, i;
	uint32_t *flist;
	uint8_t pk[TOX_PUBLIC_KEY_SIZE];
	char pk_hex[2*TOX_PUBLIC_KEY_SIZE + 1];
	char *name, *sm;
	size_t namelen, smlen;
	uint64_t last_online;
	TOX_USER_STATUS ustatus;
	TOX_CONNECTION conn;

	len = tox_self_get_friend_list_size(tox);
	if ((flist = calloc(sizeof(uint32_t), len)) == NULL)
		return 0;

	tox_self_get_friend_list(tox, flist);

	PPP(MAKE_KEYWORD("@type"), MAKE_SYMBOL("chatlist-start"));

	for (i = 0; i < len; ++i) {
		if (!tox_friend_get_public_key(tox, flist[i], pk, NULL))
			goto err;
		bzero(pk_hex, sizeof(pk_hex));
		bin2hex(pk, TOX_PUBLIC_KEY_SIZE, pk_hex);

		last_online = tox_friend_get_last_online(tox, flist[i], NULL);
		ustatus = tox_friend_get_status(tox, flist[i], NULL);
		conn = tox_friend_get_connection_status(tox, flist[i], NULL);

		namelen = tox_friend_get_name_size(tox, flist[i], NULL);
                if ((name = calloc(1, namelen+1)) == NULL)
			goto err;
		if (!tox_friend_get_name(tox, flist[i], name, NULL)) {
			free(name);
			goto err;
		}

		smlen = tox_friend_get_status_message_size(tox, flist[i], NULL);
		if ((sm = calloc(1, smlen+1)) == NULL) {
			free(name);
			goto err;
		}
		if (!tox_friend_get_status_message(tox, flist[i], sm, NULL)) {
			free(name);
			free(sm);
			goto err;
		}

		PPP(MAKE_KEYWORD("@type"),		MAKE_SYMBOL("chatlist-entry"),
		    MAKE_KEYWORD("@status"),		MAKE_SYMBOL("t"),
		    MAKE_KEYWORD("friend-number"),	MAKE_INTEGER(flist[i]),
		    MAKE_KEYWORD("public-key"),		MAKE_STRING(pk_hex),
		    MAKE_KEYWORD("last-seen"),		MAKE_INTEGER(last_online),
		    MAKE_KEYWORD("status"),		convert_user_status(ustatus),
		    MAKE_KEYWORD("name"),		make_string_check(name, namelen),
		    MAKE_KEYWORD("status-msg"),		make_string_check(sm, smlen),
		    MAKE_KEYWORD("conn-status"),	convert_connection(conn));

		free(name);
		free(sm);
	}

	PPP(MAKE_KEYWORD("@type"),	MAKE_SYMBOL("chatlist-end"),
	    MAKE_KEYWORD("@status"),	MAKE_SYMBOL("t"));

	free(flist);
	return 1;

err:
	PPP(MAKE_KEYWORD("@type"),	MAKE_SYMBOL("chatlist-end"),
	    MAKE_KEYWORD("@status"),	NULL);
	free(flist);
	return 1;
}

int
hquit(Tox *tox, struct cons *opts)
{
	PPP(MAKE_KEYWORD("@type"),	MAKE_SYMBOL("quit"),
	    MAKE_KEYWORD("@status"),	MAKE_SYMBOL("t"));
	return 0;
}



Tox *
init_tox(void)
{
	Tox *tox;
	struct Tox_Options opts;
	TOX_ERR_NEW err_new;
	uint8_t *data;
	FILE *f;
	size_t fsize, r, i;
	TOX_ERR_BOOTSTRAP err_boot;
	char *errstr;

	tox_options_default(&opts);
	opts.ipv6_enabled = 0;
	opts.udp_enabled = 1;

	if (savepath == NULL || (f = fopen(savepath, "r")) == NULL)
		tox = tox_new(&opts, &err_new);
	else {
		fseek(f, 0, SEEK_END);
		fsize = ftell(f);
		fseek(f, 0, SEEK_SET);

		if ((data = malloc(fsize)) == NULL)
			return NULL;
		r = fread(data, 1, fsize, f);
		fclose(f);

		if (r != fsize) {
			free(data);
			return NULL;
		}

		opts.savedata_type = TOX_SAVEDATA_TYPE_TOX_SAVE;
		opts.savedata_data = data;
		opts.savedata_length = fsize;
		tox = tox_new(&opts, &err_new);
		free(data);
		fclose(f);
	}

	if (err_new != TOX_ERR_NEW_OK)
		return NULL;

	for (i = 0; i < nodes_len; ++i) {
		tox_bootstrap(tox, nodes[i].host, nodes[i].port, nodes[i].pkey, &err_boot);
		if (err_boot == TOX_ERR_BOOTSTRAP_OK)
			continue;

		switch (err_boot) {
		case TOX_ERR_BOOTSTRAP_NULL:
			errstr = "one of the args was nil";
			break;
		case TOX_ERR_BOOTSTRAP_BAD_HOST:
			errstr = "bad host";
			break;
		case TOX_ERR_BOOTSTRAP_BAD_PORT:
			errstr = "bad port";
			break;
		default:
			errstr = "unknown error";
			break;
		}

		PPP(MAKE_KEYWORD("@type"),	MAKE_KEYWORD("bootstrap"),
		    MAKE_KEYWORD("@status"),	NULL,
		    MAKE_KEYWORD("@err"),	MAKE_STRING(errstr),
		    MAKE_KEYWORD("host"),	MAKE_STRING(nodes[i].host),
		    MAKE_KEYWORD("port"),	MAKE_INTEGER(nodes[i].port),
		    MAKE_KEYWORD("public-key"),	MAKE_STRING(nodes[i].pkey));
	}

	return tox;
}

int
save_tox(Tox *tox)
{
	uint8_t *data;
	size_t len;
	FILE *f;

	if (savepath == NULL)
		return 1;

	len = tox_get_savedata_size(tox);
	if ((data = malloc(len)) == NULL)
		return 0;
	tox_get_savedata(tox, data);

	if ((f = fopen(savepath_tmp, "w")) == NULL) {
		free(data);
		return 0;
	}
	fwrite(data, 1, len, f);
	fclose(f);
	free(data);

	rename(savepath_tmp, savepath);
	return 1;
}

int
stdin_ready()
{
	struct pollfd pfd = {0, POLLIN, -1};
	if (poll(&pfd, 1, 0) == -1)
		err(1, "poll");
	return (pfd.revents & POLLIN);
}

int
process_plist(Tox *tox, struct cons *cmd)
{
	struct atom *at_type;

	struct {
		struct atom type;
		int (*fn)(Tox*, struct cons *cmd);
	} handlers[] = {
		{INIT_SYMBOL("self-set-name"),			hself_set_name},
		{INIT_SYMBOL("self-set-status-message"),	hself_set_status_msg},
		{INIT_SYMBOL("self-get-address"),		hself_get_addr},
		{INIT_SYMBOL("friend-add"),			hfriend_add},
		{INIT_SYMBOL("friend-send-message"),		hfriend_send_msg},
		{INIT_SYMBOL("get-chatlist"),			hget_chatlist},
		{INIT_SYMBOL("quit"),				hquit},
		{INIT_SYMBOL(""),				NULL},
	}, *h;

	if ((at_type = plist_get(cmd, &INIT_KEYWORD("@type"))) == NULL) {
		warnx("missing :@type");
		return 0;
	}

	for (h = handlers; h->fn != NULL; h++) {
		if (eq(at_type, &h->type))
			return h->fn(tox, cmd);
	}

	warnx("unknown @type %s", at_type->str);
	return 0;
}

int
process_stdin(Tox *tox)
{
	char *line = NULL;
	size_t linesize = 0;
	ssize_t linelen;
	struct cons *cmd;
	int r;

	if ((linelen = getline(&line, &linesize, stdin)) == -1) {
		warn("getline");
		return 0;
	}

	cmd = read_list(line);
	free(line);

	if (cmd == NULL) {
		warnx("read_list");
		return 0;
	}

	r = process_plist(tox, cmd);
	list_free(cmd);
	return r;
}

void
add_bootstrap(const char *b)
{
	struct cons *p;
	struct atom *i;
	char *host, *pk;
	uint16_t port;

	if ((p = read_list(b)) == NULL)
		errx(1, "invalid bootstrap plist: %s", b);

	if (nodes_len == NODES_LEN)
		errx(1, "more than %d nodes given", NODES_LEN);

	if ((i = plist_get(p, &INIT_KEYWORD("host"))) == NULL)
		errx(1, "missing :host in %s", b);
	if (i->type != ASTR)
		errx(1, ":host must be a string in %s", b);
	if ((host = strdup(i->str)) == NULL)
		err(1, "strdup");

	if ((i = plist_get(p, &INIT_KEYWORD("port"))) == NULL)
		errx(1, "missing :port in %s", b);
	if (i->type != AINT)
		errx(1, ":port must be an integer in %s", b);
	port = i->integer;

	if ((i = plist_get(p, &INIT_KEYWORD("public-key"))) == NULL)
		errx(1, "missing :public-key in %s", b);
	if (i->type != ASTR)
		errx(1, ":public-key must be a string in %s", b);
	if ((pk = strdup(i->str)) == NULL)
		err(1, "strdup");

	nodes[nodes_len].host = host;
	nodes[nodes_len].port = port;
	nodes[nodes_len].pkey = pk;
	nodes_len++;

	list_free(p);
}

int
parse_testmode()
{
	char *line = NULL;
	size_t linesize = 0;
	ssize_t linelen;
	struct cons *cmd;

	if ((linelen = getline(&line, &linesize, stdin)) == -1)
		err(1, "getline");
	cmd = read_list(line);
	pp(cmd);
	free(line);
	list_free(cmd);
	return 0;
}

void __attribute__((__noreturn__))
usage(const char *me, int s)
{
	fprintf(stderr, "USAGE: %s [-B boostrap] [-s savepath] [-T test-mode]\n", me);
	exit(s);
}

int
main(int argc, char **argv)
{
	int ch;
	Tox *tox;
	enum test_mode tm;

	savepath = NULL;
	savepath_tmp = NULL;

	tm = TEST_NONE;
	while ((ch = getopt(argc, argv, "B:s:T:")) != -1) {
		switch (ch) {
		case 'B':
			add_bootstrap(optarg);
			break;

		case 's':
			if ((savepath = strdup(optarg)) == NULL)
				err(1, "strdup");
			if (asprintf(&savepath_tmp, "%s.tmp", savepath) == -1)
				err(1, "asprintf");
			break;

		case 'T':
			switch (*optarg) {
			case 'p':
				tm = TEST_PARSE;
				break;
			default:
				errx(1, "unknown test type: %c", *optarg);
			}
			break;

		default:
			usage(*argv, 1);
		}
	}

	switch (tm) {
	case TEST_PARSE:
		return parse_testmode();
	default:
		break;
	}

	signal(SIGINT, sigint_handler);

	if ((tox = init_tox()) == NULL)
		errx(1, "can't initialize tox");

	tox_callback_friend_request(tox, handle_friend_request);
	tox_callback_friend_message(tox, handle_friend_message);
	tox_callback_self_connection_status(tox, handle_conn_status);
	tox_callback_friend_name(tox, handle_friend_name);
	tox_callback_friend_status_message(tox, handle_friend_status_message);
	tox_callback_friend_status(tox, handle_friend_status);
	tox_callback_friend_connection_status(tox, handle_friend_connection_status);
	tox_callback_friend_read_receipt(tox, handle_friend_read_receipt);

	if (!save_tox(tox))
		errx(1, "cannot save tox");

	hself_get_addr(tox, NULL);
	handle_conn_status(tox, tox_self_get_connection_status(tox), NULL);

	while (!intr) {
		tox_iterate(tox, NULL);
		if (stdin_ready())
			if (!process_stdin(tox))
				break;
		usleep(1000 * tox_iteration_interval(tox));
	}

	if (!save_tox(tox))
		errx(1, "cannot save tox");
	tox_kill(tox);
	return 0;
}
