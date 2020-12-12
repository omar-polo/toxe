#include <ctype.h>
#include <err.h>
#include <inttypes.h>
#include <poll.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <tox/tox.h>
#include <unistd.h>

#include "toxe.h"

char *savepath, *savepath_tmp;


/* plist impl. */

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
pcons(struct cons *plist, struct atom *key, struct atom *val)
{
	return append(plist, cons(key, cons(val, NULL)));
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

struct cons *
read_plist(const char *line)
{
#define ASSERT_KEYWORD	if (*line != ':') goto err
#define ASSERT_STRING	if (*line != '\"' || *line == ')') goto err

	enum {IN_KEY, IN_NUM, IN_STR} state = IN_KEY;
	int esc = 0;
	int64_t num;
	const char *s;
	struct cons *ret;

	ret = NULL;
	line = skip_blanks(line);
	if (*line != '(')
		return 0;
        line = skip_blanks(line+1);
	ASSERT_KEYWORD;
	s = line;

	for (s = line + 1; *line; line++) {
		switch (state) {
		case IN_STR:
			if (esc) {
				esc = 0;
				continue;
			}
			if (*line == '\\') {
				esc = 1;
				continue;
			}
			if (*line == '"') {
				ret = append(ret, cons(make_strkey(s, line-s, ASTR), NULL));
				line = skip_blanks(line+1);
				if (*line == ')')
					return ret; /* done */
				ASSERT_KEYWORD;
				s = line+1; /* skip : */
				state = IN_KEY;
			}
			break;

		case IN_NUM:
			if (!isdigit(*line)) {
				ret = append(ret, cons(make_integer(num), NULL));
				line = skip_blanks(line);

				if (*line == ')')
					return ret; /* done */

				ASSERT_KEYWORD;
				s = ++line;
				state = IN_KEY;
				continue;
			}
			num = num * 10 + *line - '0';
			break;

		case IN_KEY:
			if (isspace(*line)) {
				ret = append(ret, cons(make_strkey(s, line-s, AKEY), NULL));
				line = skip_blanks(line+1);
				if (isdigit(*line)) {
					state = IN_NUM;
					num = *line - '0';
				} else {
					ASSERT_STRING;
					s = line+1;
					state = IN_STR;
				}
			}
			break;
		}
	}

	return NULL;
err:
	list_free(ret);
	return NULL;

#undef ASSERT_STRING
#undef ASSERT_KEYWORD
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



void
handle_friend_request(Tox *tox, const uint8_t *pk, const uint8_t *msg,
    size_t len, void *udata)
{
	struct cons *pl;
	struct atom *message;
	char key[2*TOX_PUBLIC_KEY_SIZE + 1];

	bzero(key, sizeof(key));
	bin2hex(pk, TOX_PUBLIC_KEY_SIZE, key);

	if (toxe_is_valid_utf8((const char*)msg, len))
		message = make_strkey(msg, len, ASTR);
	else
		message = MAKE_STRING("<invalid utf8 message>");

	pl = plist(NULL,
	    MAKE_KEYWORD("@type"),	MAKE_STRING("friend-request"),
	    MAKE_KEYWORD("public-key"),	MAKE_STRING(pk),
	    MAKE_KEYWORD("message"),	message,
	    NULL);

	pp(pl);
	list_free(pl);
}

void
handle_friend_message(Tox *tox, uint32_t fnum, TOX_MESSAGE_TYPE t,
    const uint8_t *msg, size_t len, void *udata)
{
	struct cons *pl;
	struct atom *message;

	if (toxe_is_valid_utf8((const char*)msg, len))
		message = make_strkey(msg, len, ASTR);
	else
		message = MAKE_STRING("<invalid utf8 message>");

	switch (t) {
	case TOX_MESSAGE_TYPE_NORMAL:
		message = MAKE_STRING("normal");
		break;
	case TOX_MESSAGE_TYPE_ACTION:
		message = MAKE_STRING("action");
		break;
	default:
		message = MAKE_STRING("unknown");
		break;
	}

	pl = plist(NULL,
	    MAKE_KEYWORD("@type"),		MAKE_STRING("friend-message"),
	    MAKE_KEYWORD("friend-number"),	MAKE_INTEGER(fnum),
	    MAKE_KEYWORD("message"),		message,
	    NULL);

	pp(pl);
	list_free(pl);
}

void
handle_conn_status(Tox *tox, TOX_CONNECTION status, void *udata)
{
	struct cons *pl;
	struct atom *st;

	switch (status) {
	case TOX_CONNECTION_NONE:
		st = MAKE_STRING("offline");
		break;
	case TOX_CONNECTION_TCP:
		st = MAKE_STRING("tcp");
		break;
	case TOX_CONNECTION_UDP:
		st = MAKE_STRING("udp");
		break;
	default:
		st = MAKE_STRING("unknown");
		break;
	}

	pl = plist(NULL,
	    MAKE_KEYWORD("@type"),	MAKE_STRING("connection-status"),
	    MAKE_KEYWORD("status"),	st,
	    NULL);

	pp(pl);
	list_free(pl);
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
	if (i->type != ASTR)
		return 0;

	if (eq(i, &INIT_KEYWORD("normal")))
		*ret = TOX_MESSAGE_TYPE_NORMAL;
	else if (eq(i, &INIT_KEYWORD("action")))
		*ret = TOX_MESSAGE_TYPE_ACTION;
	else
		return 0;
	return 1;
}

int
extract_pk(struct cons *plist, uint8_t *pk)
{
	struct atom *i;

	if ((i = plist_get(plist, &INIT_KEYWORD("private-key"))) == NULL)
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
	char *name, *errstr;
	struct cons *pl;

	if (!extract_name(opts, &name)) {
		errstr = "name: wrong type or missing";
		goto err;
	}

	pl = plist(NULL,
	    MAKE_KEYWORD("@status"),	MAKE_STRING("t"),
	    NULL);
	pp(pl);
	list_free(pl);
	return 1;

err:
	pl = plist(NULL,
	    MAKE_KEYWORD("@status"),	NULL,
	    MAKE_KEYWORD("@err"),	MAKE_STRING(errstr),
	    NULL);
	pp(pl);
	list_free(pl);
	return 1;
}

int
hself_set_status_msg(Tox *tox, struct cons *opts)
{
	char *msg, *errstr;
	struct cons *pl;

	if (!extract_message(opts, &msg)) {
		errstr = "message: wrong type or missing";
		goto err;
	}

	tox_self_set_status_message(tox, msg, strlen(msg), NULL);
	pl = plist(NULL,
	    MAKE_KEYWORD("@status"),	MAKE_STRING("t"),
	    NULL);
	pp(pl);
	list_free(pl);
	return 1;

err:
	pl = plist(NULL,
	    MAKE_KEYWORD("@status"),	NULL,
	    NULL);
	pp(pl);
	list_free(pl);
	return 1;
}

int
hself_get_addr(Tox *tox, struct cons *opts)
{
	uint8_t self[TOX_ADDRESS_SIZE];
	char hex[2*TOX_ADDRESS_SIZE+1];
	struct cons *pl;

	tox_self_get_address(tox, self);
	bin2hex(self, sizeof(self), hex);

	pl = plist(NULL,
	    MAKE_KEYWORD("address"), MAKE_STRING(hex),
	    NULL);
	pp(pl);
	list_free(pl);
	return 1;
}

int
hfriend_add(Tox *tox, struct cons *opts)
{
	char *msg, *errstr;
	uint8_t pk[TOX_PUBLIC_KEY_SIZE];
	TOX_ERR_FRIEND_ADD err;
	struct cons *pl;

	if (!extract_pk(opts, pk)) {
		errstr = "public-key: missing or wrong type";
		goto err;
	}

	if (!extract_message(opts, &msg))
		msg = NULL;

	if (msg == NULL)
		tox_friend_add_norequest(tox, pk, &err);
	else
		tox_friend_add(tox, pk, msg, strlen(msg), &err);

	if (err != TOX_ERR_FRIEND_ADD_OK) {
		errstr = "failure to send request";
		goto err;
	}

	pl = plist(NULL,
	    MAKE_KEYWORD("@status"),	MAKE_STRING("t"),
	    NULL);
	pp(pl);
	list_free(pl);
	return 1;

err:
	pl = plist(NULL,
	    MAKE_KEYWORD("@status"),	NULL,
	    MAKE_KEYWORD("@err"),	MAKE_STRING(errstr),
	    NULL);
	pp(pl);
	list_free(pl);
	return 1;
}

int
hfriend_send_msg(Tox *tox, struct cons *opts)
{
	uint32_t fnum;
	char *msg, *errstr;
	TOX_MESSAGE_TYPE t;
	TOX_ERR_FRIEND_SEND_MESSAGE err;
	struct cons *pl;

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

	tox_friend_send_message(tox, fnum, t, msg, strlen(msg), &err);
	if (err != TOX_ERR_FRIEND_SEND_MESSAGE_OK) {
		errstr = "failure to send the message";
		goto err;
	}

	pl = plist(NULL,
	    MAKE_KEYWORD("@status"),	MAKE_STRING("t"),
	    NULL);
	free(pl);
	return 1;

err:
	pl = plist(NULL,
	    MAKE_KEYWORD("@status"),	NULL,
	    MAKE_KEYWORD("@err"),	MAKE_STRING(errstr),
	    NULL);
	pp(pl);
	free(pl);
	return 1;
}

int
hquit(Tox *tox, struct cons *opts)
{
	struct cons *pl;

	pl = plist(NULL,
	    MAKE_KEYWORD("@status"), MAKE_STRING("t"),
	    NULL);
	pp(pl);
	list_free(pl);
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
	size_t fsize, r;

	tox_options_default(&opts);
	opts.ipv6_enabled = 0;

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
	struct cons *pl;

	struct {
		struct atom type;
		int (*fn)(Tox*, struct cons *cmd);
	} handlers[] = {
		{INIT_STRING("self-set-name"),			hself_set_name},
		{INIT_STRING("self-set-status-message"),	hself_set_status_msg},
		{INIT_STRING("self-get-address"),		hself_get_addr},
		{INIT_STRING("friend-add"),			hfriend_add},
		{INIT_STRING("friend-send-message"),		hfriend_send_msg},
		{INIT_STRING("quit"),				hquit},
		{INIT_STRING(""),				NULL},
	}, *h;

	if ((at_type = plist_get(cmd, &INIT_KEYWORD("@type"))) == NULL) {
		warnx("missing :@type");
		return 0;
	}

	for (h = handlers; h->fn != NULL; h++) {
		if (eq(at_type, &h->type))
			return h->fn(tox, cmd);
	}

	pl = plist(NULL,
	    MAKE_KEYWORD("@status"),	NULL,
	    MAKE_KEYWORD("@err"),	MAKE_STRING("unknown command"),
	    NULL);
	pp(pl);
	list_free(pl);

	return 1;
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

	cmd = read_plist(line);
	free(line);

	if (cmd == NULL) {
		warnx("read_plist");
		return 0;
	}

	r = process_plist(tox, cmd);
	list_free(cmd);
	return r;
}

__dead void
usage(const char *me, int s)
{
	fprintf(stderr, "USAGE: %s [-s savepath]\n", me);
	exit(s);
}

int
main(int argc, char **argv)
{
	int ch;
	Tox *tox;

	savepath = NULL;
	savepath_tmp = NULL;

	while ((ch = getopt(argc, argv, "s:")) != -1) {
		switch (ch) {
		case 's':
			if ((savepath = strdup(optarg)) == NULL)
				err(1, "strdup");
			if (asprintf(&savepath_tmp, "%s.tmp", savepath) == -1)
				err(1, "asprintf");
			break;

		default:
			usage(*argv, 1);
		}
	}

	if ((tox = init_tox()) == NULL)
		errx(1, "can't initialize tox");

	tox_callback_friend_request(tox, handle_friend_request);
	tox_callback_friend_message(tox, handle_friend_message);
	tox_callback_self_connection_status(tox, handle_conn_status);

	if (!save_tox(tox))
		errx(1, "cannot save tox");

	while (1) {
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
