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

#ifndef TOXE_H
#define TOXE_H

enum test_mode { TEST_NONE, TEST_PARSE };

struct atom {
#define AKEY 0
#define ASYM 1
#define ASTR 2
#define AINT 3
	int type;
	char *str;
	int64_t integer;
};

#define INIT_KEYWORD(s)	((struct atom){AKEY, (s), 0})
#define INIT_SYMBOL(s)	((struct atom){ASYM, (s), 0})
#define INIT_STRING(s)	((struct atom){ASTR, (s), 0})
#define INIT_INTEGER(i)	((struct atom){AINT, NULL, i})

#define MAKE_KEYWORD(s)	(make_strkey((s), strlen(s), AKEY))
#define MAKE_SYMBOL(s)	(make_strkey((s), strlen(s), ASYM))
#define MAKE_STRING(s)	(make_strkey((s), strlen(s), ASTR))
#define MAKE_INTEGER(i)	(make_integer(i))

struct cons {
	struct atom *car;
	struct cons *cdr;
};

#define CADR(x)	(car(cdr(x)))
#define CDDR(x) (cdr(cdr(x)))

/* Pretty Print Plist */
#define PPP(...)					\
	do {						\
		struct cons *pl;			\
		pl = plist(NULL, __VA_ARGS__, NULL);	\
		pp(pl);					\
		list_free(pl);				\
	} while (0)

void			 sigint_handler(int);

struct atom		*make_string_check(const char*, size_t);
struct atom		*amke_string(const char*, size_t);
struct atom		*make_strkey(const char*, size_t, int);
struct atom		*make_integer(int64_t);
struct cons		*cons(struct atom*, struct cons*);
void			 list_free(struct cons*);
struct cons		*append(struct cons*, struct cons*);
struct cons		*push_back(struct cons*, struct atom*);
struct atom		*car(struct cons*);
struct cons		*cdr(struct cons*);
int			 eq(const struct atom*restrict, const struct atom*restrict);
struct atom		*plist_get(struct cons*restrict, const struct atom*restrict);
struct cons		*plist(struct cons*, ...);
void			 pp_atom(struct atom*);
void			 pp(struct cons*);

const char		*skip_blanks(const char*);
const char		*read_atom(const char*, struct cons**);
struct cons 		*read_list(const char*);

void			 bin2hex(const uint8_t*, size_t, char*);
void			 hex2bin(const char*, size_t, uint8_t*);

struct atom		*convert_user_status(TOX_USER_STATUS);
struct atom		*convert_connection(TOX_CONNECTION);
struct atom		*convert_message_type(TOX_MESSAGE_TYPE);

void			 handle_friend_request(Tox*, const uint8_t*, const uint8_t*, size_t, void*);
void			 handle_friend_message(Tox*, uint32_t, TOX_MESSAGE_TYPE, const uint8_t*, size_t, void*);
void			 handle_conn_status(Tox*, TOX_CONNECTION, void*);
void			 handle_friend_name(Tox*, uint32_t, const uint8_t*, size_t, void*);
void			 handle_friend_status_message(Tox*, uint32_t, const uint8_t*, size_t, void*);
void			 handle_friend_status(Tox*, uint32_t, TOX_USER_STATUS, void*);
void			 handle_friend_connection_status(Tox*, uint32_t, TOX_CONNECTION, void*);
void			 handle_friend_read_receipt(Tox*, uint32_t, uint32_t, void*);

int			 extract_friend_number(struct cons*, uint32_t*);
int			 extract_message(struct cons*, char**);
int			 extract_message_type(struct cons*, TOX_MESSAGE_TYPE*);
int			 extract_pk(struct cons*, uint8_t*);
int			 extract_name(struct cons*, char**);

int			 hself_set_name(Tox*, struct cons*);
int			 hself_set_status_msg(Tox*, struct cons*);
int			 hself_get_addr(Tox*, struct cons*);
int			 hfriend_add(Tox*, struct cons*);
int			 hfriend_send_msg(Tox*, struct cons*);
int			 hget_chatlist(Tox*, struct cons*);
int			 hquit(Tox*, struct cons*);

Tox			*init_tox(void);
int			 save_tox(Tox*);
int			 stdin_ready(void);
int			 process_plist(Tox*, struct cons*);
int			 process_stdin(Tox*);
void			 add_bootstrap(const char*);
int			 parse_testmode();
void			 usage(const char*, int)
	__attribute__((__noreturn__));

/* utf8.c */

int			 toxe_is_valid_utf8(const char*, size_t);

#endif
