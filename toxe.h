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

struct atom		*make_strkey(const char*, size_t, int);
struct atom		*make_integer(int64_t);
struct cons		*cons(struct atom*, struct cons*);
void			 list_free(struct cons*);
struct cons		*pcons(struct cons*, struct atom*, struct atom*);
struct cons		*append(struct cons*, struct cons*);
struct atom		*car(struct cons*);
struct cons		*cdr(struct cons*);
int			 eq(const struct atom*restrict, const struct atom*restrict);
struct atom		*plist_get(struct cons*restrict, const struct atom*restrict);
struct cons		*plist(struct cons*, ...);
void			 pp_atom(struct atom*);
void			 pp(struct cons*);

const char		*skip_blanks(const char*);
struct cons		*read_plist(const char*);

void			 bin2hex(const uint8_t*, size_t, char*);
void			 hex2bin(const char*, size_t, uint8_t*);

void			 handle_friend_request(Tox*, const uint8_t*, const uint8_t*, size_t, void*);
void			 handle_friend_message(Tox*, uint32_t, TOX_MESSAGE_TYPE, const uint8_t*, size_t, void*);
void			 handle_conn_status(Tox*, TOX_CONNECTION, void*);

int			 extract_friend_number(struct cons*, uint32_t*);

int			 hself_set_name(Tox*, struct cons*);
int			 hself_set_status_msg(Tox*, struct cons*);
int			 hself_get_addr(Tox*, struct cons*);
int			 hfriend_add(Tox*, struct cons*);
int			 hfriend_send_msg(Tox*, struct cons*);
int			 hquit(Tox*, struct cons*);

Tox			*init_tox(void);
int			 save_tox(Tox*);
int			 stdin_ready(void);
int			 process_plist(Tox*, struct cons*);
int			 process_stdin(Tox*);
__dead void		 usage(const char*, int);

/* utf8.c */

int			 toxe_is_valid_utf8(const char*, size_t);

#endif
