#include <emacs-module.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <tox/tox.h>

/* go figure... */
int	plugin_is_GPL_compatible;

#define UNUSED(d) ((void)(d))

#define T(env)		((env)->intern((env), "t"))
#define NIL(env)	((env)->intern((env), "nil"))

/* should be present at the top of elisp-callable functions that
 * needs to access to the tox instance. */
#define GUARD_EMACS_FN(env)				\
	if (tox == NULL)				\
		return NIL(env);			\

#define RAISED_ERROR(env)						\
	((env)->non_local_exit_check(env) == emacs_funcall_exit_return)

static Tox *tox = NULL;

int		toxe_is_valid_utf8(const char*, size_t);


typedef emacs_value (*emacs_fn_t)(emacs_env*, ptrdiff_t, emacs_value*, void*);


/* helpers from c <-> elisp */

/* assume out is 2*len (plus a NUL byte) */
static void
toxe_bin2hex(const uint8_t *b, size_t len, char *out)
{
	size_t i;

	for (i = 0; i < len; i++, out += 2)
		sprintf(out, "%02X", b[i]);
}

/* assume b is large len/2 */
static void
toxe_hex2bin(const char *h, size_t len, uint8_t *b)
{
	size_t i;

	for (i = 0; i < len; ++i, h += 2)
		sscanf(h, "%2hhx", &b[i]);
}

/* make an elisp string out of what's in str.  str is long len bytes,
 * and it's not NUL terminated. */
static int
toxe_make_string(emacs_env *env, const char *str, size_t len,
    emacs_value *ret)
{
	char *dup;

	if (len > PTRDIFF_MAX) {
		env->non_local_exit_signal(env, env->intern(env, "overflow-error"),
		    NIL(env));
		return 0;
	}

	if (!toxe_is_valid_utf8(str, len)) {
		env->non_local_exit_signal(env, env->intern(env, "toxe-malformed-string"),
		    NIL(env));
		return 0;
	}

	if ((dup = calloc(1, len+1)) == NULL) {
		env->non_local_exit_signal(env, env->intern(env, "toxe-malformed-string"),
		    NIL(env));
		return 0;
	}
	memcpy(dup, str, len);

	/* even if dup is one byte longer than str, we only added a
	 * NUL byte at the end.  Emacs API requires that
	 * string[length] == '\0', so we don't need to add one to len
	 * here. */
	*ret = env->make_string(env, dup, len);

	free(dup);
	return env->non_local_exit_check(env) == emacs_funcall_exit_return;
}

static int
toxe_message_type_to_symbol(emacs_env *env, TOX_MESSAGE_TYPE t, emacs_value *ret)
{
	switch (t) {
	case TOX_MESSAGE_TYPE_NORMAL:
		*ret = env->intern(env, "toxe-message-type-normal");
		break;
	case TOX_MESSAGE_TYPE_ACTION:
		*ret = env->intern(env, "toxe-message-type-action");
		break;
	default:
		*ret = env->intern(env, "toxe-message-type-unknown");
		break;
	}

	return env->non_local_exit_check(env) == emacs_funcall_exit_return;
}

/* parses the string v into the public key pk.  raise an error if the
 * pk is not valid.  Pk is pre-allocated. */
static int
toxe_extract_pk(emacs_env *env, emacs_value v, uint8_t *pk)
{
	char hex[2*TOX_PUBLIC_KEY_SIZE + 1];
	ptrdiff_t len;

	bzero(hex, sizeof(hex));

	env->copy_string_contents(env, v, NULL, &len);
	if (RAISED_ERROR(env))
		return 0;
	if (len != sizeof(hex)) {
		env->non_local_exit_signal(env, env->intern(env, "wrong-type-argument"),
		    NIL(env));
		return 0;
	}
	if (!env->copy_string_contents(env, v, hex, &len))
		return 0;

	toxe_hex2bin(hex, len, pk);
	return 1;
}

static int
toxe_extract_message_type(emacs_env *env, emacs_value m, TOX_MESSAGE_TYPE *ret)
{
	if (env->eq(env, m, env->intern(env, "toxe-message-type-normal"))) {
		*ret = TOX_MESSAGE_TYPE_NORMAL;
		return 1;
	}
	if (env->eq(env, m, env->intern(env, "toxe-message-type-action"))) {
		*ret = TOX_MESSAGE_TYPE_ACTION;
		return 1;
	}
	env->non_local_exit_signal(env, env->intern(env, "wrong-type-argument"), m);
	return 0;
}


/* helpers for handling toxcore */

/* call the hook toxe-friend-request-hook */
static void
toxe_handle_friend_request(Tox *tox, const uint8_t *public_key, const uint8_t *message,
    size_t len, void *udata)
{
	char key[2*TOX_PUBLIC_KEY_SIZE + 1];
	emacs_value pk, msg, run_hook_with_args, args[3];
	emacs_env *env = udata;

	bzero(key, sizeof(key));
	toxe_bin2hex(public_key, TOX_PUBLIC_KEY_SIZE, key);
	pk = env->make_string(env, key, sizeof(key)-1);
	if (env->non_local_exit_check(env) != emacs_funcall_exit_return)
		return;

	if (!toxe_make_string(env, message, len, &msg))
		return;

	run_hook_with_args = env->intern(env, "run-hook-with-args");
	args[0] = env->intern(env, "toxe-friend-request-hook");
	args[1] = pk;
	args[2] = msg;
	env->funcall(env, run_hook_with_args, 3, args);
}

/* call the hook toxe-friend-message-hook */
static void
toxe_handle_friend_message(Tox *tox, uint32_t friend_number, TOX_MESSAGE_TYPE type,
    const uint8_t *message, size_t len, void *udata)
{
	emacs_value fn, msg_type, msg, run_hook_with_args, args[4];
	emacs_env *env = udata;

	fn = env->make_integer(env, friend_number);
	if (env->non_local_exit_check(env) != emacs_funcall_exit_return)
		return;

	if (!toxe_message_type_to_symbol(env, type, &msg_type))
		return;

	if (!toxe_make_string(env, message, len, &msg))
		return;

	run_hook_with_args = env->intern(env, "run-hook-with-args");
	args[0] = env->intern(env, "toxe-friend-message-hook");
	args[1] = fn;
	args[2] = msg_type;
	args[3] = msg;
	env->funcall(env, run_hook_with_args, 4, args);
}


/* binding to elisp */

#define TOXE_START				\
	"(toxe-start)\n"			\
	"\n"					\
	"Starts the client."
static emacs_value
toxe_start(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
	TOX_ERR_NEW err_new;

	UNUSED(n); UNUSED(args); UNUSED(ptr);

	tox = tox_new(NULL, &err_new);
	if (err_new != TOX_ERR_NEW_OK) {
		tox = NULL;	 /* just in case */
		return NIL(env); /* TODO: singal an error */
	}

	tox_callback_friend_request(tox, toxe_handle_friend_request);
	tox_callback_friend_message(tox, toxe_handle_friend_message);

	return T(env);
}

#define TOXE_STOP				\
	"(toxe-stop)\n"				\
	"\n"					\
	"Shuts down toxe."
static emacs_value
toxe_stop(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
	UNUSED(n); UNUSED(args); UNUSED(ptr);

	if (tox != NULL) {
		tox_kill(tox);
		tox = NULL;
	}
	return NIL(env);
}

#define TOXE_SELF_SET_NAME			\
	"(toxe-self-set-name NAME)"		\
	"\n"					\
	"Set your name to NAME."
static emacs_value
toxe_self_set_name(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
	char *name;
	size_t len;

	UNUSED(n); UNUSED(ptr);
	GUARD_EMACS_FN(env);

	env->copy_string_contents(env, args[0], NULL, &len);
	if (RAISED_ERROR(env))
		return NIL(env);
	if ((name = calloc(1, len)) == NULL)
		return NIL(env);
	if (!env->copy_string_contents(env, args[0], name, &len)) {
		free(name);
		return NIL(env);
	}

	tox_self_set_name(tox, (const uint8_t*)name, len, NULL);
	free(name);

	return args[0];
}

#define TOXE_ITERATION_INTERVAL					\
	"(toxe-iteration-interval)"				\
	"\n"							\
	"The number of seconds as floating point to wait "	\
	"between calls to `toxe-iterate'."
static emacs_value
toxe_iteration_interval(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
	int mils;
	double seconds;

	UNUSED(n); UNUSED(args); UNUSED(ptr);
	GUARD_EMACS_FN(env);

	mils = tox_iteration_interval(tox);
	seconds = mils * 1000.0;
	return env->make_float(env, seconds);
}

#define TOXE_ITERATE					\
	"(toxe-iterate)"				\
	"\n"						\
	"Receives and process the network messages."
static emacs_value
toxe_iterate(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
	UNUSED(n); UNUSED(args); UNUSED(ptr);
	GUARD_EMACS_FN(env);
	tox_iterate(tox, env);
	return NIL(env);
}

#define TOXE_SELF_GET_ADDRESS			\
	"(toxe-self-get-address)"		\
	"\n"					\
	"Return our address."
static emacs_value
toxe_self_get_address(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
	uint8_t self[TOX_ADDRESS_SIZE];
	char hex[2*TOX_ADDRESS_SIZE+1];

	UNUSED(n); UNUSED(args); UNUSED(ptr);
	GUARD_EMACS_FN(env);

	tox_self_get_address(tox, self);
	toxe_bin2hex(self, sizeof(self), hex);
	return env->make_string(env, hex, sizeof(hex)-1);
}

#define TOXE_FRIEND_ADD					\
	"(toxe-friend-add PK MSG)"			\
	"\n"						\
	"Send a friend request to PK with message MSG."
static emacs_value
toxe_friend_add(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
	emacs_value ret;
	uint8_t pk[TOX_PUBLIC_KEY_SIZE];
	char *msg;
	ptrdiff_t len;
	TOX_ERR_FRIEND_ADD err;

	UNUSED(n); UNUSED(ptr);
	GUARD_EMACS_FN(env);

        if (!toxe_extract_pk(env, args[0], pk))
		return NIL(env);

	env->copy_string_contents(env, args[1], NULL, &len);
	if ((msg = calloc(1, len)) == NULL) {
		env->non_local_exit_signal(env, env->intern(env, "error"), NIL(env));
		return NIL(env);
	}

	if (!env->copy_string_contents(env, args[1], msg, &len)) {
		free(msg);
		env->non_local_exit_signal(env, env->intern(env, "error"), NIL(env));
		return NIL(env);
	}

	tox_friend_add(tox, pk, msg, len, &err);
	free(msg);

	switch (err) {
	case TOX_ERR_FRIEND_ADD_OK:
		ret = T(env);
		break;
	/* TODO translate possible error codes for elisp? */
	default:
		ret = NIL(env);
		break;
	}

	return ret;
}

#define TOXE_FRIEND_ADD_NOREQUEST			\
	"(toxe-friend-add PK)"				\
	"\n"						\
	"Reply to a friend request sent by PK."
static emacs_value
toxe_friend_add_norequest(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
	emacs_value ret;
	uint8_t pk[TOX_PUBLIC_KEY_SIZE];
	TOX_ERR_FRIEND_ADD err;

	UNUSED(n); UNUSED(ptr);
	GUARD_EMACS_FN(env);

        if (!toxe_extract_pk(env, args[0], pk))
		return NIL(env);

	tox_friend_add_norequest(tox, pk, &err);
	switch (err) {
	case TOX_ERR_FRIEND_ADD_OK:
		ret = T(env);
		break;
	/* TODO translate possible error codes for elisp? */
	default:
		ret = NIL(env);
	}

	return ret;
}

#define TOXE_FRIEND_SEND_MESSAGE					\
	"(toxe-friend-send-message FRIEND-NUMBER TYPE MSG)"		\
	"\n"								\
	"Send a message of type TYPE and content MSG to FRIEND-NUMBER,"	\
	" return the message id."
static emacs_value
toxe_friend_send_message(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
	uint32_t fnum, msgid;
	TOX_MESSAGE_TYPE t;
	char *message;
	size_t len;
	TOX_ERR_FRIEND_SEND_MESSAGE err;

	UNUSED(n); UNUSED(ptr);
	GUARD_EMACS_FN(env);

	fnum = env->extract_integer(env, args[0]);
	if (RAISED_ERROR(env))
		return NIL(env);

	if (!toxe_extract_message_type(env, args[1], &t))
		return NIL(env);

	env->copy_string_contents(env, args[2], NULL, &len);
	if (RAISED_ERROR(env))	/* wrong type */
		return NIL(env);
	if ((message = calloc(1, len)) == NULL)
		return NIL(env);
	if (!env->copy_string_contents(env, args[2], message, &len)) {
		free(message);
		return NIL(env);
	}

	msgid = tox_friend_send_message(tox, fnum, t, message, len, &err);
	free(message);

	switch (err) {
	case TOX_ERR_FRIEND_SEND_MESSAGE_OK:
		return env->make_integer(env, msgid);
	default:
		/* TODO: specific return value/signals for the
		 * various errors? */
		return NIL(env);
	}
}


/* initialisation */

static int
defun(emacs_env *env, const char *sym, ptrdiff_t min_arity, ptrdiff_t max_arity,
    emacs_fn_t func, const char *docs, void *data)
{
	emacs_value args[2];

	args[0] = env->intern(env, sym);
	args[1] = env->make_function(env, min_arity, max_arity, func, docs, data);
	env->funcall(env, env->intern(env, "defalias"), 2, args);
	return env->non_local_exit_check(env) == emacs_funcall_exit_return;
}

int
emacs_module_init(struct emacs_runtime *ert)
{
	emacs_env *env;
	emacs_value provide, toxe_core;

	/* dynamic size is smaller than static size: loading a module
	 * on a old emacs version? */
	if (ert->size < sizeof(*ert))
		return 1;

	env = ert->get_environment(ert);

	/* dynamic size is smaller than static size: loading a module
	 * on a old emacs version? */
	if (env->size < sizeof(*env))
		return 1;

	/* define the functions */
	defun(env, "toxe-start", 0, 0, toxe_start, TOXE_START, 0);
	defun(env, "toxe-stop", 0, 0, toxe_stop, TOXE_STOP, 0);
	defun(env, "toxe-self-set-name", 1, 1, toxe_self_set_name,
	    TOXE_SELF_SET_NAME, 0);
	defun(env, "toxe-iteration-interval", 0, 0, toxe_iteration_interval,
	    TOXE_ITERATION_INTERVAL, 0);
	defun(env, "toxe-iterate", 0, 0, toxe_iterate, TOXE_ITERATE, 0);
	defun(env, "toxe-self-get-address", 0, 0, toxe_self_get_address,
	    TOXE_SELF_GET_ADDRESS, 0);
	defun(env, "toxe-friend-add", 0, 2, toxe_friend_add, TOXE_FRIEND_ADD, 0);
	defun(env, "toxe-friend-add-norequest", 0, 2, toxe_friend_add_norequest,
	    TOXE_FRIEND_ADD_NOREQUEST, 0);
	defun(env, "toxe-friend-send-message", 0, 3, toxe_friend_send_message,
	    TOXE_FRIEND_SEND_MESSAGE, 0);

	/* (provide 'toxe-core) */
	provide = env->intern(env, "provide");
	toxe_core = env->intern(env, "toxe-core");
	env->funcall(env, provide, 1, &toxe_core);

	return 0;
}
