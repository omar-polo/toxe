#include <emacs-module.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <tox/tox.h>
#include <unistd.h>

/* go figure... */
int	plugin_is_GPL_compatible;

#define UNUSED(d) ((void)(d))

#define INTERN(env, s)	((env)->intern((env), (s)))

#define T(env)		((env)->intern((env), "t"))
#define NIL(env)	((env)->intern((env), "nil"))

/* should be present at the top of elisp-callable functions that
 * needs to access to the tox instance. */
#define GUARD_EMACS_FN(env)				\
	if (tox == NULL)				\
		return NIL(env);			\

#define RAISED_ERROR(env)						\
	((env)->non_local_exit_check(env) != emacs_funcall_exit_return)

static Tox *tox = NULL;

int		toxe_is_valid_utf8(const char*, size_t);


typedef emacs_value (*emacs_fn_t)(emacs_env*, ptrdiff_t, emacs_value*, void*);


/* helpers */

/* return NULL on allocation failure of if the file doesn't exists,
 * otherwise return the content of the file.  If failed, check for a
 * non-local exit. */
static char *
toxe_slurp_file(emacs_env *env, const char *path, size_t *ret_len)
{
	char *data;
	size_t fsize;
	FILE *f;

	if ((f = fopen(path, "r")) == NULL)
		return NULL;

	fseek(f, 0, SEEK_END);
	fsize = ftell(f);
	fseek(f, 0, SEEK_SET);

	if ((data = malloc(fsize)) == NULL) {
		fclose(f);

		/* TODO: better signal? */
		env->non_local_exit_signal(env, env->intern(env, "overflow-error"),
		    NIL(env));
		return NULL;
	}

	fread(data, fsize, 1, f);
	fclose(f);

	*ret_len = fsize;
	return data;
}

/* print the given string though emacs message function.  one-effort
 * function. */
static void
debug_message(emacs_env *env, char *s)
{
	emacs_value message, str;

	if (s == NULL)
		return;

	message = env->intern(env, "message");
	str = env->make_string(env, s, strlen(s));
	env->funcall(env, message, 1, &str);
}


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

/* convert M to a string.  If return false, a signal was raised,
 * don't use RET_S or RET_LEN in those cases.  Furthermore, RET_LEN
 * can be NULL, but RET_S can't. */
static int
toxe_extract_string(emacs_env *env, emacs_value m, char **ret_s, size_t *ret_len)
{
	size_t len;

	if (ret_len != NULL)
		*ret_len = 0;

	env->copy_string_contents(env, m, NULL, &len);
	if (RAISED_ERROR(env))
		return 0;

	if ((*ret_s = calloc(1, len)) == NULL) {
		/* TODO: better error? */
		env->non_local_exit_signal(env, env->intern(env, "overflow-error"),
		    NIL(env));
		return 0;
	}
	if (!env->copy_string_contents(env, m, *ret_s, &len)) {
		free(ret_s);
		*ret_s = NULL;
		return 0;
	}

	if (ret_len != NULL)
		*ret_len = len;
	return 1;
}

static emacs_value
toxe_err_new_to_symbol(emacs_env *env, TOX_ERR_NEW t)
{
	switch (t) {
	case TOX_ERR_NEW_OK:
		return T(env);
	case TOX_ERR_NEW_NULL:
		return INTERN(env, "toxe-err-new-null");
	case TOX_ERR_NEW_MALLOC:
		return INTERN(env, "toxe-err-new-malloc");
	case TOX_ERR_NEW_PORT_ALLOC:
		return INTERN(env, "toxe-err-new-port-alloc");
	case TOX_ERR_NEW_PROXY_BAD_TYPE:
		return INTERN(env, "toxe-err-new-proxy-bad-type");
	case TOX_ERR_NEW_PROXY_BAD_HOST:
		return INTERN(env, "toxe-err-new-proxy-bad-host");
	case TOX_ERR_NEW_PROXY_BAD_PORT:
		return INTERN(env, "toxe-err-new-proxy-bad-port");
	case TOX_ERR_NEW_PROXY_NOT_FOUND:
		return INTERN(env, "toxe-err-new-proxy-not-found");
	case TOX_ERR_NEW_LOAD_ENCRYPTED:
		return INTERN(env, "toxe-err-new-load-encrypted");
	case TOX_ERR_NEW_LOAD_BAD_FORMAT:
		return INTERN(env, "toxe-err-new-load-bad-format");
	default:
		return INTERN(env, "toxe-err-new-unknown");
	}
}

static emacs_value
toxe_bootstrap_err_to_symbol(emacs_env *env, TOX_ERR_BOOTSTRAP t)
{
	switch (t) {
	case TOX_ERR_BOOTSTRAP_OK:
		return T(env);
	case TOX_ERR_BOOTSTRAP_NULL:
		return INTERN(env, "toxe-err-bootstrap-null");
	case TOX_ERR_BOOTSTRAP_BAD_HOST:
		return INTERN(env, "toxe-err-bootstrap-bad-host");
	case TOX_ERR_BOOTSTRAP_BAD_PORT:
		return INTERN(env, "toxe-err-bootstrap-bad-port");
	default:
		return INTERN(env, "toxe-err-bootstrap-unknown");
	}
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

	debug_message(env, "toxe-core: called toxe-friend-request-hook");
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

	debug_message(env, "toxe-core: called toxe-friend-message-hook");
}

/* call the hook toxe-self-connection-status-hook */
static void
toxe_handle_self_connection_status(Tox *tox, TOX_CONNECTION status, void *udata)
{
	emacs_value run_hook_with_args, args[2];
	emacs_env *env = udata;

	UNUSED(tox);

	args[0] = env->intern(env, "toxe-self-connection-status-hook");
	if (RAISED_ERROR(env))
		return;

	switch (status) {
	case TOX_CONNECTION_NONE:
		args[1] = env->intern(env, "toxe-connection-none");
		break;
	case TOX_CONNECTION_TCP:
		args[1] = env->intern(env, "toxe-connection-tcp");
		break;
	case TOX_CONNECTION_UDP:
		args[1] = env->intern(env, "toxe-connection-udp");
		break;
	default:
		args[1] = env->intern(env, "toxe-connection-unknown");
		break;
	}

	if (RAISED_ERROR(env))
		return;

	run_hook_with_args = env->intern(env, "run-hook-with-args");
	env->funcall(env, run_hook_with_args, 2, args);

	debug_message(env, "toxe-core: called toxe-self-connection-status-hook");
}


/* binding to elisp */

#define TOXE__START				\
	"(toxe--start PATH)\n"			\
	"\n"					\
	"Starts the client storing data in PATH."
static emacs_value
toxe__start(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
	TOX_ERR_NEW err_new;
	struct Tox_Options opts;
	char *path;
	uint8_t *data;
	size_t len;

	UNUSED(n); UNUSED(ptr);

	if (tox != NULL) {
		env->non_local_exit_signal(env, INTERN(env, "error"), NIL(env));
		return NIL(env);
	}

	tox_options_default(&opts);

	tox = NULL;
	if (env->is_not_nil(env, args[0])) {
		if (!toxe_extract_string(env, args[0], &path, NULL)) {
			debug_message(env, "toxe: failed to extract the string");
			return NIL(env);
		}

		if ((data = toxe_slurp_file(env, path, &len)) == NULL) {
			free(path);

			if (RAISED_ERROR(env)) {
				return NIL(env);
			}
			debug_message(env, "toxe: the file doesn't exists");
		} else {
			opts.savedata_type = TOX_SAVEDATA_TYPE_TOX_SAVE;
			opts.savedata_data = data;
			opts.savedata_length = len;

			debug_message(env, "toxe: calling tox with options");
			tox = tox_new(&opts, &err_new);

			free(data);
			free(path);
		}
	}

	if (tox == NULL)
		tox = tox_new(NULL, &err_new);

	if (err_new != TOX_ERR_NEW_OK) {
		tox = NULL;
		env->non_local_exit_signal(env, INTERN(env, "toxe-new-error"),
		    toxe_err_new_to_symbol(env, err_new));
		return NIL(env);
	}

	tox_callback_friend_request(tox, toxe_handle_friend_request);
	tox_callback_friend_message(tox, toxe_handle_friend_message);
	tox_callback_self_connection_status(tox, toxe_handle_self_connection_status);

	return T(env);
}

#define TOXE__BOOTSTRAP							\
	"(toxe-bootstrap HOST PORT PUBLIC-KEY)"				\
	"\n"								\
	"Send a ``get-nodes'' request to the given bootstrap node "	\
	"with HOST, PORT and PUBLIC-KEY."
static emacs_value
toxe__bootstrap(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
	char *host, *pk;
	uint16_t port;
	TOX_ERR_BOOTSTRAP err;

	UNUSED(n); UNUSED(ptr);
	GUARD_EMACS_FN(env);

	if (!toxe_extract_string(env, args[0], &host, NULL))
		return NIL(env);

	port = env->extract_integer(env, args[1]);
	if (RAISED_ERROR(env)) {
		free(host);
		return NIL(env);
	}

	if (!toxe_extract_string(env, args[2], &pk, NULL)) {
		free(host);
		return NIL(env);
	}

	tox_bootstrap(tox, host, port, pk, &err);
	free(host);
	free(pk);

	if (err != TOX_ERR_BOOTSTRAP_OK)
		return toxe_bootstrap_err_to_symbol(env, err);
	return T(env);
}

#define TOXE__SAVE							\
	"(toxe-save PATH TMP)"						\
	"\n"								\
	"Save the status to PATH, using TMP as temporary storage."
static emacs_value
toxe__save(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
	char *path, *tmp;
	uint8_t *data;
	FILE *f;
	size_t len, r;

	UNUSED(n); UNUSED(ptr);
	GUARD_EMACS_FN(env);

	if (!toxe_extract_string(env, args[0], &path, NULL))
		return NIL(env);
	if (!toxe_extract_string(env, args[1], &tmp, NULL)) {
		free(path);
		return NIL(env);
	}

	len = tox_get_savedata_size(tox);
	if ((data = malloc(len)) == NULL) {
		free(path);
		free(tmp);
		/* TODO: raise */
		return NIL(env);
	}
	tox_get_savedata(tox, data);

	if ((f = fopen(tmp, "w")) == NULL) {
		free(data);
		free(path);
		free(tmp);
		/* TODO: raise */
		return NIL(env);
	}
	r = fwrite(data, 1, len, f);
	fclose(f);

	if (r == len)
		rename(tmp, path);
	unlink(tmp);

	free(data);
	free(tmp);
	free(path);

	if (r == len)
		return T(env);
	return NIL(env);
}

#define TOXE__STOP				\
	"(toxe--stop)\n"			\
	"\n"					\
	"Shuts down toxe."
static emacs_value
toxe__stop(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
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

#define TOXE_SELF_SET_STATUS_MESSAGE		\
	"(toxe-self-set-status-message msg)"	\
	"\n"					\
	"Set your name status message to msg."
static emacs_value
toxe_self_set_status_message(emacs_env *env, ptrdiff_t n, emacs_value *args,
    void *ptr)
{
	char *msg;
	size_t len;

	UNUSED(n); UNUSED(ptr);
	GUARD_EMACS_FN(env);

	env->copy_string_contents(env, args[0], NULL, &len);
	if (RAISED_ERROR(env))
		return NIL(env);
	if ((msg = calloc(1, len)) == NULL)
		return NIL(env);
	if (!env->copy_string_contents(env, args[0], msg, &len)) {
		free(msg);
		return NIL(env);
	}

	tox_self_set_status_message(tox, (const uint8_t*)msg, len, NULL);
	free(msg);

	return args[0];
}

#define TOXE_ITERATION_INTERVAL					\
	"(toxe-iteration-interval)"				\
	"\n"							\
	"The number of milliseconds to wait between calls to "	\
	"`toxe-iterate'."
static emacs_value
toxe_iteration_interval(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
	UNUSED(n); UNUSED(args); UNUSED(ptr);
	GUARD_EMACS_FN(env);

	return env->make_integer(env, tox_iteration_interval(tox));
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
	defun(env, "toxe--start", 1, 1, toxe__start, TOXE__START, 0);
	defun(env, "toxe--bootstrap", 3, 3, toxe__bootstrap, TOXE__BOOTSTRAP, 0);
	defun(env, "toxe--save", 2, 2, toxe__save, TOXE__SAVE, 0);
	defun(env, "toxe--stop", 0, 0, toxe__stop, TOXE__STOP, 0);
	defun(env, "toxe-self-set-name", 1, 1, toxe_self_set_name,
	    TOXE_SELF_SET_NAME, 0);
	defun(env, "toxe-self-set-status-message", 1, 1,
	    toxe_self_set_status_message, TOXE_SELF_SET_STATUS_MESSAGE, 0);
	defun(env, "toxe-iteration-interval", 0, 0, toxe_iteration_interval,
	    TOXE_ITERATION_INTERVAL, 0);
	defun(env, "toxe-iterate", 0, 0, toxe_iterate, TOXE_ITERATE, 0);
	defun(env, "toxe-self-get-address", 0, 0, toxe_self_get_address,
	    TOXE_SELF_GET_ADDRESS, 0);
	defun(env, "toxe-friend-add", 2, 2, toxe_friend_add, TOXE_FRIEND_ADD, 0);
	defun(env, "toxe-friend-add-norequest", 1, 1, toxe_friend_add_norequest,
	    TOXE_FRIEND_ADD_NOREQUEST, 0);
	defun(env, "toxe-friend-send-message", 3, 3, toxe_friend_send_message,
	    TOXE_FRIEND_SEND_MESSAGE, 0);

	/* (provide 'toxe-core) */
	provide = env->intern(env, "provide");
	toxe_core = env->intern(env, "toxe-core");
	env->funcall(env, provide, 1, &toxe_core);

	return 0;
}
