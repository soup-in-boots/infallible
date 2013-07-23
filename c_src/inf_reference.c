#include "erl_nif.h"
#define MAX_GENERATIONS 16

static ErlNifResourceType* inf_reference_RESOURCE = NULL;

typedef struct
{
    ErlNifEnv *env;
    ErlNifRWLock *lock;
    ERL_NIF_TERM term;
    int generations;
} inf_reference_handle;

// Prototypes
static ERL_NIF_TERM inf_reference_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM inf_reference_read_lock(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM inf_reference_read_unlock(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM inf_reference_write_lock(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM inf_reference_write_unlock(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM inf_reference_read(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM inf_reference_write(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] =
{
    {"new", 1, inf_reference_new},
    {"read_lock", 1, inf_reference_read_lock},
    {"read_unlock", 1, inf_reference_read_unlock},
    {"write_lock", 1, inf_reference_write_lock},
    {"write_unlock", 1, inf_reference_write_unlock},
    {"do_read", 1, inf_reference_read},
    {"do_write", 2, inf_reference_write}
};

static ERL_NIF_TERM inf_reference_new(ErlNifEnv* env, int argc,
                                   const ERL_NIF_TERM argv[])
{
    inf_reference_handle* handle = enif_alloc_resource(inf_reference_RESOURCE,
                                                    sizeof(inf_reference_handle));
    handle->env = enif_alloc_env();
    handle->lock = enif_rwlock_create("inf_reference_lock");
    handle->term = enif_make_copy(handle->env, argv[0]);
    ERL_NIF_TERM result = enif_make_resource(env, handle);
    enif_release_resource(handle);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), result);
}


static ERL_NIF_TERM inf_reference_write_lock(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    inf_reference_handle *handle;
    if (!enif_get_resource(env, argv[0], inf_reference_RESOURCE, (void**)(&handle))) {
        return enif_make_badarg(env);
    }
    enif_rwlock_rwlock(handle->lock);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM inf_reference_write_unlock(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    inf_reference_handle *handle;
    if (!enif_get_resource(env, argv[0], inf_reference_RESOURCE, (void**)(&handle))) {
        return enif_make_badarg(env);
    }
    enif_rwlock_rwunlock(handle->lock);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM inf_reference_read_lock(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    inf_reference_handle *handle;
    if (!enif_get_resource(env, argv[0], inf_reference_RESOURCE, (void**)(&handle))) {
        return enif_make_badarg(env);
    }
    enif_rwlock_rlock(handle->lock);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM inf_reference_read_unlock(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    inf_reference_handle *handle;
    if (!enif_get_resource(env, argv[0], inf_reference_RESOURCE, (void**)(&handle))) {
        return enif_make_badarg(env);
    }
    enif_rwlock_runlock(handle->lock);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM inf_reference_read(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    inf_reference_handle *handle;
    ERL_NIF_TERM ret;
    if (!enif_get_resource(env, argv[0], inf_reference_RESOURCE, (void**)(&handle))) {
        ret = enif_make_badarg(env);
        return ret;
    }

    ret = enif_make_copy(env, handle->term);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), ret);
}

static ERL_NIF_TERM inf_reference_write(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    inf_reference_handle *handle;
    if (!enif_get_resource(env, argv[0], inf_reference_RESOURCE, (void**)(&handle))) {
        return enif_make_badarg(env);
    }

    if (++handle->generations >= MAX_GENERATIONS) {
        handle->generations = 0;
        enif_clear_env(handle->env);
    }

    handle->term = enif_make_copy(handle->env, argv[1]);

    return enif_make_atom(env, "ok");
}

static void inf_reference_resource_cleanup(ErlNifEnv* env, void* arg)
{
    inf_reference_handle *handle = (inf_reference_handle*)arg;
    /* Delete any dynamically allocated memory stored in inf_reference_handle */
    /* inf_reference_handle* handle = (inf_reference_handle*)arg; */
    enif_rwlock_destroy(handle->lock);
    enif_free_env(handle->env);
}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    ErlNifResourceFlags flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    ErlNifResourceType* rt = enif_open_resource_type(env, NULL,
                                                     "inf_reference_resource",
                                                     &inf_reference_resource_cleanup,
                                                     flags, NULL);
    if (rt == NULL)
        return -1;

    inf_reference_RESOURCE = rt;

    return 0;
}

ERL_NIF_INIT(inf_reference, nif_funcs, &on_load, NULL, NULL, NULL);
