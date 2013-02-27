#include <erl_nif.h>
#include <systemd/sd-journal.h>
#include <alloca.h>

#define SD_JOURNAL_SUPPRESS_LOCATION

static ERL_NIF_TERM journal_sendv(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
        int arity = 0; 
        struct iovec *iov = NULL; 
        const ERL_NIF_TERM* tuple;
        ErlNifBinary* buffer;  
        
        if (!enif_get_tuple(env, argv[0], &arity, &tuple)) {
            sd_journal_perror("transfer parameter wrong"); 
            return enif_make_badarg(env);
        } 

        iov = alloca(arity * sizeof(struct iovec));
        for(int i=0; i < arity; i++){
            if (enif_inspect_binary(env, tuple[1], buffer)) {
                memcpy(&iov[i].iov_base, &buffer, sizeof(buffer)) ;  
                iov[i].iov_len = sizeof(buffer);
            }
        }

        if(sd_journal_sendv(iov, arity) >= 0)
            return enif_make_atom(env, "OK");

        sd_journal_perror("wirte operation failed"); 
        return enif_make_atom(env,"Error");
}

static ERL_NIF_TERM journal_perror(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
        ErlNifBinary* buffer; 
        const char* message; 
       
        if (!enif_inspect_binary(env, argv[0], buffer)) {
            sd_journal_perror("transfer parameter wrong"); 
            return enif_make_badarg(env);
        } 

        memcpy(&message, &buffer, sizeof(buffer));  
        if (sd_journal_perror(message) >= 0)
            return enif_make_atom(env, "OK");

        sd_journal_perror("wirte operation failed"); 
        return enif_make_atom(env,"Error"); 
}

static ErlNifFunc nif_funcs[] = {
    {"journal_sendv", 1, journal_sendv},
    {"journal_perror", 1, journal_perror}
};
ERL_NIF_INIT(journal, nif_funcs, NULL, NULL, NULL, NULL)