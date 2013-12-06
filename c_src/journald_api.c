/* 
 Copyright 2010-2013, Travelping GmbH <info@travelping.com>

 Permission is hereby granted, free of charge, to any person obtaining a
 copy of this software and associated documentation files (the "Software"),
 to deal in the Software without restriction, including without limitation
 the rights to use, copy, modify, merge, publish, distribute, sublicense,
 and/or sell copies of the Software, and to permit persons to whom the
 Software is furnished to do so, subject to the following conditions:

 The above copyright notice and this permission notice shall be included in
 all copies or substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 DEALINGS IN THE SOFTWARE.
*/


#include <erl_nif.h>
#include <systemd/sd-journal.h>
#include <alloca.h>
#include <assert.h>
#include <unistd.h>
#include <stdio.h>

static ErlNifResourceType *file_descriptor = NULL;

static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_error;
static ERL_NIF_TERM atom_eagain;

void dtor_fd(ErlNifEnv* env, void* obj)
{
    int* fd = (int*) obj;
    close(*fd); 
}

// definition of return value
static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    ErlNifResourceType *fd = enif_open_resource_type(env, NULL, "int", dtor_fd, ERL_NIF_RT_CREATE, NULL);
    if (fd == NULL) return -1;

    file_descriptor = fd;

    atom_ok = enif_make_atom(env, "ok");
    atom_error = enif_make_atom(env, "error");
    atom_eagain = enif_make_atom(env, "eagain");

    return 0;
}

/* Sendv 
    convert every parameter of the list  to binary and copy that into a iovec and call sd_journal_sendv(2) 
*/
static ERL_NIF_TERM nif_sendv(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{  
    unsigned int len=0, i=0;
    struct iovec *iov = NULL; 
    ErlNifBinary item;
    ERL_NIF_TERM buffer, tail;
        
    if (!enif_get_list_cell(env, argv[0], &buffer, &tail) 
        || !enif_get_list_length(env, argv[0], &len) || len==0)
            return atom_error; 

    iov = alloca(len * sizeof(struct iovec));
    do {
        if (enif_inspect_iolist_as_binary(env, buffer, &item))        //binary conversion from list element 
        {            
            iov[i].iov_base = item.data;                              //copy process into iovec 
            iov[i].iov_len = item.size;
            i++;
        }
        else len--;                            // skip bad argument

    } while(enif_get_list_cell(env, tail, &buffer, &tail));

    if (sd_journal_sendv(iov, len) == 0) 
        return atom_ok;
        
    return atom_error; 
}
/*
    convert the first parameter to binary and the secound and third to int and call sd_journal_stream_fd(3)
*/
static ERL_NIF_TERM nif_stream_fd(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) 
{
    int priority = -1, level_prefix = -1;
    int *fd;
    unsigned int len=0;
    char* syslog_id;
        
    fd = enif_alloc_resource(file_descriptor, sizeof(int));

    if (!enif_get_list_length(env,argv[0],&len)
        || !enif_get_int(env, argv[1], &priority) 
        || !enif_get_int(env, argv[2], &level_prefix))  // binary conversion 
            return enif_make_badarg(env);           

    len++;
    syslog_id = alloca(len);
        
    if(!enif_get_string(env, argv[0], syslog_id, len, ERL_NIF_LATIN1))
        return enif_make_badarg(env);
        
    *fd = sd_journal_stream_fd(syslog_id, priority, level_prefix);
    if (*fd < 0)
        return atom_error;

    ERL_NIF_TERM nif_fd = enif_make_resource(env, fd);
    enif_release_resource(fd);

    return nif_fd; 
}

static ERL_NIF_TERM nif_write_fd(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int *fd;
    ErlNifBinary msg;
    
    if(!enif_get_resource(env, argv[0], file_descriptor, (void**) &fd))
        return enif_make_badarg(env);
    if(!enif_inspect_iolist_as_binary(env, argv[1], &msg))
        return enif_make_badarg(env);
    
    if(write(*fd, msg.data, msg.size) < 0)
        return atom_error;
    
    return atom_ok;
}

static ERL_NIF_TERM nif_close_fd(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int *fd;

    if(!enif_get_resource(env, argv[0], file_descriptor, (void**) &fd))
        return enif_make_badarg(env);
    if(close(*fd) < 0)
        return atom_error;
    
    return atom_ok;
}

static ErlNifFunc nif_funcs[] = { 
    {"sendv_nif", 1, nif_sendv},
    {"stream_fd", 3, nif_stream_fd},
    {"write_fd", 2, nif_write_fd},
    {"close_fd", 1, nif_close_fd}
};

ERL_NIF_INIT(journald_api, nif_funcs, load, NULL, NULL, NULL)
