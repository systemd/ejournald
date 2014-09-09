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

//to suppress logging information about this file
#define SD_JOURNAL_SUPPRESS_LOCATION

#include "erl_nif.h"
#include <stdlib.h>
#include <string.h>
#include <systemd/sd-journal.h>
#include <systemd/sd-id128.h>
#include <alloca.h>
#include <assert.h>
#include <unistd.h>
#include <stdio.h>

static ErlNifResourceType *file_descriptor = NULL;

//A pointer to the head of the journal will be stored in journal_container.
//journal_container_type is just the ErlNifResourceType for journal_container 
//to allocate it later in "open" as this type.

static ErlNifResourceType *journal_container_type = NULL;
typedef struct { 
    sd_journal *journal_pointer;
    ErlNifTid tid;
    ErlNifPid pid;
    unsigned int notifier_flag; //0 should be standard, set to 1 if notifier thread should close
    unsigned int notifier_used; //just to check is its active
} journal_container;

//Some standard return values
static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_error;
static ERL_NIF_TERM atom_eaddrnotavail;

static ERL_NIF_TERM return_error(ErlNifEnv* env, const char* reason)
{
    return enif_make_tuple2(env, atom_error, enif_make_atom(env, reason));
}

void dtor_fd(ErlNifEnv* env, void* obj)
{
    int* fd = (int*) obj;
    close(*fd); 
}

void close_journal_container(journal_container* jc)
{

    //case notifier not used
    if(jc->notifier_used==0){
        sd_journal_close(jc->journal_pointer);
    }
    else{
        //close the notifier thread
        jc->notifier_flag=1;
        enif_thread_join(jc->tid, NULL);
        sd_journal_close(jc->journal_pointer);
    }
}

void dtor_jc(ErlNifEnv* env, void* obj)
{
    journal_container* jc = (journal_container*) obj;
    close_journal_container(jc);
}

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    ErlNifResourceType *fd = enif_open_resource_type(env, NULL, "int", dtor_fd, ERL_NIF_RT_CREATE, NULL);
    if (fd == NULL) return -1;
    file_descriptor = fd;

    ErlNifResourceType *rt = enif_open_resource_type(env, NULL, "journal_container_type", dtor_jc, ERL_NIF_RT_CREATE, NULL);
    if (rt == NULL)    return -1;
    journal_container_type = rt;
 
    atom_ok = enif_make_atom(env, "ok");
    atom_error = enif_make_atom(env, "error");
    atom_eaddrnotavail = enif_make_atom(env, "eaddrnotavail");

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
            return enif_make_badarg(env);

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

    enif_get_list_length(env,argv[0],&len);
    enif_get_int(env, argv[1], &priority); 
    enif_get_int(env, argv[2], &level_prefix);  // binary conversion 

    len++;
    syslog_id = alloca(len);
        
    enif_get_string(env, argv[0], syslog_id, len, ERL_NIF_LATIN1);
        
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

/*
    Opens the journal and stores a pointer in a journal_container. To share the pointer 
    to the journal with the calling Erlang module it is made a resource via 
    enif_make_resource.
*/
static ERL_NIF_TERM nif_open(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

    journal_container *jc;

    jc = enif_alloc_resource(journal_container_type, sizeof(journal_container));

    int r = sd_journal_open(&(jc->journal_pointer), SD_JOURNAL_LOCAL_ONLY);
    if (r < 0) 
        return return_error(env, "failed_to_open_journal");

    jc->notifier_used=0;
    
    ERL_NIF_TERM nif_journal_pointer = enif_make_resource(env, jc);
    enif_release_resource(jc);
    
    return enif_make_tuple2(env, atom_ok, nif_journal_pointer);
    
}

static ERL_NIF_TERM nif_open_directory(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

    journal_container *jc;
    unsigned int ip;

    if (!enif_get_list_length(env, argv[0], &ip))
        return enif_make_badarg(env);
    ip++; //string array is larger by one entry

    char field[ip];
    if (!enif_get_string(env, argv[0], field, ip, ERL_NIF_LATIN1))
        return enif_make_badarg(env);

    jc = enif_alloc_resource(journal_container_type, sizeof(journal_container));
    
    int r = sd_journal_open_directory(&(jc->journal_pointer), field, 0);
    if (r < 0)
        return return_error(env, "failed_to_open_journal");
    
    jc->notifier_used=0;

    ERL_NIF_TERM nif_journal_pointer = enif_make_resource(env, jc);
    enif_release_resource(jc);
    
    return enif_make_tuple2(env, atom_ok, nif_journal_pointer);
    
}


/*
    Closes the journal. The resource allocated by enif_make_resource in the nif_open() call
    will be automatically collected by the Erlang garbage collector.
*/
static ERL_NIF_TERM nif_close(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

    journal_container *jc;

    if (!enif_get_resource(env, argv[0], journal_container_type, (void **) &jc))
        return enif_make_badarg(env);
  
    close_journal_container(jc);

    return atom_ok;
}


/*
       sets the head of the journal on step forwards/backwards (according to given specifications
    made by e.g. add_match...)
*/
static ERL_NIF_TERM nif_next(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

    journal_container *jc;

    enif_get_resource(env, argv[0], journal_container_type, (void **) &jc);
        
    int r = sd_journal_next(jc->journal_pointer);    
    if (r < 0) 
        return return_error(env, "failed_to_move_forwards");
    if (r == 0) 
        return atom_eaddrnotavail;
    else 
        return atom_ok;
}

static ERL_NIF_TERM nif_previous(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

    journal_container *jc;

    enif_get_resource(env, argv[0], journal_container_type, (void **) &jc);
        
    int r = sd_journal_previous(jc->journal_pointer);
    if (r < 0) 
        return return_error(env, "failed_to_move_backwards");
    if (r == 0) 
        return atom_eaddrnotavail;
    else 
        return atom_ok;
}

/*
    gets the entries of the current head position (according to given specifications
    made by e.g. argv[1], add_match...). There are two arguments. First the journal 
    pointer, second the entry-string which should be matched.
*/
static ERL_NIF_TERM nif_get_data(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

    journal_container *jc;
    unsigned int ip;

    enif_get_resource(env, argv[0], journal_container_type, (void **) &jc);
    enif_get_list_length(env, argv[1], &ip);
    ip++; //string array is larger by one entry

    char field[ip];
    enif_get_string(env, argv[1], field, ip, ERL_NIF_LATIN1);

    int r;
    char *d;
    size_t l;

    r = sd_journal_get_data(jc->journal_pointer, field, (void *) &d, &l);
    if (r < 0)
        return return_error(env, "failed_to_get_data");

    ERL_NIF_TERM term = enif_make_string_len(env, d , l, ERL_NIF_LATIN1);
    return enif_make_tuple2(env, atom_ok, term);
}

/*
      add a match to the match list. There are three arguments. First the journal pointer,
    second the match-string and third the length of this string.
*/
static ERL_NIF_TERM nif_add_match(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

    journal_container *jc;
    unsigned int ip;

    enif_get_resource(env, argv[0], journal_container_type, (void **) &jc);
    enif_get_list_length(env, argv[1], &ip);
    ip++; //string array is larger by one entry
    
    char field[ip];
    enif_get_string(env, argv[1], field, ip, ERL_NIF_LATIN1);

    int r = sd_journal_add_match(jc->journal_pointer, (void *) field, 0);
    if (r < 0)
        return return_error(env, "failed_to_add_match");

    return atom_ok;
}

static ERL_NIF_TERM nif_flush_matches(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

    journal_container *jc;

    enif_get_resource(env, argv[0], journal_container_type, (void **) &jc);
        
    sd_journal_flush_matches(jc->journal_pointer);
    
    return atom_ok;
}


static ERL_NIF_TERM nif_add_disjunction(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

    journal_container *jc;

    enif_get_resource(env, argv[0], journal_container_type, (void **) &jc);
        
    int r = sd_journal_add_disjunction(jc->journal_pointer);
    if (r < 0)
        return return_error(env, "failed_to_add_disjunction");

    return atom_ok;
}

// not supported for now
//static ERL_NIF_TERM nif_add_conjunction(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
//
//    journal_container *jc;
//
//    if (!enif_get_resource(env, argv[0], journal_container_type, (void **) &jc))
//        return enif_make_badarg(env);
//        
//    int r = sd_journal_add_conjunction(jc->journal_pointer);    
//    if (r < 0)
//        return return_error(env, "failed_to_add_conjunction");
//
//    return atom_ok;
//}

static ERL_NIF_TERM nif_seek_head(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

    journal_container *jc;

    enif_get_resource(env, argv[0], journal_container_type, (void **) &jc);
        
    int r = sd_journal_seek_head(jc->journal_pointer);    
    if (r < 0)
        return return_error(env, "failed_to_find_head");

    return atom_ok;
}

static ERL_NIF_TERM nif_seek_tail(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

    journal_container *jc;

    enif_get_resource(env, argv[0], journal_container_type, (void **) &jc);
        
    int r = sd_journal_seek_tail(jc->journal_pointer);    
    if (r < 0)
        return return_error(env, "failed_to_find_tail");

    return atom_ok;
}

static ERL_NIF_TERM nif_get_cursor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

    journal_container *jc;

    enif_get_resource(env, argv[0], journal_container_type, (void **) &jc);
        
    char *cursor;
    int r = sd_journal_get_cursor(jc->journal_pointer, &cursor);    
    if (r < 0)
        return return_error(env, "failed_to_get_cursor");

    ERL_NIF_TERM term;
    memcpy(enif_make_new_binary(env, strlen(cursor), &term), cursor, strlen(cursor));

    //cursor got allocated in sd_journal_get_cursor
    free(cursor);

    return enif_make_tuple2(env, atom_ok, term);
}

static ERL_NIF_TERM nif_test_cursor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

    journal_container *jc;
    ErlNifBinary p;

    enif_get_resource(env, argv[0], journal_container_type, (void **) &jc);
    enif_inspect_binary(env, argv[1], &p);

    int r = sd_journal_test_cursor(jc->journal_pointer, (const char *) p.data);
    if (r < 0) 
        return return_error(env, "failed_to_test_cursor");
    else if (r == 0) 
        return atom_eaddrnotavail;
    else return atom_ok;
}

static ERL_NIF_TERM nif_seek_cursor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

    journal_container *jc;
    ErlNifBinary p;

    enif_get_resource(env, argv[0], journal_container_type, (void **) &jc);
    enif_inspect_binary(env, argv[1], &p);

    int r = sd_journal_seek_cursor(jc->journal_pointer, (const char *) p.data);
    if (r < 0)
        return return_error(env, "failed_to_seek_cursor");

    return atom_ok;
}

static ERL_NIF_TERM nif_query_unique(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

    journal_container *jc;
    unsigned int ip;

    enif_get_resource(env, argv[0], journal_container_type, (void **) &jc);
    enif_get_list_length(env, argv[1], &ip);
    ip++; //string array is larger by one entry
    
    char field[ip];
    enif_get_string(env, argv[1], field, ip, ERL_NIF_LATIN1);

    int r = sd_journal_query_unique(jc->journal_pointer, field);
    if (r < 0)
        return return_error(env, "failed_to_seek_cursor");

    return atom_ok;
}

static ERL_NIF_TERM nif_enumerate_unique(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

    journal_container *jc;

    enif_get_resource(env, argv[0], journal_container_type, (void **) &jc);

    int r;
    char *d;
    size_t l;

    r = sd_journal_enumerate_unique(jc->journal_pointer, (void *) &d, &l);
    if( r < 0) return enif_make_tuple2(env, atom_error, enif_make_string(env,"enumerating failed",ERL_NIF_LATIN1));
    else if (r == 0) return atom_eaddrnotavail;
 
    ERL_NIF_TERM term = enif_make_string_len(env, d , l, ERL_NIF_LATIN1);

    return enif_make_tuple2(env, atom_ok, term);
}

static ERL_NIF_TERM nif_restart_unique(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

    journal_container *jc;

    enif_get_resource(env, argv[0], journal_container_type, (void **) &jc);

    sd_journal_restart_unique(jc->journal_pointer);    

    return atom_ok;
}

static ERL_NIF_TERM nif_enumerate_data(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

    journal_container *jc;

    enif_get_resource(env, argv[0], journal_container_type, (void **) &jc);

    int r;
    char *d;
    size_t l;

    r = sd_journal_enumerate_data(jc->journal_pointer, (void *) &d, &l);
    if( r < 0) 
        return return_error(env, "failed_to_enumerate");
    else if (r == 0) 
        return atom_eaddrnotavail;
 
    ERL_NIF_TERM term = enif_make_string_len(env, d , l, ERL_NIF_LATIN1);

    return enif_make_tuple2(env, atom_ok, term);
}

static ERL_NIF_TERM nif_restart_data(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

    journal_container *jc;

    enif_get_resource(env, argv[0], journal_container_type, (void **) &jc);
        
    sd_journal_restart_data(jc->journal_pointer);    

    return atom_ok;
}

static void * notifier_run(void *arg){

    journal_container *jc = (journal_container *) arg;
    ErlNifEnv *t_env;
    int changes;
     
    changes = sd_journal_wait(jc->journal_pointer, (uint64_t) 1000);
    while(1){

        if( jc->notifier_flag == 1 ){
            jc->notifier_flag = 0; 
            enif_thread_exit(NULL);
        }

        //waits 1000 microseconds for a new journal entry
        changes = sd_journal_wait(jc->journal_pointer, (uint64_t) 1000);

        //new entry appended at the end of the journal
        if( changes == SD_JOURNAL_APPEND ){
            t_env = enif_alloc_env();
            if(!enif_send(NULL, &(jc->pid), t_env, enif_make_atom(t_env, "journal_append"))){
                enif_thread_exit(NULL);
                enif_clear_env(t_env);
            }
        }
        //journal files were added or removed
        else if( changes == SD_JOURNAL_INVALIDATE ){
            t_env = enif_alloc_env();
            if(!enif_send(NULL, &(jc->pid), t_env, enif_make_atom(t_env, "journal_invalidate"))){
                enif_thread_exit(NULL);
                enif_clear_env(t_env);
            }
        }
    }

    return NULL;
}

static ERL_NIF_TERM nif_open_notifier (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

    journal_container *jc;
    enif_get_resource(env, argv[0], journal_container_type, (void **) &jc);

    if(jc->notifier_used == 1)
        return return_error(env, "notifier_already_exists");

    if(jc->notifier_flag==1)
        return return_error(env, "old_notifier_not_closed_yet");

    ErlNifPid pid;
    enif_get_local_pid(env, argv[1], &pid);

    jc->pid = pid;
    jc->notifier_flag = 0;
        
    if(enif_thread_create("notifier_worker", &jc->tid, notifier_run, (void *) jc, NULL) != 0)
        return return_error(env, "thread_creation_failed");

    jc->notifier_used = 1;

    return atom_ok;
}

static ERL_NIF_TERM nif_close_notifier (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

    journal_container *jc;
    enif_get_resource(env, argv[0], journal_container_type, (void **) &jc);

    if(jc->notifier_used == 0)
        return return_error(env, "no_notifier_available");

    //setting the notifier flag will close the notifier thread
    jc->notifier_flag=1;
    jc->notifier_used=0;

    return atom_ok;
}

static ERL_NIF_TERM nif_get_realtime_usec (ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]){

    journal_container *jc;
    uint64_t usec;
    int r;

    enif_get_resource(env, argv[0], journal_container_type, (void **) &jc);

    r = sd_journal_get_realtime_usec(jc->journal_pointer, &usec);
    if(r < 0 ) 
        return return_error(env, "getting_realtime_failed");

    return enif_make_tuple2(env, atom_ok, enif_make_uint64(env, usec)); 
}

static ERL_NIF_TERM nif_seek_realtime_usec (ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]){

    journal_container *jc;
    uint64_t usec;
    int r;
    
    enif_get_resource(env, argv[0], journal_container_type, (void **) &jc);
    enif_get_uint64(env, argv[1], &usec);

    r = sd_journal_seek_realtime_usec(jc->journal_pointer, usec);
    if(r < 0) 
        return return_error(env, "seeking_realtime_failed");
    
    return atom_ok;
}

static ErlNifFunc nif_funcs[] =
{    
    {"sendv_nif", 1, nif_sendv},
    {"stream_fd", 3, nif_stream_fd},
    {"write_fd", 2, nif_write_fd},
    {"close_fd", 1, nif_close_fd},

    {"open", 0, nif_open},
    {"open_directory", 1, nif_open_directory},
    {"close", 1, nif_close},
    {"next", 1, nif_next},
    {"previous", 1, nif_previous},
    {"get_data", 2, nif_get_data},
    {"add_match", 2, nif_add_match},
    {"add_disjunction", 1, nif_add_disjunction},
//    {"add_conjunction", 1, nif_add_conjunction},
    {"flush_matches", 1, nif_flush_matches},
    {"seek_head", 1, nif_seek_head},
    {"seek_tail", 1, nif_seek_tail},
    {"get_cursor", 1, nif_get_cursor},
    {"test_cursor", 2, nif_test_cursor},
    {"seek_cursor", 2, nif_seek_cursor},
    {"query_unique", 2, nif_query_unique},
    {"enumerate_unique", 1, nif_enumerate_unique},
    {"restart_unique", 1, nif_restart_unique},
    {"enumerate_data", 1, nif_enumerate_data},
    {"restart_data", 1, nif_restart_data},
    {"open_notifier", 2, nif_open_notifier},
    {"close_notifier", 1, nif_close_notifier},
    {"get_realtime_usec", 1, nif_get_realtime_usec},
    {"seek_realtime_usec", 2, nif_seek_realtime_usec}
};

ERL_NIF_INIT(journald_api,nif_funcs,load,NULL,NULL,NULL)
