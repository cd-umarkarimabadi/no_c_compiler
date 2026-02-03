#ifndef SCHEDULER_H
#define SCHEDULER_H 

#include <vector>
#include <queue>
#include <csetjmp>
#include <pthread.h>
#include "type_checker/type_checker.h"
#include "gen/code_gen.h"
#include "noCThread/noCThread.h"
#include "concurreny_utils/queue.h"
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/select.h>
#include <noCThread/noCThread.h>
#include "collections/collections.h"
#include "concurreny_utils/ticket_lock.h"
#include <sys/timerfd.h>
#include <pthread.h>
#include <iostream>
#include <unistd.h>
#include <assert.h>
#include "vm/noC_vm.h"
#include "allocator/gc.h"
#include "concurreny_utils/queue.h"
#include "utils/utils.h"
#include "vm/runtime_obj.h"
#include "scheduler/scheduler.h"
#include <sys/socket.h>
#include "string.h"
#include <arpa/inet.h>
#include "utils/utils.h"
#include <csignal>
#include "scheduler/scheduler_base.h"


enum struct event_status {
    READY,
    PENDING,
    FAILED,
};

enum struct io_event_type {
    SLEEP_TIMER,
    FILE_HANDLE,
    SOCKET
};

enum struct io_success {
    SUCCESS,
    WOULD_BLOCK,
    FAILED
};

enum struct io_type {
    READ,
    WRITE
};

struct io_accept_status {
    int client_fd;
    io_success status;
};

struct io_read_write_status {
    int bytes_transferred;
    io_success status;
};

struct timer_handle {
    int timer_fd;
};

struct socket_handle {
    int socket_handle;
};

struct io_event {
    // Exclusive ownership
    int descriptor_fd;
    io_event_type type;
    event_status status;
    stack<NoCRuntimeObjectContainer*>* restore_operand_context;
    // Shared ownership
    noCThread* waiting_thread;
};

struct io_manager {
    // Exclusive ownership
    TicketLock* lock;
    map<int, io_event*>* event_map;
    map<int, noCThread*>* thread_blocked_descriptor;
    int epoll_fd;

    ~io_manager() {
        delete this->lock;
        delete this->event_map;
        delete this->thread_blocked_descriptor;
    }
};

struct Worker {

    // Exclusive ownership
    int workerIdentifierIndex;
    bool gc_request_flag;
    bool is_paused_for_gc;
    noCConcurrentQueue<noCThread*>* runQueue;
    list<noCThread*>* thread_list;
    pthread_t* kernel_thread;
    jmp_buf reset_worker_stack;


    // Shared ownership
    io_manager* io_mng;
    GC_Flag* gc_flags;
    Scheduler* scheduler;

    ~Worker() {
        delete this->kernel_thread;
        delete this->runQueue;
        delete this->thread_list;

    }
};

struct Scheduler {
    // Exclusive ownership
    vector<Worker*>* workers;
    ~Scheduler() {
        for (auto worker : *this->workers) {
            delete worker;
        }
        delete this->workers;
    }
};

struct Worker_loop_args {
    Worker* worker;
    Scheduler* scheduler;
};

struct ThreadLocalWorkerContext {
public:
    Worker* worker = NULL;
    Scheduler* scheduler = NULL;
    static ThreadLocalWorkerContext* get() {
        static thread_local ThreadLocalWorkerContext instance;
        return &instance;
    }
};

Scheduler* createScheduler();
void schedule_new_thread(Scheduler* scheduler, noCThread* newThread);
void run_scheduler(Scheduler* scheduler, int num_threads, noCThread* mainThread);


void park_thread_on_fd(noCThread* thread, int fd, vector<NoCRuntimeObjectContainer*>* operands_to_save);
void park_thread(noCThread* thread);
void resume_thread(noCThread* thread);

void push_new_task(noCThread* thread);

noCThread* createThreadFromParent(noCThread* parent_thread,
    InstructionStream* newIns);

void threadStatusFinished(noCThread* noCThread);
void freeNoCThread(Scheduler* scheduler, noCThread* thread);
void update_thread_state_strong(noCThread * thread, ThreadState_E state);
bool update_thread_state_weak(noCThread * thread, ThreadState_E state);
bool threadStateAtLeast(noCThread* thread, ThreadState_E target);
void check_gc_signal(noCThread* thread);

void native_sleep(noCThread* thread, int num_seconds);

// socket 
int create_socket();
int socket_connect(
    noCThread* thread,
    int socket_fd,
    char* address,
    int port);
void close_socket(noCThread * thread, int sock_fd);

int server_socket_listen(noCThread * thread,
    int socket_fd,
    int port);

io_read_write_status io_on_socket(
    io_type io_type,
    int socket_fd,
    char* output_buffer,
    size_t buffer_len);

io_accept_status accept_on_socket(int socket_fd);

// Internal functions
noCThread* getTask(Worker* worker);
Worker* getPrivateWorker(noCThread* thread);

#endif // SCHEDULER_H 