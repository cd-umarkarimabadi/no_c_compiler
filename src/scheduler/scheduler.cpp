#include "scheduler/scheduler.h"
#include <sys/epoll.h>
#include <fcntl.h>

void resume_thread_from_io_event(io_manager* mng, io_event* event);

int create_epoll_instance() {
    int epoll_fd = epoll_create1(0);
    if (epoll_fd == -1) {
        throw runtime_error("fatal error :: failed to create epoll instance");
    }
    return epoll_fd;
}

io_manager* create_io_manager() {
    io_manager* ev_mng = new io_manager();
    ev_mng->event_map = new map<int, io_event*>();
    ev_mng->thread_blocked_descriptor = new map<int, noCThread*>();
    ev_mng->epoll_fd = create_epoll_instance();
    ev_mng->lock = new TicketLock();
    return ev_mng;
}

io_event* create_event(noCThread* waiting_thraed, int fd, io_event_type type) {
    io_event* ev = new io_event();
    ev->status = event_status::PENDING;
    ev->descriptor_fd = fd;
    ev->type = type;
    ev->waiting_thread = waiting_thraed;
    ev->restore_operand_context = new stack<NoCRuntimeObjectContainer*>();
    return ev;
}

int create_timer(int num_seconds) {
    int timer_fd = timerfd_create(CLOCK_MONOTONIC, 0);
    if (timer_fd == -1) {
        throw runtime_error("timer could not be created");
    }
    struct itimerspec spec;
    spec.it_value.tv_sec = num_seconds;
    spec.it_value.tv_nsec = 0;
    spec.it_interval.tv_sec = 0;
    spec.it_interval.tv_nsec = 0;
    if (timerfd_settime(timer_fd, 0, &spec, NULL) == -1) {
        throw runtime_error("could not set timer");
    }
    return timer_fd;
}

io_event* get_event(io_manager* event_mng, int fd) {
    auto defer_unlock = TicketLockUnLockDefer(event_mng->lock);
    return event_mng->event_map->at(fd);
}

void add_event(io_manager* event_manager, io_event* ev) {

    ticket_lock_lock(event_manager->lock);

    if (event_manager->event_map->count(ev->descriptor_fd) != 0) {
        ticket_lock_unlock(event_manager->lock);
        throw runtime_error("event for fd already exists, this should never happen");
    }
    event_manager->event_map->insert_or_assign(ev->descriptor_fd, ev);
    ticket_lock_unlock(event_manager->lock);

    struct epoll_event event;
    event.data.fd = ev->descriptor_fd;
    // Note EPOLLET | Needed to the event does not keep getting triggerred if we only read partial bytes  
    if (ev->type == io_event_type::SLEEP_TIMER) {
        // For timers themseleves we dont want to register any write events since it will be fire for no reason
        event.events = EPOLLIN | EPOLLET;
    }
    else {
        event.events = EPOLLIN | EPOLLOUT | EPOLLET | EPOLLRDHUP;
    }
    if (epoll_ctl(event_manager->epoll_fd, EPOLL_CTL_ADD, ev->descriptor_fd, &event) == -1) {
        throw runtime_error("failed to add epoll event");
    }
}

void delete_event(io_manager* event_mng, io_event* ev) {
    auto defer_unlock = TicketLockUnLockDefer(event_mng->lock);
    if (epoll_ctl(event_mng->epoll_fd, EPOLL_CTL_DEL, ev->descriptor_fd, NULL) == -1) {
        throw runtime_error("terminal error :: failed to delete io event from epoll");
    }
    event_mng->event_map->erase(ev->descriptor_fd);
    close(ev->descriptor_fd);
    delete ev;
}

bool is_thread_block_on_this_descriptor(io_manager* event_mng, int fd) {
    auto defer_unlock = TicketLockUnLockDefer(event_mng->lock);
    return event_mng->thread_blocked_descriptor->count(fd) > 0;
}

void delete_thread_blocked_desc(io_manager* mng, int fd) {
    auto defer_unlock = TicketLockUnLockDefer(mng->lock);
    mng->thread_blocked_descriptor->erase(fd);
}

void insert_thread_blocked_desc(io_manager* mng, int fd, noCThread* thread) {
    auto defer_unlock = TicketLockUnLockDefer(mng->lock);
    mng->thread_blocked_descriptor->insert_or_assign(fd, thread);
}


void run_io_event_loop(io_manager* io_event_manager) {

#define MAX_EVENTS 64
#define EPOLL_RETURN_IMMEDIATELY 0

    struct epoll_event events[MAX_EVENTS];
    int event_count = epoll_wait(io_event_manager->epoll_fd, events, MAX_EVENTS, EPOLL_RETURN_IMMEDIATELY);
    if (event_count == -1) {
        throw runtime_error("epoll wait failed with terminal error");
    }
    for (int i = 0; i < event_count; i++) {
        struct epoll_event event = events[i];
        io_event* io_event = get_event(io_event_manager, events[i].data.fd);
        assert(io_event != NULL);

        if (event.events & (EPOLLERR | EPOLLHUP | EPOLLRDHUP)) {
            io_event->status = event_status::FAILED;
        }
        else if (event.events & (EPOLLIN | EPOLLOUT)) {
            // TODO :: If the thread is waiting on read io and the event type does not match then we should not resume the thread 
            // For now there is no real side effect, the thread restarts, checks the descriptors, finds its non blocking parks itself 
            io_event->status = event_status::READY;
        }
        else {
            assert_never_reaches("Missing case for epoll event");
        }
        // We do not want to resume a thread unless it is blocked on this event
        // for example :: if we are processing some bytes but while doing so we epoll fires another event 
        bool is_blocked = is_thread_block_on_this_descriptor(io_event_manager, event.data.fd);
        if (is_blocked) {
            resume_thread_from_io_event(io_event_manager, io_event);
            if (io_event->type == io_event_type::SLEEP_TIMER) {
                delete_event(io_event_manager, io_event);
            }
        }
    }
}

int create_timer_event(io_manager * ev_mng, noCThread * thread, int num_seconds) {
    int timer_fd = create_timer(num_seconds);
    io_event* ev = create_event(thread, timer_fd, io_event_type::SLEEP_TIMER);
    add_event(ev_mng, ev);
    return timer_fd;
}

io_manager* get_event_mng(noCThread * thread) {
    Worker* worker = getPrivateWorker(thread);
    io_manager* ev = worker->io_mng;
    return ev;
}

void close_socket(noCThread * thread, int sock_fd) {
    auto event_mng = get_event_mng(thread);
    io_event* event = get_event(event_mng, sock_fd);
    delete_event(event_mng, event);
}

int create_socket() {
    int sockfd = socket(AF_INET, SOCK_STREAM, 0);
    return sockfd;
}

sockaddr_in create_socket_address(char* address, int port) {
    struct sockaddr_in server_addr;
    memset(&server_addr, 0, sizeof(server_addr));
    server_addr.sin_family = AF_INET;
    server_addr.sin_port = htons(port);
    server_addr.sin_addr.s_addr = address == NULL ? INADDR_ANY : inet_addr(address);
    return server_addr;
}

void set_socket_to_non_blocking(int socket_fd) {
    // NOTE (umar) For now I will change the mode to be non blocking only afer the connect 
    // This just makes it easier right now for me in the run time to know if I can read/write to a socket 
    int flags = fcntl(socket_fd, F_GETFL, 0);
    if (flags == -1) {
        throw runtime_error("failed to get flags of socket fd");
    }
    int fcnlt_result = fcntl(socket_fd, F_SETFL, flags | O_NONBLOCK);
    if (fcnlt_result == -1) {
        throw runtime_error("failed to set non blocking flags on socket descriptor");
    }
}

int server_socket_listen(noCThread * thread,
    int socket_fd,
    int port) {

    struct sockaddr_in socket_address = create_socket_address(NULL, port);
    if (bind(socket_fd, (struct sockaddr*)&socket_address, sizeof(socket_address)) < 0) {
        return -1;
    }

    if (listen(socket_fd, 128) < 0) {
        return -1;
    }

    set_socket_to_non_blocking(socket_fd);
    auto event_mng = get_event_mng(thread);
    auto io_event = create_event(thread, socket_fd, io_event_type::SOCKET);
    add_event(event_mng, io_event);

    return 0;
}

int socket_connect(
    noCThread * thread,
    int socket_fd,
    char* address,
    int port) {

    struct sockaddr_in server_addr = create_socket_address(address, port);

    int connect_result = connect(socket_fd, (struct sockaddr*)&server_addr, sizeof(server_addr));
    if (connect_result != 0) {
        return -1;
    }

    set_socket_to_non_blocking(socket_fd);
    auto event_mng = get_event_mng(thread);
    auto io_event = create_event(thread, socket_fd, io_event_type::SOCKET);
    add_event(event_mng, io_event);
    return 0;
}

void park_thread(noCThread* thread) {
    update_thread_state_weak(thread, ThreadState_E::Parked);
    Worker* worker = getPrivateWorker(thread);
    longjmp(worker->reset_worker_stack, 0);
}

void park_thread_on_fd(noCThread* thread, int fd, vector<NoCRuntimeObjectContainer*>* operands_to_save) {
    auto event_mng = get_event_mng(thread);
    auto event = get_event(event_mng, fd);
    if (event == NULL) {
        throw runtime_error("bad fd :: no event bound to this fd");
    }
    for (auto operand : *operands_to_save) {
        event->restore_operand_context->push(operand);
    }

    insert_thread_blocked_desc(event_mng, fd, thread);
    park_thread(thread);
}

void native_sleep(noCThread * thread, int num_seconds) {
    io_manager* ev_mng = get_event_mng(thread);
    int fd = create_timer_event(ev_mng, thread, num_seconds);
    auto operands_to_save = vector<NoCRuntimeObjectContainer*>(); // no operands to save
    park_thread_on_fd(thread, fd, &operands_to_save);
}

io_accept_status accept_on_socket(int socket_fd) {
    int result = accept(socket_fd, NULL, NULL);
    if (result >= 0) {

        return io_accept_status{
                    .client_fd = result,
                    .status = io_success::SUCCESS
        };
    }

    if (errno == EAGAIN || errno == EWOULDBLOCK) {
        return io_accept_status{
            .client_fd = -1,
            .status = io_success::WOULD_BLOCK
        };
    }

    return io_accept_status{
            .client_fd = -1,
            // TODO :: Add some failure reason 
            .status = io_success::FAILED
    };
}

io_read_write_status io_on_socket(
    io_type io_type,
    int socket_fd,
    char* output_buffer,
    size_t buffer_len) {

    /*
    This commment also applies to io_accept connect
        // when park_thread_on_fd is when when we register the (fd, thread) in the thread_blocked_descriptor
        // If a thread is resumed it is removed from that mapping
        // This means whenever we can read/write from an fd and it is not going to be non blocking
        // The fd should not be registed in the thread_blocked_descriptor map
    */
    int bytes_transferred = io_type == io_type::READ ? read(socket_fd, output_buffer, buffer_len) : write(socket_fd, output_buffer, buffer_len);
    if (bytes_transferred >= 0) {
        return io_read_write_status{
            .bytes_transferred = bytes_transferred,
            .status = io_success::SUCCESS
        };
    }

    if (errno == EAGAIN || errno == EWOULDBLOCK) {
        return io_read_write_status{
            .bytes_transferred = 0,
            .status = io_success::WOULD_BLOCK
        };
    }

    return io_read_write_status{
            .bytes_transferred = 0,
            // TODO :: Add some failure reason 
            .status = io_success::FAILED
    };
}

void pause_worker_for_gc(Worker * worker) {
    if (worker->gc_request_flag) {
        GC_Flag* gc_flags = worker->gc_flags;
        pthread_mutex_lock(gc_flags->lock);
        worker->is_paused_for_gc = true;
        while (gc_flags->state->load() != GC_State::NotCollecting) {
            pthread_cond_wait(gc_flags->gc_cond, gc_flags->lock);
        }
        pthread_mutex_unlock(gc_flags->lock);
        assert(!worker->gc_request_flag);
    }
}

void check_gc_signal(noCThread * thread) {
    Worker* worker = getPrivateWorker(thread);
    if (worker->gc_request_flag) {
        pause_worker_for_gc(worker);
    }
}

void push_task(noCThread * thread) {
    Worker* worker = getPrivateWorker(thread);
    auto runQueue = worker->runQueue;
    // todo :: a hack because I want to defere allocation of the heap  
    // TODO BUG :: This is a nonatomic load 
    auto threadState = getThreadState(thread);
    if (threadState != ThreadState_E::Created) {
        update_thread_state_strong(thread, ThreadState_E::Runnable);
    }
    noCQueue_enqueue(runQueue, thread);
}

Scheduler* createScheduler() {
    Scheduler* scheduler = new Scheduler();
    scheduler->workers = new vector<Worker*>();
    return scheduler;
}

void worker_loop(Worker * worker);
void* worker_loop_inner(void* arg) {
    Worker_loop_args* loop_arg = (Worker_loop_args*)arg;

    ThreadLocalWorkerContext::get()->worker = loop_arg->worker;
    ThreadLocalWorkerContext::get()->scheduler = loop_arg->scheduler;

    worker_loop(loop_arg->worker);
    return NULL;
}

noCThread* getTask(Worker * worker) {
    auto runQueue = worker->runQueue;
    noCThread* thread = noCQueue_deque(runQueue);
    return thread;
}

noCThread* steal_task(Worker * worker) {
    auto victims = worker->scheduler->workers;
    for (size_t i = 0; i < victims->size(); i++) {
        Worker* victim = victims->at(i);
        if (victim->workerIdentifierIndex != worker->workerIdentifierIndex) {
            noCThread* task = getTask(victims->at(i));
            if (task == NULL) {
                continue;
            }
            return task;
        }
    }
    return NULL;
}

void init_thread_heap(noCThread * thread) {
    Heap* local_heap = init_heap();
    if (local_heap == NULL) {
        // Can we save everything by waiting for GC from other threads 
        throw runtime_error("Could not allocate local heap");
    }
    thread->tcb->heap = local_heap;
    auto threadHandleRefTracker = createExternalHandler(local_heap, thread, ExternalHandleType::NoCThread);
    thread->heapRefTracker = threadHandleRefTracker;
}

noCThread* createThreadFromParent(noCThread * parent_thread,
    InstructionStream * newIns) {

    Worker* worker = getPrivateWorker(parent_thread);
    noCThread* cached_thread = NULL;
    int cached_thread_index = 0;
    for (auto thread : *worker->thread_list) {
        if (thread->tcb->hasNoRefrence) {
            cached_thread = thread;
            break;
        }
        cached_thread_index++;
    }

    if (cached_thread == NULL) {
        auto fork_thread = createNoCThread(newIns,
            parent_thread->tcb->symbolTable,
            parent_thread->tcb->constPool);
        fork_thread->parent = parent_thread;
        fork_thread->tcb->scheduler = parent_thread->tcb->scheduler;
        fork_thread->tcb->workerIndex = parent_thread->tcb->workerIndex;
        init_thread_heap(fork_thread);
        worker->thread_list->push_back(fork_thread);
        return fork_thread;
    }

    // Remove cached thread from thead list, scheduling will put it back on the list
    auto thread_list = worker->thread_list;
    auto thread_list_it = thread_list->begin();
    advance(thread_list_it, cached_thread_index);
    thread_list->erase(thread_list_it);

    // Empty operand as sanity check and reset operands, both are sanity, ideal the operands the current activation should be empty and null when a thread exits 
    cached_thread->tcb->currentActivationFrame = NULL;

    while (!cached_thread->tcb->operands->empty()) {
        cached_thread->tcb->operands->pop();
    }

    // Set up new ins stream
    // The instruction stream cannot be deleted because the vector is shared :: fix this 
    // delete cached_thread->tcb->instructionStream;
    cached_thread->tcb->instructionStream = newIns;
    cached_thread->tcb->Ip = newIns->startInsIndx;
    cached_thread->tcb->workerIndex = parent_thread->tcb->workerIndex;

    cached_thread->tcb->hasNoRefrence = false;
    auto threadHandleRefTracker = createExternalHandler(cached_thread->tcb->heap, cached_thread, ExternalHandleType::NoCThread);
    cached_thread->heapRefTracker = threadHandleRefTracker;
    return cached_thread;
}

void resume_thread(noCThread* thread) {
    push_task(thread);
}

void resume_thread_from_io_event(io_manager* mng, io_event* event) {
    auto thread = event->waiting_thread;

    // restore all stack operand that were saved 
    while (!event->restore_operand_context->empty()) {
        NoCRuntimeObjectContainer* operand = event->restore_operand_context->top();
        event->restore_operand_context->pop();
        thread->tcb->operands->push(operand);
    }

    // Thread is no longer blocked on this descriptor
    delete_thread_blocked_desc(mng, event->descriptor_fd);
    push_task(event->waiting_thread);
}

volatile bool shutdown_requested = false;
void shutdown_handler(int signum) {
    shutdown_requested = true;
}

void worker_loop(Worker * worker) {
    setjmp(worker->reset_worker_stack);
    for (;;) {
        if (shutdown_requested) {
            break;
        }
        noCThread* thread = getTask(worker);
        if (thread == NULL) {
            pause_worker_for_gc(worker);
            thread = steal_task(worker);
            if (thread == NULL) {
                pause_worker_for_gc(worker);
                // Run event loop for any IO
                run_io_event_loop(worker->io_mng);
                continue;
            }
            thread->tcb->workerIndex = worker->workerIdentifierIndex;
        }
        // TODO :: BIG HACK -- THINK OF A WAY TO NOT DO THIS AND DEFER HEAP ALLOCATION WHEN WE NEED IT 
        auto state = getThreadState(thread);
        if (state == ThreadState_E::Created) {
            if (!thread->tcb->heap) {
                init_thread_heap(thread);
            }
        }
        update_thread_state_strong(thread, ThreadState_E::Running);
        runThread(thread);
    }
}

Worker* create_worker(pthread_t * kernel_thread, GC_Flag * gc_flags, int worker_identifer_index, io_manager* io_mng) {
    Worker* worker = new Worker();
    worker->runQueue = new noCConcurrentQueue<noCThread*>();
    worker->kernel_thread = kernel_thread;
    worker->workerIdentifierIndex = worker_identifer_index;
    worker->gc_flags = gc_flags;
    worker->thread_list = new list<noCThread*>();
    worker->io_mng = io_mng;
    return worker;
}

void run_scheduler(Scheduler * scheduler, int num_threads, noCThread * mainThread) {

    if (num_threads <= 0 || mainThread == NULL) {
        throw runtime_error("Number of thread cannot be less than zero || main thread cannot be null");
    }
    signal(SIGINT, shutdown_handler);
    signal(SIGTERM, shutdown_handler);

    GC_Flag* gc_flags = create_gc_flags();
    io_manager* io_mng = create_io_manager();

    for (int i = 0; i < num_threads; i++) {
        pthread_t* kernelThread = new pthread_t();
        Worker* worker = create_worker(kernelThread, gc_flags, i, io_mng);
        scheduler->workers->push_back(worker);
        worker->scheduler = scheduler;
    }

    for (auto worker : *scheduler->workers) {
        Worker_loop_args* looper_arg = new Worker_loop_args();
        looper_arg->scheduler = scheduler;
        looper_arg->worker = worker;
        if (pthread_create(worker->kernel_thread, NULL, worker_loop_inner, looper_arg) != 0) {
            throw runtime_error("failed to create thread");
        }
    }

    // Set up worker index for main thread 
    // Add it to worker thread list 
    // push task to the worker 
    // I use the worker index to not get a reference to worker but this is fine to do.
    // TODO :: Change to just take reference to worker object 
    mainThread->tcb->scheduler = scheduler;
    mainThread->tcb->workerIndex = 0;
    Worker* worker = getPrivateWorker(mainThread);
    worker->thread_list->push_back(mainThread);

    push_task(mainThread);

    for (auto worker : *scheduler->workers) {
        pthread_join(*worker->kernel_thread, NULL);
    }
    delete io_mng;
    delete gc_flags;
}

void schedule_new_thread(Scheduler * scheduler, noCThread * newThread) {
    Worker* worker = scheduler->workers->at(newThread->tcb->workerIndex);
    assert(worker != NULL);
    push_task(newThread);
}

Worker* getPrivateWorker(noCThread * thread) {
    Scheduler* scheduler = (Scheduler*)thread->tcb->scheduler;
    Worker* worker = scheduler->workers->at(thread->tcb->workerIndex);
    assert(worker != NULL);
    return worker;
}