#ifndef NO_C_TICKET_LOCK_H
#define NO_C_TICKET_LOCK_H
#include <atomic>
using namespace std;

struct TicketLock {
    // Exclusive owernship
    atomic<int>* ticket_queue;
    atomic<int>* ticket_served;
    TicketLock() {
        this->ticket_queue = new atomic<int>(0);
        this->ticket_served = new atomic<int>(0);
    }

    ~TicketLock() {
        delete this->ticket_queue;
        delete this->ticket_served;
    }
};

void ticket_lock_lock(TicketLock* lock);
void ticket_lock_unlock(TicketLock* lock);

struct TicketLockUnLockDefer {
    TicketLock* lock;

    TicketLockUnLockDefer(TicketLock* lock) {
        this->lock = lock;
        ticket_lock_lock(this->lock);
    }
    ~TicketLockUnLockDefer() {
        ticket_lock_unlock(this->lock);
    }
};

#endif 