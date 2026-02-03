#include "concurreny_utils/ticket_lock.h"
#include "collections/collections.h"

void ticket_lock_lock(TicketLock* lock) {
    int ticket = lock->ticket_queue->fetch_add(1);
    // busy Spin
    // TODO :: Can we just cache the atomic load and spin on a local copy  
    // but on a local copy we do not get the write barrier that we need 

    // TODO 
    // In high conention everyone core would be spinning on this 
    // This would create a high amount of coherence traffic
    // TODO :: Shavit, Nir queue locks in theory does improve things since a thread only sping on its predessor queue 
    // You will have to pad the queue memory on seperate cache lines to actually get this performance to avoid the false sharing 
    // Not sure how practical this is 
    while (ticket != lock->ticket_served->load()) {

    }
}

void ticket_lock_unlock(TicketLock* lock) {
    lock->ticket_served->fetch_add(1);
}



