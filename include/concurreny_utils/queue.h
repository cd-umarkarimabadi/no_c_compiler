#ifndef NO_C_QUEUE_H
#define NO_C_QUEUE_H
#include "concurreny_utils/ticket_lock.h"
#include "stddef.h"

template <typename T>
struct Node {
public:
    T value;
    Node<T>* next;
    Node() {}

    Node(T value) {
        this->value = value;
    }
};

template <typename T>
struct noCConcurrentQueue {
public:
    // Exclusive ownership
    Node<T>* head;
    Node<T>* tail;
    TicketLock* headLock;
    TicketLock* tailLock;

    noCConcurrentQueue() {
        head = new Node<T>();
        tail = head;
        headLock = new TicketLock();
        tailLock = new TicketLock();
    }

    ~noCConcurrentQueue() {
        Node<T>* current_node = head;
        while (current_node != NULL) {
            Node<T>* to_delete = current_node;
            current_node = current_node->next;
            delete to_delete;
        }
        delete this->headLock;
        delete this->tailLock;
    }
};

template <typename T>
void noCQueue_enqueue(noCConcurrentQueue<T>* queue, T value) {
    Node<T>* newNode = new Node<T>(value);
    ticket_lock_lock(queue->tailLock);
    queue->tail->next = newNode;
    queue->tail = newNode;
    queue->tail->next = NULL;
    ticket_lock_unlock(queue->tailLock);
};

template <typename T>
bool noCQueue_enqueue_is_empty(noCConcurrentQueue<T>* queue, T value) {
    return queue->head == queue->tail;
}

template <typename T>
T noCQueue_deque(noCConcurrentQueue<T>* queue) {

    // Note(TODO) This actually missing a linearization point -- because tail.next can be updated and then suspended
    // Because the head has not been updated this returns NULL
    // this is not really a problem unless queue.empty is used as some termination flag 
    if (queue->head == queue->tail) {
        return NULL;
    }

    ticket_lock_lock(queue->headLock);

    if (queue->head == queue->tail) {
        return NULL;
    }

    assert(queue->head->next != NULL);

    T valueToReturn = queue->head->next->value;
    assert(valueToReturn != NULL);
    Node<T>* headToDelete = queue->head;
    queue->head = queue->head->next;
    delete headToDelete;
    ticket_lock_unlock(queue->headLock);
    return valueToReturn;
};
#endif // NO_C_QUEUE_H