
#ifndef ALLOCATOR_H
#define ALLOCATOR_H
#include "stddef.h"
#include <cstdint>
#include <atomic>
#include <pthread.h>
#include "concurreny_utils/ticket_lock.h"
using namespace std;
#define NODE_MAGIC          0xABCDABDD
#define SIZE_MASK           0x0000FFFF

#define NON_MANGED_BIT_MASK     0X8U // a non managed objects is an object that is not a NoCRuntimeObjectContainer ;; the only object that uses this right now is the RuntimeDimension object 
#define PINNED_BIT_MASK     0X4U
#define MARKED_BIT_MASK     0X2U
#define INUSE_BIT_MASK      0x1U

struct Header {
public:
    // todo :: size_t will take the architecture size
    // not sure if these objects will need to be that big 
    size_t magic;
    size_t availableSize;
    size_t size_prev_data;
    // TODO :: Mark this as debug flag  
    // inuse [0:0]
    // marked [1:1] 
    // pinned [2:2]
    // nonmanged [3:3]
    uint32_t meta_data;
};

struct HeapNode {
public:
    Header header;
    HeapNode* next;
    HeapNode* prev;
};

struct FreeList {
public:
    HeapNode* head;
};

struct HeapStats {
    size_t total_live_data_in_bytes;
    size_t total_heap_size;
    int num_pages_allocated;
    int num_blocks_allocated;
    int num_blocks_freed;
};

struct Page {
    Page* next_page;
    HeapNode* node_start;
    char* page_end;
};


// We have to give the page node 
struct GlobalFreePageList {
    TicketLock* lock;

    // Protected by ticket lock 
    Page* head;
    Page* tail;
    // end protected by ticket lock

    GlobalFreePageList() {
        this->lock = new TicketLock();
        this->head = NULL;
        this->tail = NULL;
    }
};

// Should be local heap to the thread 
struct Heap {
public:
    HeapStats stats;
    Page root_page;
    Page* tail_page;
    FreeList freeList;
};

enum struct GC_State {
    Collection,
    NotCollecting,
};

struct GC_Flag {
    // Exclusive owernship
    atomic<GC_State>* state;
    pthread_mutex_t* lock; // to be use for the gc_cond;
    pthread_cond_t* gc_cond;

    ~GC_Flag() {
        delete this->state;
        delete this->lock;
        delete this->gc_cond;
    }
};


enum struct GC_DEBUG_LOG {
    VERBOSE,
    NONE,
};


GC_Flag* create_gc_flags();
Heap* init_heap();
void* noC_alloc(Heap* heap, size_t size);
void* noC_alloc_pin(Heap * heap, size_t request_size);

void* noC_alloc_pin_non_managed(Heap * heap, size_t request_size);

void noC_free(Heap* heap, HeapNode* nodeToFree);
void noC_free(Heap* heap, HeapNode* nodeToFree);
void garbage_collect(GC_DEBUG_LOG log_level);
void print_heap_stats(Heap * heap);

// Pinning objects -- To construct one root object; we create the root and iterate over the children and allocate them 
// The root needs to be pinned ( not collectable ) so it does not get free'd if we need more memory to allocate a child since the root object is not rooted in some reference object  
void pin_object(void* runtimeObject);
void unpin_object(void* runtimeObject);

// Cheeky utilities 
void assert_node(HeapNode* node);
void print_heap_stats(Heap* heap);
void assert_heap_integrity(Heap* heap);
void assert_heap_integrity(Heap * heap, bool print_data);



#endif // ALLOCATOR_H