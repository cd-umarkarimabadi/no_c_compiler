#include "allocator/allocator.h"
#include <iostream>
#include <assert.h>
#include <iostream>
#include "utils/utils.h"
#include "noCThread/noCThread.h"
#include <stddef.h>
#include "allocator/allocator.h"
#include "allocator/gc.h"
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <unistd.h>
#include <string.h>
#include "type_checker/type_checker.h"
#include "scheduler/scheduler.h"
#include <iostream>
#include <assert.h>
#include "scheduler/sleep_lock.h"

static uint16_t FREE_CONSTANT = 0xFEEE;

size_t size_of_smallest_noc_object() {
    return get_size_of_smallest_object();
}

#define SAFE_ITERATE_NEXT(currentNode, page) ({ \
    char* nextNodeAddress = get_next_node_address(currentNode); \
    bool isNodeValid = isNodeInPage(page, nextNodeAddress); \
    if (!isNodeValid) { \
        break; \
    } \
    nextNodeAddress; \
})

void merge_heap(Heap* heap);

size_t assert_size_of_smallest_no_c_object() {
    size_t sizeof_smallest_object = size_of_smallest_noc_object();
    assert(sizeof_smallest_object > sizeof(FREE_CONSTANT));
    return sizeof_smallest_object;
}
// Get the page size from the OS, should be 4K
size_t DEFAULT_PAGE_SIZE = getpagesize();
size_t __ = assert_size_of_smallest_no_c_object();
#define USE_GUARD_PAGES 0


size_t get_available_data_in_page(Page* page) {
    return (page->page_end - (char*)page) - sizeof(Page);
}

size_t get_page_size(Page* page) {
    return (page->page_end - (char*)page);
}

// TODO :: Gloabl free list should come from mmap and not from c++ allocator 
GlobalFreePageList* freePageList = new GlobalFreePageList();

Page* get_page_from_free_page_list(GlobalFreePageList* freePageList, size_t request_size) {
    auto defer_unlock = TicketLockUnLockDefer(freePageList->lock);

    Page* current_page = freePageList->head;
    Page* page_before_found_page = NULL;
    Page* found_page = NULL;

    while (current_page != NULL) {
        size_t available_data_in_page = get_available_data_in_page(current_page);
        if (available_data_in_page >= request_size) {
            found_page = current_page;
            break;
        }
        page_before_found_page = current_page;
        current_page = current_page->next_page;
    }

    if (found_page == NULL) {
        return NULL;
    }

    if (found_page == freePageList->head) {
        if (freePageList->head == freePageList->tail) {
            freePageList->head = NULL;
            freePageList->tail = NULL;
        }
        else {
            freePageList->head = freePageList->head->next_page;
        }
    }

    else if (found_page == freePageList->tail) {
        freePageList->tail = page_before_found_page;
        freePageList->tail->next_page = NULL;
    }
    else {
        page_before_found_page->next_page = found_page->next_page;
    }

    found_page->next_page = NULL;
    return found_page;
}

inline bool is_node_in_use(Header* header) {
    return (header->meta_data & INUSE_BIT_MASK) != 0;
}

inline bool is_node_marked(Header* header) {
    return (header->meta_data & MARKED_BIT_MASK) != 0;
}

inline bool is_node_non_managed_object(HeapNode* node) {
    return (node->header.meta_data & NON_MANGED_BIT_MASK) != 0;
}

inline void reset_pin_bit(HeapNode* node) {
    node->header.meta_data = node->header.meta_data & ~PINNED_BIT_MASK;
}

inline void reset_mark_bit(HeapNode* node) {
    node->header.meta_data = node->header.meta_data & ~MARKED_BIT_MASK;
}

inline void reset_in_use_bit(HeapNode* node) {
    node->header.meta_data = node->header.meta_data & ~INUSE_BIT_MASK;
}

inline void reset_node_bits(HeapNode* node) {
    assert_node(node);
    reset_mark_bit(node);
    reset_in_use_bit(node);
    reset_pin_bit(node);
}

inline bool is_pinned(HeapNode* nodeToFree) {
    return (nodeToFree->header.meta_data & PINNED_BIT_MASK) != 0;
}

NoCRuntimeObjectContainer* mapHeapNodeToNoCObject(HeapNode* heapNode) {
    char* object_address = (char*)(heapNode)+sizeof(HeapNode);
    return (NoCRuntimeObjectContainer*)(object_address);
}

HeapNode* mapNoCObjectToHeapNode(char* runtimeObject) {
    char* node_address = runtimeObject - sizeof(HeapNode);
    HeapNode* node = (HeapNode*)node_address;
    assert_node(node);
    return node;
}

HeapNode* mapNoCObjectToHeapNode(NoCRuntimeObjectContainer* runtimeObject) {
    return mapNoCObjectToHeapNode((char*)(runtimeObject));
}

void pin_object(void* runtimeObject) {
    auto heapNode = mapNoCObjectToHeapNode((NoCRuntimeObjectContainer*)runtimeObject);
    assert(heapNode);
    heapNode->header.meta_data |= PINNED_BIT_MASK;
}

void unpin_object(void* runtimeObject) {
    auto heapNode = mapNoCObjectToHeapNode((NoCRuntimeObjectContainer*)runtimeObject);
    assert(heapNode);
    heapNode->header.meta_data = heapNode->header.meta_data & ~PINNED_BIT_MASK;
}

void set_marked_bit_flag(NoCRuntimeObjectContainer * runtimeObject) {
    assert(runtimeObject != &nullObject);
    HeapNode* node = mapNoCObjectToHeapNode(runtimeObject);
    node->header.meta_data |= MARKED_BIT_MASK;
}

void set_non_managed_bit_flag(HeapNode* node) {
    node->header.meta_data |= NON_MANGED_BIT_MASK;
}

void set_marked_bit_flag(RuntimeDimension * dimension) {
    HeapNode* node = mapNoCObjectToHeapNode((char*)(dimension));
    node->header.meta_data |= MARKED_BIT_MASK;
}

void set_marked_bit_flag(BoxedDescriptor * descriptor) {
    HeapNode* node = mapNoCObjectToHeapNode((char*)(descriptor));
    node->header.meta_data |= MARKED_BIT_MASK;
}

void assert_node(HeapNode * node) {
    assert(node->header.magic == NODE_MAGIC);
}

void print_node(HeapNode* node) {
    bool in_use = is_node_in_use(&node->header);
    cout << "| NODE , inuse :: " << in_use << " size :: " << node->header.availableSize << " block addr :: " << node << " | ->" << flush;
}

void add_page_to_free_page_list(GlobalFreePageList* freePageList, Page* page) {
    if (freePageList->head == NULL) {
        freePageList->head = page;
        freePageList->head->next_page = freePageList->tail;
        freePageList->tail = freePageList->head;
    }
    else {
        freePageList->tail->next_page = page;
        freePageList->tail = page;
    }
}

void set_marked_bit_flag(NoCRuntimeObjectContainer * runtimeObject);
void set_marked_bit_flag(RuntimeDimension* dimension);
void mark(NoCRuntimeObjectContainer* runtimeObject, SymbolTable* symbolTable);
char* get_next_node_address(HeapNode* currentNode);
bool isNodeInPage(Page* page, char* node_address);

GC_Flag* create_gc_flags() {
    GC_Flag* flag = new GC_Flag();
    pthread_mutex_t* lock = new pthread_mutex_t();
    pthread_cond_t* cond = new pthread_cond_t();
    flag->gc_cond = cond;
    flag->lock = lock;
    flag->state = new atomic<GC_State>();
    flag->state->store(GC_State::NotCollecting);
    return flag;
}

bool isObjectMarked(NoCRuntimeObjectContainer * runtimeObject) {
    HeapNode* node = mapNoCObjectToHeapNode(runtimeObject);
    return is_node_marked(&node->header);
}

uint16_t* get_free_magic_address(HeapNode* node) {
    size_t free_magic_size = sizeof(FREE_CONSTANT);
    assert(node->header.availableSize >= free_magic_size);
    auto objectAddress = mapHeapNodeToNoCObject(node);
    auto free_magic = (uint16_t*)(objectAddress);
    return free_magic;
}

void assert_selected_node(HeapNode* node) {
    assert(node->header.magic == NODE_MAGIC);
    assert(!is_node_marked(&node->header));
    auto free_magic_p = get_free_magic_address(node);
    assert(*free_magic_p == FREE_CONSTANT);
    assert(!is_node_marked(&node->header));
    assert(!is_node_in_use(&node->header));
}

void assert_selected_node_not_free(HeapNode * node) {
    assert(node->header.magic == NODE_MAGIC);
    auto free_magic_p = get_free_magic_address(node);
    assert(*free_magic_p != FREE_CONSTANT);
    assert(is_node_in_use(&node->header));
}

void assert_free_list_is_free(FreeList * freeList) {
    HeapNode* node = freeList->head;
    while (node != NULL) {
        bool in_use = is_node_in_use(&node->header);
        assert(!in_use);
        assert_node(node);
        node = node->next;
    }
}

char* get_next_node_address(HeapNode * currentNode) {
    assert_node(currentNode);
    char* nextNodeAddress = (char*)(currentNode)+sizeof(HeapNode) + currentNode->header.availableSize;
    return nextNodeAddress;
}

bool isBoundaryTagValid(Header * header, Page * page) {
    if (header == NULL) {
        return false;
    }
    if ((void*)header >= (void*)page && (void*)header < page->page_end) {
        return (header->magic == NODE_MAGIC);
    }
    return false;
}

bool isNodeInPage(Page * page, char* node_address) {
    // We need to make sure we at least have minimum sizeof(HeapNode) to read from the node
    if ((node_address + sizeof(HeapNode)) >= page->page_end) {
        return false;
    }

    return isBoundaryTagValid((Header*)node_address, page);
}

void assert_free_list(FreeList* freeList, bool print) {
    if (print) {
        cout << "-- FREE LIST -- " << endl;
        cout << endl;
    }
    HeapNode* node = freeList->head;
    while (node != NULL) {
        assert_node(node);
        if (print) {
            print_node(node);
        }
        node = node->next;
    }
    if (print) {
        cout << endl;
        cout << "-- END FREE LIST -- " << endl;
    }
}

void assert_heap_integrity(Heap * heap) {
    assert_heap_integrity(heap, false);
    assert_free_list(&heap->freeList, false);
}

void assert_heap_integrity(Heap * heap, bool print_data) {
    int num_heap_blocks_traversed = 0;
    int num_pages_traversed = 0;
    Page* page = &heap->root_page;
    HeapNode* currentNode = NULL;
    while (page != NULL) {
        if (print_data) {
            cout << "page [" << num_pages_traversed << "]" << endl;
        }
        num_pages_traversed++;
        currentNode = page->node_start;
        while (currentNode != NULL) {
            bool isNodeValid = isNodeInPage(page, (char*)currentNode);
            if (!isNodeValid) {
                break;
            }
            if (print_data) {
                print_node(currentNode);
            }
            num_heap_blocks_traversed++;
            char* nextNodeAddress = get_next_node_address(currentNode);
            isNodeValid = isNodeInPage(page, (char*)nextNodeAddress);
            if (!isNodeValid) {
                break;
            }
            currentNode = (HeapNode*)nextNodeAddress;;
        }
        page = page->next_page;
    }
    assert(num_heap_blocks_traversed == heap->stats.num_blocks_allocated);
    assert(num_pages_traversed == heap->stats.num_pages_allocated);
}


void mark_noC_thread(noCThread* thread) {
    set_marked_bit_flag((NoCRuntimeObjectContainer*)thread->heapRefTracker);
    await_handle_t* await_handle = (await_handle_t*)thread->tcb->await_handle;

    assert(await_handle != NULL);

    if (await_handle->error != &nullObject) {
        set_marked_bit_flag(await_handle->error);
    }

    if (await_handle->error != &nullObject) {
        set_marked_bit_flag(await_handle->result);
    }
}

void mark_thread_stack(noCThread* thread) {
    ActivationFrame* frame = thread->tcb->currentActivationFrame;
    while (frame != NULL) {
        assert(frame->frameRefTracker != NULL);
        if (isObjectMarked((NoCRuntimeObjectContainer*)frame->frameRefTracker)) {
            frame = frame->previousFrame;
            continue;
        }
        set_marked_bit_flag((NoCRuntimeObjectContainer*)frame->frameRefTracker);
        for (auto &localVar : *frame->storage) {
            mark(localVar.second, thread->tcb->symbolTable);
        }
        frame = frame->previousFrame;
    }
}

void mark(NoCRuntimeObjectContainer * runtimeObject, SymbolTable * symbolTable) {
    if (runtimeObject == NULL) {
        assert_never_reaches("null objects in should be the nullObject in runtime_object.h and not an the actual NULL value");
    }

    if (runtimeObject->type == RuntimeObjectType::Null) {
        assert(runtimeObject == &nullObject);
        return;
    }

    if (isObjectMarked(runtimeObject)) {
        return;
    }

    set_marked_bit_flag(runtimeObject);

    switch (runtimeObject->type) {
    case RuntimeObjectType::StructObjType:
    case RuntimeObjectType::NativeStringStruct:
    {
        StructLayout * structLayout = NULL;
        // TODO :: Change this to a look up
        for (auto structEntryPair : *symbolTable->structSymbolTable->table) {
            auto structEntry = structEntryPair.second;
            if (structEntry->identifier_index == runtimeObject->type_number) {
                structLayout = structEntry;
                break;
            }
        }
        assert(structLayout != NULL);
        NoCStructContainer* structRuntimeObject = (NoCStructContainer*)runtimeObject;
        for (auto field : *structLayout->fields) {
            NoCRuntimeObjectContainer* childRuntimeObject = (NoCRuntimeObjectContainer*)structRuntimeObject->fields[field->mem_offset];
            if (childRuntimeObject != NULL) {
                mark(childRuntimeObject, symbolTable);
            }
        }
        break;
    }
    case RuntimeObjectType::Closure:
    {
        NoCStructContainer* closure = (NoCStructContainer*)runtimeObject;
        assert(closure->header.type == RuntimeObjectType::Closure);
        NoCRuntimeObjectContainer* fp = (NoCRuntimeObjectContainer*)closure->fields[0];;
        assert(fp->type == RuntimeObjectType::Int);
        mark(fp, symbolTable);
        ActivationFrame * static_link = (ActivationFrame*)closure->fields[1];
        assert(static_link != NULL);

        while (static_link != NULL) {

            set_marked_bit_flag((NoCRuntimeObjectContainer*)static_link->owningThread->heapRefTracker);
            if (isObjectMarked((NoCRuntimeObjectContainer*)static_link->frameRefTracker)) {
                static_link = static_link->staticLink; // 
                continue;
            }

            set_marked_bit_flag((NoCRuntimeObjectContainer*)static_link->frameRefTracker);

            for (auto localVar : *static_link->storage) {
                mark(localVar.second, symbolTable);
            }
            if (static_link->staticLink == NULL) {
                break;
            }
            static_link = static_link->staticLink;
        }

        break;
    }
    case RuntimeObjectType::Double:
    case RuntimeObjectType::Int:
    case RuntimeObjectType::Char:
    case RuntimeObjectType::Bool:
    case RuntimeObjectType::NonNativeStringWrapper:
    {
        break;
    }

    case RuntimeObjectType::ExternalHandle:
    {
        auto handle = (NoCExternalHandleRuntimeObject*)(runtimeObject);
        if (handle->handleType == ExternalHandleType::Future) {
            noCThread* future_target = (noCThread*)handle->handle;

            if (!isObjectMarked((NoCRuntimeObjectContainer*)future_target->heapRefTracker)) {
                mark_noC_thread(future_target);
                mark_thread_stack(future_target);
            }
        }

        else if (handle->handleType == ExternalHandleType::Socket) {
            BoxedDescriptor* descriptor = (BoxedDescriptor*)handle->handle;
            set_marked_bit_flag(descriptor);
        }
        break;
    }
    case RuntimeObjectType::NDimensionVector:
    {
        NoCVectorRuntimeObject* vectorRuntime = (NoCVectorRuntimeObject*)runtimeObject;
        auto dimension = vectorRuntime->slice.dimension;
        while (dimension != NULL) {
            set_marked_bit_flag(dimension);
            dimension = dimension->next;
        }
        size_t len = get_len_of_vector(vectorRuntime);
        for (size_t i = 0; i < len; i++) {
            NoCRuntimeObjectContainer* element = (NoCRuntimeObjectContainer*)vectorRuntime->start_of_vec[i];
            mark(element, symbolTable);
        }
        break;
    }
    default:
    {
        throw runtime_error("RuntimeObjectType missing case or worse -- heap corruption");
    }
    }
}

void add_to_free_list(Heap* heao, HeapNode * elementToAdd);
void init_node(HeapNode * newNode, size_t availableSize, size_t size_prev_node);

struct page_alloc_result {
    char* page_mem;
    size_t page_size;
};

page_alloc_result alloc_page_block_from_OS(size_t request_size) {
    // < PAGE(4KB aligned )|GUARD_PAGE::PROT:PROT_READ(4KB) >   
    int number_pages = (request_size / DEFAULT_PAGE_SIZE);
    number_pages = number_pages == 0 ? 1 : number_pages;
    size_t size_without_guard = (DEFAULT_PAGE_SIZE * number_pages);
    size_t total_size = size_without_guard + (USE_GUARD_PAGES * DEFAULT_PAGE_SIZE);
    char* page_mem = (char*)mmap(NULL, total_size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (page_mem == MAP_FAILED) {
        throw runtime_error("alloc_page_block_from_OS :: Failed to mmap block");
    }
    if (USE_GUARD_PAGES) {
        char* guard_page = page_mem + size_without_guard;
        int result = mprotect(guard_page, DEFAULT_PAGE_SIZE, PROT_READ);
        if (result != 0) {
            throw runtime_error("alloc_page_block_from_OS:: Failed to mmprotect guard page");
        }
    }
    return page_alloc_result{
        .page_mem = page_mem,
        .page_size = size_without_guard
    };
}

Heap* init_heap() {

    // 8MB heap size, assuming default page size is 4KB 
    size_t initial_heap_size = 2048 * DEFAULT_PAGE_SIZE;
    page_alloc_result alloc_result = alloc_page_block_from_OS(initial_heap_size);
    void* page_mem = alloc_result.page_mem;
    size_t page_size = alloc_result.page_size;
    /*
      <- Heap Header ---------------------------------------------------------------------------------------->|
      | <HeapStats stats> | page::next_page | page::node_start | page::page_end | tailpage* | freelist::head )| <first_node> |
    */
    size_t sizeofHeapHeader = sizeof(Heap);
    void* first_node_in_heap_address = (char*)(page_mem)+sizeofHeapHeader;
    char* page_end = (char*)(page_mem)+page_size;

    size_t available_space_for_node = page_size - sizeofHeapHeader - sizeof(HeapNode);
    init_node((HeapNode*)first_node_in_heap_address, available_space_for_node, 0);
    HeapNode* first_node_in_heap = (HeapNode*)first_node_in_heap_address;
    Heap* heap = (Heap*)page_mem;
    heap->root_page.node_start = first_node_in_heap;
    heap->root_page.page_end = page_end;
    heap->freeList.head = first_node_in_heap;
    heap->stats.total_heap_size = page_size;

    assert_node(heap->freeList.head);
    assert(heap->root_page.node_start->header.magic == NODE_MAGIC);
    heap->tail_page = &heap->root_page;
    heap->stats.num_pages_allocated = 1;
    heap->stats.num_blocks_allocated = 1;
    return heap;
}

HeapNode* mapFreePageToHeapNode(Page * page, size_t page_size) {
    char* node_address = (char*)(page)+sizeof(Page);
    HeapNode * node_head = (HeapNode*)node_address;
    size_t node_avail_size = page_size - sizeof(Page);
    init_node(node_head, node_avail_size, 0);
    return node_head;
}

void insert_page_in_tail(Heap * heap, Page * page) {
    heap->tail_page->next_page = page;
    heap->tail_page = page;
}

void init_node(HeapNode * newNode, size_t size, size_t size_prev_node) {
    newNode->header.meta_data = 0;
    newNode->header.magic = NODE_MAGIC;
    newNode->header.size_prev_data = size_prev_node;
    newNode->header.availableSize = size;
    newNode->next = NULL;
    newNode->prev = NULL;
    auto free_magic_address = get_free_magic_address(newNode);
    assert(*free_magic_address != FREE_CONSTANT);
    *free_magic_address = FREE_CONSTANT;
}

HeapNode* find_first_free_node(FreeList * freeList, size_t request_size) {
    HeapNode* selectedNode = freeList->head;
    while (selectedNode != NULL) {
        if (selectedNode->header.availableSize >= request_size) {
            break;
        }
        selectedNode = selectedNode->next;
    }
    return selectedNode;
}

void add_to_free_list(Heap* heap, HeapNode * elementToAdd) {
    FreeList* freeList = &heap->freeList;
    HeapNode* currentHead = freeList->head;
    if (currentHead == NULL) {
        freeList->head = elementToAdd;
        return;
    }
    elementToAdd->next = currentHead;
    elementToAdd->prev = NULL;
    currentHead->prev = elementToAdd;
    freeList->head = elementToAdd;

}

size_t get_full_node_size(HeapNode * node) {
    return node->header.availableSize + sizeof(HeapNode);
}

Header* get_prev_node_header(HeapNode * node) {
    char* node_address = (char*)(node);
    char* prev_header_address = node_address - node->header.size_prev_data - sizeof(HeapNode);
    Header* header = (Header*)(prev_header_address);
    return header;
}

void remove_from_free_list(FreeList * freeList, HeapNode * node_to_remove) {
    // Not connected to free list 
    if (node_to_remove != freeList->head && (node_to_remove->prev == NULL && node_to_remove->next == NULL)) {
        return;
    }
    // [head] [maybenull] 
    if (freeList->head == node_to_remove) {
        freeList->head = freeList->head->next;
        if (freeList->head) {
            freeList->head->prev = NULL;
        }
    }
    else {
        // [not null] [node] [maybe null]
        node_to_remove->prev->next = node_to_remove->next;
        if (node_to_remove->next) {
            node_to_remove->next->prev = node_to_remove->prev;
        }
    }
    node_to_remove->prev = NULL;
    node_to_remove->next = NULL;
}

HeapNode* map_to_heap_node(Header * header) {
    return (HeapNode*)header;
}

void clear_page_data(Page* page) {
    char* start_of_object = (char*)page->node_start;
    size_t available_data_in_page = get_available_data_in_page(page);
    memset(start_of_object, 0, available_data_in_page);
}

void clear_heap_node_data(HeapNode* node) {
    char* start_of_object = (char*)mapHeapNodeToNoCObject(node);
    memset(start_of_object, 0, node->header.availableSize);
}

// TODO :: The itearotor becomes broken if you try to update the nodes sizes as you are iterating 
// For now will just call this merge_heap function and merge right side neighbours :: If I move to segregated fits I will not need this 
void merge_heap(Heap * heap) {
    Page* page = &heap->root_page;
    while (page != NULL) {
        HeapNode * currentNode = page->node_start;
        while (currentNode != NULL) {
            if (is_node_in_use(&currentNode->header)) {
                currentNode = (HeapNode*)SAFE_ITERATE_NEXT(currentNode, page);
                continue;
            }
            remove_from_free_list(&heap->freeList, currentNode);

            // Keep iterating over the boundary tags if they are adjacent and free will be merge it  
            Header * rightBoundaryTag = (Header*)get_next_node_address(currentNode);
            while (isBoundaryTagValid(rightBoundaryTag, page)) {
                if (is_node_in_use(rightBoundaryTag)) {
                    break;
                }

                HeapNode* rightNode = map_to_heap_node(rightBoundaryTag);
                remove_from_free_list(&heap->freeList, rightNode);

                currentNode->header.availableSize += get_full_node_size(rightNode);
                heap->stats.num_blocks_allocated--;

                char* next_node_address = get_next_node_address(currentNode);
                Header* right_header_after_merge = (Header*)next_node_address;
                if (isBoundaryTagValid(right_header_after_merge, page)) {
                    rightBoundaryTag = right_header_after_merge;
                }
                else {
                    break;
                }
            }

            // Clear all data and add back to free list 
            char* start_of_object = (char*)mapHeapNodeToNoCObject(currentNode);
            memset(start_of_object, 0, currentNode->header.availableSize);
            auto free_magic_address = get_free_magic_address(currentNode);
            *free_magic_address = FREE_CONSTANT;

            add_to_free_list(heap, currentNode);

            // Update the right side with the correct size 
            char* next_node_address = get_next_node_address(currentNode);
            Header* right_header_after_collapse = (Header*)next_node_address;
            if (isBoundaryTagValid(right_header_after_collapse, page)) {
                right_header_after_collapse->size_prev_data = currentNode->header.availableSize;
                currentNode = (HeapNode*)next_node_address;
            }
            else {
                break;
            }
        }
        page = page->next_page;
    }
}

void noC_free(HeapNode * nodeToFree) {
    nodeToFree->header.meta_data = 0;
}

void print_heap_stats(Heap * heap) {
    cout << "=== Heap Stats ===" << endl;
    cout << "Live Data In Bytes :: " << heap->stats.total_live_data_in_bytes << endl;
    cout << "Total Heap Size In Bytes :: " << heap->stats.total_heap_size << endl;
    cout << "Heap utlisation % " << ((double)heap->stats.total_live_data_in_bytes / (double)heap->stats.total_heap_size) * 100 << endl;
    cout << "Number pages allocated " << heap->stats.num_pages_allocated << endl;
    cout << "=== Heap Stats ===" << endl;
}

void free_heap_pages(Heap * heap) {
    ticket_lock_lock(freePageList->lock);
    // Note (umar) We never free the first page because the thread can be reused and we always keep the first page 
    Page* page = heap->root_page.next_page;
    while (page != NULL) {
        Page* page_to_clear = page;
        page = page->next_page; // now we can clear the page_to_clear
        clear_page_data(page_to_clear);
        add_page_to_free_page_list(freePageList, page_to_clear);
    }
    ticket_lock_unlock(freePageList->lock);

    // Reinitialise first node in page and reinit free list :: The first page is embdedded into the heap and you cannot call clear_page_data(page);
    HeapNode* first_node_in_root_page = heap->root_page.node_start;
    size_t real_page_size = heap->root_page.page_end - (char*)first_node_in_root_page;
    memset(first_node_in_root_page, 0, real_page_size);

    size_t available_space_for_node = heap->root_page.page_end - (char*)first_node_in_root_page - sizeof(HeapNode);
    init_node(first_node_in_root_page, available_space_for_node, 0);

    heap->root_page.next_page = NULL;
    heap->tail_page = &heap->root_page;

    // reinit stats 
    heap->stats.num_blocks_allocated = 1;
    heap->stats.num_blocks_freed = 0;
    heap->stats.num_pages_allocated = 1;
    heap->stats.total_heap_size = available_space_for_node; // TODO ?? is this the correct size??
    heap->stats.total_live_data_in_bytes = 0;

    // reinit free list 
    heap->freeList.head = NULL;
    add_to_free_list(heap, first_node_in_root_page);

}

void garbage_collect(GC_DEBUG_LOG log_level) {
    if (USE_GUARD_PAGES) {
        cout << "[WARN] GC Collection will not run when using guard pages" << endl;
        return;
    }

    // TODO :: HACK Nasty thread local hack because refactoring code is going to be a pain since the code was never designed to take a reference
    // to the worker but overtime it eventually required a reference 
    Worker* worker = ThreadLocalWorkerContext::get()->worker;
    Scheduler* scheduler = ThreadLocalWorkerContext::get()->scheduler;
    assert(worker != NULL);
    assert(scheduler != NULL);

    GC_Flag* gc_flags = worker->gc_flags;
    GC_State notCollecting = GC_State::NotCollecting;
    GC_State collecting = GC_State::Collection;
    if (!gc_flags->state->compare_exchange_strong(notCollecting, collecting)) {
        pthread_mutex_lock(gc_flags->lock);
        worker->is_paused_for_gc = true;
        while (gc_flags->state->load() != GC_State::NotCollecting) {
            pthread_cond_wait(gc_flags->gc_cond, gc_flags->lock);
        }
        pthread_mutex_unlock(gc_flags->lock);
        return;
    }
    assert(gc_flags->state->load() == GC_State::Collection);

    // Wait for all workers to go to sleep 
    // We actually dont care if all workers are "actually" asleep because this is controlled by the conditional flag 
    // The gc_request_flag flag is a non atomic and lets us know that we no heap mutations will occur
    for (auto worker : *scheduler->workers) {
        worker->gc_request_flag = true;
    }
    worker->is_paused_for_gc = true;
    size_t num_workers_stopped = 0;
    while (num_workers_stopped != scheduler->workers->size()) {
        num_workers_stopped = 0;
        for (auto worker : *scheduler->workers) {
            if (worker->is_paused_for_gc) {
                num_workers_stopped++;
            }
        }
    }
    // Mark all threads in all workers
    for (auto worker : *scheduler->workers) {
        for (auto thread : *worker->thread_list) {
            // if thread is finishd no point doing a scan but a threads activation frame might still be refernenced
            // It can only be references through future objects so if a thread holds a future then the thread will be marked  

            if (getThreadState(thread) == ThreadState_E::Finished || thread->tcb->hasNoRefrence) {
                continue;
            }

            mark_noC_thread(thread);
            mark_thread_stack(thread);
        }
    }

    // Free all Heap node in all threads in all workers, if a variables has been shared 
    // It would have been marked in the previous phase
    for (auto worker : *scheduler->workers) {
        for (auto thread : *worker->thread_list) {

            // Marked for deallocation/reuse and no point scanning again
            if (thread->tcb->hasNoRefrence) {
                continue;
            }

            Heap* heap = thread->tcb->heap;
            heap->stats.total_live_data_in_bytes = 0;

            assert_heap_integrity(heap);
            Page* page = &heap->root_page;
            while (page != NULL) {
                HeapNode* currentNode = page->node_start;
                while (currentNode != NULL) {
                    if (!is_node_marked(&currentNode->header)) {
                        if (is_node_in_use(&currentNode->header)) {

                            // For any nodes that are pinned we will still keep the marked as inuse
                            if (is_pinned(currentNode)) {
                                goto ITERATE_NEXT_NODE;
                            }

                            if (is_node_non_managed_object(currentNode)) {
                                noC_free(currentNode);
                                goto ITERATE_NEXT_NODE;
                            }

                            auto runtimeObject = mapHeapNodeToNoCObject(currentNode);
                            // Closure references are handle like any other normal object; the mark phase will trace all their activation frames and keep them referenced
                            // if (runtimeObject->type == RuntimeObjectType::Closure) 
                            if (runtimeObject->type == RuntimeObjectType::ExternalHandle) {
                                NoCExternalHandleRuntimeObject* handle = (NoCExternalHandleRuntimeObject*)runtimeObject;
                                switch (handle->handleType) {
                                case ExternalHandleType::ActivationFrame:
                                {
                                    ActivationFrame* frame = (ActivationFrame*)handle->handle;
                                    delete frame;
                                    break;
                                }
                                case ExternalHandleType::NoCThread:
                                {
                                    noCThread* thread_from_handle = (noCThread*)handle->handle;
                                    thread_from_handle->tcb->hasNoRefrence = true;
                                    break;
                                }
                                case ExternalHandleType::Future:
                                case ExternalHandleType::Socket:
                                case ExternalHandleType::File:
                                {
                                    // Future|Socket|File do not allocate any non native objects - they just hold pointers to "managed" objects 
                                    // TODO :: Implement managed objects 
                                    break;
                                }
                                case ExternalHandleType::Lock:
                                {
                                    sleep_lock* lock = (sleep_lock*)handle->handle;
                                    delete lock;
                                    break;
                                }
                                default:
                                {
                                    assert_never_reaches("GC_Collection :: Missed handling case of external handle type");
                                }
                                }
                            }
                            noC_free(currentNode);
                        }
                    }
                    else {
                        reset_mark_bit(currentNode);
                        heap->stats.total_live_data_in_bytes += currentNode->header.availableSize + sizeof(HeapNode);
                    }
                ITERATE_NEXT_NODE:;
                    char* nextNodeAddress = SAFE_ITERATE_NEXT(currentNode, page);
                    currentNode = (HeapNode*)nextNodeAddress;
                }
                page = page->next_page;
            }

            if (log_level == GC_DEBUG_LOG::VERBOSE) {
                print_heap_stats(heap);
            }

            if (thread->tcb->hasNoRefrence) {
                free_heap_pages(thread->tcb->heap);
            }
            else {
                merge_heap(heap);
                assert_heap_integrity(heap);
            }

        }
    }

    for (auto worker : *scheduler->workers) {
        worker->gc_request_flag = false;
    }
    // Hold the lock for the duration of the broadcast
    pthread_mutex_lock(gc_flags->lock);
    gc_flags->state->store(GC_State::NotCollecting);
    int broad_cast_result = pthread_cond_broadcast(gc_flags->gc_cond);
    if (broad_cast_result != 0) {
        throw runtime_error("gc_collect::pthread_cond_broadcast failed, this should never happen");
    }
    pthread_mutex_unlock(gc_flags->lock);
}

void grow_heap(Heap * heap, size_t request_size);
void grow_heap(Heap * heap) {
    grow_heap(heap, 0);
}

void grow_heap(Heap * heap, size_t request_size) {

    // These are just random numbers to grow the heap * 2 and *3 come from nowhere 
    size_t new_heap_size = heap->stats.total_heap_size * 2;
    if (request_size > new_heap_size) {
        new_heap_size = request_size * 3;
    }

    Page* new_page = get_page_from_free_page_list(freePageList, new_heap_size);
    if (new_page == NULL) {
        page_alloc_result result = alloc_page_block_from_OS(new_heap_size);
        new_page = (Page*)result.page_mem;
        new_page->page_end = result.page_mem + result.page_size;
        char* node_address = (char*)(new_page)+sizeof(Page);
        HeapNode * node_head = (HeapNode*)node_address;
        new_page->node_start = node_head;
    }

    size_t page_size = get_page_size(new_page);
    size_t node_avail_size = page_size - sizeof(Page) - sizeof(HeapNode);
    HeapNode * node_head = (HeapNode*)new_page->node_start;
    init_node(node_head, node_avail_size, 0);

    assert(((char*)node_head) + sizeof(HeapNode) + node_head->header.availableSize == (char*)new_page->page_end);
    assert_node(new_page->node_start);

    heap->stats.num_pages_allocated++;
    heap->stats.num_blocks_allocated++;
    heap->stats.total_heap_size += get_page_size(new_page);

    insert_page_in_tail(heap, new_page);
    add_to_free_list(heap, new_page->node_start);
}

HeapNode* find_first_free_node_if_fail_grow_heap(Heap * heap, size_t request_size) {
    HeapNode* selectedNode = find_first_free_node(&heap->freeList, request_size);
    if (selectedNode == NULL) {
        garbage_collect(GC_DEBUG_LOG::NONE);
        // Regardlesss we are just gonna grow the heap after a garbage collection
        grow_heap(heap);
        selectedNode = find_first_free_node(&heap->freeList, request_size);
        if (selectedNode == NULL) {
            return NULL;
        }
    }
    assert_selected_node(selectedNode);
    return selectedNode;
}

void* noC_alloc_pinned(Heap * heap, size_t request_size, bool pin_object) {
    if (USE_GUARD_PAGES) {
        // For guard pages we just get the block available on the free list and then remove this 
        // The data is aligned towards the end of the block
        HeapNode* selectedNode = find_first_free_node_if_fail_grow_heap(heap, request_size);
        if (selectedNode == NULL) {
            return NULL;
        }
        selectedNode->header.availableSize = selectedNode->header.availableSize;
        selectedNode->header.meta_data |= INUSE_BIT_MASK;

        char* end_address_of_selected_node = (char*)selectedNode + sizeof(HeapNode) + selectedNode->header.availableSize;
        char* return_obj_pos = end_address_of_selected_node - request_size;
        memset(return_obj_pos, 0, request_size);
        remove_from_free_list(&heap->freeList, selectedNode);
        return return_obj_pos;
    }
    void* noC_alloc_helper(Heap * heap, size_t request_size, bool pin_object);
    return noC_alloc_helper(heap, request_size, pin_object);
}

void* noC_alloc(Heap * heap, size_t request_size) {
    return noC_alloc_pinned(heap, request_size, false);
}

void* noC_alloc_pin_non_managed(Heap * heap, size_t request_size) {
    void* object_to_return = noC_alloc_pinned(heap, request_size, true);
    HeapNode* objects_heap_node = mapNoCObjectToHeapNode((char*)object_to_return);
    set_non_managed_bit_flag(objects_heap_node);
    return object_to_return;
}

void* noC_alloc_pin(Heap * heap, size_t request_size) {
    return noC_alloc_pinned(heap, request_size, true);
}

void* noC_alloc_helper(Heap * heap, size_t request_size, bool pin_object) {
    FreeList* freeList = &heap->freeList;
    assert_free_list_is_free(freeList);

    HeapNode* selectedNode = find_first_free_node_if_fail_grow_heap(heap, request_size);
    if (selectedNode == NULL) {
        return NULL;
    }
    char* selected_node_raw_pointer = (char*)(selectedNode);
    size_t node_plus_request_size = request_size + sizeof(HeapNode);
    // we are substracting the sizeof(HeapNode) becaus ethat is the spcae of the node taken after the split  
    int smallest_no_c_object = size_of_smallest_noc_object();
    int space_left_after_split = selectedNode->header.availableSize - request_size - sizeof(HeapNode);

    // Split node 
    if (space_left_after_split >= smallest_no_c_object) {
        selectedNode->header.availableSize = request_size;
        HeapNode* newNode = (HeapNode*)(selected_node_raw_pointer + node_plus_request_size);
        init_node(newNode, space_left_after_split, request_size);
        assert(newNode);
        heap->stats.num_blocks_allocated++;
        add_to_free_list(heap, newNode);
    }
    assert_selected_node(selectedNode);
    remove_from_free_list(freeList, selectedNode);
    selectedNode->header.meta_data |= INUSE_BIT_MASK;
    if (pin_object) {
        selectedNode->header.meta_data |= PINNED_BIT_MASK;
    }

    heap->stats.total_live_data_in_bytes += request_size + sizeof(HeapNode);
    char* return_obj_pos = selected_node_raw_pointer + sizeof(HeapNode);
    memset(return_obj_pos, 0, request_size);
    assert_free_list(&heap->freeList, false);
    return return_obj_pos;
}