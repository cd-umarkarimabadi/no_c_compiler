# noC Compiler

The noC compiler is a statically typed langauge that is compiled down to a virtual stack based instruction set. 

It supports the following

- Struct-based OOP (no classes or inheritance)
- Full Generic Support for struct definitions and functions
- Full functional programming support with closures and high level functions
- Integrated error handling as language primitives with `throws` `try` and `defer`
- Virtual Threads as the basis for threading with non-blocking I/O
- Garbage Collection for memory management

---
**IMPORTANT**

* **Video Guide:** Watch the [YouTube video](https://www.youtube.com/watch?v=SQRuOVZjaeA) for a detailed walkthrough of how everything works.
* **Full Docs:** Reference `/docs/compiler_doc.pdf` for the complete explanation of the langauge.
---

Example program that shows nearly all the features

Taken from `integration_tests.noc`

```
typedef function run_test_func :: () -> void;

struct test_case {
    string name;
    run_test_func test_case_fn;
}

function test_case_add(array_list<test_case> test_cases, 
                      string test_case_name, 
                      run_test_func test_case_func) {
                        
    test_case = new test_case();
    test_case.name = test_case_name;
    test_case.test_case_fn = test_case_func;
    array_list_add(test_cases, test_case);
}

function gc_collect_verbose() {
    gc_collect(GC_LOG_LEVEL.VERBOSE);
}

function run_all_test_cases() {

    function run_all_test_cases_inner() throws {

        test_cases = new array_list<test_case>();
        init_array_list(test_cases);

        test_case_add(test_cases, "run_collection_demo", run_collection_demo);
        test_case_add(test_cases, "run_tree_traversal", run_tree_traversal_print);
        test_case_add(test_cases, "test_multi_dimension", test_multi_dimension);
        test_case_add(test_cases, "run collection demo", run_collection_demo);
        test_case_add(test_cases, "push_min_stack_test",  push_min_stack_test);
        test_case_add(test_cases, "tree node iterator", tree_node_iterator);
        test_case_add(test_cases, "run functional demo", run_functional_demo_test);
        test_case_add(test_cases, "eval reverse polish test", eval_reverse_polish_test); // throws is not part of fn so so call with try catch will never be called
        test_case_add(test_cases, "thread awaitable", thread_awaitable_with_reuse); // throws is not part of fn so so call with try catch will never be called
        test_case_add(test_cases, "Running GC",  gc_collect_verbose);
        test_case_add(test_cases, "Locking demo",  locking_demo_with_no_routines);

        num_test_cases = size_of_array_list(test_cases);

        for (i=0; i<num_test_cases; i++) {
            test_case = try array_list_at(test_cases, i);
            printf("Running Test %s\n", test_case.name);
            test_case_fn = test_case.test_case_fn;
            test_case_fn();
            printf("\n");
        }
    }

    err = run_all_test_cases_inner();
    if (err != null) {
        printf("Integration tests failed\n");
        print_err(err);
        return;
    } 
    printf("Test cases passed!\n");
}

```


## How to build

The langauge will only work on Linux as it takes a direct dependancy to epoll for I/O and pthreads.

Windows/mac/freebsd are not supported 

The docker file has all the dependancies that we need 


```
./docker.sh build
./docker.sh run 
```

Once inside the container you can use make to build the compiler and then use the schell script to run the comppiler

```
make 

./run_compiler.sh

```


## How to debug 

The docker file includes gdb and you can use vs code to debug 

- Make sure the docker container is running
- Install vs code remote explorer
- Connect to the docker container using vs code remote explorer 
- Make sure you have the launch.json and task.json, these can be found in the .vscode 
- Press play on "Debug Compiler" from vscode
- You should be able to debug 


## Disclaimers

You can read the the disclaimers in the compiler_doc.pdf but to summarise 

The langauge still has many bugs and is still left incomplete

* The epoll implementation is incomplete
* Garbage collection is bound to have bugs and suffers from blow up
* The lifetimes of objects has not been sorted out, everything in the compiler has global lifetimes.
* The langauge and runtime still need more testing and the code needs better refactoring

However even with these bugs the feature set is still complete and does provide an intuition into how compilers and runtimes work. 

I recommend watching the video associated with this to learn more or you can read the documentation in the docs folder  




