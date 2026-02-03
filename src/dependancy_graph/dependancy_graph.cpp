
#include <string>
#include <ast/ast.h>
#include "collections/collections.h"
#include "parser/parser.h"
using namespace std;

enum class vertex_colour {
    white,
    grey,
    black
};

struct file_parse_unit_dg {
    file_parse_unit* unit;
    vertex_colour color;
};

struct file_src_mapping {
    // Make that unique to the map 
    map<string, file_parse_unit_dg*>* src_mapping;
    ~file_src_mapping() {
        if (src_mapping) {
            for (auto kv : *src_mapping) {
                delete(kv.second);
            }
            delete src_mapping;
        }
    }
};

file_src_mapping* create_mapping() {
    file_src_mapping* mapping = new file_src_mapping();
    mapping->src_mapping = new map<string, file_parse_unit_dg*>();
    return mapping;
}

file_parse_unit_dg* create_unit_dg(file_parse_unit* unit, vertex_colour color) {
    file_parse_unit_dg* unit_dg = new file_parse_unit_dg();
    unit_dg->unit = unit;
    unit_dg->color = color;
    return unit_dg;
}

file_parse_unit_dg* search_by_name(file_src_mapping* mapping, string file_name) {
    if (mapping->src_mapping->count(file_name) == 0) {
        return NULL;
    }
    return mapping->src_mapping->at(file_name);
}

void dfs(file_parse_unit_dg* unit,
    file_src_mapping* mapping,
    queue<file_parse_unit*>* depedancy_order,
    queue<file_parse_unit*>* cycle_detection
) {
    cycle_detection->push(unit->unit);
    unit->color = vertex_colour::grey;
    for (string import : *unit->unit->imports) {
        auto dependancy_unit = search_by_name(mapping, import);
        if (dependancy_unit == NULL) {
            throw runtime_error("import " + import + " does not exist ");
        }
        if (dependancy_unit->color == vertex_colour::grey) {
            string full_path = "[";
            while (!cycle_detection->empty()) {
                full_path += cycle_detection->front()->file_name;
                full_path += ", ";
                cycle_detection->pop();
            }
            full_path += dependancy_unit->unit->file_name + " ]";
            throw runtime_error("cyclic depedancy detected " + full_path);
        }
        if (dependancy_unit->color == vertex_colour::black) {
            continue;
        }
        dfs(dependancy_unit, mapping, depedancy_order, cycle_detection);
    }
    unit->color = vertex_colour::black;
    cycle_detection->pop();
    depedancy_order->push(unit->unit);
}

queue<file_parse_unit*>* create_depedancy_order(vector<file_parse_unit*>* all_files) {
    file_src_mapping* mapping = create_mapping();
    for (auto unit : *all_files) {
        file_parse_unit_dg* unit_dg = create_unit_dg(unit, vertex_colour::white);
        mapping->src_mapping->insert_or_assign(unit_dg->unit->file_name, unit_dg);
    }
    file_parse_unit_dg* main_file_unit_dg = search_by_name(mapping, "main.noc");
    if (main_file_unit_dg == NULL) {
        throw runtime_error("main.noc file does not exist'");
    }

    queue<file_parse_unit*>* depedancy_order = new queue<file_parse_unit*>();
    queue<file_parse_unit*>* cycle_detection = new queue<file_parse_unit*>();
    dfs(main_file_unit_dg, mapping, depedancy_order, cycle_detection);

    delete mapping;
    delete cycle_detection;
    return depedancy_order;
}