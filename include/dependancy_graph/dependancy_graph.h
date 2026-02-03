#ifndef __DEPDENDANCY_GRAPH_H__
#define __DEPDENDANCY_GRAPH_H__
#include "parser/parser.h"
#include "collections/collections.h"

queue<file_parse_unit*>* create_depedancy_order(vector<file_parse_unit*>* all_files);

#endif // __DEPDENDANCY_GRAPH_H__