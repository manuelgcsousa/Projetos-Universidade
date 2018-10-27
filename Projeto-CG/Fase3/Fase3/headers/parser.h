#ifndef CG_TP_PARSER_H
#define CG_TP_PARSER_H

#include <iostream>
#include <string.h>
#include "tinyxml2.h"
#include "structs.h"

using namespace tinyxml2;
using namespace std;

void parseGroup(const XMLElement* child, Group g, int i, vector<FileOper*>& files);

void parser(const char* config, vector<FileOper*>& files);

#endif //CG_TP_PARSER_H
