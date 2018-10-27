#ifndef CG_TP_EXTRACTCOORDS_H
#define CG_TP_EXTRACTCOORDS_H

#include <iostream>
#include <fstream>
#include <tuple>
#include <cfloat>
#include "structs.h"

using namespace std;

typedef tuple<double, double, double> coords;

void extractCoords(vector<FileOper*>& files, vector<coords>& triangles);

#endif //CG_TP_EXTRACTCOORDS_H
