#ifndef CG_TP_EXTRACTCOORDS_H
#define CG_TP_EXTRACTCOORDS_H

#include <iostream>
#include <fstream>
#include <vector>
#include <tuple>

using namespace std;

typedef tuple<double, double, double> coords;

void extractCoords(char** files, vector<coords>& triangles);

#endif //CG_TP_EXTRACTCOORDS_H
