#include "extractCoords.h"

void extractCoords(vector<FileOper*>& files, vector<coords>& triangles)
{
    int cc = 0;
    double tmpCoords[3];

    for (vector<FileOper*>::iterator it = files.begin(); it != files.end(); it++) {
        FileOper* tmp = *it;

        ifstream trigsFile;
        trigsFile.open(tmp->fileName);

        string line;
        if (trigsFile.is_open()) {
            while (getline(trigsFile, line)) {
                string sTmp;
                for (int i = 0; i <= line.length(); i++) {
                    if (line[i] == ' ' || line[i] == '\0') {
                        tmpCoords[cc] = stod(sTmp);
                        sTmp.clear();
                        cc++;
                    } else {
                        sTmp.push_back(line[i]);
                    }
                }
                coords aux = make_tuple(tmpCoords[0], tmpCoords[1], tmpCoords[2]);
                triangles.push_back(aux);
                tmp->totalVertexes++;

                cc = 0;
            }
        }
        coords mark1 = make_tuple(DBL_MIN, DBL_MIN, DBL_MIN);
        triangles.push_back(mark1);
        coords mark2 = make_tuple(DBL_MIN, DBL_MIN, DBL_MIN);
        triangles.push_back(mark2);
        coords mark3 = make_tuple(DBL_MIN, DBL_MIN, DBL_MIN);
        triangles.push_back(mark3);
    }
}