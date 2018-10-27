#include "extractCoords.h"

void extractCoords(char** files, vector<coords>& triangles)
{
    int cc = 0;
    double tmpCoords[3];

    for (int i = 0; files[i] != nullptr; i++) {
        ifstream trigsFile;
        trigsFile.open(files[i]);

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
                cc = 0;
            }
        }
    }
}