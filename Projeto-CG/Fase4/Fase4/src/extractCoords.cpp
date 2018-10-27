#include "extractCoords.h"

void extractCoords(int* vertexCount,
                   vector<FileOper*>& files,
                   vector<coords>& triangles,
                   vector<coords>& normal_points,
                   vector<coords>& textures_points)
{
    int count = 1;
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

                if (count % 3 == 1) {
                    // Coordenadas de figuras.
                    triangles.push_back(aux);
                    tmp->totalVertexes++;
                    (*vertexCount)++;
                } else if (count % 3 == 2) {
                    // Coordenadas das normais.
                    normal_points.push_back(aux);
                } else if (count % 3 == 0){
                    // Coordenadas de texturas.
                    textures_points.push_back(aux);
                }
                count++;
                cc = 0;
            }
        }

        coords mark1 = make_tuple(DBL_MIN, DBL_MIN, DBL_MIN);
        triangles.push_back(mark1);
        normal_points.push_back(mark1);
        textures_points.push_back(mark1);

        coords mark2 = make_tuple(DBL_MIN, DBL_MIN, DBL_MIN);
        triangles.push_back(mark2);
        normal_points.push_back(mark2);
        textures_points.push_back(mark2);

        coords mark3 = make_tuple(DBL_MIN, DBL_MIN, DBL_MIN);
        triangles.push_back(mark3);
        normal_points.push_back(mark3);
        textures_points.push_back(mark3);
    }
}