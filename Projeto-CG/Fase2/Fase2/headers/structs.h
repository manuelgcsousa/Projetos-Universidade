#ifndef CG_TP_STRUCTS_H
#define CG_TP_STRUCTS_H

#include <iostream>
#include <vector>
#include <string.h>

using namespace std;

typedef struct loper {
    char* opName;
    double x, y, z, angle;
    struct loper* next;
} LOper;

typedef struct oper {
    double x, y, z, angle;
    char* typeOper;
} Oper;

typedef struct foper {
    char* fileName;
    vector<Oper*> operations;
} FileOper;

typedef LOper* Group[10];


void initGroup(Group g);

void concatOper(Group g, int i, char opName[], double x, double y, double z, double angle);

void addFileOper(char* fileName, Group g, vector<FileOper*>& files);

// Função auxiliar - DEBUG.
void printFileOperVector(vector<FileOper*>& files);

#endif //CG_TP_STRUCTS_H
