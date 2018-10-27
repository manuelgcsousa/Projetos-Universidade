#ifndef CLASS11_CATMULL_H
#define CLASS11_CATMULL_H

#include <math.h>

void buildRotMatrix(float *x, float *y, float *z, float *m);

void cross(float *a, float *b, float *res);

void normalize(float *a);

void multMatrixVector(float *m, float *v, float *res);

void multMatrix14(double uW[1][4], double m[4][4], double res[1][4]);

void multMatrix1441(double trans[1][4], double vH[4][1], double* res);

void getCatmullRomPoint(double t, double* p0, double* p1, double* p2, double* p3, double* pos, float *deriv);

// given  global t, returns the point in the curve
void getGlobalCatmullRomPoint(float gt, double pos[4], float *deriv, double curvePoints[][3], int size);

#endif //CLASS11_CATMULL_H
