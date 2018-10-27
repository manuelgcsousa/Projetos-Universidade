#include "catmull.h"

void buildRotMatrix(float *x, float *y, float *z, float *m)
{
    m[0] = x[0]; m[1] = x[1]; m[2] = x[2]; m[3] = 0;
    m[4] = y[0]; m[5] = y[1]; m[6] = y[2]; m[7] = 0;
    m[8] = z[0]; m[9] = z[1]; m[10] = z[2]; m[11] = 0;
    m[12] = 0; m[13] = 0; m[14] = 0; m[15] = 1;
}

void cross(float *a, float *b, float *res)
{
    res[0] = a[1]*b[2] - a[2]*b[1];
    res[1] = a[2]*b[0] - a[0]*b[2];
    res[2] = a[0]*b[1] - a[1]*b[0];
}

void normalize(float *a)
{
    double l = sqrt(a[0]*a[0] + a[1] * a[1] + a[2] * a[2]);
    a[0] = a[0]/l;
    a[1] = a[1]/l;
    a[2] = a[2]/l;
}

void multMatrixVector(float *m, float *v, float *res)
{
    for (int j = 0; j < 4; ++j) {
        res[j] = 0;
        for (int k = 0; k < 4; ++k) {
            res[j] += v[k] * m[j * 4 + k];
        }
    }

}

void multMatrix14(double uW[1][4], double m[4][4], double res[1][4])
{
    res[0][0] = uW[0][0] * m[0][0] + uW[0][1] * m[1][0] + uW[0][2] * m[2][0] + uW[0][3] * m[3][0];
    res[0][1] = uW[0][0] * m[0][1] + uW[0][1] * m[1][1] + uW[0][2] * m[2][1] + uW[0][3] * m[3][1];
    res[0][2] = uW[0][0] * m[0][2] + uW[0][1] * m[1][2] + uW[0][2] * m[2][2] + uW[0][3] * m[3][2];
    res[0][3] = uW[0][0] * m[0][3] + uW[0][1] * m[1][3] + uW[0][2] * m[2][3] + uW[0][3] * m[3][3];
}

void multMatrix1441(double trans[1][4], double vH[4][1], double* res)
{
    *res = trans[0][0] * vH[0][0] + trans[0][1] * vH[1][0] + trans[0][2] * vH[2][0] + trans[0][3] * vH[3][0];
}

void getCatmullRomPoint(double t, double* p0, double* p1, double* p2, double* p3, double* pos, float *deriv)
{
    double x, y, z;

    double res[1][4];

    double time[1][4] = { { powf(t,3), powf(t,2), t, 1 } };

    // catmull-rom matrix
    double m[4][4] = { {-0.5f,  1.5f, -1.5f,  0.5f},
                       { 1.0f, -2.5f,  2.0f, -0.5f},
                       {-0.5f,  0.0f,  0.5f,  0.0f},
                       { 0.0f,  1.0f,  0.0f,  0.0f} };

    double px[4][1] = { { p0[0] }, { p1[0] }, { p2[0] }, { p3[0] } };
    double py[4][1] = { { p0[1] }, { p1[1] }, { p2[1] }, { p3[1] } };
    double pz[4][1] = { { p0[2] }, { p1[2] }, { p2[2] }, { p3[2] } };

    // Compute res = T * M
    multMatrix14(time, m, res);

    // Compute x = res * px
    multMatrix1441(res, px, &x);

    // Compute y = res * py
    multMatrix1441(res, py, &y);

    // Compute z = res * pz
    multMatrix1441(res, pz, &z);

    pos[0] = x;
    pos[1] = y;
    pos[2] = z;

    // compute deriv = T' * A
    float a[4][4] = { 0 };
    float pxx[4];
    float pyy[4];
    float pzz[4];
    float dT[4] = { 3 * powf(t,2), static_cast<float>(2 * t), 1, 0 };

    pxx[0] = px[0][0]; pxx[1] = px[1][0]; pxx[2] = px[2][0]; pxx[3] = px[3][0];
    pyy[0] = py[0][0]; pyy[1] = py[1][0]; pyy[2] = py[2][0]; pyy[3] = py[3][0];
    pzz[0] = pz[0][0]; pzz[1] = pz[1][0]; pzz[2] = pz[2][0]; pzz[3] = pz[3][0];

    multMatrixVector(reinterpret_cast<float *>(*m), pxx, a[0]);
    multMatrixVector(reinterpret_cast<float *>(*m), pyy, a[1]);
    multMatrixVector(reinterpret_cast<float *>(*m), pzz, a[2]);
    multMatrixVector(*a, dT, deriv);
}

// given  global t, returns the point in the curve
void getGlobalCatmullRomPoint(float gt, double pos[4], float *deriv, double curvePoints[][3], int size)
{
    double t = gt * size; // this is the real global t
    int index = floor(t);  // which segment
    t = t - index; // where within  the segment

    // indices store the points
    int indices[4];
    indices[0] = (index + size-1)%size;
    indices[1] = (indices[0]+1)%size;
    indices[2] = (indices[1]+1)%size;
    indices[3] = (indices[2]+1)%size;

    getCatmullRomPoint(t, curvePoints[indices[0]], curvePoints[indices[1]], curvePoints[indices[2]], curvePoints[indices[3]], pos, deriv);
}
