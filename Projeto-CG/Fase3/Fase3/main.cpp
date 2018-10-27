#include<stdio.h>
#include<stdlib.h>

#define _USE_MATH_DEFINES
#include <math.h>
#include <vector>

// #include <IL/il.h>

#ifdef __APPLE__
#include <GLUT/glut.h>
#else
#include <GL/glew.h>
#include <GL/glut.h>
#include <iostream>
#endif

/* Library Includes. */
#include <math.h>
#include <stdio.h>
#include <vector>
#include <tuple>

/* Personal Includes. */
#include "structs.h"
#include "parser.h"
#include "extractCoords.h"

using namespace std;

typedef tuple<double, double, double> coords;

/* Variáveis globais e estruturas de dados principais. */
float alpha = 1;
float beta = 1;
float r = 70;
float y[3] = { 0, 0, 1 };
double colorR = 1, colorG = 0, colorB = 1;

double** vertexes;
GLuint buffers[20];

vector<FileOper*> files; /* Nome dos ficheiros e suas respetivas transformações. */
vector<coords> triangles; /* Coordenadas de todos os triângulos. */


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


void changeSize(int w, int h)
{
    // Prevent a divide by zero, when window is too short
    // (you cant make a window with zero width).
    if (h == 0) h = 1;

    // compute window's aspect ratio
    float ratio = w * 1.0 / h;

    // Set the projection matrix as current
    glMatrixMode(GL_PROJECTION);
    // Load Identity Matrix
    glLoadIdentity();

    // Set the viewport to be the entire window
    glViewport(0, 0, w, h);

    // Set perspective
    gluPerspective(45.0f, ratio, 1.0f, 1000.0f);

    // return to the model view matrix mode
    glMatrixMode(GL_MODELVIEW);
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


int coordsToArray(vector<coords>& points, double curvePoints[][3])
{
    int i = 0;
    vector<coords>::iterator it;

    for (it = points.begin(); it != points.end(); it++) {
        coords aux = *it;

        curvePoints[i][0] = get<0>(aux);
        curvePoints[i][1] = get<1>(aux);
        curvePoints[i][2] = get<2>(aux);

        i++;
    }

    return i;
}


void translateTime(Oper* op, int i)
{
    static double tt[20];
    static double time = 0;
    static double elapsedTime[20];
    int size;
    TimeTransform* tf  = op->tf;
    double curvePoints[tf->points.size()][3];

    size = coordsToArray(tf->points, curvePoints);

    double pos[4];
    float deriv[4];

    glBegin(GL_LINE_LOOP);
        int npoints = 100;
        for (int i = 0; i < npoints; i++) {
            getGlobalCatmullRomPoint((double) i / npoints, pos, deriv, curvePoints, size);
            glVertex3d(pos[0], pos[1], pos[2]);
        }
    glEnd();

    getGlobalCatmullRomPoint(tt[i], pos, deriv, curvePoints, size);

    // Ver o tempo
    if (time < files.size()) {
        double ttt = glutGet(GLUT_ELAPSED_TIME);
        elapsedTime[i] = 1/(tf->time*1000);
        time++;
    }
    tt[i] += elapsedTime[i];

    // Derivada nao necessaria
    /*
    float *x = deriv;
    normalize(x);

    float z[4];
    cross(x,y,z);
    normalize(z);

    cross(z,x,y);
    normalize(y);

    float rM[16];
    buildRotMatrix(x, y, z, rM);
    */

    glTranslated(pos[0], pos[1], pos[2]);

    //glMultMatrixf(rM);
}

void rotateTime(Oper* op, int i)
{
    TimeTransform* tf = op->tf;
    double time = tf->time;

    double static angle[20];

    angle[i] += 360/(time*1000);

    glRotated(angle[i],op->x,op->y,op->y);
}

void drawArrays()
{
    int i = 0;
    vector<FileOper*>::iterator itFile;
    vector<Oper*>::iterator itOper;

    itFile = files.begin();

    while (itFile != files.end()) {
        FileOper* fo = *itFile;

        glPushMatrix();

        itOper = fo->operations.begin();
        while (itOper != fo->operations.end()) {
            Oper* op = *itOper;

            if (strcmp(op->typeOper, "scale") == 0)
                glScaled(op->x, op->y, op->z);

            if (strcmp(op->typeOper, "rotate") == 0) {
                if(op->tf == nullptr)
                    glRotated(op->angle, op->x, op->y, op->z);
                else
                    rotateTime(op,i);
            }

            if (strcmp(op->typeOper, "translate") == 0) {
                if (op->tf == nullptr)
                    glTranslated(op->x, op->y, op->z);
                else
                    translateTime(op,i);
            }

            itOper++;
        }

        // Desenhar array de vértices.
        glBindBuffer(GL_ARRAY_BUFFER, buffers[i]);
        glBufferData(GL_ARRAY_BUFFER, fo->totalVertexes * 8 * 3, vertexes[i], GL_STATIC_DRAW);
        glVertexPointer(3, GL_DOUBLE, 0, 0);
        glDrawArrays(GL_TRIANGLES, 0, fo->totalVertexes);

        glPopMatrix();

        i++;
        itFile++;
    }
}


void renderScene(void)
{
	// clear buffers
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	// set the camera
	glLoadIdentity();
    /* gluLookAt(5.0, 5.0, 5.0,
		      0.0, 0.0, 0.0,
              0.0f, 1.0f, 0.0f); */

    gluLookAt(r * cos(beta) * sin(alpha), r * sin(beta), r * cos(beta) * cos(alpha),
              0.0, 0.0, 0.0,
              0.0f, 1.0f, 0.0f);



    glEnableClientState(GL_VERTEX_ARRAY);
    int size = static_cast<int>(files.size());
    glGenBuffers(size, buffers);

    // put drawing instructions and geometric transformations here
    drawArrays();


	// End of frame
	glutSwapBuffers();
}


void processSpecialKeys(int key, int xx, int yy)
{
    switch (key) {
        case GLUT_KEY_RIGHT:
            alpha += 0.05;
            glutPostRedisplay();
            break;
        case GLUT_KEY_LEFT:
            alpha -= 0.1;
            glutPostRedisplay();
            break;
        case GLUT_KEY_UP:
            beta += 0.1;
            glutPostRedisplay();
            break;
        case GLUT_KEY_DOWN:
            beta -= 0.1;
            glutPostRedisplay();
            break;
        default:
            cout << "Não conheço esse comando!" << "\n";
    }
}


bool isMark(coords aux_1, coords aux_2, coords aux_3)
{
    coords mark = make_tuple(DBL_MIN, DBL_MIN, DBL_MIN);

    return (aux_1 == mark && aux_2 == mark && aux_3 == mark);
}

void fillVertexes()
{
    int p, i = 0;
    bool flag;
    vertexes = (double**)malloc(sizeof(double*) * files.size());

    vector<FileOper*>::iterator itFile;
    vector<coords>::iterator itTri;

    itFile = files.begin();
    itTri = triangles.begin();
    while (itTri != triangles.end()) {
        FileOper* fo = *itFile;

        vertexes[i] = (double*)malloc(sizeof(double) * fo->totalVertexes * 3);

        p = 0;
        flag = false;
        while (!flag) {
            coords aux_1 = *itTri; itTri++;
            coords aux_2 = *itTri; itTri++;
            coords aux_3 = *itTri; itTri++;

            if (!isMark(aux_1, aux_2, aux_3)) {
                // Primeiro ponto do triângulo.
                vertexes[i][p]     = get<0>(aux_1);
                vertexes[i][p + 1] = get<1>(aux_1);
                vertexes[i][p + 2] = get<2>(aux_1);

                // Segundo ponto do triângulo.
                vertexes[i][p + 3] = get<0>(aux_2);
                vertexes[i][p + 4] = get<1>(aux_2);
                vertexes[i][p + 5] = get<2>(aux_2);

                // Terceiro ponto do triângulo.
                vertexes[i][p + 6] = get<0>(aux_3);
                vertexes[i][p + 7] = get<1>(aux_3);
                vertexes[i][p + 8] = get<2>(aux_3);

                p += 9;
            } else {
                flag = true;
                i++;
            }
        }
        itFile++;
    }
}


int main(int argc, char** argv)
{
    /* Coloca no "vector<FileOper*> files" o nome de todos os ficheiros, e respetivas transformações. */
    parser("../solar_system_orbits.xml", files);

    /* Percore o "vector<FileOper*> files", abrindo cada ficheiro encontrado,
     * e guardando as coordenadas extraídas no "vector<coords> triangles". */
    extractCoords(files, triangles);

    fillVertexes();

    //printFileOperVector(files); // Função auxiliar para imprimir informação do vector<FileOper*>.

    // init GLUT and the window
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_DEPTH | GLUT_DOUBLE | GLUT_RGBA);
    glutInitWindowPosition(100, 100);
    glutInitWindowSize(1920, 1080);
    glutCreateWindow("CG - TP");

    // Required callback registry
    glutDisplayFunc(renderScene);
    glutReshapeFunc(changeSize);
    glutIdleFunc(renderScene);

    // put here the registration of the keyboard callbacks
    glutSpecialFunc(processSpecialKeys);

#ifndef __APPLE__
    glewInit();
#endif

    // OpenGL settings
    glEnable(GL_DEPTH_TEST);
    glEnable(GL_CULL_FACE);
    glCullFace(GL_BACK);
    //glFrontFace(GL_CW);
    glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);

    // enter GLUT's main cycle
    glutMainLoop();

    return 1;
}
