#define _USE_MATH_DEFINES
#include <math.h>
#ifdef __APPLE__
#include <GLUT/glut.h>
#else
#include <GL/glew.h>
#include <GL/glut.h>
#include <iostream>
#endif

/* Library Includes. */
#include <stdio.h>
#include <stdlib.h>
#include <vector>
#include <tuple>
#include <IL/il.h>

/* Personal Includes. */
#include "structs.h"
#include "parser.h"
#include "extractCoords.h"
#include "catmull.h"

using namespace std;

typedef tuple<double, double, double> coords;

/* Variáveis globais e estruturas de dados principais. */
float y[3] = { 0, 0, 1 };
double colorR = 1, colorG = 0, colorB = 1;
int mode = 1;
double camX, camY, camZ, raio = 50, alfa = 0.8, beta = 0.3;
int startX, startY, tracking = 0;

int vertexCount = 0;
double *vertexes, *normals, *textures;
GLuint buffers[1];
GLuint norms[1];
GLuint ids[1];
GLuint* texIds;

vector<Light*> lights; /* Componentes da luz a serem aplicadas no sistema. */
vector<FileOper*> files; /* Nome dos ficheiros e suas respetivas transformações. */
vector<coords> triangles; /* Coordenadas de todos os triângulos. */
vector<coords> normal_points; /* Coordenadas dos pontos das normais. */
vector<coords> textures_points; /* Coordenadas de todos os pontos das diferentes texturas. */


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

void changeCams()
{
    camX = raio * cos(beta) * sin(alfa);
    camY = raio * sin(beta);
    camZ = raio * cos(beta) * cos(alfa);
}

void processSpecialKeys(int key, int xx, int yy)
{
    switch(key) {
        case GLUT_KEY_RIGHT:
            alfa -= 0.1;
            break;

        case GLUT_KEY_LEFT:
            alfa += 0.1;
            break;

        case GLUT_KEY_UP:
            beta += 0.1;
            if (beta > 1.5) beta = 1.5;
            break;

        case GLUT_KEY_DOWN:
            beta -= 0.1;
            if (beta < -1.5) beta = -1.5;
            break;

        case GLUT_KEY_PAGE_DOWN:
            raio -= 2;
            if (raio < 0.1) raio = 0.1;
            break;

        case GLUT_KEY_PAGE_UP:
            raio += 2;
            break;

        case GLUT_KEY_F1:
            mode = !mode;
            printf("mode: %d\n", mode);
            break;
    }

    changeCams();
    glutPostRedisplay();
}

void processMouseButtons(int button, int state, int xx, int yy)
{
    if (state == GLUT_DOWN)  {
        startX = xx;
        startY = yy;
        if (button == GLUT_LEFT_BUTTON)
            tracking = 1;
        else if (button == GLUT_RIGHT_BUTTON)
            tracking = 2;
        else
            tracking = 0;
    }
    else if (state == GLUT_UP) {
        if (tracking == 1) {
            alfa += (xx - startX);
            beta += (yy - startY);
        } else if (tracking == 2) {
            raio -= yy - startY;
            if (raio < 3)
                raio = 3.0;
        }
        tracking = 0;
    }
}

void processMouseMotion(int xx, int yy)
{
    int deltaX, deltaY;
    int alphaAux, betaAux;
    int rAux;

    if (!tracking)
        return;

    deltaX = xx - startX;
    deltaY = yy - startY;

    if (tracking == 1) {
        alphaAux = alfa + deltaX;
        betaAux = beta + deltaY;

        if (betaAux > 85.0)
            betaAux = 85.0;
        else if (betaAux < -85.0)
            betaAux = -85.0;

        rAux = raio;
    } else if (tracking == 2) {
        alphaAux = alfa;
        betaAux = beta;
        rAux = raio - deltaY;
        if (rAux < 3)
            rAux = 3;
    }

    camX = rAux * sin(alphaAux * 3.14 / 180.0) * cos(betaAux * 3.14 / 180.0);
    camZ = rAux * cos(alphaAux * 3.14 / 180.0) * cos(betaAux * 3.14 / 180.0);
    camY = rAux * sin(betaAux * 3.14 / 180.0);
}

void processColors(Colors* col)
{
    GLfloat amb[4] = {0.2, 0.2, 0.2, 1.0};
    GLfloat dif[4] = {1.0, 1.0, 1.0, 1.0};
    GLfloat spec[4] = {0.0, 0.0, 0.0, 1.0};
    GLfloat emis[4] = {0.0, 0.0, 0.0, 1.0};

    if (strcmp(col->component, "diffuse") == 0) {
        dif[0] = static_cast<GLfloat>(col->r);
        dif[1] = static_cast<GLfloat>(col->g);
        dif[2] = static_cast<GLfloat>(col->b);
        dif[3] = 1;

        glMaterialfv(GL_FRONT, GL_DIFFUSE, dif);
    }

    if (strcmp(col->component, "specular") == 0) {
        spec[0] = static_cast<GLfloat>(col->r);
        spec[1] = static_cast<GLfloat>(col->g);
        spec[2] = static_cast<GLfloat>(col->b);
        spec[3] = 1;

        glMaterialfv(GL_FRONT, GL_SPECULAR, spec);
    }

    if (strcmp(col->component, "emissive") == 0) {
        emis[0] = static_cast<GLfloat>(col->r);
        emis[1] = static_cast<GLfloat>(col->g);
        emis[2] = static_cast<GLfloat>(col->b);
        emis[3] = 1;

        glMaterialfv(GL_FRONT, GL_EMISSION, emis);
    }

    if (strcmp(col->component, "ambient") == 0) {
        amb[0] = static_cast<GLfloat>(col->r);
        amb[1] = static_cast<GLfloat>(col->g);
        amb[2] = static_cast<GLfloat>(col->b);
        amb[3] = 1;

        glMaterialfv(GL_FRONT, GL_AMBIENT, amb);
    }
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

    glRotated(angle[i], op->x, op->y, op->y);
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
                    rotateTime(op, i);
            }

            if (strcmp(op->typeOper, "translate") == 0) {
                if (op->tf == nullptr)
                    glTranslated(op->x, op->y, op->z);
                else
                    translateTime(op, i);
            }
            itOper++;
        }

        if (fo->col != NULL) {
            processColors(fo->col);
        }

        glBindTexture(GL_TEXTURE_2D, texIds[i]);

        glDrawArrays(GL_TRIANGLES, (i * fo->totalVertexes), fo->totalVertexes);

        glBindTexture(GL_TEXTURE_2D, 0);

        glPopMatrix();

        i++;
        itFile++;
    }
}

void processLight()
{
    GLfloat pos[4] = {0.0, 0.0, 1.0, 0.0};
    GLfloat amb[4] = {0.2, 0.2, 0.2, 1.0};
    GLfloat diff[4] = {1.0, 1.0, 1.0, 1.0};
    GLfloat spotDir[3] = {0.0, 0.0, -1.0};

    vector<Light*>::iterator it;
    for (it = lights.begin(); it != lights.end(); it++) {
        Light* lt = *it;

        if (strcmp(lt->source, "POINT") == 0) {
            pos[0] = static_cast<GLfloat>(lt->x);
            pos[1] = static_cast<GLfloat>(lt->y);
            pos[2] = static_cast<GLfloat>(lt->z);
            pos[3] = 1;

            glEnable(GL_LIGHT0);
            glLightfv(GL_LIGHT0, GL_POSITION, pos);
            glLightfv(GL_LIGHT0, GL_AMBIENT, amb);
            glLightfv(GL_LIGHT0, GL_DIFFUSE, diff);
        } else if (strcmp(lt->source, "DIRECTIONAL") == 0) {
            pos[0] = static_cast<GLfloat>(lt->x);
            pos[1] = static_cast<GLfloat>(lt->y);
            pos[2] = static_cast<GLfloat>(lt->z);
            pos[3] = 0;

            glEnable(GL_LIGHT0);
            glLightfv(GL_LIGHT0, GL_POSITION, pos);
            glLightfv(GL_LIGHT0, GL_AMBIENT, amb);
            glLightfv(GL_LIGHT0, GL_DIFFUSE, diff);
        } else if (strcmp(lt->source, "SPOTLIGHT") == 0) {
            spotDir[0] = static_cast<GLfloat>(lt->x);
            spotDir[1] = static_cast<GLfloat>(lt->y);
            spotDir[2] = static_cast<GLfloat>(lt->z);

            pos[0] = static_cast<GLfloat>(lt->x);
            pos[1] = static_cast<GLfloat>(lt->y);
            pos[2] = static_cast<GLfloat>(lt->z);
            pos[3] = 1;

            glEnable(GL_LIGHT0);
            glLightfv(GL_LIGHT0, GL_POSITION, pos);
            glLightfv(GL_LIGHT0, GL_DIFFUSE, diff);
            glLightfv(GL_LIGHT0, GL_SPOT_DIRECTION, spotDir);
            glLightf(GL_LIGHT0, GL_SPOT_CUTOFF, 45.0);
            glLightf(GL_LIGHT0, GL_SPOT_EXPONENT, 0.0);
        }
    }
}

void renderScene(void)
{
    // clear buffers
    glClearColor(0.0f,0.0f,0.0f,0.0f);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    // set the camera
    glLoadIdentity();

    gluLookAt(camX, camY, camZ,
              0.0, 0.0, 0.0,
              0.0, 1.0, 0.0);

    processLight();

    glBindBuffer(GL_ARRAY_BUFFER, buffers[0]);
    glVertexPointer(3, GL_DOUBLE, 0, 0);

    glBindBuffer(GL_ARRAY_BUFFER, norms[0]);
    glNormalPointer(GL_DOUBLE, 0, 0);

    glBindBuffer(GL_ARRAY_BUFFER, ids[0]);
    glTexCoordPointer(2, GL_DOUBLE, 0, 0);

    // put drawing instructions and geometric transformations here
    drawArrays();

    // Reset
    float amb[4] = {0.2, 0.2, 0.2, 1.0};
    float dif[4] = {1.0, 1.0, 1.0, 1.0};
    float spec[4] = {0.0, 0.0, 0.0, 1.0};
    float emi[4] = {0.0, 0.0, 0.0, 1.0};

    glMaterialfv(GL_FRONT, GL_AMBIENT, amb);
    glMaterialfv(GL_FRONT, GL_DIFFUSE, dif);
    glMaterialfv(GL_FRONT, GL_SPECULAR, spec);
    glMaterialfv(GL_FRONT, GL_EMISSION, emi);

    // End of frame
    glutSwapBuffers();
}

int loadTexture(string s)
{
    unsigned int t, tw, th;
    unsigned char *texData;
    unsigned int texID;

    ilInit();
    ilEnable(IL_ORIGIN_SET);
    ilOriginFunc(IL_ORIGIN_LOWER_LEFT);
    ilGenImages(1, &t);
    ilBindImage(t);
    ilLoadImage((ILstring)s.c_str());
    tw = ilGetInteger(IL_IMAGE_WIDTH);
    th = ilGetInteger(IL_IMAGE_HEIGHT);
    ilConvertImage(IL_RGBA, IL_UNSIGNED_BYTE);
    texData = ilGetData();

    glGenTextures(1, &texID);

    glBindTexture(GL_TEXTURE_2D, texID);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);

    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, tw, th, 0, GL_RGBA, GL_UNSIGNED_BYTE, texData);
    glGenerateMipmap(GL_TEXTURE_2D);

    glBindTexture(GL_TEXTURE_2D, 0);

    return texID;
}

bool isMark(coords aux_1, coords aux_2, coords aux_3)
{
    coords mark = make_tuple(DBL_MIN, DBL_MIN, DBL_MIN);

    return (aux_1 == mark && aux_2 == mark && aux_3 == mark);
}

void fillArrays()
{
    glGenBuffers(1, buffers);
    glGenBuffers(1, norms);
    glGenBuffers(1, ids);

    int pV = 0, pN = 0, pT = 0;
    bool flag;
    int i = 0;

    vertexes = (double*)malloc(sizeof(double) * (vertexCount * 3));
    normals = (double*)malloc(sizeof(double) * (vertexCount * 3));
    textures = (double*)malloc(sizeof(double) * (vertexCount * 2));

    vector<coords>::iterator itTri;
    vector<coords>::iterator itNorm;
    vector<coords>::iterator itTex;

    itTri = triangles.begin();
    itNorm = normal_points.begin();
    itTex = textures_points.begin();

    while (itTri != triangles.end()) {
        flag = false;
        while (!flag) {
            coords tri_1 = *itTri; itTri++;
            coords tri_2 = *itTri; itTri++;
            coords tri_3 = *itTri; itTri++;

            coords norm_1 = *itNorm; itNorm++;
            coords norm_2 = *itNorm; itNorm++;
            coords norm_3 = *itNorm; itNorm++;

            coords tex_1 = *itTex; itTex++;
            coords tex_2 = *itTex; itTex++;
            coords tex_3 = *itTex; itTex++;

            if (!isMark(tri_1, tri_2, tri_3)) {
                /* Primeiro ponto do triângulo. */
                vertexes[pV]     = get<0>(tri_1);
                vertexes[pV + 1] = get<1>(tri_1);
                vertexes[pV + 2] = get<2>(tri_1);

                normals[pN]      = get<0>(norm_1);
                normals[pN + 1]  = get<1>(norm_1);
                normals[pN + 2]  = get<2>(norm_1);

                textures[pT]     = get<0>(tex_1);
                textures[pT + 1] = get<1>(tex_1);

                /* Segundo ponto do triângulo. */
                vertexes[pV + 3] = get<0>(tri_2);
                vertexes[pV + 4] = get<1>(tri_2);
                vertexes[pV + 5] = get<2>(tri_2);

                normals[pN + 3]  = get<0>(norm_2);
                normals[pN + 4]  = get<1>(norm_2);
                normals[pN + 5]  = get<2>(norm_2);

                textures[pT + 2] = get<0>(tex_2);
                textures[pT + 3] = get<1>(tex_2);

                /* Terceiro ponto do triângulo. */
                vertexes[pV + 6] = get<0>(tri_3);
                vertexes[pV + 7] = get<1>(tri_3);
                vertexes[pV + 8] = get<2>(tri_3);

                normals[pN + 6]  = get<0>(norm_3);
                normals[pN + 7]  = get<1>(norm_3);
                normals[pN + 8]  = get<2>(norm_3);

                textures[pT + 4] = get<0>(tex_3);
                textures[pT + 5] = get<1>(tex_3);

                pV += 9; pN += 9; pT += 6;
            } else {
                flag = true;
            }
        }
    }

    glBindBuffer(GL_ARRAY_BUFFER, buffers[0]);
    glBufferData(GL_ARRAY_BUFFER, vertexCount * 8 * 3, vertexes, GL_STATIC_DRAW);

    glBindBuffer(GL_ARRAY_BUFFER, norms[0]);
    glBufferData(GL_ARRAY_BUFFER, vertexCount * 8 * 3, normals, GL_STATIC_DRAW);

    glBindBuffer(GL_ARRAY_BUFFER, ids[0]);
    glBufferData(GL_ARRAY_BUFFER, vertexCount * 8 * 2, textures, GL_STATIC_DRAW);

    free(vertexes);
    free(normals);
}

int main(int argc, char** argv)
{
    /* Coloca no "vector<FileOper*> files" o nome de todos os ficheiros, e respetivas transformações,
     * bem como a informação sobre as luzes. */
    parser("../scenes/solar_system_orbits.xml", files, lights);

    /* Percore o "vector<FileOper*> files", abrindo cada ficheiro encontrado,
     * e guardando as coordenadas extraídas no "vector<coords> triangles". */
    extractCoords(&vertexCount, files, triangles, normal_points, textures_points);

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

    changeCams();

    // put here the registration of the keyboard and mouse callbacks
    glutSpecialFunc(processSpecialKeys);
    glutMouseFunc(processMouseButtons);
    glutMotionFunc(processMouseMotion);

#ifndef __APPLE__
    glewInit();
#endif

    /* Preenche os arrays double* vertexes, double* normals e double* textures
     * com os vértices a serem desenhados sobre a forma de VBO's. */
    fillArrays();

    /* Extrai todos os nomes dos ficheiros das texturas, atribuindo um ID a cada textura. */
    int i = 0;
    texIds = (GLuint*)malloc(sizeof(GLuint) * files.size());
    vector<FileOper*>::iterator it;
    for (it = files.begin(); it != files.end(); i++, it++) {
        FileOper* fo = *it;
        if (fo->texture != NULL) {
            char path[50] = "../textures/";
            char* tex = strcat(path, fo->texture);
            texIds[i] = loadTexture(string(tex));
        }
    }

    // OpenGL settings
    glEnable(GL_DEPTH_TEST);
    glEnable(GL_CULL_FACE);
    glCullFace(GL_BACK);
    glEnableClientState(GL_VERTEX_ARRAY);
    glEnableClientState(GL_NORMAL_ARRAY);
    glEnableClientState(GL_TEXTURE_COORD_ARRAY);
    glEnable(GL_LIGHTING);
    glEnable(GL_COLOR_MATERIAL);
	glEnable(GL_TEXTURE_2D);

    // enter GLUT's main cycle
    glutMainLoop();

    return 1;
}
