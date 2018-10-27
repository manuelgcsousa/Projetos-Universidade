#ifdef __APPLE__
#include <GLUT/glut.h>
#else
#include <GL/glut.h>
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
float r = 50;
double colorR = 1, colorG = 0, colorB = 1;

vector<FileOper*> files; /* Nome dos ficheiros e suas respetivas transformações. */
vector<coords> triangles; /* Coordenadas de todos os triângulos. */


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


bool isMark(coords aux_1, coords aux_2, coords aux_3)
{
    coords mark = make_tuple(DBL_MIN, DBL_MIN, DBL_MIN);

    return (aux_1 == mark && aux_2 == mark && aux_3 == mark);
}

void drawTriangles(void)
{
    bool flag;
    vector<coords>::iterator itTri;
    vector<FileOper*>::iterator itFile;
    vector<Oper*>::iterator itOper;

    itTri = triangles.begin();
    itFile = files.begin();
    while (itTri != triangles.end()) {
        flag = true;
        FileOper* fo = *itFile;

        itOper = fo->operations.begin();
        glPushMatrix();
        while (itOper != fo->operations.end()) { // Fazer transformações.
            Oper* op = *itOper;

            if (strcmp(op->typeOper, "scale") == 0)
                glScalef(op->x, op->y, op->z);

            if (strcmp(op->typeOper, "rotate") == 0)
                glRotatef(op->angle, op->x, op->y, op->z);

            if (strcmp(op->typeOper, "translate") == 0)
                glTranslatef(op->x, op->y, op->z);

            itOper++;
        }

        while (itTri != triangles.end() && flag != false) {
            coords aux_1 = *itTri; itTri++;
            coords aux_2 = *itTri; itTri++;
            coords aux_3 = *itTri; itTri++;

            if (!isMark(aux_1, aux_2, aux_3)) {
                glBegin(GL_TRIANGLES);
                    //glColor3f(colorR, colorG, colorB);
                    //glColor3f(0, 0, 1);
                    glVertex3d(get<0>(aux_1), get<1>(aux_1), get<2>(aux_1));
                    glVertex3d(get<0>(aux_2), get<1>(aux_2), get<2>(aux_2));
                    glVertex3d(get<0>(aux_3), get<1>(aux_3), get<2>(aux_3));
                    //colorR -= 0.001; colorG += 0.001; colorB -= 0.001;
                glEnd();
            } else {
                flag = false;
            }
        }

        glPopMatrix();
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

    // put drawing instructions and geometric transformations here
    drawTriangles();

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


int main(int argc, char** argv)
{
    /* Coloca no "vector<FileOper*> files" o nome de todos os ficheiros, e respetivas transformações. */
    parser("../solar_system.xml", files);

    /* Percore o "vector<FileOper*> files", abrindo cada ficheiro encontrado,
     * e guardando as coordenadas extraídas no "vector<coords> triangles". */
    extractCoords(files, triangles);

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

    // put here the registration of the keyboard callbacks
    glutSpecialFunc(processSpecialKeys);

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
