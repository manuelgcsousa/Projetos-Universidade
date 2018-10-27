#ifdef __APPLE__
#include <GLUT/glut.h>
#else
#include <GL/glut.h>
#endif

// Library Includes.
#include <math.h>
#include <stdio.h>
#include <vector>
#include <tuple>

// Personal Includes.
#include "parser.h"
#include "extractCoords.h"

using namespace std;

typedef tuple<double, double, double> coords;

// Variáveis globais.
float alpha = 1;
float beta = 1;
float r = 20;
double colorR = 1, colorG = 0, colorB = 1;

vector<coords> triangles; // Coordenadas de todos os triângulos.


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
	gluPerspective(45.0f ,ratio, 1.0f ,1000.0f);

	// return to the model view matrix mode
	glMatrixMode(GL_MODELVIEW);
}


void drawTriangles(void)
{
    vector<coords>::iterator it;
    it = triangles.begin();

    while (it != triangles.end()) {

        if(colorR <= 0.4) colorR = 1;
        if(colorG >= 0-5) colorG = 0;
        if(colorB <= 0.2) colorB = 1;

        coords aux_1 = *it; it++;
        coords aux_2 = *it; it++;
        coords aux_3 = *it; it++;

        glBegin(GL_TRIANGLES);
            glColor3f(colorR, colorG, colorB);
            //glColor3f(1, 1, 1);
            glVertex3d(get<0>(aux_1), get<1>(aux_1), get<2>(aux_1));
            glVertex3d(get<0>(aux_2), get<1>(aux_2), get<2>(aux_2));
            glVertex3d(get<0>(aux_3), get<1>(aux_3), get<2>(aux_3));
            colorR -= 0.1; colorG += 0.1; colorB -= 0.1;
 		glEnd();
    }
}


void renderScene(void)
{
	// clear buffers
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	// set the camera
	glLoadIdentity();
    /*
    gluLookAt(10.0, 10.0, 10.0,
		      0.0, 0.0, 0.0,
              0.0f, 1.0f, 0.0f);
    */

    gluLookAt(r * cos(beta) * sin(alpha), r * sin(beta), r * cos(beta) * cos(alpha),
              0.0, 0.0, 0.0,
              0.0f, 1.0f, 0.0f);


    // put the geometric transformations here

    // put drawing instructions here
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
    char** files = XMLParser("../config.xml");
    if (files == nullptr)
        cout << "Error parsing the file!" << "\n";
    else
        extractCoords(files, triangles);

    // init GLUT and the window
	glutInit(&argc, argv);
	glutInitDisplayMode(GLUT_DEPTH | GLUT_DOUBLE | GLUT_RGBA);
	glutInitWindowPosition(100, 100);
	glutInitWindowSize(800, 800);
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
    glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);

    // enter GLUT's main cycle
	glutMainLoop();

	return 1;
}
