#include <iostream>
#include <fstream>
#include <string>
#include <math.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h> /* chamadas ao sistema: defs e decls essenciais */

using namespace std;

void multMatrix14(double uW[1][4], double m[4][4],double res[1][4])
{
    res[0][0] = (double)(uW[0][0] * m[0][0] + uW[0][1] * m[1][0] + uW[0][2] * m[2][0] + uW[0][3] * m[3][0]);
    res[0][1] = (double)(uW[0][0] * m[0][1] + uW[0][1] * m[1][1] + uW[0][2] * m[2][1] + uW[0][3] * m[3][1]);
    res[0][2] = (double)uW[0][0] * m[0][2] + uW[0][1] * m[1][2] + uW[0][2] * m[2][2] + uW[0][3] * m[3][2];
    res[0][3] = (double)uW[0][0] * m[0][3] + uW[0][1] * m[1][3] + uW[0][2] * m[2][3] + uW[0][3] * m[3][3];
}

void multMatrix1441(double trans[1][4], double vH[4][1], double* res)
{
    *res = trans[0][0] * vH[0][0] + trans[0][1] * vH[1][0] + trans[0][2] * vH[2][0] + trans[0][3] * vH[3][0];
}

void generatePlane(char* l, char* fileName)
{
    double length = atof(l);

    ofstream file;
    file.open(fileName, ios::app);

    if (file.is_open()) {
        // Triângulo 1 => Coordenadas do triângulo à esquerda.
        file << "" << (-length/2) << " 0 " << (-length/2) << "\n";
        file << "" << (-length/2) << " 0 " << (length/2) << "\n";
        file << "" << (length/2) << " 0 " << (length/2) << "\n";

        // Triângulo 2 => Coordenadas do triângulo à direita.
        file << "" << (-length/2) << " 0 " << (-length/2) << "\n";
        file << "" << (length/2) << " 0 " << (length/2) << "\n";
        file << "" << (length/2) << " 0 " << (-length/2) << "\n";
    }

    file.close();
}


void generateBox(char* a, char* b, char* c, int div, char* fileName)
{
    double length = atof(a);
    double height = atof(b);
    double width = atof(c);

    double x, y, z;

    double divX = length/div;
    double divY = height/div;
    double divZ = width/div;

    ofstream file;
    file.open(fileName, ios::app);

    if (file.is_open()) {

        // Base da caixa.
        z = width/2;
        for (int i = 0; i < div; i++) {
            x = -length/2;
            for (int j = 0; j < div; j++) {
                file << "" << x << " 0 " << (z - divZ) << "\n";
                file << "" << (x + divX) << " 0 " << z << "\n";
                file << "" << x << " 0 " << z << "\n";

                file << "" << x << " 0 " << (z - divZ) << "\n";
                file << "" << (x + divX) << " 0 " << (z - divZ) << "\n";
                file << "" << (x + divX) << " 0 " << z << "\n";

                x += divX;
            }
            z -= divZ;
        }

        // Face traseira da caixa. (Esquerda)
        x = -length/2;
        z = -width/2;
        for (int i = 0; i < div; i++) {
            y = height;
            for (int j = 0; j < div; j++) {
                file << "" << x << " " << y << " " << z << "\n";
                file << "" << x << " " << (y - divY) << " " << z << "\n";
                file << "" << x << " " << (y - divY) << " " << (z + divZ) << "\n";

                file << "" << x << " " << y << " " << z << "\n";
                file << "" << x << " " << (y - divY) << " " << (z + divZ) << "\n";
                file << "" << x << " " << y << " " << (z + divZ) << "\n";

                y -= divY;
            }
            z += divZ;
        }

        // Face esquerda da caixa.     (Frontal)
        x = -length/2;
        z = width/2;
        for (int i = 0; i < div; i++) {
            y = height;
            for (int j = 0; j < div; j++) {
                file << "" << x << " " << y << " " << z << "\n";
                file << "" << x << " " << (y - divY) << " " << z << "\n";
                file << "" << (x + divX) << " " << y << " " << z << "\n";

                file << "" << x << " " << (y - divY) << " " << z << "\n";
                file << "" << (x + divX) << " " << (y - divY) << " " << z << "\n";
                file << "" << (x + divX) << " " << y << " " << z << "\n";

                y -= divY;
            }
            x += divX;
        }

        // Face direita da caixa.  (Traseira)
        x = -length/2;
        z = -width/2;
        for (int i = 0; i < div; i++) {
            y = height;
            for (int j = 0; j < div; j++) {
                file << "" << x << " " << y << " " << z << "\n";
                file << "" << (x + divX) << " " << y << " " << z << "\n";
                file << "" << (x + divX) << " " << (y - divY) << " " << z << "\n";

                file << "" << x << " " << y << " " << z << "\n";
                file << "" << (x + divX) << " " << (y - divY) << " " << z << "\n";
                file << "" << x << " " << (y - divY) << " " << z << "\n";

                y -= divY;
            }
            x += divX;
        }

        // Face frontal da caixa.  (Direita)
        x = length/2;
        z = width/2;
        for (int i = 0; i < div; i++) {
            y = height;
            for (int j = 0; j < div; j++) {
                file << "" << x << " " << y << " " << z << "\n";
                file << "" << x << " " << (y - divY) << " " << z << "\n";
                file << "" << x << " " << (y - divY) << " " << (z - divZ) << "\n";

                file << "" << x << " " << y << " " << z << "\n";
                file << "" << x << " " << (y - divY) << " " << (z - divZ) << "\n";
                file << "" << x << " " << y << " " << (z - divZ) << "\n";

                y -= divY;
            }
            z -= divZ;
        }

        // Tampa superior da caixa.
        y = height;
        z = -width/2;
        for (int i = 0; i < div; i++) {
            x = -length/2;
            for (int j = 0; j < div; j++) {
                file << "" << (x + divX) << " " << y << " " << z << "\n";
                file << "" << x << " " << y << " " << z << "\n";
                file << "" << x << " " << y << " " << (z + divZ) << "\n";

                file << "" << (x + divX) << " " << y << " " << z << "\n";
                file << "" << x << " " << y << " " << (z + divZ) << "\n";
                file << "" << (x + divX) << " " << y << " " << (z + divZ) << "\n";

                x += divX;
            }
            z += divZ;
        }
    }

    file.close();
}


void generateSphere(char* r, char* sl, char* st, char* fileName)
{
    double radius = atof(r);
    int slices = atoi(sl);
    int stacks = atoi(st);

    double alpha = 0;
    double deltaAlpha = (2 * M_PI) / slices;
    double beta = 0;
    double deltaBeta = M_PI / stacks;

    double p1x_S, p1y_S, p1z_S, p2x_S, p2y_S, p2z_S, p3x_S, p3y_S, p3z_S, p4x_S, p4y_S, p4z_S;

    ofstream file;
    file.open(fileName, ios::app);

    if (file.is_open()) {
        for (int i = 0; i < stacks; i++) {
            alpha = 0;
            for (int j = 0; j < slices; j++) {

                p1x_S = radius * sin(beta) * sin(alpha);
                p1y_S = radius * cos(beta);
                p1z_S = radius * sin(beta) * cos(alpha);

                p2x_S = radius * sin(beta + deltaBeta) * sin(alpha);
                p2y_S = radius * cos(beta + deltaBeta);
                p2z_S = radius * sin(beta + deltaBeta) * cos(alpha);

                p3x_S = radius * sin(beta) * sin(alpha + deltaAlpha);
                p3y_S = radius * cos(beta);
                p3z_S = radius * sin(beta) * cos(alpha + deltaAlpha);

                p4x_S = radius * sin(beta + deltaBeta) * sin(alpha + deltaAlpha);
                p4y_S = radius * cos(beta + deltaBeta);
                p4z_S = radius * sin(beta + deltaBeta) * cos(alpha + deltaAlpha);

                file << p1x_S << " " << p1y_S << " " << p1z_S << "\n";
                file << p2x_S << " " << p2y_S << " " << p2z_S << "\n";
                file << p4x_S << " " << p4y_S << " " << p4z_S << "\n";

                file << p1x_S << " " << p1y_S << " " << p1z_S << "\n";
                file << p4x_S << " " << p4y_S << " " << p4z_S << "\n";
                file << p3x_S << " " << p3y_S << " " << p3z_S << "\n";

                alpha += deltaAlpha;
            }
            beta += deltaBeta;
        }
    }
    file.close();
}


void generateCone(char* r, char* h, char* sl, char* st, char* fileName)
{
    double radius = atof(r);
    double height = atof(h);
    double slices = atof(sl);
    double stacks = atof(st);

    double hip = ( height/stacks );
    double it = 0;

    double alpha = 0;
    double delta = (2 * M_PI) / slices;
    double beta = (radius / stacks);
    double radiusAux = radius;

    ofstream file;
    file.open(fileName, ios::app);

    if (file.is_open()) {
        for (int i = 0; i < slices; i++) {

            file << "" << "0" << " 0 " << "0" << "\n";
            file << "" << (radius * sin(alpha + delta)) << " 0 " << (radius * cos(alpha + delta)) << "\n";
            file << "" << (radius * sin(alpha)) << " 0 " << (radius * cos(alpha)) << "\n";

            it = 0;
            for (int j = 0; j < stacks - 1; j++) {
                radiusAux = radius - (j * beta);

                file << "" << ((radiusAux - beta) * sin(alpha + delta)) << " " << (it + hip) << " " <<  ((radiusAux - beta) * cos(alpha + delta)) << "\n";
                file << "" << ((radiusAux - beta) * sin(alpha)) << " " << (it + hip) << " " << ((radiusAux - beta) * cos(alpha)) << "\n";
                file << "" << (radiusAux * sin(alpha)) << " " << it << " " << (radiusAux * cos(alpha)) << "\n";

                file << "" << ((radiusAux - beta) * sin(alpha + delta)) << " " << (it + hip) << " " <<  ((radiusAux - beta) * cos(alpha + delta)) << "\n";
                file << "" << (radiusAux * sin(alpha)) << " " << it << " " << (radiusAux * cos(alpha)) << "\n";
                file << "" << (radiusAux * sin(alpha)) << " " << it << " " << (radiusAux * cos(alpha)) << "\n";

                it += hip;
            }

            file << "" << "0 " << height << " 0" << "\n";
            file << "" << (beta * sin(alpha)) << " " << ( it ) << " " << (beta * cos(alpha)) << "\n";
            file << "" << (beta * sin(alpha + delta)) << " " << ( it) << " " << (beta * cos(alpha + delta)) << "\n";

            alpha += delta;
        }
    }

    file.close();
}


void generateDiamond(char* r, char* fileName)
{
    double radius = atof(r);
    double radiusL = radius + 2;
    double alpha = M_PI/3;

    ofstream file;
    file.open(fileName, ios::app);

    if (file.is_open()) {
        for (int i = 0; i < 6; i++) {
            // Hexágono superior.
            file << "0 2 0" << "\n";
            file << (radius * sin(i * alpha)) << " 2 " << (radius * cos(i * alpha)) << "\n";
            file << (radius * sin((i + 1) * alpha)) << " 2 " << (radius * cos((i + 1) * alpha)) << "\n";

            // Triângulo 1 do Hexágono inferior.
            file << (radiusL * sin((i + 1) * alpha)) << " 1 " << (radiusL * cos((i + 1) * alpha)) << "\n";
            file << (radius * sin((i + 1) * alpha)) << " 2 " << (radius * cos((i + 1) * alpha)) << "\n";
            file << (radius * sin(i * alpha)) << " 2 " << (radius * cos(i * alpha)) << "\n";

            // Triângulo 2 do Hexágono inferior.
            file << (radiusL * sin((i + 1) * alpha)) << " 1 " << (radiusL * cos((i + 1) * alpha)) << "\n";
            file << (radius * sin(i * alpha)) << " 2 " << (radius * cos(i * alpha)) << "\n";
            file << (radiusL * sin(i * alpha)) << " 1 " << (radiusL * cos(i * alpha)) << "\n";

            // Completar pirâmide hexagonal.
            file << (radiusL * sin((i + 1) * alpha)) << " 1 " << (radiusL * cos((i + 1) * alpha)) << "\n";
            file << (radiusL * sin(i * alpha)) << " 1 " << (radiusL * cos(i * alpha)) << "\n";
            file << "0 -6 0" << "\n";
        }
    }
    file.close();
}

void generateCylinder(char* radius, char* ht, char* ss, char* stacks, char* fileName)
{
    double r = atoi(radius);
    int height = atoi(ht);
    int slices = atoi(ss);

    double delta = 2 * M_PI / slices;
    double alpha = 0;

    ofstream file;
    file.open(fileName, ios::app);

    if (file.is_open()) {
        for (int i = 0; i < slices; i++) {
            file << "0 0 0" << "\n";
            file << (r * sin(alpha + delta)) << " 0 " << (r * cos(alpha + delta)) << "\n";
            file << (r * sin(alpha)) << " 0 " << (r * cos(alpha)) << "\n";

            file << (r * sin(alpha + delta)) << " " << height << " " << (r * cos(alpha + delta)) << "\n";
            file << (r * sin(alpha)) << " " << height << " " << (r * cos(alpha)) << "\n";
            file << (r * sin(alpha)) << " 0 " << (r * cos(alpha)) << "\n";

            file << (r * sin(alpha + delta)) << " " << height << " " << (r * cos(alpha + delta)) << "\n";
            file << (r * sin(alpha)) << " 0 " << (r * cos(alpha)) << "\n";
            file << (r * sin(alpha + delta)) << " 0 " << (r * cos(alpha + delta)) << "\n";

            file << "0 " << height << " 0" << "\n";
            file << (r * sin(alpha)) << " " << height << " " << (r * cos(alpha)) << "\n";
            file << (r * sin(alpha + delta)) << " " << height << " " << (r * cos(alpha + delta)) << "\n";
            alpha += delta;
        }
    }
    file.close();
}


void generateFigure(double u, double v, int indices[16],double points[290][3], double r[3])
{
    double uW[1][4] = { {powf(u,3), powf(u,2), u, 1} };


    double m[4][4] = { {-1.0f,  3.0f, -3.0f,  1.0f},
                       { 3.0f, -6.0f,  3.0f,  0.0f},
                       {-3.0f,  3.0f,  0.0f,  0.0f},
                       { 1.0f,  0.0f,  0.0f,  0.0f} };

    double mT[4][4] = { {-1.0f,  3.0f, -3.0f, 1.0f},
                        { 3.0f, -6.0f,  3.0f, 0.0f},
                        {-3.0f,  3.0f,  0.0f, 0.0f},
                        { 1.0f,  0.0f,  0.0f, 0.0f} };

    double vH[4][1] = { {powf(v,3)},
                        {powf(v,2)},
                        {powf(v,1)},
                        {powf(v,0)} };

    double pointsX[4][4];
    double pointsY[4][4];
    double pointsZ[4][4];

    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            pointsX[i][j] = points[indices[i*4+j]][0];
            pointsY[i][j] = points[indices[i*4+j]][1];;
            pointsZ[i][j] = points[indices[i*4+j]][2];;
        }
    }

    double res[1][4];
    double resX[1][4];
    double resY[1][4];
    double resZ[1][4];
    double transX[1][4];
    double transY[1][4];
    double transZ[1][4];
    double x;
    double y;
    double z;

    /*  Computar Res = U * M    */
    multMatrix14(uW, m, res);

    /*  Computar ResX = Res * Px; ResY = Res * Py; ResZ = Res * pz  */
    multMatrix14(res, pointsX, resX);
    multMatrix14(res, pointsY, resY);
    multMatrix14(res, pointsZ, resZ);

    /*  Computar TransX = resX * M'; TransY = resY * M'; TransZ = resZ * M'  */
    multMatrix14(resX, mT, transX);
    multMatrix14(resY, mT, transY);
    multMatrix14(resZ, mT, transZ);

    /*  Computar x = transX * V; y = transY * V; z = transZ * V  */
    multMatrix1441(transX,vH,&x);
    multMatrix1441(transY,vH,&y);
    multMatrix1441(transZ,vH,&z);

    /*  Colocar os resultados no vetor res   */
    r[0] = x;
    r[1] = y;
    r[2] = z;
}

void generateBezier(char* tesselation, char* fileRead, char* fileWrite)
{
    int lines = 0;
    int tes = atoi(tesselation);
    int nPatches = 0, nCtrlPoints = 0, indCtrlPoints = 0, ind;
    double point;
    size_t p;
    string line;
    string tmp;
    string sep = ", ";

    ifstream fileR;
    fileR.open(fileRead);

    ofstream file;
    file.open(fileWrite, ios::app);

    if (fileR.is_open()) {
        getline(fileR, line);
        nPatches = stoi(line); // Extrair número de patches. */
    } else {
        cout << "Error parsing bezier patches." << endl;
        return;
    }

    int indices[nPatches][16]; // Matriz que contém os indices de cada patch.

    /* Extraír os indices dos control points. */
    for (int i = 0; i < nPatches; i++) {
        getline(fileR, line);

        while ((p = line.find(sep)) != string::npos) {
            tmp = line.substr(0, p);
            line.erase(0, p + 2);

            ind = stoi(tmp);
            indices[i][indCtrlPoints] = ind;

            indCtrlPoints++;
        }

        ind = stoi(line);
        indices[i][indCtrlPoints] = ind;

        indCtrlPoints = 0;
    }

    /* Extrair número de control points. */
    getline(fileR, line);
    nCtrlPoints = stoi(line);

    double points[nCtrlPoints][3]; // Matriz que contém os pontos de cada control point.

    /* Extrair todos os control points. */
    for (int i = 0; i < nCtrlPoints; i++) {
        getline(fileR, line);

        while ((p = line.find(sep)) != string::npos) {
            tmp = line.substr(0, p);
            line.erase(0, p + 2);

            point = stod(tmp);
            points[i][indCtrlPoints] = point;

            indCtrlPoints++;
        }

        point = stod(line);
        points[i][indCtrlPoints] = point;

        indCtrlPoints = 0;
    }

    double res[3];

    /* Computar Todos os Pontos Necessarios para Desenhar o Teapot a Partir de Bezier. */
    for (int i = 0; i < nPatches; i++) {
        for (int u = 0; u < tes; u++) {
            for (int v = 0; v < tes; v++) {
                double uD = (double)u/tes;
                double vD = (double)v/tes;
                generateFigure(uD,vD,indices[i], points, res);

                // Escrever para o Ficheiro
                if (file.is_open()) {
                    file << res[0] << " " << res[1] << " " << res[2] << endl;
                    lines++;
                } else {
                    cout << "fileWriter not open" << endl;
                }
            }
        }
    }
    int rest = lines % 3;
    for (int i = 0; i < (3 - rest); i++) {
        file << res[0] << " " << res[1] << " " << res[2] << endl;
    }
}

int main(int argc, char** argv)
{
    if (strcmp(argv[1], "plane") == 0)
        generatePlane(argv[2], argv[3]);
    if (strcmp(argv[1], "box") == 0) {
        if (argc == 6)
            generateBox(argv[2], argv[3], argv[4], 1, argv[5]);
        if (argc == 7)
            generateBox(argv[2], argv[3], argv[4], atoi(argv[5]), argv[6]);
    }
    if (strcmp(argv[1], "sphere") == 0)
        generateSphere(argv[2], argv[3], argv[4], argv[5]);
    if (strcmp(argv[1], "cone") == 0)
        generateCone(argv[2], argv[3], argv[4], argv[5], argv[6]);
    if (strcmp(argv[1], "diamond") == 0)
        generateDiamond(argv[2], argv[3]);
    if (strcmp(argv[1], "cylinder") == 0)
        generateCylinder(argv[2], argv[3], argv[4], argv[5], argv[6]);
    if (strcmp(argv[1], "bezier") == 0)
        generateBezier(argv[2], argv[3], argv[4]);

    system("./script.sh");

    return 0;
}
