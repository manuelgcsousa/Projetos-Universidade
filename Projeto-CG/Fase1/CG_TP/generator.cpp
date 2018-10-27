#include <iostream>
#include <fstream>
#include <math.h>
#include <string.h>
#include <stdlib.h>

using namespace std;


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


void generateSphere(char* r, char* sl, char* st, char* fileName) {
    int radius = atoi(r);
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

    double alpha = 0;
    double delta = (2 * M_PI) / slices;

    ofstream file;
    file.open(fileName, ios::app);

    if (file.is_open()) {
        for (int i = 0; i < slices; i++) {
            file << "" << "0" << " 0 " << "0" << "\n";
            file << "" << (radius * sin(alpha + delta)) << " 0 " << (radius * cos(alpha + delta)) << "\n";
            file << "" << (radius * sin(alpha)) << " 0 " << (radius * cos(alpha)) << "\n";

            file << "" << "0 " << height << " 0" << "\n";
            file << "" << (radius * sin(alpha)) << " 0 " << (radius * cos(alpha)) << "\n";
            file << "" << (radius * sin(alpha + delta)) << " 0 " << (radius * cos(alpha + delta)) << "\n";

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

void generateCylinder(char* radius, char* ht, char* ss, char* stacks, char* fileName) {
    int r = atoi(radius);
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

    return 0;
}