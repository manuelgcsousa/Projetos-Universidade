#include "parser.h"

using namespace tinyxml2;
using namespace std;

void parseGroup(const XMLElement* child, Group g, int i, vector<FileOper*>& files)
{
    for (const XMLElement* child2 = child->FirstChildElement(); child2; child2 = child2->NextSiblingElement())
    {
        TimeTransform* tf = NULL;

        if (strcmp((char*)child2->Value(), "translate") == 0) {
			char opName[15];
			strcpy(opName, (char*)child2->Value());

            double x = 0, y = 0, z = 0, time = 0;
			vector<coords> points; // Vetor que irá colecionar todos os pontos de um determinado "translate";

            char* timeStr;
            if ((timeStr = (char*)child2->Attribute("time"))) {
                time = atof(timeStr);

                for (const XMLElement* child3 = child2->FirstChildElement(); child3; child3 = child3->NextSiblingElement()) {
                    char* pointX = (char*)child3->Attribute("X");
                    if (pointX != NULL) x = atof(pointX);

                    char* pointY = (char*)child3->Attribute("Y");
                    if (pointY != NULL) y = atof(pointY);

                    char* pointZ = (char*)child3->Attribute("Z");
                    if (pointZ != NULL) z = atof(pointZ);

                    concatPoint(x, y, z, points);
                }

                tf = new TimeTransform();
                tf->time = time;
                tf->points = points;
            } else { // translate poderá também ser estático.
                char* translateX = (char*)child2->Attribute("X");
                if (translateX != NULL) x = atof(translateX);

                char* translateY = (char*)child2->Attribute("Y");
                if (translateY != NULL) y = atof(translateY);

                char* translateZ = (char*)child2->Attribute("Z");
                if (translateZ != NULL) z = atof(translateZ);
            }

            concatOper(g, i, opName, x, y, z, -1, tf);
        }

        if (strcmp((char*)child2->Value(), "rotate") == 0) {
            char opName[10];
            strcpy(opName, (char*)child2->Value());

            double x = 0, y = 0, z = 0, time, angle = -1;

            char* rotateX = (char*)child2->Attribute("X");
            if (rotateX != NULL) x = atof(rotateX);

            char* rotateY = (char*)child2->Attribute("Y");
            if (rotateY != NULL) y = atof(rotateY);

            char* rotateZ = (char*)child2->Attribute("Z");
            if (rotateZ != NULL) z = atof(rotateZ);

            char* timeStr;
            char* rotateAngle;
            if ((timeStr = (char*)child2->Attribute("time"))) {
                time = atoi(timeStr);
                vector<coords> points;

                tf = new TimeTransform();
                tf->time = time;
                tf->points = points;
            } else if ((rotateAngle = (char*)child2->Attribute("angle"))) {
                angle = atof(rotateAngle);
            }

            concatOper(g, i, opName, x, y, z, angle, tf);
        }

        if (strcmp((char*)child2->Value(), "scale") == 0) {
            char opName[10];
            strcpy(opName, (char*)child2->Value());

            double x = 0, y = 0, z = 0;

            char* scaleX = (char*)child2->Attribute("X");
            if (scaleX != NULL) x = atof(scaleX);

            char* scaleY = (char*)child2->Attribute("Y");
            if (scaleY != NULL) y = atof(scaleY);

            char* scaleZ = (char*)child2->Attribute("Z");
            if (scaleZ != NULL) z = atof(scaleZ);

            concatOper(g, i, opName, x, y, z, -1, tf);
        }

        if (strcmp((char*)child2->Value(), "models") == 0) {
            for (const XMLElement* child3 = child2->FirstChildElement(); child3; child3 = child3->NextSiblingElement()) {
                char dir[12] = "../objects/";
                char file[15];

                char* fileName = (char*)child3->Attribute("file");
                strcpy(file, fileName);

                char file3d[40]; file3d[0] = '\0';
                strcat(file3d, dir);
                strcat(file3d, file);

                addFileOper(file3d, g, files);
            }
        }

        if (strcmp((char*)child2->Value(), "group") == 0) {
            parseGroup(child2, g, i + 1, files);

            for (int j = i + 1; j < 10; j++)
                g[j] = NULL;
        }
    }
}

void parser(const char* config, vector<FileOper*>& files)
{
    int i = 0;
    Group g;
    initGroup(g);

    XMLDocument doc;
    XMLError err = doc.LoadFile(config);

    if (!err) {
        XMLElement* elem = doc.FirstChildElement();
        for (const XMLElement* child = elem->FirstChildElement(); child; child = child->NextSiblingElement()) {
            if (strcmp(child->Value(), "group") == 0) {
                for (int j = i; j < 10; j++)
                    g[j] = NULL;

                parseGroup(child, g, i, files);
            }
        }
    }
}
