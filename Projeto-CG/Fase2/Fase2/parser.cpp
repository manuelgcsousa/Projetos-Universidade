#include "parser.h"

void parseGroup(const XMLElement* child, Group g, int i, vector<FileOper*>& files)
{
    for (const XMLElement* child2 = child->FirstChildElement(); child2; child2 = child2->NextSiblingElement())
    {
        if (strcmp((char*)child2->Value(), "translate") == 0) {
            char opName[15];
            strcpy(opName, (char*)child2->Value());

            double x = 0;
            double y = 0;
            double z = 0;

            char* translateX = (char*)child2->Attribute("X");
            if (translateX != NULL) x = atof(translateX);

            char* translateY = (char*)child2->Attribute("Y");
            if (translateY != NULL) y = atof(translateY);

            char* translateZ = (char*)child2->Attribute("Z");
            if (translateZ != NULL) z = atof(translateZ);

            concatOper(g, i, opName, x, y, z, -1);
        }

        if (strcmp((char*)child2->Value(), "rotate") == 0) {
            char opName[10];
            strcpy(opName, (char*)child2->Value());

            double x = 0;
            double y = 0;
            double z = 0;
            double angle = 0;

            char* rotateX = (char*)child2->Attribute("X");
            if (rotateX != NULL) x = atof(rotateX);

            char* rotateY = (char*)child2->Attribute("Y");
            if (rotateY != NULL) y = atof(rotateY);

            char* rotateZ = (char*)child2->Attribute("Z");
            if (rotateZ != NULL) z = atof(rotateZ);

            char* rotateAngle = (char*)child2->Attribute("angle");
            if (rotateAngle != NULL) angle = atof(rotateAngle);

            concatOper(g, i, opName, x, y, z, angle);
        }

        if (strcmp((char*)child2->Value(), "scale") == 0) {
            char opName[10];
            strcpy(opName, (char*)child2->Value());

            double x = 0;
            double y = 0;
            double z = 0;

            char* scaleX = (char*)child2->Attribute("X");
            if (scaleX != NULL) x = atof(scaleX);

            char* scaleY = (char*)child2->Attribute("Y");
            if (scaleY != NULL) y = atof(scaleY);

            char* scaleZ = (char*)child2->Attribute("Z");
            if (scaleZ != NULL) z = atof(scaleZ);

            concatOper(g, i, opName, x, y, z, -1);
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