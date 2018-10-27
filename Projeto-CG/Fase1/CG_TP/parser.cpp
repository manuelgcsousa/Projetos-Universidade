#include "parser.h"

char** XMLParser(char* xmlDoc)
{
    XMLDocument doc;
    XMLError err = doc.LoadFile(xmlDoc);

    int i = 0;

    char** files = (char**)malloc(sizeof(char*) * 15); // Aumentar se for necessário o tamanho de cada string.

    char dir[4] = "../";
    char file[15];
    if (!err) {
        XMLElement* elem = doc.FirstChildElement();
        for (const XMLElement* child = elem->FirstChildElement(); child; i++, child = child->NextSiblingElement()) {
            char* tmp = (char*)child->Attribute("file");
            strcpy(file, tmp);

            char file3d[30];
            file3d[0] = '\0';
            strcat(file3d, dir);
            strcat(file3d, file);

            files[i] = (char*)malloc(sizeof(char) * 15);
            strcpy(files[i], file3d);

            memset(file, 0, 15);
            memset(file3d, 0, 30);
        }
    }

    return files;
}