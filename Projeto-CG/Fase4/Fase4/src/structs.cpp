#include "structs.h"

/**
 * Faz com que todas as posições da estrutura Group,
 * apontem para NULL.
 *
 * @param g
 */
void initGroup(Group g)
{
    for (int i = 0; i < 10; i++) {
        g[i] = NULL;
    }
}

/**
 * Adiciona um tuplo ao vector de coordenadas.
 *
 * @param x
 * @param y
 * @param z
 * @param points
 */
void concatPoint(double x, double y, double z, vector<coords>& points)
{
    coords point = make_tuple(x, y, z);

    points.push_back(point);
}

/**
 * Concatena num novo nodo a uma lista ligada da estrutura de dados Group
 * Se não existir nenhuma lista numa certa posição, é alocada uma lista.
 *
 * @param g
 * @param i
 * @param opName
 * @param x
 * @param y
 * @param z
 * @param angle
 */
void concatOper(Group g, int i, char opName[], double x, double y, double z, double angle, TimeTransform* tf)
{
    LOper* main = g[i];

    LOper* novo = (LOper*)malloc(sizeof(struct loper));
    novo->opName = (char*)malloc(sizeof(char) * 50);
    strcpy(novo->opName, opName);
    novo->x = x;
    novo->y = y;
    novo->z = z;
    novo->angle = angle;
    novo->tf = tf;
    novo->next = NULL;

    if (g[i] == NULL) {
        g[i] = novo;
    } else {
        while (main->next != NULL) {
            main = main->next;
        }

        main->next = novo;
    }
}

/**
 * Adiciona um novo FileOper ao vector de que guarda todos os FileOper* do programa.
 *
 * @param fileName
 * @param texture
 * @param col
 * @param g
 * @param files
 */
void addFileOper(char* fileName, char* texture, Colors* col, Group g, vector<FileOper*>& files)
{
    FileOper* fo = new FileOper();    
	fo->fileName = new char[30];

    vector<Oper*> operations;
    for (int i = 0; g[i]; i++) {
        LOper* tmp;
        for (tmp = g[i]; tmp; tmp = tmp->next) {
            Oper* op = new Oper();
            op->typeOper = new char[10];

            strcpy(op->typeOper, tmp->opName);
            op->x = tmp->x;
            op->y = tmp->y;
            op->z = tmp->z;
            op->angle = tmp->angle;
            op->tf = tmp->tf;

            operations.push_back(op);
        }
    }
	
	// char* fileName;
    strcpy(fo->fileName, fileName);
    
	// char* texture;
	if (texture != NULL) {
		fo->texture = new char[30];
		strcpy(fo->texture, texture);
	} else fo->texture = NULL;
	
	// int totalVertexes;
	fo->totalVertexes = 0;
	
	// Colors* col;	
	if (col != NULL) {
		fo->col = new Colors();
        (fo->col)->component = new char[15];
        strcpy((fo->col)->component, col->component);

		(fo->col)->r = col->r;
		(fo->col)->g = col->g;
		(fo->col)->b = col->b;
	} else fo->col = NULL;
	
	// vector<Oper*> operations;
	fo->operations = operations;

    files.push_back(fo);
}

void printFileOperVector(vector<FileOper*>& files)
{
    for (vector<FileOper*>::iterator it = files.begin(); it != files.end(); it++) {
        cout << "OUTRO FICHEIRO!" << endl;
        FileOper* tmp = *it;
        cout << "FileName: " << tmp->fileName << endl;

        if (tmp->col == NULL) cout << "Texture: " << tmp->texture << endl;
        else {
            Colors* c = tmp->col;
            cout << "=> Model: " << tmp->texture << endl;
            cout << "* Colors *" << endl;
            cout << "-> R: " << c->r << endl;
            cout << "-> G: " << c->g << endl;
            cout << "-> B: " << c->b << endl;
        }

        vector<Oper*> aux = tmp->operations;
        for (vector<Oper*>::iterator it2 = aux.begin(); it2 != aux.end(); it2++) {
            Oper* tmp2 = *it2;
            cout << "TypeOper: " << tmp2->typeOper << endl;

            TimeTransform* tf_tmp = tmp2->tf;

            if (tf_tmp) {
                cout << "Time: " << tf_tmp->time << endl;
                vector<coords> tf_coords = tf_tmp->points;
                for (vector<coords>::iterator it3 = tf_coords.begin(); it3 != tf_coords.end(); it3++) {
                    coords c = *it3;
                    cout << "=> TimeTransform <=" << endl;
                    cout << "Points: X = " << get<0>(c) << " / Y = " << get<1>(c) << " / Z = " << get<2>(c) << endl;
                }
            } else {
                cout << "X = " << tmp2->x << " " << "Y = " << tmp2->y << " " << "Z = " << tmp2->z << " " << "Angle = " << tmp2->angle << endl;
            }

            cout << "" << endl;
        }
    }
}

