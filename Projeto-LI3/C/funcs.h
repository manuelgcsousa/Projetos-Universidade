#include <libxml/tree.h>
#include <libxml/parser.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <glib.h>
#include "interface.h"

struct TCD_istruct {
	long contributor[20];
	long total;
	GTree* avlIDs;
	GTree* avlCont;
};

typedef struct cont{
	char* userName;
	int rep;
} *User;

typedef struct node {
	char* title;
	int idRevisao;
	char* timestamp;
	long tamanho;
	long words;
	struct node *next;
} *ValueNode;

typedef struct growArray {
	int ocupacao;
	int size;
	char* prefix;
	char** arrayPrefix;
} *ArrayPrefix;


/* Funções Auxiliares Querys */

void mete_zeros_long(long array[20]);

int tamanho(ValueNode revisoes);

gboolean iter_all(gpointer key, gpointer value, gpointer data);

long menor(int N, long ids[N]);

gboolean maiores(gpointer key, gpointer value, gpointer data);

void ordenaArray(int N, long ids[N]);

ValueNode da_maior_tamanho(ValueNode revs);

gboolean largest(gpointer key, gpointer value, gpointer data);

ValueNode da_maior_size(ValueNode node);

gboolean moreWords(gpointer key, gpointer value, gpointer data);

int isPrefix(char* arrayPref, char* arrayTitulo);

int da_posicao(ArrayPrefix pref);

gboolean titles_pref(gpointer key, gpointer value, gpointer data);

void swap(char* X[], int a, int b);

int mystrcmp(char s1[], char s2[]);

int partition (char* A[], int p, int r);

void quicksort (char* A[], int p, int r);


/* Funções Primeira AVL */

gint funcIDs (gconstpointer a, gconstpointer b);

GTree *initIDs();

int temIDRep (ValueNode revList, int idRevisao);

void contaPalavras(char* article, long* words, long* tamanho);

void insere_avlIDs(GTree* avl, int artID, char* title, int idRevisao, char* timestamp, long words, long tamanho, char* article);


/* Funções Segunda AVL */

gint funcCont(gconstpointer a, gconstpointer b);

GTree *initCont();

int existeRev (ValueNode node, int revID);

void insere_avlCont(TAD_istruct avl, char* username, int id, int artID, int revID);


/* Funções Principais */

void getAllInfo(TAD_istruct avl, xmlDocPtr doc, xmlNodePtr cur);

void parse_Snapshot(xmlDocPtr* doc, xmlNodePtr* cur, char* path);

























