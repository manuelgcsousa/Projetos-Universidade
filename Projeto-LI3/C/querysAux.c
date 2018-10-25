#include <libxml/tree.h>
#include <libxml/parser.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <glib.h>
#include "interface.h"
#include "funcs.h"



#define MAX_VALUE 10000000


/** \brief Array todo a zeros
Inicializa um array todo a zeros
@param array[20] array a inicializar.
 */
void mete_zeros_long(long array[20]) {
	long i = 0;
	for(;i<20;i++){
		array[i]=0;
	}
}


/** \brief Auxiliar load
Função auxiliar da load que usa xml para percorrer todos os snapshots, 
e deste modo carregá-los nas estruturas correspondentes 
@param avl estrutura principal
@param doc apontador xml
@param cur apontador cur.
 */
void getAllInfo(TAD_istruct avl, xmlDocPtr doc, xmlNodePtr cur) {
	int i = 0;
	int a = 0;
    long word = 0;
	long tam = 0;
	char* username = NULL;
	int userID = 0;
	char* title = NULL;
	int artID = 0;
	int revID = 0;
	char* timestamp = NULL;
	char* article = NULL;
	cur=cur->xmlChildrenNode->next;
	while(cur!=NULL){
		if(strcmp((char*)cur->name,"page")==0){
			xmlNodePtr page = cur->xmlChildrenNode;
			while(page!=NULL){
				if(strcmp((char*)page->name,"title")==0){
					title = (char*)xmlNodeListGetString(doc,page->xmlChildrenNode,100);
				}
				if(strcmp((char*)page->name,"id")==0){
					artID =atoi((char*)xmlNodeListGetString(doc,page->xmlChildrenNode,100));
					(avl->total)++;
					i++;
				}
				if(strcmp((char*)page->name,"revision")==0){
					xmlNodePtr revision=page->xmlChildrenNode;
					while(revision!=NULL){
						if(strcmp((char*)revision->name,"id")==0) {
							revID =atoi((char*)xmlNodeListGetString(doc,revision->xmlChildrenNode,100));
						}
						if(strcmp((char*)revision->name,"timestamp")==0) {
							timestamp = (char*)xmlNodeListGetString(doc,revision->xmlChildrenNode,100);
						}
						if(strcmp((char*)revision->name,"contributor")==0){
							xmlNodePtr contributor=revision->xmlChildrenNode;
							while(contributor!=NULL){
								if(strcmp((char*)contributor->name,"username")==0) {
									username = (char*)xmlNodeListGetString(doc,contributor->xmlChildrenNode,100);
								}
								if(strcmp((char*)contributor->name,"id")==0) {
									userID = atoi((char*)xmlNodeListGetString(doc,contributor->xmlChildrenNode,100));
									if (userID == 28903366) a++;
                                    insere_avlCont(avl, username, userID, artID, revID);
								}
								contributor=contributor->next;
							}
						}
						if(strcmp((char*)revision->name,"text")==0 && (char*)xmlNodeListGetString(doc,revision->xmlChildrenNode,1)!=NULL){
							article = (char*)xmlNodeListGetString(doc,revision->xmlChildrenNode,1);
							//printf("%s\n", article);
                            contaPalavras(article, &word, &tam);
							insere_avlIDs(avl->avlIDs, artID, title, revID, timestamp, word, tam, article);
						}
						revision=revision->next;
					}
				}
				page=page->next;
			}
		}
		cur=cur->next;
	}
}

/** \brief Percorre o Snapshot
Faz parse do Snapshot passado como argumento. 
@param doc apontador xml
@param cur apontador xml
@param path caminho onde se encontra o snapshot
*/
void parse_Snapshot(xmlDocPtr* doc, xmlNodePtr* cur, char* path) {
	*doc = xmlParseFile(path);
	if (*doc == NULL ) {
		fprintf(stderr,"Document not parsed successfully. \n");
	}

	*cur = xmlDocGetRootElement(*doc);
	if (*cur == NULL) {
		fprintf(stderr,"empty document\n");
		xmlFreeDoc(*doc);
	}
}

 
/** \brief Tamanho estrutura
Calcula o tamanho do número de revisões 
@param revisões lista ligada das revisões
@return tamanho da lista ligada*/
int tamanho(ValueNode revisoes) {
	int res = 0;
	ValueNode aux = revisoes;
	while(aux!=NULL){
		res++;
		aux = aux->next;
	}
	return res;
}

/** \brief Iteração GTree
Função que define o modo como a àrvore é percorrida 
@param key chave da árvore
@param value nodo da avl
@param data guarda o resultado
@return FALSE */
gboolean iter_all(gpointer key, gpointer value, gpointer data) {
	*(int*)data += tamanho((ValueNode)(value));
 	return FALSE;
}

/** \brief Menor do array
Percorre o array em busca do menor elemento
@param N tamanho do array
@param ids[N] array de long
@return Devolve o indice onde se encontra o menor elemento do array 
*/
long menor(int N, long ids[N]) {
	long i;
	long res = MAX_VALUE;
	long apont = 0;
	for(i = 0; i < N; i++){
		if(ids[i] < res){
			res = ids[i];
			apont = i;
		}
	}
	return apont;
}

/** \brief Maiores
Função auxiliar que percorre a árvore substituindo o índice que 
contém o menor elemento pelo maior.
@param key chave da árvore
@param value nodo da avl
@param data guarda o resultado
@return FALSE
*/
gboolean maiores(gpointer key, gpointer value, gpointer data) {
	long* aux = (long*)data;
	User conts = (User)value;
	if(conts->rep > aux[menor(10, aux)]) {
		long aa = menor(10,aux);
		aux[aa] = conts->rep;
		aux[aa+10] = (long)GPOINTER_TO_INT(key);
	}
	return FALSE;
}

/** \brief Ordena Array 
Ordena um array por ordem decrescente. 
@param N tamanho do array
@param ids[N] array de long
*/
void ordenaArray(int N, long ids[N]) {
	int i = 0;
	int j = 0;
	long auxReps[N/2];
	long auxIDs[N/2];
	
	for(i = 0; i < (N/2); i++){
		auxReps[i] = ids[i];
		auxIDs[i] = ids[i + (N/2)];
	}

	int pos;
	long tempReps;
	long tempIDs;
	for(i = 0; i < (N/2); i++) {
		pos = i;
		for (j = i + 1; j < (N/2); j++) {
			if (auxReps[pos] < auxReps[j]) pos = j;
			if (auxReps[pos] == auxReps[j]) {
				if (auxIDs[pos] > auxIDs[j]) pos = j;
			}
		}
		if (pos != i) {
			tempReps = auxReps[i];
			tempIDs = auxIDs[i];
			
			auxReps[i] = auxReps[pos];
			auxIDs[i] = auxIDs[pos];
			
			auxReps[pos] = tempReps;
			auxIDs[pos] = tempIDs;
		}
	}

	for (i = 0; i < (N/2); i++) {
		ids[i] = auxReps[i];
		ids[i + (N/2)] = auxIDs[i];
	}
}


/** \brief Maior elemento 
Percorre uma lista ligada
@param revs lista ligada
@return Devolve a estrutura que contém o com maior número de revisões 
*/
ValueNode da_maior_tamanho(ValueNode revs) {
	ValueNode new = revs;
	int size = revs->tamanho;
	
	while(revs!=NULL){
		if(revs->tamanho>size){
			size = revs->tamanho;
			new=revs;
		}
		revs=revs->next;
	}
	return new;
}

/** \brief Iteração GTree
Função que define o modo como a àrvore é percorrida 
@param key chave da árvore
@param value nodo da avl
@param data guarda o resultado
@return FALSE */
gboolean largest(gpointer key, gpointer value, gpointer data) {
	long* larg_cont = (long*) data;
	ValueNode cont_size = (ValueNode)value;

	int id = GPOINTER_TO_INT(key);
	ValueNode max_size = da_maior_tamanho(cont_size);

	long menorindice = menor(20, larg_cont);
	long menorsize = larg_cont[menor(20, larg_cont)];

	if(max_size->tamanho > menorsize) {
		larg_cont[menorindice] = max_size->tamanho;
		larg_cont[menorindice + 20] = id;
	}

	return FALSE;
}


/** \brief Maior tamanho 
Percorre uma lista ligada
@param revs lista ligada
@return Devolve a estrutura que contém o maior tamanho.
*/
ValueNode da_maior_size (ValueNode node) {
	ValueNode new = node;
	int size = node->words;

	while (node != NULL) {
		if (node->words > size) {
			size = node->words;
			new = node;
		}
		node = node->next;
	}
	return new;
}

/* \brief Iteração GTree
Carrega na estrutra as palavras que são contadas
@param key chave da árvore
@param value nodo da avl
@param data guarda o resultado
@return FALSE
*/
gboolean moreWords (gpointer key, gpointer value, gpointer data) {
	long* largest_art = (long*) data;
	int tamanhoArray = (int)largest_art[0];
	largest_art = largest_art+1;
	ValueNode node = (ValueNode)value;
	int id = GPOINTER_TO_INT(key);


	ValueNode max_size = da_maior_size(node);


	long menorindice = menor(tamanhoArray, largest_art);

	long menorsize = largest_art[menorindice];

	
	if (max_size->words > menorsize) {
		largest_art[menorindice] = max_size->words;
		largest_art[menorindice + tamanhoArray] = id;
	}

	return FALSE;
}

/** \brief Testa prefixo
Verifica se uma palavra é prefixo de outra 
@param arrayPref array com o prefixo
@param arrayTitulo array com o titulo
@return resultado da comparação.*/ 
int isPrefix(char* arrayPref, char* arrayTitulo) {
	int i=0;
 
	if(strlen(arrayPref) > strlen(arrayTitulo)) return 0;

	while (arrayPref[i] != '\0' && arrayPref[i]==arrayTitulo[i]) {
		i++;			
	}
	if(i == strlen(arrayPref)) return 1;

	return 0; 

}

/** \brief Posição do Array
Percorre um array
@param pref Array com os prefixos
@return A posição do array que contém o maior elemento 
*/
int da_posicao(ArrayPrefix pref) {
	int i;
	for(i = 0; i < pref->size && pref->arrayPrefix[i]!=NULL; i++);
	return i;
}	

/** \brief Titulos dos prefixos 
Função usada quando se percorre a àrvore com o intuito de a usar mais
tarde para comparar os vários prefixos 
@param key chave da árvore
@param value nodo da avl
@param data guarda o resultado
@return FALSE*/
gboolean titles_pref(gpointer key, gpointer value, gpointer data) {
	ValueNode tit = (ValueNode) value;
	ArrayPrefix aux = (ArrayPrefix) data;
	int i = 0;

	if(isPrefix(aux->prefix, tit->title)) {
		if (aux->ocupacao == aux->size){
			char** new = (char**)malloc(sizeof(char*)*(aux->size+50));
			aux->size+=50;

			for(i=0; i < aux->ocupacao; i++) {
				new[i] = aux->arrayPrefix[i];
			}

			for (i = aux->ocupacao; i < aux->size; i++) {
				new[i] = NULL;
			}

			free(aux->arrayPrefix);
			aux->arrayPrefix=new;
		}

		int pos = da_posicao(aux);
		aux->arrayPrefix[pos] = tit->title;
		aux->ocupacao++;
	}

	return FALSE;
}

/** \brief Troca
Função que troca dois elementos 
@param X array de char
@param a posição do array
@param b posição do array
*/
void swap(char* X[], int a, int b) {
	char* aux;
	aux = X[a];
	X[a] = X[b];
	X[b] = aux;
}

/** \brief Strings
Compara duas strings. 
@param s1 array de char
@param s2 array de char
@return resultado da comparação*/
int mystrcmp(char s1[], char s2[]) {
	int a = strlen(s1);
	int b = strlen(s2);
	
	char s1new[a];
	strcpy(s1new,s1);
	
	char s2new[b];
	strcpy(s2new,s2);

	int i, j;
    
 	for(j = 0; s1new[j] != '\0'; j++) {
 		if(s1new[j]>=97 && s1new[j]<=122) {
 			char a = s1new[j];
 			a -= 32;
 			s1new[j] = a;
 		}
 	}
 	for(j = 0; s2new[j] != '\0'; j++) {
 		if(s2new[j]>=97 && s2new[j]<=122) {
 			char b = s2new[j];
 			b -= 32;
 			s2new[j] = b;
 		}
 	}

 	i = 0;
	while ((s1new[i] == s2new[i]) && (s1new[i] != '\0') && (s2new[i] != '\0')) {
		i++;
	}

	return (s1new[i] - s2new[i]);
}

/** \brief Partição
Função auxiliar usada para ordenação. Divide um array em dois
@param A array de char
@param p divide o array
@param r posição no array
@return posição array.
 */
int partition (char* A[], int p, int r) {
	int i, j;
	char* x;
	x = A[r];
	i = p-1;
	for (j = p; j < r; j++) {
		if ((mystrcmp(A[j], x) < 0) || (mystrcmp(A[j], x) == 0)) {
			i++;
			swap(A, i, j);
		}
	}		
	swap(A, i+1, r);
	return i+1;
}


/** \brief Ordena Array 
Ordena um array por ordem decrescente. 
@param A array de char*
@param p posição do array
@param r posição do array
*/
void quicksort (char* A[], int p, int r) {
	int q;
	if (p < r) {
		q = partition(A, p, r);
		quicksort(A, p, q-1);
		quicksort(A, q+1, r);
	}
}


