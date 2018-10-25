#include <libxml/tree.h>
#include <libxml/parser.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <glib.h>
#include "funcs.h"

/** \brief Função de comparação. 
É usada na construção de uma árvore GTree. 
@param a 	um valor
@param b	o valor com o qual vai ser comparado
@return  um valor dependendo do resultado.
 */
gint funcCont(gconstpointer a, gconstpointer b) {
	if (a > b) return 1;
	else if (a == b) return 0;
		 else return -1;
}


/** \brief Inicializa a AVL
Função auxiliar usada na construção de uma avl. Inicializa toda a estrutura, 
alocando o espaço necessário. 
@return 	Gtree.
 */
GTree *initCont(){
	GTree *avl = NULL;  
	avl = g_tree_new(funcCont);
	return avl;
}

/** \brief Compara os ids
Verfica se existe algum id numa determinada lista de ids.
@param node 	uma estrutura 
@param revID 	id de uma revisão
@return	int caso seja True ou False.*/
int existeRev (ValueNode node, int revID) {
	ValueNode aux = node;

	while (aux != NULL) {
		if (aux->idRevisao == revID) return 1;
		aux = aux->next;
	}

	return 0;
}

/** \brief Insere na GTree 
Cria uma GTree correspondente aos IDs
@param avl 		onde serão adicionados os valores 
@param userName	o nome do autor
@param id	id do autor
@param artID  id dos artigos
@param revID	id das revisões.	
*/ 
void insere_avlCont(TAD_istruct avl, char* username, int id, int artID, int revID) {
    gpointer value = g_tree_lookup(avl->avlCont, GINT_TO_POINTER(id));

	if (value == NULL) {
		User userinfo = (User)malloc(sizeof(struct cont));
		userinfo->userName=username;
		userinfo->rep=1;
		g_tree_insert(avl->avlCont, GINT_TO_POINTER(id), userinfo);
	} 
	else {
		gpointer valueIDs = g_tree_lookup(avl->avlIDs, GINT_TO_POINTER(artID));
		ValueNode node = (ValueNode) valueIDs;
		if (!existeRev(node, revID)) {
			User val = (User)value;
			(val->rep)++;
		}	
	}
}
