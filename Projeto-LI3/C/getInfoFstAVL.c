#include "funcs.h"
#include "interface.h"


/** \brief Função de comparação. 
É usada na construção de uma árvore GTree. 
@param a 	um valor
@param b	o valor com o qual vai ser comparado
@return  um valor dependendo do resultado.
 */
gint funcIDs (gconstpointer a, gconstpointer b) {
	if (a > b) return 1;
	else if (a == b) return 0;
		 else return -1;
}

/** \brief Inicializa a AVL
Função auxiliar usada na construção de uma avl. Inicializa toda a estrutura, 
alocando o espaço necessário. 
@return 	Gtree.
 */
GTree *initIDs(){
	GTree *avl = NULL;  
	avl = g_tree_new(funcIDs);
	return avl;
}

/** \brief Compara os ids
Verfica se existe algum id numa determinada lista de ids.
@param revList 	uma estrutura 
@param idRevisao 	id de uma revisão
@return	int caso seja True ou False.*/
int temIDRep (ValueNode revList, int idRevisao) { 
	ValueNode aux = revList;
	while (aux != NULL) {
		if(aux->idRevisao == idRevisao){
			return 1;
		}
		aux = aux->next;
	}
	return 0;
}

/** \brief Conta as palavras
Conta o número de palavras que um texto possui. 
@param article	Array de artigos
@param words	palavras
@param tamanho	tamanho das palavras.
*/
void contaPalavras(char* article, long* words, long* tamanho) {
    int i;
    int palavras = *words;

    //i é igual aos caracteres
    for (i = 0, palavras = 0; article[i] != '\0'; i++) {
         if ((article[i] != ' ') && (article[i] != '\n') && (article[i] != '\t') && ((article[i + 1] == ' ') || (article[i + 1] == '\0') || (article[i + 1] == '\t') || (article[i + 1] == '\n'))) 
         	palavras++;
    } 

    *tamanho = i - 1;
    *words = palavras;
}

/** \brief Insere na GTree 
Cria uma GTree correspondente aos IDs
@param avl 		onde serão adicionados os valores 
@param artID	o artigo do ID
@param title	titulo dos artigos
@param idRevisão  id das revisões
@param timestamp	timesstamp dos artigos
@param words	número de palavras do artigo
@param tamanho	tamanho do artigo
@param article 	artigo.	 	*/ 
void insere_avlIDs(GTree* avl, int artID, char* title, int idRevisao, char* timestamp, long words, long tamanho, char* article) {
	if (artID == 7023) printf("ID(7023) = %ld\n", words);
    if (artID == 25391) printf("ID(25391) = %ld\n", words);
    gpointer value = g_tree_lookup(avl, GINT_TO_POINTER(artID));
	if (value == NULL) {
		ValueNode new = (ValueNode)malloc(sizeof(struct node));
		new->title = title;
		new->idRevisao = idRevisao;
		new->timestamp = timestamp;
		new->words = words;
		new->tamanho = tamanho;
		new->next = NULL;
		g_tree_insert(avl, GINT_TO_POINTER(artID), new);
	} else {
		ValueNode revList = (ValueNode) value;
		if (!temIDRep(revList, idRevisao)) {
			ValueNode new = (ValueNode)malloc(sizeof(struct node));
			new->title = title;
			new->idRevisao = idRevisao;
			new->timestamp = timestamp;
			new->words = words;
			new->tamanho = tamanho;
			new->next = NULL;
			new->next = revList;
			g_tree_remove(avl,GINT_TO_POINTER(artID));
			g_tree_insert(avl,GINT_TO_POINTER(artID),new);
		}
	}
}
