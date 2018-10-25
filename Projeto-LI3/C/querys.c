#include "funcs.h"
#include "interface.h"

/** \brief Incializa a estrutura de dados
Aloca o espaço necessário para a criação das estruturas
@return retorna a estrutura principal. */
TAD_istruct init() {

	TAD_istruct avl = (TAD_istruct)malloc(sizeof(struct TCD_istruct)); 
	avl->total = 0; 
  	
  	GTree *avlIDs = initIDs();
	avlIDs = g_tree_new(funcIDs);

	GTree *avlCont = initCont();
	avlCont = g_tree_new(funcCont);

	avl->avlIDs = avlIDs;
	avl->avlCont = avlCont;

	mete_zeros_long(avl->contributor);

	return avl;
}

/** \brief Função de load
Carrega para a estrutura todos os daoos necessários 
@param qs estrutura principal
@param nsnaps	numero de snaps
@param snaps_paths[]	caminho para os snaps
@return estrutura principal.*/
TAD_istruct load(TAD_istruct qs, int nsnaps, char* snaps_paths[]) {
	
	xmlDocPtr doc; 
	xmlNodePtr cur;
    
    int i;
	for(i = 0;  i < nsnaps; i++) {
		parse_Snapshot(&doc, &cur, snaps_paths[i]);
		getAllInfo(qs, doc, cur);
	}

	return qs;
}

/* \brief Todos os arquivos
Conta todos os artigos presentes numa estrutura
@param qs estrutura principal
@return  número total de artigos. */
long all_articles(TAD_istruct qs) {
	return (qs->total);
}

/** \brief Artigos únicos
Devolve o número de artigos únicos encontrados nos vários backups analisados.
@param qs estrutura principal
@return número de artigos únicos.
 */
long unique_articles(TAD_istruct qs) {
	return (long)g_tree_nnodes(qs->avlIDs);
}

/** \brief Todas as revisões 
Retorna quantas revisões foram efetuadas nos backups.
@param qs estrutura principal
@return número total de revisões. */
long all_revisions(TAD_istruct qs) {
	long n_revisoes = 0;
	g_tree_foreach(qs->avlIDs, (GTraverseFunc)iter_all, &n_revisoes);
	return n_revisoes; 
}

/** Devolve um array com os 10 autores que mais contiruíram para o maior número de revisões. */ 
long* top_10_contributors(TAD_istruct qs) {
	g_tree_foreach(qs->avlCont, (GTraverseFunc)maiores, qs->contributor);
	ordenaArray(20, qs->contributor);
	return (qs->contributor)+10;
}

/** \brief Nome dos contribuidores
Devolve o nome do autor com um determinado identificador. 
Se esse autor não existir a função retorna NULL.
@param contributor_id id do contribuidor
@param qs estrutura principal
@return nome do autor.
 */ 
char* contributor_name(long contributor_id, TAD_istruct qs) {
	User aux;
	aux = (User)g_tree_lookup(qs->avlCont, GINT_TO_POINTER((int)contributor_id));
	if(aux == NULL) return NULL; 
	else return (aux)->userName;
}

/** \brief Artigos maiores
Retorna um array com os identificadores dos 20 maiores artigos.
@param qs estrutura principal
@return array com os 20 maiores artigos.
 */
long* top_20_largest_articles(TAD_istruct qs) {
	long* largest_cont = (long*)malloc(sizeof(long)*40);

	int i = 0;
	for(; i < 40; i++) {
		largest_cont[i] = 0;
	}

	g_tree_foreach(qs->avlIDs, (GTraverseFunc)largest, largest_cont);

	ordenaArray(40, largest_cont);
    
	return largest_cont + 20;
}


/** \brief Titulo do artigo
Percorre a estrutura em busca do titulo de um artigo
@param article_id id do artigo
@param qs estrutura principal.
@return  titulo do artigo com um determinado identificador*/
char* article_title(long article_id, TAD_istruct qs){
	char* title;
	ValueNode value = (ValueNode)g_tree_lookup(qs->avlIDs,GINT_TO_POINTER((int)article_id));
	
	if (value != NULL) title = value->title;
	else title = NULL;

	return title;
}

/** \brief Artigos com mais palavras
Percorre a estrutura em busca dos artigos com 
mais palavras.
@param n número de identificadores
@param qs estrutura principal
@return Array com n posições que contém o maior número de palavras
*/
long* top_N_articles_with_more_words(int n, TAD_istruct qs) {
	long* largest_articles = (long*)malloc(sizeof(long) * (2*n + 1));

	int i = 0;
	for(; i < (2*n + 1); i++) {
		largest_articles[i] = i;
	}

	largest_articles[0] = n;

	g_tree_foreach(qs->avlIDs, (GTraverseFunc)moreWords, largest_articles);

	ordenaArray(2*n, largest_articles+1);

	return largest_articles + 1 + n;
}

/** \brief Titulos com prefixo 
Percorre a estrutura principal em busca 
dos prefixos.
@param prefix Array os prefixos
@param qs Estrutura principal
@return Array de titulos de artigos*/
char** titles_with_prefix(char* prefix, TAD_istruct qs) {
	ArrayPrefix pArray = (ArrayPrefix)malloc(sizeof(struct growArray));
	
	pArray->arrayPrefix = (char**)malloc(sizeof(char*)*100);
	pArray->ocupacao = 0;
	pArray->size = 100;
	pArray->prefix = prefix;

	int i;
	for(i = 0; i < 100; i++) pArray->arrayPrefix[i]=NULL;
	
	g_tree_foreach(qs->avlIDs,(GTraverseFunc)titles_pref,pArray); 
	
	int p = da_posicao(pArray);
	pArray->arrayPrefix[p] = NULL;
	quicksort(pArray->arrayPrefix, 0, p - 1);
	return	pArray->arrayPrefix;
}

/** \brief TimeStamp dos artigos
Percorre a estrutura em busca dis timestamp
@param article_id id dos artigos
@param revision_id id da revisão
@param qs estrutura principal
@ return Retorna o timestamp para uma certa revisão de um artigo.
 */
char* article_timestamp(long article_id, long revision_id, TAD_istruct qs) {
	char* timestamp = NULL;
	ValueNode revisions = (ValueNode)g_tree_lookup(qs->avlIDs,GINT_TO_POINTER(article_id));

	while (revisions != NULL) {
		if(revisions->idRevisao == revision_id) timestamp = revisions->timestamp;
		revisions = revisions->next;
	}

	return timestamp;
}


/** \brief Limpa a estrutura
Liberta toda a memória alocada 
@param estrutura principal*/
TAD_istruct clean(TAD_istruct qs){
	g_tree_destroy(qs->avlIDs);
	g_tree_destroy(qs->avlCont);
	free(qs);
	return NULL;
}
