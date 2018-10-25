#include <stdio.h>
#include <stdlib.h>

/*
long all_articles (TAD_istruct qs) {

}

long unique_articles (TAD_istruct qs) {

}

long all_revisions (TAD_istruct qs) {

}

long* top_10_contributors (TAD_istruct qs) {

}

char* contributor_name (long contributor_id, TAD_istruct qs) {

}

long* top_20_largest_articles (TAD_istruct qs) {

}

char* article_title (long article_id, TAD_istruct qs) {

}

long* top_N_articles_with_more_words (int n, TAD_istruct qs) {

}

char** titles_with_prefix (char* prefix, TAD_istruct qs) {

}

char* article_timestamp (long article_id, long revision_id, TAD_istruct qs) {

}
*/

int interpretador () {

	int n, opcao = 0;
	long contributor_id, article_id, revision_id; 
	char ch;
	
	while (opcao >= 0 || opcao < 11) {	
		printf("------------------------------------------------------- Interrogações: ----------------------------------------------------- \n");
		printf("\n");
		printf("1) -> Quantos artigos foram encontrados nos backups analisados? \n");
		printf("\n");
		printf("2) -> Quais os artigos únicos (com identificador único) encontrados nos vários backups analisados? \n");
		printf("\n");
		printf("3) -> Quantas revisões foram efetuadas nos diversos backups? \n");
		printf("\n");
		printf("4) -> Quais os autores que contribuíram para um maior número de versões dos artigos? \n");
		printf("\n");
		printf("5) -> Qual o nome do autor de um determinado identificador? \n");
		printf("\n");
		printf("6) -> Quais são os identificadores dos 20 artigos que possuem textos com maior tamanho? \n");
		printf("\n");
		printf("7) -> Qual o título de um determinado identificador? \n");
		printf("\n");
		printf("8) -> Quais os identificadores de um determinado número de artigos os quais possuem textos com o maior número de palavras? \n");
		printf("\n");
		printf("9) -> Quais os títulos dos artigos que começam com um determinado prefixo? \n");
		printf("\n");
		printf("10) -> Qual a timestamp para uma certa revisão de um artigo? \n");
		printf("\n");
		printf("0) -> Sair \n");
		printf("\n");
		
		scanf("%d", &opcao);
		while ((ch = getchar()) != '\n' && ch != EOF);
		printf("\n");

		switch (opcao) {
			case 1: 
				printf("=> Querry 1 \n");
				printf("\n");
				//all_articles(qs);
				break;
			case 2: 
				printf("=> Querry 2 \n");
				printf("\n");
				//unique_articles(qs);
				break;
			case 3: 
				printf("=> Querry 3 \n");
				printf("\n");
				//all_revisions(qs);
				break;
			case 4:	
				printf("=> Querry 4 \n");
				printf("\n");
				//top_10_contributors(qs);
				break; 
			case 5:	
				printf("Introduza o identificador do contribuidor: ");
				scanf("%ld", &contributor_id);
				printf("\n");
				printf("=> Querry 5 \n");
				printf("\n");
				//contributor_name(contributor_id, qs);
				break;
			case 6:	
				printf("=> Querry 6 \n");
				printf("\n");
				//top_20_largest_articles(qs);
				break;
			case 7:	
				printf("Introduza o identificador do artigo: ");
				scanf("%ld", &article_id);
				printf("\n");
				printf("=> Querry 7 \n");
				printf("\n");
				//article_title(article_id, qs);
				break;
			case 8:
				printf("Introduza o número de artigos: ");
				scanf("%d", &n); 
				printf("\n");
				printf("=> Querry 8 \n");
				printf("\n");
				//top_N_articles_with_more_words(n, qs);
				break;
			case 9: 
				printf("=> Querry 9 \n");
				printf("\n");
				//titles_with_prefix(&prefix, qs);
				break;
			case 10: 
				printf("Introduza o identificador do artigo: ");
				scanf("%ld", &article_id);
				printf("Introduza o identificador da revisão: ");
				scanf("%ld", &revision_id);
				printf("\n");
				printf("=> Querry 10 \n");
				printf("\n");
				//article_timestamp(article_id, revision_id, qs);
				break;
			case 0: 
				return 0;
		} 																	
	}

	return 0;
}

int main () {
	interpretador();
	return 0;
}
