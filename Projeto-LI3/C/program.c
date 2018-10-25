#include "interface.h"
#include <stdio.h>

int main() {

	TAD_istruct avl = init();

	char* snaps_path[3] = {"snapshot_dec16","snapshot_jan17","snapshot_fev17"};

	load(avl, 3, snaps_path);

    long a = all_articles(avl);
	printf("ALL_ARTICLES = %ld\n",a );
	printf("\n");

	long b = unique_articles(avl);
	printf("UNIQUE_ARTICLES = %ld\n", b );
	printf("\n");

	long c = all_revisions(avl);
	printf("ALL_REVISÕES = %ld\n", c);
    printf("\n");

	long* d = top_10_contributors(avl);
	long aa = 0;
	printf("top_10_contributors: \n");
    for(;aa<10;aa++){
		printf("%ld \n", d[aa]);
	}
    printf("\n");

	char* e = contributor_name(28903366,avl);
	printf("%s\n", e);
    char* ee = contributor_name(194203,avl);
	printf("%s\n", ee);
	char* eee = contributor_name(1000,avl);
	printf("%p\n", eee);
    printf("\n");

	long* f = top_20_largest_articles(avl);
	long ff = 0;
    printf("top_20_largest_articles: \n");
	for(;ff<20;ff++) printf("-> %ld\n",f[ff]);
    printf("\n");
    
	char* g = article_title(15910, avl);
    printf("ARTICLE_TITLE = %s\n", g);
	char* gg = article_title(25507,avl);
	printf("ARTICLE_TITLE = %s\n", gg );
	char* ggg = article_title(1111,avl);
	printf("ARTICLE_TITLE = %s\n", ggg);
	printf("\n");
    
	long* h = top_N_articles_with_more_words(30,avl);
	long hh = 0;
	printf("top_N_articles_with_more_words: \n");
    for(;hh<20;hh++) printf("  %ld \n", h[hh]);
    printf("\n");

	char* string = "Anax";
	char ** i = titles_with_prefix(string,avl);
	printf("titles_with_prefix: \n");
    for(;*i!=NULL;i=i+1){
		printf("%s\n",*i );
	}
	printf("\n");

	//char* string = "Hist";
	//char** i = titles_with_prefix(string,avl);
	//int aaaaa= 0;
	//for(;i[aaaaa]!=NULL && aaaaa<1000000000;aaaaa++){
	//	printf("%s\n", i[aaaaa]);
	//}
	//printf("%d\n",aaaaa );

	char* j = article_timestamp(12,763082287,avl);
	printf("ARTICLE_TIMESTAMP = %s\n", j);
	char* jj = article_timestamp(12,755779730,avl);
	printf("ARTICLE_TIMESTAMP = %s\n", jj);
	char* jjj = article_timestamp(12,4479730,avl);
	printf("ARTICLE_TIMESTAMP = %s\n", jjj);
    printf("\n");  

	avl = clean(avl);
	return 0;
} 
