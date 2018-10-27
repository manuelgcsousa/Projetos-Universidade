%option yylineno

%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <glib.h>

typedef struct cont {
	int reps;
	char* authorName;
} Cont;

char token[10000];

GString* mainAuthor;
GString* author;
GString* authors;

GList* auths;
GHashTable* contributors;

void writeNodeToFile(GList* auths);
gint compareReps(gconstpointer a, gconstpointer b, gpointer user_data);
void addToList(gpointer key, gpointer value, gpointer user_data);
void extractNodes(GString* authors);
void cleanToken(char token[], GString* authors);
void processaAuthors(char* s);
%}


%%

author[ ]*=.*   { processaAuthors(yytext); }

.|\n            { }

<<EOF>>         { 
				  g_hash_table_foreach(contributors, (GHFunc)addToList, NULL);
				  auths = g_list_sort_with_data(auths, (GCompareDataFunc)compareReps, NULL);		  
				  
				  writeNodeToFile(auths);				
 
				  FILE* f = fopen("graph.gv", "a");
				  fprintf(f, "}\n");
				  fclose(f);	
				  return 0; 
				}

%%


void writeNodeToFile(GList* auths)
{
	int i = 0;
    FILE* f = fopen("graph.gv", "a");
	
	while (auths != NULL && i < 10) {
		Cont* tmp = (Cont*)auths->data;
				
		fprintf(f, "    \"%s\" -> \"%s\" [label=\"%d\"];\n", mainAuthor->str, tmp->authorName, tmp->reps);

		i++;
		auths = auths->next;
	}
	
    fclose(f);
}

gint compareReps(gconstpointer a, gconstpointer b, gpointer user_data)
{
	Cont* aux_1 = (Cont*) a;
	Cont* aux_2 = (Cont*) b;

	if (aux_1->reps < aux_2->reps) return 1;
	else if (aux_1->reps > aux_2->reps) return -1;
		 else return 0;	
}

void addToList(gpointer key, gpointer value, gpointer user_data)
{
	Cont* c = (Cont*)malloc(sizeof(struct cont));
	c->authorName = (char*)malloc(sizeof(char) * 30);
	
	strcpy(c->authorName, (char*)key);
	c->reps = GPOINTER_TO_INT(value);

	auths = g_list_prepend(auths, c);	
}

void extractNodes(GString* authors)
{
	int i = 0;

    if (strstr(authors->str, mainAuthor->str) != NULL) {
        // Get all the individual authors.
        while (authors->str[i] != '\0') {
            if (authors->str[i + 1] == '\0' || (authors->str[i] == ' ' && (authors->str[i + 1] == 'a' || authors->str[i + 1] == ','))) {
                if (authors->str[i + 1] == '\0') {
                    author = g_string_append_c(author, authors->str[i]);
                    author = g_string_append_c(author, authors->str[i + 1]);
                }
    
                if (author->str[author->len - 1] == ' ') {
                    author->str[author->len - 1] = '\0';
                } else {
                    g_string_append_c(author, '\0');
                }			
    
                // Process the author that was extracted, if it is different from the main one.
				if (strcmp(author->str, mainAuthor->str) != 0) {
					GString* key = g_string_new(author->str);
					
					gboolean auth = g_hash_table_contains(contributors, author->str);
					if (auth == FALSE) {
						int count = 1;
						g_hash_table_insert(contributors, key->str, GINT_TO_POINTER(count));
					} else {
						gpointer value = g_hash_table_lookup(contributors, author->str);
						value++;
						g_hash_table_insert(contributors, key->str, value);
					}
                }

                g_string_erase(author, 0, -1);
    
                if (authors->str[i + 1] == '\0') {
                    i++;
                } else {
                    i += 4;
                }
            } else {
                author = g_string_append_c(author, authors->str[i]);
            }
    
            i++;
        }
    
        g_string_erase(author, 0, -1);
    }
}

void cleanToken(char token[], GString* authors)
{
    int k = 0;

    // Clean token - get only the authors.
    while (token[k] != '{' && token[k] != '"') {
        k++;
    }

    k++;

    while ( (token[k] != '}' || token[k] != '"') && token[k + 1] != ',') {
        authors = g_string_append_c(authors, token[k]);
        k++;
    }

    // Insert final char in the authors string.
    authors = g_string_append_c(authors, '\0');
}

void processaAuthors(char* s)
{
	strcpy(token, s);
	cleanToken(token, authors);

    extractNodes(authors);

	g_string_erase(authors, 0, -1);	
}

int main(int argc, char** argv)
{
	if (strcmp(argv[1], "") == 0) {
		printf("\n=> Error. Main Author not found!\n");
		return -1;
	}

	FILE* f = fopen("graph.gv", "a");
	fprintf(f, "digraph G {\n");
	fprintf(f, "	layout=\"circo\";\n");
	fclose(f);
	
    mainAuthor = g_string_new(argv[1]);
	authors = g_string_new(NULL);
	author = g_string_new(NULL);
	auths = NULL;
	contributors = g_hash_table_new(g_str_hash, g_str_equal);	

	yylex();

	return 0;
}
