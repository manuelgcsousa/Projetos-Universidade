%option yylineno

%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <glib.h>

char token[10000];

GString* author;	
GString* authors;
GString* code;

GHashTable* indice;

void processaAuthor(char* s);
void printNode(gpointer key, gpointer value, gpointer user_data);
%}


%%

\@[^ ]*/,		{ code = g_string_new(strstr(yytext, "{") + 1); }

author[ ]*=.*	{ processaAuthor(yytext); g_string_erase(code, 0, -1); }

.|\n			{ }	

<<EOF>>			{ g_hash_table_foreach(indice, (GHFunc)printNode, NULL); return 0; }

%%


void printNode(gpointer key, gpointer value, gpointer user_data)
{
	FILE* f = fopen("indice.txt", "a");

	GSList* regs = (GSList*)value;
	
	GString* s = (GString*)key;
	fprintf(f, "%s >\t", (char*)s);

	while (regs != NULL) {
		GString* tmp = regs->data;
		fprintf(f, "%s ", tmp->str);

		regs = regs->next;
	}
	fprintf(f,"\n");
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

void insertAuthor(GString* authors)
{
	int i = 0;

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
			
			gboolean auth = g_hash_table_contains(indice, author->str);
			if (auth == FALSE) {
				GSList* new_regs = NULL;
				new_regs = g_slist_append(new_regs, g_string_new(code->str));
				
				GString* key = g_string_new(author->str);	
				g_hash_table_insert(indice, key->str, new_regs);
			} else {
				GSList* old_regs = (GSList*)g_hash_table_lookup(indice, author->str);
				GSList* n = g_slist_append(old_regs, g_string_new(code->str));
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

void processaAuthor(char* s)
{	
	strcpy(token, s);
	cleanToken(token, authors);

    insertAuthor(authors);

    g_string_erase(authors, 0, -1);
}	

int main(int argc, char** argv)
{	
	authors = g_string_new(NULL);			
	author = g_string_new(NULL);
	indice = g_hash_table_new(g_str_hash, g_str_equal);
	
	yylex();
	
	return 0;
}
