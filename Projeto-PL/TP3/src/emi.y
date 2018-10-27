%{
#define _GNU_SOURCE
#include "structs.h"

int asprintf(char** strp, const char* fmt, ...);
int yylex();
void yyerror(char* c);
%}

%union  { char* s; }
%token EMIGRANTE OPERACAO STR
%type <s> EMIGRANTE OPERACAO STR Z S Dados Infs f

%%
Z : S                                           { }
;

S :                                             { }
  | S EMIGRANTE '{' Dados '}'                   { LPersonalData* pd = createLPersonalData(); insertHTCountries(pd); Events* ev = createEvent(pd->id, pd->name); insertHTEvents(ev); }
  | S OPERACAO '{' Infs '}'                     { LOperations* op = createLOperations(); insertHTOperations(atoi(lineReader[0]), op); }
;

Dados :                                         { $$ = ""; }
      | Dados f ';'                             { strcpy(lineReader[indReader], $2); indReader++; }
;

Infs :                                          { $$ = ""; }
     | Infs f ';'                               { strcpy(lineReader[indReader], $2); indReader++; }
;

f : STR                                         { $$ = $1;  }
  | f STR                                       { asprintf(&$$, "%s %s", $1, $2); }
;
%%

#include "lex.yy.c"

int main() {
    initLineReader();
    initFile();
    countries = g_hash_table_new(g_str_hash, g_str_equal);
    ids = g_hash_table_new(g_int_hash, g_int_equal);
    yyparse();
    /*
    for (int i = 0; i < 8; i++) {
        printf("%s\n", lineReader[i]);
    } */
    g_hash_table_foreach(countries, (GHFunc)printHTCountries, NULL);
    g_hash_table_foreach(ids, (GHFunc)printHTOperations, NULL);
    endFile();
    return 0; 
}

void yyerror(char* s) {
    fprintf(stderr, "%s, '%s', line %d \n", s, yytext, yylineno);
}
