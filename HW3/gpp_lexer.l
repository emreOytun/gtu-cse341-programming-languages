%{
#include "y.tab.h"
#include "gpp.h"
%}

%%
and                     { return KW_AND; }
or                      { return KW_OR; }
not                     { return KW_NOT; }
equal                   { return KW_EQUAL; }
less                    { return KW_LESS; }
nil                     { return KW_NIL; }
list                    { return KW_LIST; }
append                  { return KW_APPEND; }
concat                  { return KW_CONCAT; }
set                     { return KW_SET; }
def                     { return KW_DEF; }
for                     { return KW_FOR; }
if                      { return KW_IF; }
exit                    { return KW_EXIT; }
load                    { return KW_LOAD; }
display                 { return KW_DISPLAY; }
true                    { return KW_TRUE; }
false                   { return KW_FALSE; }
[+]                     { return OP_PLUS; }
[-]                     { return OP_MINUS; }
[/]                     { return OP_DIV; }
[*]                     { return OP_MULT; }
[(]                     { return OP_OP; }
[)]                     { return OP_CP; }
,                       { return OP_COMMA; }
;;.*                    { return COMMENT; }
[0-9]+b[0-9]*			{ yylval.str = strdup(yytext); return VALUEF; }
[a-zA-Z][a-zA-Z0-9]*    { yylval.str = strdup(yytext); return IDENTIFIER; }

[ \t\n]                 ; /* Whitespace and unknown characters are ignored. */
\r				    ; /* Return carriage is ignored. */
.                       { yylval.str = strdup(yytext); return SYNTAX_ERROR; }
%%

int yywrap (void) { return 1;}

/*
int main(int argc, char *argv[]) {
    
	if (argc == 1) {
		printf("Interpreter started... \n");
		yylex();	
		return 0;
	}
	
	else if (argc == 2) {
        FILE *input_file = fopen(argv[1], "r");
		if (input_file == NULL) {
			perror("Error opening input file");
			return 1;
		}

		yyin = input_file;
		yylex();

		fclose(input_file);
		return 0;
    }
	
	else {
		printf("You should enter only 1 or 2 arguments. Program is closed.\n");
		return 0;
	}
}
*/