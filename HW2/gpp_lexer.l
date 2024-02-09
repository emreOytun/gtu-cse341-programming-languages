%{
#include <stdio.h>
%}

/* Define token names */

%%
and                     { printf("KW_AND\n"); }
or                      { printf("KW_OR\n"); }
not                     { printf("KW_NOT\n"); }
equal                   { printf("KW_EQUAL\n"); }
less                    { printf("KW_LESS\n"); }
nil                     { printf("KW_NIL\n"); }
list                    { printf("KW_LIST\n"); }
append                  { printf("KW_APPEND\n"); }
concat                  { printf("KW_CONCAT\n"); }
set                     { printf("KW_SET\n"); }
def                     { printf("KW_DEF\n"); }
for                     { printf("KW_FOR\n"); }
if                      { printf("KW_IF\n"); }
exit                    { printf("KW_EXIT\n"); }
load                    { printf("KW_LOAD\n"); }
display                 { printf("KW_DISPLAY\n"); }
true                    { printf("KW_TRUE\n"); }
false                   { printf("KW_FALSE\n"); }
\+                      { printf("OP_PLUS\n"); }
\-                      { printf("OP_MINUS\n"); }
\/                      { printf("OP_DIV\n"); }
\*                      { printf("OP_MULT\n"); }
\(                      { printf("OP_OP\n"); }
\)                      { printf("OP_CP\n"); }
,                       { printf("OP_COMMA\n"); }
;;.*                    { printf("COMMENT\n"); }
[0-9]+b[0-9]+			{ printf("VALUEF\n"); }
[a-zA-Z][a-zA-Z0-9]*    { printf("IDENTIFIER\n"); }

[ \t\n]                 ; 													/* Whitespace and unknown characters are ignored. */
\r				    ;													    /* Return carriage is ignored. */
.                       { 
							printf("SYNTAX_ERROR %s cannot be tokenized \n", yytext);
							return 0;
						}
%%

int yywrap() {}

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