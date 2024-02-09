%{
int yylex();
#include "gpp.h"

struct ExpressionNode {
	char type; // 'v':valuef, 'i':identifier, 'f':function call, 'o':operation
	char* valuef;
	char* id;
	char op; // '+', '-', '*', '/
	int numActualParams;
	struct ExpressionNode* first;
	struct ExpressionNode* second;
	struct ExpressionNode* third;
};

struct FunctionNode {
	char* id;
	char** parameters;
	int numParameters;
	struct ExpressionNode* body;
};

struct Variable {
	char* id;
	char* valuef;
};

void yyerror(const char *s);
void freeExpNodePtr(struct ExpressionNode* expNodePtr);
void addVariableToList(struct Variable* varPtr);
void addFunctionNodeToList(struct FunctionNode* funcNodePtr);
struct FunctionNode* searchFunction(char* id);
struct Variable* searchVarible(char* id);
void convertFractionToInteger(char* fraction, int* num, int* denom);
char* evalFunction(struct FunctionNode* funcNodePtr, char* actualParam1, char* actualParam2, char* actualParam3);
char* evalExpression(struct ExpressionNode* expNode);
char* concatStr(char* str1, char* str2);

struct Variable** varPtrs;
int lastVarIdx;
int varsSize;

struct FunctionNode** funcNodePtrs;
int lastFuncIdx;
int funcsSize;

%}

%union {
	char* str;
	struct ExpressionNode* expNodePtr;
}

%token <str> IDENTIFIER
%token <str> VALUEF
%token <str> SYNTAX_ERROR
%token KW_AND KW_OR KW_NOT KW_EQUAL KW_LESS KW_NIL KW_LIST KW_APPEND KW_CONCAT KW_SET KW_DEF KW_FOR KW_IF KW_EXIT KW_LOAD KW_DISPLAY KW_TRUE KW_FALSE
%token OP_PLUS OP_MINUS OP_DIV OP_MULT OP_OP OP_CP OP_COMMA
%token COMMENT

%type <expNodePtr> EXP // Change the type here

%start STARTLIST

%%

STARTLIST: STARTLIST START | START;

START: 
	EXP { 
		char* result = evalExpression($1);
		printf("%s\n", result); 
		free(result);
	}
	| FUNCTION { printf("#FUNCTION\n"); }
	| OP_OP KW_EXIT OP_CP { exit(0); }
	| SYNTAX_ERROR { yyerror(concatStr("Syntax error! Error in tokenizing ", $1)); };

EXP: OP_OP OP_PLUS EXP EXP OP_CP {
		struct ExpressionNode* expNodePtr = (struct ExpressionNode*) malloc(sizeof(struct ExpressionNode));
		expNodePtr->type = 'o';
		expNodePtr->op = '+';
		expNodePtr->first = $3;
		expNodePtr->second = $4;
		$$ = expNodePtr;
	}
    | OP_OP OP_MINUS EXP EXP OP_CP { 
		struct ExpressionNode* expNodePtr = (struct ExpressionNode*) malloc(sizeof(struct ExpressionNode));
		expNodePtr->type = 'o';
		expNodePtr->op = '-';
		expNodePtr->first = $3;
		expNodePtr->second = $4;
		$$ = expNodePtr;
	}
	| OP_OP OP_MULT EXP EXP OP_CP {
		struct ExpressionNode* expNodePtr = (struct ExpressionNode*) malloc(sizeof(struct ExpressionNode));
		expNodePtr->type = 'o';
		expNodePtr->op = '*';
		expNodePtr->first = $3;
		expNodePtr->second = $4;
		$$ = expNodePtr;
	}
    | OP_OP OP_DIV EXP EXP OP_CP {
		struct ExpressionNode* expNodePtr = (struct ExpressionNode*) malloc(sizeof(struct ExpressionNode));
		expNodePtr->type = 'o';
		expNodePtr->op = '/';
		expNodePtr->first = $3;
		expNodePtr->second = $4;
		$$ = expNodePtr;
	}
    | OP_OP IDENTIFIER OP_CP {
		struct ExpressionNode* expNodePtr = (struct ExpressionNode*) malloc(sizeof(struct ExpressionNode));
		expNodePtr->type = 'f';
		expNodePtr->id = $2;
		expNodePtr->numActualParams = 0;
		$$ = expNodePtr;
	}
    | OP_OP IDENTIFIER EXP OP_CP {
		struct ExpressionNode* expNodePtr = (struct ExpressionNode*) malloc(sizeof(struct ExpressionNode));
		expNodePtr->type = 'f';
		expNodePtr->id = $2;
		expNodePtr->numActualParams = 1;
		expNodePtr->first = $3;
		$$ = expNodePtr;
	}
    | OP_OP IDENTIFIER EXP EXP OP_CP {  
		struct ExpressionNode* expNodePtr = (struct ExpressionNode*) malloc(sizeof(struct ExpressionNode));
		expNodePtr->type = 'f';
		expNodePtr->id = $2;
		expNodePtr->numActualParams = 2;
		expNodePtr->first = $3;
		expNodePtr->second = $4;		
		$$ = expNodePtr;
	}
    | OP_OP IDENTIFIER EXP EXP EXP OP_CP {
		struct ExpressionNode* expNodePtr = (struct ExpressionNode*) malloc(sizeof(struct ExpressionNode));
		expNodePtr->type = 'f';
		expNodePtr->id = $2;
		expNodePtr->numActualParams = 3;
		expNodePtr->first = $3;
		expNodePtr->second = $4;
		expNodePtr->third = $5;
		$$ = expNodePtr;
	}
    | IDENTIFIER {
		struct ExpressionNode* expNodePtr = (struct ExpressionNode*) malloc(sizeof(struct ExpressionNode));
		expNodePtr->type = 'i';
		expNodePtr->id = $1;
		$$ = expNodePtr;
	}
	| VALUEF {
		int num;
		int denom;
		convertFractionToInteger($1, &num, &denom);
		if (denom == 0) {
			yyerror("Syntax error! Denom cannot be 0 in VALUEF.");
		}
	
		struct ExpressionNode* expNodePtr = (struct ExpressionNode*) malloc(sizeof(struct ExpressionNode));
		expNodePtr->type = 'v';
		expNodePtr->valuef = $1;
		$$ = expNodePtr;
	};
	
FUNCTION: OP_OP KW_DEF IDENTIFIER EXP OP_CP {
		struct FunctionNode* funcNodePtr = (struct FunctionNode*) malloc(sizeof(struct FunctionNode));
		funcNodePtr->id = $3;
		funcNodePtr->numParameters = 0;
		funcNodePtr->body = $4;
        addFunctionNodeToList(funcNodePtr);
	}
	| OP_OP KW_DEF IDENTIFIER IDENTIFIER EXP OP_CP {
		struct FunctionNode* funcNodePtr = (struct FunctionNode*) malloc(sizeof(struct FunctionNode));
		funcNodePtr->id = $3;
		funcNodePtr->numParameters = 1;
		funcNodePtr->parameters = (char**) malloc(sizeof(char*) * 1);
		(funcNodePtr->parameters)[0] = $4;
		funcNodePtr->body = $5;
        addFunctionNodeToList(funcNodePtr);
	}
	| OP_OP KW_DEF IDENTIFIER IDENTIFIER IDENTIFIER EXP OP_CP {
		struct FunctionNode* funcNodePtr = (struct FunctionNode*) malloc(sizeof(struct FunctionNode));
		funcNodePtr->id = $3;
		funcNodePtr->numParameters = 2;
		funcNodePtr->parameters = (char**) malloc(sizeof(char*) * 2);
		(funcNodePtr->parameters)[0] = $4;
		(funcNodePtr->parameters)[1] = $5;
		funcNodePtr->body = $6;
        addFunctionNodeToList(funcNodePtr);
	}
	| OP_OP KW_DEF IDENTIFIER IDENTIFIER IDENTIFIER IDENTIFIER EXP OP_CP {
		struct FunctionNode* funcNodePtr = (struct FunctionNode*) malloc(sizeof(struct FunctionNode));
		funcNodePtr->id = $3;
		funcNodePtr->numParameters = 3;
		funcNodePtr->parameters = (char**) malloc(sizeof(char*) * 3);
		(funcNodePtr->parameters)[0] = $4;
		(funcNodePtr->parameters)[1] = $5;
		(funcNodePtr->parameters)[2] = $6;
		funcNodePtr->body = $7;
        addFunctionNodeToList(funcNodePtr);
	};

%%

void freeExpNodePtr(struct ExpressionNode* expNodePtr) {
	free(expNodePtr->id);
	free(expNodePtr->valuef);
}

void addVariableToList(struct Variable* varPtr) {
	if (lastVarIdx == varsSize - 1) {
		varsSize = varsSize * 2;
		struct Variable** newList = (struct Variable**)malloc(varsSize * sizeof(struct Variable*));
		int i;
		for (i = 0; i <= lastVarIdx; ++i) {
			newList[i] = varPtrs[i];
		}		
		varPtrs = newList;
	}
	varPtrs[++lastVarIdx] = varPtr;
}

void addFunctionNodeToList(struct FunctionNode* funcNodePtr) {
	if (lastFuncIdx == funcsSize - 1) {
		funcsSize = funcsSize * 2;
		struct FunctionNode** newList = (struct FunctionNode**)malloc(funcsSize * sizeof(struct FunctionNode*));
		int i;
		for (i = 0; i <= lastFuncIdx; ++i) {
			newList[i] = funcNodePtrs[i];
		}		
		funcNodePtrs = newList;
	}
	funcNodePtrs[++lastFuncIdx] = funcNodePtr;
}

void convertFractionToInteger(char* fraction, int* num, int* denom) {
    *num = 0;
    *denom = 0;
    int isNegative = 1; // Flag to track the sign

    // Iterate through the characters in the string
    char* ptr;
    int beforeB = 1;
    for (ptr = fraction; *ptr != '\0'; ++ptr) {
        // If the character is a digit, update the corresponding value
        if (*ptr == 'b') {
            beforeB = 0;
            isNegative = 1; // Reset the sign flag for the denominator part
        } else if (*ptr == '-') {
            isNegative = -1; // Set the sign flag for negative numbers
        } else if (beforeB == 1) {
            *num = (*num) * 10 + isNegative * (*ptr - '0');
        } else {
            *denom = (*denom) * 10 + (*ptr - '0');
        }
    }
}

char* evalExpression(struct ExpressionNode* expNode) {
	if (expNode == NULL) {
		return NULL;
	}

	if (expNode->type == 'v') {
		return strdup(expNode->valuef);
	}

	if (expNode->type == 'o') {
		char* resultString;
		char* evalLeft = evalExpression(expNode->first);
		char* evalRight = evalExpression(expNode->second);
		
		int numLeft;
		int denomLeft;
		convertFractionToInteger(evalLeft, &numLeft, &denomLeft);
			
		int numRight;
		int denomRight;
		convertFractionToInteger(evalRight, &numRight, &denomRight);
		
		int resultNum;
		int resultDenom;
		
		if (expNode->op == '+') {
			resultNum = numLeft * denomRight + numRight * denomLeft;
			resultDenom = denomLeft * denomRight;		
		}
		else if (expNode->op == '-') {
			resultNum = numLeft * denomRight - numRight * denomLeft;
			resultDenom = denomLeft * denomRight;		
		}
		
		else if (expNode->op == '*') {
			resultNum = numLeft * numRight;
			resultDenom = denomLeft * denomRight;		
		}
		
		else if (expNode->op == '/') {
			if (numRight == 0 || denomLeft == 0) {
				yyerror("Syntax error! Division by 0 is not allowed.");
			}
			resultNum = numLeft * denomRight;
			resultDenom = denomLeft * numRight;		
		}
		
		// Determine the size needed for the result string
		int size = snprintf(NULL, 0, "%d b %d", resultNum, resultDenom);
		
		// Allocate memory for the result string
		resultString = (char*) malloc(size + 1); // Add 1 for the null terminator

		snprintf(resultString, size + 1, "%db%d", resultNum, resultDenom);	
		
		free(evalRight);
		free(evalLeft);
		return resultString;
	}
	
	if (expNode->type == 'f') {
		/* Check if there is such function */
		
		struct FunctionNode* func = searchFunction(expNode->id);
		if (func == NULL) {
			yyerror("Syntax error! Function is not defined.");
		}
		
		/* Check if numOfParameters matching */
		
		if (func->numParameters != expNode->numActualParams) {
			yyerror("Syntax error! Actual parameters are not matching with function parameters.");
		}
		
		/* Eval the parameters and give it to the evalFunc() */
		
		char* evalFirst = evalExpression(expNode->first);
		char* evalSecond = evalExpression(expNode->second);
		char* evalThird = evalExpression(expNode->third);
		char* result = evalFunction(func, evalFirst, evalSecond, evalThird);
		
		// Free the eval results and return
		
		free(evalFirst);
		free(evalSecond);
		return result;
	}
	
	if (expNode->type == 'i') {	
		// Check if there is such variable with given id
		struct Variable* var = searchVarible(expNode->id);

		if (var == NULL) {
			yyerror("Syntax error! Variable is not defined.");
		}
		
		// Return a copy of valuef of this varible
		return strdup(var->valuef);
	}
	
	yyerror("Syntax error! Problem with evaluating expression.");
	return NULL;
}

struct FunctionNode* searchFunction(char* id) {
	if (lastFuncIdx == -1) {
		return NULL;
	}

	int i;
	for (i = 0; i <= lastFuncIdx; ++i) {
		if (strcmp((funcNodePtrs[i])->id, id) == 0) {
			return funcNodePtrs[i];
		}
	}	
	return NULL;
}

struct Variable* searchVarible(char* id) {
	if (lastVarIdx == -1) {
		return NULL;
	}
	
	int i;
	for (i = lastVarIdx; i >= 0; --i) {
		if (strcmp((varPtrs[i])->id, id) == 0) {
			return varPtrs[i];
		}
	}	
	return NULL;
}

char* evalFunction(struct FunctionNode* funcNodePtr, char* actualParam1, char* actualParam2, char* actualParam3) {
	// Create dynamically-bind variables and add to variable list
	
	if (funcNodePtr->numParameters >= 1) {
		struct Variable* var1 = (struct Variable*) malloc(sizeof(struct Variable) * 1);
		var1->id = (funcNodePtr->parameters)[0];
		var1->valuef = actualParam1;
		addVariableToList(var1);
	}
	
	if (funcNodePtr->numParameters >=2) {
		struct Variable* var2 = (struct Variable*) malloc(sizeof(struct Variable) * 1);
		var2->id = (funcNodePtr->parameters)[1];
		var2->valuef = actualParam2;
		addVariableToList(var2);
	}
	
	
	if (funcNodePtr->numParameters >=3) {
		struct Variable* var3 = (struct Variable*) malloc(sizeof(struct Variable) * 1);
		var3->id = (funcNodePtr->parameters)[2];
		var3->valuef = actualParam3;
		addVariableToList(var3);
	}
		
	// Call Eval expression
	char* result = evalExpression(funcNodePtr->body);

	// Free variables (dont free char* id and char* valuef. char* id is the exact one inside FunctionNode and char* valuef is deleted in evalExpression())
	int i;
	for (i = 0; i < funcNodePtr->numParameters; ++i) {
		free(varPtrs[lastVarIdx - i]);
	}
	lastVarIdx = lastVarIdx - (funcNodePtr->numParameters);
	
	// Return the result
	return result;
}

void yyerror(const char *s) {
	if (strcmp("syntax error", s) == 0) {
		fprintf(stderr, "Syntax error!\n");
	}
	else {
		fprintf(stderr, "%s\n", s);
    }
	exit(EXIT_FAILURE);
}

char* concatStr(char* s1, char* s2) {
	// Allocate memory for the concatenated string
    char *result = (char *)malloc(strlen(s1) + strlen(s2) + 1);

    // Check if memory allocation was successful
    if (result == NULL) {
        fprintf(stderr, "Memory allocation failed\n");
        exit(EXIT_FAILURE);
    }

    // Copy the first string into the result
    strcpy(result, s1);

    // Concatenate the second string onto the result
    strcat(result, s2);

    return result;
}

int main(int argc, char *argv[]) {
    lastVarIdx = -1;
	varsSize = 1000;

	lastFuncIdx = -1;
	funcsSize = 1000;
	
	varPtrs = (struct Variable**)malloc(varsSize * sizeof(struct Variable*));
	funcNodePtrs = (struct FunctionNode**)malloc(funcsSize * sizeof(struct FunctionNode*));
	
	if (argc == 1)  {
		yyparse();
		return 0;
	}
	
	else if (argc == 2) {
        FILE *input_file = fopen(argv[1], "r");
		if (input_file == NULL) {
			perror("Error opening input file");
			return 1;
		}

		yyin = input_file;
		yyparse();

		fclose(input_file);
		return 0;
    }
	
	else {
		printf("You should enter only 1 or 2 arguments. Program is closed.\n");
		return 0;
	}
}
