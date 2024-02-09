/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

#ifndef YY_YY_Y_TAB_H_INCLUDED
# define YY_YY_Y_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token kinds.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    YYEMPTY = -2,
    YYEOF = 0,                     /* "end of file"  */
    YYerror = 256,                 /* error  */
    YYUNDEF = 257,                 /* "invalid token"  */
    IDENTIFIER = 258,              /* IDENTIFIER  */
    VALUEF = 259,                  /* VALUEF  */
    SYNTAX_ERROR = 260,            /* SYNTAX_ERROR  */
    KW_AND = 261,                  /* KW_AND  */
    KW_OR = 262,                   /* KW_OR  */
    KW_NOT = 263,                  /* KW_NOT  */
    KW_EQUAL = 264,                /* KW_EQUAL  */
    KW_LESS = 265,                 /* KW_LESS  */
    KW_NIL = 266,                  /* KW_NIL  */
    KW_LIST = 267,                 /* KW_LIST  */
    KW_APPEND = 268,               /* KW_APPEND  */
    KW_CONCAT = 269,               /* KW_CONCAT  */
    KW_SET = 270,                  /* KW_SET  */
    KW_DEF = 271,                  /* KW_DEF  */
    KW_FOR = 272,                  /* KW_FOR  */
    KW_IF = 273,                   /* KW_IF  */
    KW_EXIT = 274,                 /* KW_EXIT  */
    KW_LOAD = 275,                 /* KW_LOAD  */
    KW_DISPLAY = 276,              /* KW_DISPLAY  */
    KW_TRUE = 277,                 /* KW_TRUE  */
    KW_FALSE = 278,                /* KW_FALSE  */
    OP_PLUS = 279,                 /* OP_PLUS  */
    OP_MINUS = 280,                /* OP_MINUS  */
    OP_DIV = 281,                  /* OP_DIV  */
    OP_MULT = 282,                 /* OP_MULT  */
    OP_OP = 283,                   /* OP_OP  */
    OP_CP = 284,                   /* OP_CP  */
    OP_COMMA = 285,                /* OP_COMMA  */
    COMMENT = 286                  /* COMMENT  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif
/* Token kinds.  */
#define YYEMPTY -2
#define YYEOF 0
#define YYerror 256
#define YYUNDEF 257
#define IDENTIFIER 258
#define VALUEF 259
#define SYNTAX_ERROR 260
#define KW_AND 261
#define KW_OR 262
#define KW_NOT 263
#define KW_EQUAL 264
#define KW_LESS 265
#define KW_NIL 266
#define KW_LIST 267
#define KW_APPEND 268
#define KW_CONCAT 269
#define KW_SET 270
#define KW_DEF 271
#define KW_FOR 272
#define KW_IF 273
#define KW_EXIT 274
#define KW_LOAD 275
#define KW_DISPLAY 276
#define KW_TRUE 277
#define KW_FALSE 278
#define OP_PLUS 279
#define OP_MINUS 280
#define OP_DIV 281
#define OP_MULT 282
#define OP_OP 283
#define OP_CP 284
#define OP_COMMA 285
#define COMMENT 286

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 49 "gpp_interpreter.y"

	char* str;
	struct ExpressionNode* expNodePtr;

#line 134 "y.tab.h"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;


int yyparse (void);


#endif /* !YY_YY_Y_TAB_H_INCLUDED  */
