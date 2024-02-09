/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison implementation for Yacc-like parsers in C

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

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output, and Bison version.  */
#define YYBISON 30802

/* Bison version string.  */
#define YYBISON_VERSION "3.8.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* First part of user prologue.  */
#line 1 "gpp_interpreter.y"

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


#line 119 "y.tab.c"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

/* Use api.header.include to #include this header
   instead of duplicating it here.  */
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

#line 239 "y.tab.c"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;


int yyparse (void);


#endif /* !YY_YY_Y_TAB_H_INCLUDED  */
/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_IDENTIFIER = 3,                 /* IDENTIFIER  */
  YYSYMBOL_VALUEF = 4,                     /* VALUEF  */
  YYSYMBOL_SYNTAX_ERROR = 5,               /* SYNTAX_ERROR  */
  YYSYMBOL_KW_AND = 6,                     /* KW_AND  */
  YYSYMBOL_KW_OR = 7,                      /* KW_OR  */
  YYSYMBOL_KW_NOT = 8,                     /* KW_NOT  */
  YYSYMBOL_KW_EQUAL = 9,                   /* KW_EQUAL  */
  YYSYMBOL_KW_LESS = 10,                   /* KW_LESS  */
  YYSYMBOL_KW_NIL = 11,                    /* KW_NIL  */
  YYSYMBOL_KW_LIST = 12,                   /* KW_LIST  */
  YYSYMBOL_KW_APPEND = 13,                 /* KW_APPEND  */
  YYSYMBOL_KW_CONCAT = 14,                 /* KW_CONCAT  */
  YYSYMBOL_KW_SET = 15,                    /* KW_SET  */
  YYSYMBOL_KW_DEF = 16,                    /* KW_DEF  */
  YYSYMBOL_KW_FOR = 17,                    /* KW_FOR  */
  YYSYMBOL_KW_IF = 18,                     /* KW_IF  */
  YYSYMBOL_KW_EXIT = 19,                   /* KW_EXIT  */
  YYSYMBOL_KW_LOAD = 20,                   /* KW_LOAD  */
  YYSYMBOL_KW_DISPLAY = 21,                /* KW_DISPLAY  */
  YYSYMBOL_KW_TRUE = 22,                   /* KW_TRUE  */
  YYSYMBOL_KW_FALSE = 23,                  /* KW_FALSE  */
  YYSYMBOL_OP_PLUS = 24,                   /* OP_PLUS  */
  YYSYMBOL_OP_MINUS = 25,                  /* OP_MINUS  */
  YYSYMBOL_OP_DIV = 26,                    /* OP_DIV  */
  YYSYMBOL_OP_MULT = 27,                   /* OP_MULT  */
  YYSYMBOL_OP_OP = 28,                     /* OP_OP  */
  YYSYMBOL_OP_CP = 29,                     /* OP_CP  */
  YYSYMBOL_OP_COMMA = 30,                  /* OP_COMMA  */
  YYSYMBOL_COMMENT = 31,                   /* COMMENT  */
  YYSYMBOL_YYACCEPT = 32,                  /* $accept  */
  YYSYMBOL_STARTLIST = 33,                 /* STARTLIST  */
  YYSYMBOL_START = 34,                     /* START  */
  YYSYMBOL_EXP = 35,                       /* EXP  */
  YYSYMBOL_FUNCTION = 36                   /* FUNCTION  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;




#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

/* Work around bug in HP-UX 11.23, which defines these macros
   incorrectly for preprocessor constants.  This workaround can likely
   be removed in 2023, as HPE has promised support for HP-UX 11.23
   (aka HP-UX 11i v2) only through the end of 2022; see Table 2 of
   <https://h20195.www2.hpe.com/V2/getpdf.aspx/4AA4-7673ENW.pdf>.  */
#ifdef __hpux
# undef UINT_LEAST8_MAX
# undef UINT_LEAST16_MAX
# define UINT_LEAST8_MAX 255
# define UINT_LEAST16_MAX 65535
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))


/* Stored state numbers (used for stacks). */
typedef yytype_int8 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif


#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YY_USE(E) ((void) (E))
#else
# define YY_USE(E) /* empty */
#endif

/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
#if defined __GNUC__ && ! defined __ICC && 406 <= __GNUC__ * 100 + __GNUC_MINOR__
# if __GNUC__ * 100 + __GNUC_MINOR__ < 407
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")
# else
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# endif
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if !defined yyoverflow

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* !defined yyoverflow */

#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  16
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   75

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  32
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  5
/* YYNRULES -- Number of rules.  */
#define YYNRULES  21
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  51

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   286


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK                     \
   ? YY_CAST (yysymbol_kind_t, yytranslate[YYX])        \
   : YYSYMBOL_YYUNDEF)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_int8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31
};

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint8 yyrline[] =
{
       0,    67,    67,    67,    70,    75,    76,    77,    79,    87,
      95,   103,   111,   118,   126,   135,   145,   151,   165,   172,
     181,   191
};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if YYDEBUG || 0
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "\"invalid token\"", "IDENTIFIER", "VALUEF",
  "SYNTAX_ERROR", "KW_AND", "KW_OR", "KW_NOT", "KW_EQUAL", "KW_LESS",
  "KW_NIL", "KW_LIST", "KW_APPEND", "KW_CONCAT", "KW_SET", "KW_DEF",
  "KW_FOR", "KW_IF", "KW_EXIT", "KW_LOAD", "KW_DISPLAY", "KW_TRUE",
  "KW_FALSE", "OP_PLUS", "OP_MINUS", "OP_DIV", "OP_MULT", "OP_OP", "OP_CP",
  "OP_COMMA", "COMMENT", "$accept", "STARTLIST", "START", "EXP",
  "FUNCTION", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-10)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int8 yypact[] =
{
      26,   -10,   -10,   -10,    44,    18,   -10,   -10,   -10,    -2,
      10,    14,    21,    21,    21,    21,   -10,   -10,    48,   -10,
       4,    34,   -10,    21,    21,    21,    21,   -10,     6,    36,
      15,    16,    19,    23,    24,   -10,    27,    38,    28,   -10,
     -10,   -10,   -10,   -10,   -10,    21,    29,   -10,    30,   -10,
     -10
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int8 yydefact[] =
{
       0,    16,    17,     7,     0,     0,     3,     4,     5,     0,
       0,     0,     0,     0,     0,     0,     1,     2,     0,    12,
       0,     0,     6,     0,     0,     0,     0,    13,     0,    16,
       0,     0,     0,     0,     0,    14,     0,    16,     0,    18,
       8,     9,    11,    10,    15,    16,     0,    19,     0,    20,
      21
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -10,   -10,    45,    -9,   -10
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
       0,     5,     6,     7,     8
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int8 yytable[] =
{
      20,     1,     2,    23,    24,    25,    26,     1,     2,     1,
       2,    28,    30,    21,    31,    32,    33,    34,    16,    36,
      38,     1,     2,     3,     1,     2,    18,    19,    46,     1,
       2,     3,    18,    27,    18,    35,    48,    29,     2,    37,
       2,    45,     2,    22,    39,    40,     4,     9,    41,    18,
      17,     9,    42,    43,     4,     0,    44,    47,    49,    50,
      10,     0,    18,    11,    18,     0,    18,     0,    12,    13,
      14,    15,    12,    13,    14,    15
};

static const yytype_int8 yycheck[] =
{
       9,     3,     4,    12,    13,    14,    15,     3,     4,     3,
       4,    20,    21,     3,    23,    24,    25,    26,     0,    28,
      29,     3,     4,     5,     3,     4,    28,    29,    37,     3,
       4,     5,    28,    29,    28,    29,    45,     3,     4,     3,
       4,     3,     4,    29,    29,    29,    28,     3,    29,    28,
       5,     3,    29,    29,    28,    -1,    29,    29,    29,    29,
      16,    -1,    28,    19,    28,    -1,    28,    -1,    24,    25,
      26,    27,    24,    25,    26,    27
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int8 yystos[] =
{
       0,     3,     4,     5,    28,    33,    34,    35,    36,     3,
      16,    19,    24,    25,    26,    27,     0,    34,    28,    29,
      35,     3,    29,    35,    35,    35,    35,    29,    35,     3,
      35,    35,    35,    35,    35,    29,    35,     3,    35,    29,
      29,    29,    29,    29,    29,     3,    35,    29,    35,    29,
      29
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr1[] =
{
       0,    32,    33,    33,    34,    34,    34,    34,    35,    35,
      35,    35,    35,    35,    35,    35,    35,    35,    36,    36,
      36,    36
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     2,     1,     1,     1,     3,     1,     5,     5,
       5,     5,     3,     4,     5,     6,     1,     1,     5,     6,
       7,     8
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYNOMEM         goto yyexhaustedlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Backward compatibility with an undocumented macro.
   Use YYerror or YYUNDEF. */
#define YYERRCODE YYUNDEF


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)




# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  FILE *yyoutput = yyo;
  YY_USE (yyoutput);
  if (!yyvaluep)
    return;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo,
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyo, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  yy_symbol_value_print (yyo, yykind, yyvaluep);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp,
                 int yyrule)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       YY_ACCESSING_SYMBOL (+yyssp[yyi + 1 - yynrhs]),
                       &yyvsp[(yyi + 1) - (yynrhs)]);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif






/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg,
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep)
{
  YY_USE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/* Lookahead token kind.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;




/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    yy_state_fast_t yystate = 0;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus = 0;

    /* Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* Their size.  */
    YYPTRDIFF_T yystacksize = YYINITDEPTH;

    /* The state stack: array, bottom, top.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss = yyssa;
    yy_state_t *yyssp = yyss;

    /* The semantic value stack: array, bottom, top.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs = yyvsa;
    YYSTYPE *yyvsp = yyvs;

  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead symbol kind.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yychar = YYEMPTY; /* Cause a token to be read.  */

  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END
  YY_STACK_PRINT (yyss, yyssp);

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    YYNOMEM;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        YYNOMEM;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          YYNOMEM;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */


  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token\n"));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = YYEOF;
      yytoken = YYSYMBOL_YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else if (yychar == YYerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      yychar = YYUNDEF;
      yytoken = YYSYMBOL_YYerror;
      goto yyerrlab1;
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 4: /* START: EXP  */
#line 70 "gpp_interpreter.y"
            { 
		char* result = evalExpression((yyvsp[0].expNodePtr));
		printf("%s\n", result); 
		free(result);
	}
#line 1287 "y.tab.c"
    break;

  case 5: /* START: FUNCTION  */
#line 75 "gpp_interpreter.y"
                   { printf("#FUNCTION\n"); }
#line 1293 "y.tab.c"
    break;

  case 6: /* START: OP_OP KW_EXIT OP_CP  */
#line 76 "gpp_interpreter.y"
                              { exit(0); }
#line 1299 "y.tab.c"
    break;

  case 7: /* START: SYNTAX_ERROR  */
#line 77 "gpp_interpreter.y"
                       { yyerror(concatStr("Syntax error! Error in tokenizing ", (yyvsp[0].str))); }
#line 1305 "y.tab.c"
    break;

  case 8: /* EXP: OP_OP OP_PLUS EXP EXP OP_CP  */
#line 79 "gpp_interpreter.y"
                                 {
		struct ExpressionNode* expNodePtr = (struct ExpressionNode*) malloc(sizeof(struct ExpressionNode));
		expNodePtr->type = 'o';
		expNodePtr->op = '+';
		expNodePtr->first = (yyvsp[-2].expNodePtr);
		expNodePtr->second = (yyvsp[-1].expNodePtr);
		(yyval.expNodePtr) = expNodePtr;
	}
#line 1318 "y.tab.c"
    break;

  case 9: /* EXP: OP_OP OP_MINUS EXP EXP OP_CP  */
#line 87 "gpp_interpreter.y"
                                   { 
		struct ExpressionNode* expNodePtr = (struct ExpressionNode*) malloc(sizeof(struct ExpressionNode));
		expNodePtr->type = 'o';
		expNodePtr->op = '-';
		expNodePtr->first = (yyvsp[-2].expNodePtr);
		expNodePtr->second = (yyvsp[-1].expNodePtr);
		(yyval.expNodePtr) = expNodePtr;
	}
#line 1331 "y.tab.c"
    break;

  case 10: /* EXP: OP_OP OP_MULT EXP EXP OP_CP  */
#line 95 "gpp_interpreter.y"
                                      {
		struct ExpressionNode* expNodePtr = (struct ExpressionNode*) malloc(sizeof(struct ExpressionNode));
		expNodePtr->type = 'o';
		expNodePtr->op = '*';
		expNodePtr->first = (yyvsp[-2].expNodePtr);
		expNodePtr->second = (yyvsp[-1].expNodePtr);
		(yyval.expNodePtr) = expNodePtr;
	}
#line 1344 "y.tab.c"
    break;

  case 11: /* EXP: OP_OP OP_DIV EXP EXP OP_CP  */
#line 103 "gpp_interpreter.y"
                                 {
		struct ExpressionNode* expNodePtr = (struct ExpressionNode*) malloc(sizeof(struct ExpressionNode));
		expNodePtr->type = 'o';
		expNodePtr->op = '/';
		expNodePtr->first = (yyvsp[-2].expNodePtr);
		expNodePtr->second = (yyvsp[-1].expNodePtr);
		(yyval.expNodePtr) = expNodePtr;
	}
#line 1357 "y.tab.c"
    break;

  case 12: /* EXP: OP_OP IDENTIFIER OP_CP  */
#line 111 "gpp_interpreter.y"
                             {
		struct ExpressionNode* expNodePtr = (struct ExpressionNode*) malloc(sizeof(struct ExpressionNode));
		expNodePtr->type = 'f';
		expNodePtr->id = (yyvsp[-1].str);
		expNodePtr->numActualParams = 0;
		(yyval.expNodePtr) = expNodePtr;
	}
#line 1369 "y.tab.c"
    break;

  case 13: /* EXP: OP_OP IDENTIFIER EXP OP_CP  */
#line 118 "gpp_interpreter.y"
                                 {
		struct ExpressionNode* expNodePtr = (struct ExpressionNode*) malloc(sizeof(struct ExpressionNode));
		expNodePtr->type = 'f';
		expNodePtr->id = (yyvsp[-2].str);
		expNodePtr->numActualParams = 1;
		expNodePtr->first = (yyvsp[-1].expNodePtr);
		(yyval.expNodePtr) = expNodePtr;
	}
#line 1382 "y.tab.c"
    break;

  case 14: /* EXP: OP_OP IDENTIFIER EXP EXP OP_CP  */
#line 126 "gpp_interpreter.y"
                                     {  
		struct ExpressionNode* expNodePtr = (struct ExpressionNode*) malloc(sizeof(struct ExpressionNode));
		expNodePtr->type = 'f';
		expNodePtr->id = (yyvsp[-3].str);
		expNodePtr->numActualParams = 2;
		expNodePtr->first = (yyvsp[-2].expNodePtr);
		expNodePtr->second = (yyvsp[-1].expNodePtr);		
		(yyval.expNodePtr) = expNodePtr;
	}
#line 1396 "y.tab.c"
    break;

  case 15: /* EXP: OP_OP IDENTIFIER EXP EXP EXP OP_CP  */
#line 135 "gpp_interpreter.y"
                                         {
		struct ExpressionNode* expNodePtr = (struct ExpressionNode*) malloc(sizeof(struct ExpressionNode));
		expNodePtr->type = 'f';
		expNodePtr->id = (yyvsp[-4].str);
		expNodePtr->numActualParams = 3;
		expNodePtr->first = (yyvsp[-3].expNodePtr);
		expNodePtr->second = (yyvsp[-2].expNodePtr);
		expNodePtr->third = (yyvsp[-1].expNodePtr);
		(yyval.expNodePtr) = expNodePtr;
	}
#line 1411 "y.tab.c"
    break;

  case 16: /* EXP: IDENTIFIER  */
#line 145 "gpp_interpreter.y"
                 {
		struct ExpressionNode* expNodePtr = (struct ExpressionNode*) malloc(sizeof(struct ExpressionNode));
		expNodePtr->type = 'i';
		expNodePtr->id = (yyvsp[0].str);
		(yyval.expNodePtr) = expNodePtr;
	}
#line 1422 "y.tab.c"
    break;

  case 17: /* EXP: VALUEF  */
#line 151 "gpp_interpreter.y"
                 {
		int num;
		int denom;
		convertFractionToInteger((yyvsp[0].str), &num, &denom);
		if (denom == 0) {
			yyerror("Syntax error! Denom cannot be 0 in VALUEF.");
		}
	
		struct ExpressionNode* expNodePtr = (struct ExpressionNode*) malloc(sizeof(struct ExpressionNode));
		expNodePtr->type = 'v';
		expNodePtr->valuef = (yyvsp[0].str);
		(yyval.expNodePtr) = expNodePtr;
	}
#line 1440 "y.tab.c"
    break;

  case 18: /* FUNCTION: OP_OP KW_DEF IDENTIFIER EXP OP_CP  */
#line 165 "gpp_interpreter.y"
                                            {
		struct FunctionNode* funcNodePtr = (struct FunctionNode*) malloc(sizeof(struct FunctionNode));
		funcNodePtr->id = (yyvsp[-2].str);
		funcNodePtr->numParameters = 0;
		funcNodePtr->body = (yyvsp[-1].expNodePtr);
        addFunctionNodeToList(funcNodePtr);
	}
#line 1452 "y.tab.c"
    break;

  case 19: /* FUNCTION: OP_OP KW_DEF IDENTIFIER IDENTIFIER EXP OP_CP  */
#line 172 "gpp_interpreter.y"
                                                       {
		struct FunctionNode* funcNodePtr = (struct FunctionNode*) malloc(sizeof(struct FunctionNode));
		funcNodePtr->id = (yyvsp[-3].str);
		funcNodePtr->numParameters = 1;
		funcNodePtr->parameters = (char**) malloc(sizeof(char*) * 1);
		(funcNodePtr->parameters)[0] = (yyvsp[-2].str);
		funcNodePtr->body = (yyvsp[-1].expNodePtr);
        addFunctionNodeToList(funcNodePtr);
	}
#line 1466 "y.tab.c"
    break;

  case 20: /* FUNCTION: OP_OP KW_DEF IDENTIFIER IDENTIFIER IDENTIFIER EXP OP_CP  */
#line 181 "gpp_interpreter.y"
                                                                  {
		struct FunctionNode* funcNodePtr = (struct FunctionNode*) malloc(sizeof(struct FunctionNode));
		funcNodePtr->id = (yyvsp[-4].str);
		funcNodePtr->numParameters = 2;
		funcNodePtr->parameters = (char**) malloc(sizeof(char*) * 2);
		(funcNodePtr->parameters)[0] = (yyvsp[-3].str);
		(funcNodePtr->parameters)[1] = (yyvsp[-2].str);
		funcNodePtr->body = (yyvsp[-1].expNodePtr);
        addFunctionNodeToList(funcNodePtr);
	}
#line 1481 "y.tab.c"
    break;

  case 21: /* FUNCTION: OP_OP KW_DEF IDENTIFIER IDENTIFIER IDENTIFIER IDENTIFIER EXP OP_CP  */
#line 191 "gpp_interpreter.y"
                                                                             {
		struct FunctionNode* funcNodePtr = (struct FunctionNode*) malloc(sizeof(struct FunctionNode));
		funcNodePtr->id = (yyvsp[-5].str);
		funcNodePtr->numParameters = 3;
		funcNodePtr->parameters = (char**) malloc(sizeof(char*) * 3);
		(funcNodePtr->parameters)[0] = (yyvsp[-4].str);
		(funcNodePtr->parameters)[1] = (yyvsp[-3].str);
		(funcNodePtr->parameters)[2] = (yyvsp[-2].str);
		funcNodePtr->body = (yyvsp[-1].expNodePtr);
        addFunctionNodeToList(funcNodePtr);
	}
#line 1497 "y.tab.c"
    break;


#line 1501 "y.tab.c"

      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", YY_CAST (yysymbol_kind_t, yyr1[yyn]), &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE (yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
      yyerror (YY_("syntax error"));
    }

  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;
  ++yynerrs;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  /* Pop stack until we find a state that shifts the error token.  */
  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYSYMBOL_YYerror;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYSYMBOL_YYerror)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  YY_ACCESSING_SYMBOL (yystate), yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", YY_ACCESSING_SYMBOL (yyn), yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturnlab;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturnlab;


/*-----------------------------------------------------------.
| yyexhaustedlab -- YYNOMEM (memory exhaustion) comes here.  |
`-----------------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturnlab;


/*----------------------------------------------------------.
| yyreturnlab -- parsing is finished, clean up and return.  |
`----------------------------------------------------------*/
yyreturnlab:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif

  return yyresult;
}

#line 203 "gpp_interpreter.y"


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
