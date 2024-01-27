/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

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

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.3"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Using locations.  */
#define YYLSP_NEEDED 0



/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     OP_OP = 258,
     OP_CP = 259,
     OP_PLUS = 260,
     OP_MINUS = 261,
     OP_DIV = 262,
     OP_MULT = 263,
     OP_COMMA = 264,
     KW_AND = 265,
     KW_OR = 266,
     KW_NOT = 267,
     KW_EQUAL = 268,
     KW_LESS = 269,
     KW_NIL = 270,
     KW_LIST = 271,
     KW_APPEND = 272,
     KW_CONCAT = 273,
     KW_SET = 274,
     KW_DEF = 275,
     KW_FOR = 276,
     KW_IF = 277,
     KW_LOAD = 278,
     KW_DISPLAY = 279,
     KW_EXIT = 280,
     KW_TRUE = 281,
     KW_FALSE = 282,
     COMMENT = 283,
     VALUEF = 284,
     IDENTIFIER = 285
   };
#endif
/* Tokens.  */
#define OP_OP 258
#define OP_CP 259
#define OP_PLUS 260
#define OP_MINUS 261
#define OP_DIV 262
#define OP_MULT 263
#define OP_COMMA 264
#define KW_AND 265
#define KW_OR 266
#define KW_NOT 267
#define KW_EQUAL 268
#define KW_LESS 269
#define KW_NIL 270
#define KW_LIST 271
#define KW_APPEND 272
#define KW_CONCAT 273
#define KW_SET 274
#define KW_DEF 275
#define KW_FOR 276
#define KW_IF 277
#define KW_LOAD 278
#define KW_DISPLAY 279
#define KW_EXIT 280
#define KW_TRUE 281
#define KW_FALSE 282
#define COMMENT 283
#define VALUEF 284
#define IDENTIFIER 285




/* Copy the first part of user declarations.  */
#line 1 "gpp_interpreter.y"

    //C DEFINITIONS
    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h>
    #include "gpp_interpreter.h"
    #define _OPEN_SYS_ITOA_EXT

    // Definitions

    extern FILE *yyin;
    
    void yyerror(char *);
    int  yylex(void);
    
    //adding function
    char* valuef_add(const char* a, const char* b);
    //Greatest common divisor function
    unsigned int gcd(unsigned int a, unsigned int b);
    //subtraction function
    char* valuef_sub(const char* a, const char* b);
    //multiply function
    char* valuef_multiply(const char* a, const char* b);
    //divide function
    char* valuef_divide(const char* a, const char* b);
    //function that define function
    void  defun_function(char*,char*);
    // one parameter function to define
    void defun_function_2(char* identifier, char* explist);
    // display one parameter function
    void displayOneParameterFunction(const char* ident, const char* exp);
    // two parameter function to define
    void defun_function_3(char* identifier, char* explist);
    // display two parameter function
    void displayTwoParameterFunction(const char* ident, const char* exp, const char* exp2);

    //function struct that has id, explist and parameters
    typedef struct Function{
        char id[50];
        char explist[100];
        int  operation;
        char valuefForOneParameter[10];
        int  boolControl;
    }Function;

    //function array struct that has size and function array
    typedef struct FuncStruct{
        int size;
        Function* funcs;
    }FuncStruct;



/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif

#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 55 "gpp_interpreter.y"
{
    char *string;
    char *valuef;
}
/* Line 193 of yacc.c.  */
#line 214 "gpp_interpreter.c"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 216 of yacc.c.  */
#line 227 "gpp_interpreter.c"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int i)
#else
static int
YYID (i)
    int i;
#endif
{
  return i;
}
#endif

#if ! defined yyoverflow || YYERROR_VERBOSE

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
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
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
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss;
  YYSTYPE yyvs;
  };

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack)					\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack, Stack, yysize);				\
	Stack = &yyptr->Stack;						\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  14
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   62

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  31
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  4
/* YYNRULES -- Number of rules.  */
#define YYNRULES  16
/* YYNRULES -- Number of states.  */
#define YYNSTATES  46

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   285

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
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
      25,    26,    27,    28,    29,    30
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint8 yyprhs[] =
{
       0,     0,     3,     6,     9,    13,    19,    25,    31,    37,
      42,    48,    55,    57,    59,    65,    72
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      32,     0,    -1,    33,    32,    -1,    34,    32,    -1,     3,
      25,     4,    -1,     3,     5,    33,    33,     4,    -1,     3,
       6,    33,    33,     4,    -1,     3,     8,    33,    33,     4,
      -1,     3,     7,    33,    33,     4,    -1,     3,    30,    33,
       4,    -1,     3,    30,    33,    33,     4,    -1,     3,    30,
      33,    33,    33,     4,    -1,    30,    -1,    29,    -1,     3,
      20,    30,    33,     4,    -1,     3,    20,    30,    30,    33,
       4,    -1,     3,    20,    30,    30,    30,    33,     4,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint8 yyrline[] =
{
       0,    75,    75,    76,    77,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    93,    94,    95
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "OP_OP", "OP_CP", "OP_PLUS", "OP_MINUS",
  "OP_DIV", "OP_MULT", "OP_COMMA", "KW_AND", "KW_OR", "KW_NOT", "KW_EQUAL",
  "KW_LESS", "KW_NIL", "KW_LIST", "KW_APPEND", "KW_CONCAT", "KW_SET",
  "KW_DEF", "KW_FOR", "KW_IF", "KW_LOAD", "KW_DISPLAY", "KW_EXIT",
  "KW_TRUE", "KW_FALSE", "COMMENT", "VALUEF", "IDENTIFIER", "$accept",
  "START", "EXP", "FUNCTION", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    31,    32,    32,    32,    33,    33,    33,    33,    33,
      33,    33,    33,    33,    34,    34,    34
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     2,     2,     3,     5,     5,     5,     5,     4,
       5,     6,     1,     1,     5,     6,     7
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     0,    13,    12,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     1,     2,     3,     0,     0,     0,
       0,     0,     0,     4,     0,     0,     0,     0,     0,    12,
       0,     9,     0,     5,     6,     8,     7,    12,     0,    14,
      10,     0,     0,    15,    11,    16
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
      -1,     4,     5,     6
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -27
static const yytype_int8 yypact[] =
{
       2,    32,   -27,   -27,    19,     2,     2,    13,    13,    13,
      13,   -26,    37,    13,   -27,   -27,   -27,    21,    13,    13,
      13,    13,    15,   -27,     4,    44,    45,    46,    49,    17,
      50,   -27,     6,   -27,   -27,   -27,   -27,    13,    51,   -27,
     -27,    52,    54,   -27,   -27,   -27
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -27,    18,    -7,   -27
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -1
static const yytype_uint8 yytable[] =
{
      18,    19,    20,    21,    22,     1,    24,    17,    31,    17,
      40,    25,    26,    27,    28,    30,    17,    32,    17,    14,
      17,     0,    38,    15,    16,    41,     7,     8,     9,    10,
      42,     2,     3,     2,     3,     2,     3,     7,     8,     9,
      10,    23,     2,     3,     2,    29,     2,    37,    33,    34,
      35,    13,    11,    36,    39,    43,    44,    12,    45,     0,
       0,     0,    13
};

static const yytype_int8 yycheck[] =
{
       7,     8,     9,    10,    30,     3,    13,     3,     4,     3,
       4,    18,    19,    20,    21,    22,     3,    24,     3,     0,
       3,    -1,    29,     5,     6,    32,     5,     6,     7,     8,
      37,    29,    30,    29,    30,    29,    30,     5,     6,     7,
       8,     4,    29,    30,    29,    30,    29,    30,     4,     4,
       4,    30,    20,     4,     4,     4,     4,    25,     4,    -1,
      -1,    -1,    30
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     3,    29,    30,    32,    33,    34,     5,     6,     7,
       8,    20,    25,    30,     0,    32,    32,     3,    33,    33,
      33,    33,    30,     4,    33,    33,    33,    33,    33,    30,
      33,     4,    33,     4,     4,     4,     4,    30,    33,     4,
       4,    33,    33,     4,     4,     4
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *bottom, yytype_int16 *top)
#else
static void
yy_stack_print (bottom, top)
    yytype_int16 *bottom;
    yytype_int16 *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yyrule)
    YYSTYPE *yyvsp;
    int yyrule;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      fprintf (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       );
      fprintf (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, Rule); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
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



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
	switch (*++yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (yyres)
	      yyres[yyn] = *yyp;
	    yyn++;
	    break;

	  case '"':
	    if (yyres)
	      yyres[yyn] = '\0';
	    return yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into YYRESULT an error message about the unexpected token
   YYCHAR while in state YYSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If YYRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static YYSIZE_T
yysyntax_error (char *yyresult, int yystate, int yychar)
{
  int yyn = yypact[yystate];

  if (! (YYPACT_NINF < yyn && yyn <= YYLAST))
    return 0;
  else
    {
      int yytype = YYTRANSLATE (yychar);
      YYSIZE_T yysize0 = yytnamerr (0, yytname[yytype]);
      YYSIZE_T yysize = yysize0;
      YYSIZE_T yysize1;
      int yysize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int yyx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *yyfmt;
      char const *yyf;
      static char const yyunexpected[] = "syntax error, unexpected %s";
      static char const yyexpecting[] = ", expecting %s";
      static char const yyor[] = " or %s";
      char yyformat[sizeof yyunexpected
		    + sizeof yyexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof yyor - 1))];
      char const *yyprefix = yyexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;

      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yycount = 1;

      yyarg[0] = yytname[yytype];
      yyfmt = yystpcpy (yyformat, yyunexpected);

      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	  {
	    if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		yycount = 1;
		yysize = yysize0;
		yyformat[sizeof yyunexpected - 1] = '\0';
		break;
	      }
	    yyarg[yycount++] = yytname[yyx];
	    yysize1 = yysize + yytnamerr (0, yytname[yyx]);
	    yysize_overflow |= (yysize1 < yysize);
	    yysize = yysize1;
	    yyfmt = yystpcpy (yyfmt, yyprefix);
	    yyprefix = yyor;
	  }

      yyf = YY_(yyformat);
      yysize1 = yysize + yystrlen (yyf);
      yysize_overflow |= (yysize1 < yysize);
      yysize = yysize1;

      if (yysize_overflow)
	return YYSIZE_MAXIMUM;

      if (yyresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *yyp = yyresult;
	  int yyi = 0;
	  while ((*yyp = *yyf) != '\0')
	    {
	      if (*yyp == '%' && yyf[1] == 's' && yyi < yycount)
		{
		  yyp += yytnamerr (yyp, yyarg[yyi++]);
		  yyf += 2;
		}
	      else
		{
		  yyp++;
		  yyf++;
		}
	    }
	}
      return yysize;
    }
}
#endif /* YYERROR_VERBOSE */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yymsg, yytype, yyvaluep)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  YYUSE (yyvaluep);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
	break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */



/* The look-ahead symbol.  */
int yychar;

/* The semantic value of the look-ahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
  
  int yystate;
  int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Look-ahead token as an internal (translated) token number.  */
  int yytoken = 0;
#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  yytype_int16 yyssa[YYINITDEPTH];
  yytype_int16 *yyss = yyssa;
  yytype_int16 *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  YYSTYPE *yyvsp;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  YYSIZE_T yystacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;


  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	yytype_int16 *yyss1 = yyss;


	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),

		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	yytype_int16 *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss);
	YYSTACK_RELOCATE (yyvs);

#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;


      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     look-ahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to look-ahead token.  */
  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a look-ahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid look-ahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
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
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the look-ahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;

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
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 4:
#line 77 "gpp_interpreter.y"
    {printf("Program Terminated"); exit(0);}
    break;

  case 5:
#line 81 "gpp_interpreter.y"
    {((yyval.valuef) = valuef_add((yyvsp[(3) - (5)].valuef),(yyvsp[(4) - (5)].valuef)));}
    break;

  case 6:
#line 82 "gpp_interpreter.y"
    {((yyval.valuef) = valuef_sub((yyvsp[(3) - (5)].valuef),(yyvsp[(4) - (5)].valuef)));}
    break;

  case 7:
#line 83 "gpp_interpreter.y"
    {((yyval.valuef) = valuef_multiply((yyvsp[(3) - (5)].valuef),(yyvsp[(4) - (5)].valuef)));}
    break;

  case 8:
#line 84 "gpp_interpreter.y"
    {((yyval.valuef) = valuef_divide((yyvsp[(3) - (5)].valuef),(yyvsp[(4) - (5)].valuef)));}
    break;

  case 9:
#line 85 "gpp_interpreter.y"
    {displayOneParameterFunction((yyvsp[(2) - (4)].string),(yyvsp[(3) - (4)].valuef));}
    break;

  case 10:
#line 86 "gpp_interpreter.y"
    {displayTwoParameterFunction((yyvsp[(2) - (5)].string),(yyvsp[(3) - (5)].valuef),(yyvsp[(4) - (5)].valuef));}
    break;

  case 11:
#line 87 "gpp_interpreter.y"
    {}
    break;

  case 12:
#line 88 "gpp_interpreter.y"
    {(yyval.valuef)=(yyvsp[(1) - (1)].string);}
    break;

  case 13:
#line 89 "gpp_interpreter.y"
    {(yyval.valuef)=(yyvsp[(1) - (1)].valuef);}
    break;

  case 14:
#line 93 "gpp_interpreter.y"
    {defun_function((yyvsp[(3) - (5)].string),(yyvsp[(4) - (5)].valuef));}
    break;

  case 15:
#line 94 "gpp_interpreter.y"
    {defun_function_2((yyvsp[(3) - (6)].string),(yyvsp[(4) - (6)].string));}
    break;

  case 16:
#line 95 "gpp_interpreter.y"
    {defun_function_3((yyvsp[(3) - (7)].string),(yyvsp[(4) - (7)].string));}
    break;


/* Line 1267 of yacc.c.  */
#line 1516 "gpp_interpreter.c"
      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;


  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
      {
	YYSIZE_T yysize = yysyntax_error (0, yystate, yychar);
	if (yymsg_alloc < yysize && yymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T yyalloc = 2 * yysize;
	    if (! (yysize <= yyalloc && yyalloc <= YYSTACK_ALLOC_MAXIMUM))
	      yyalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (yymsg != yymsgbuf)
	      YYSTACK_FREE (yymsg);
	    yymsg = (char *) YYSTACK_ALLOC (yyalloc);
	    if (yymsg)
	      yymsg_alloc = yyalloc;
	    else
	      {
		yymsg = yymsgbuf;
		yymsg_alloc = sizeof yymsgbuf;
	      }
	  }

	if (0 < yysize && yysize <= yymsg_alloc)
	  {
	    (void) yysyntax_error (yymsg, yystate, yychar);
	    yyerror (yymsg);
	  }
	else
	  {
	    yyerror (YY_("syntax error"));
	    if (yysize != 0)
	      goto yyexhaustedlab;
	  }
      }
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse look-ahead token after an
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

  /* Else will try to reuse look-ahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule which action triggered
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
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
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
		  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  *++yyvsp = yylval;


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#ifndef yyoverflow
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEOF && yychar != YYEMPTY)
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval);
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}


#line 98 "gpp_interpreter.y"


FuncStruct  functions; //for functions
int    operations = 0; // 1->sum 2->sub 3->mult 4->div
char   oneParameter[10]; // tek parametreli fonksiyonlarda tek parametrenin bilgsini tutar
int    boolCont = -1; // tek parametreli fonksiyonlarda parametrenin ne tarafta olduğunun bilgisini tutar


// Declerations

char* valuef_add(const char* a, const char* b) {
    // Parse the input fraction strings
    unsigned int num_1, denom_1;
    unsigned int num_2, denom_2;

    if (sscanf(a, "%ub%u", &num_1, &denom_1) != 2){
        if (sscanf(b, "%ub%u", &num_2, &denom_2) != 2){
            operations = 1;
            boolCont = -1;
            return NULL;
        }
        strcpy(oneParameter, b);
        operations = 1;
        boolCont = 0;
        return NULL;
    } else if (sscanf(b, "%ub%u", &num_2, &denom_2) != 2){
        strcpy(oneParameter, a);
        operations = 1;
        boolCont = 1;
        return NULL;
    }

    // Calculate the result fraction
    unsigned int result_numerator = num_1 * denom_2 + num_2 * denom_1;
    unsigned int result_denominator = denom_1 * denom_2;

    unsigned int gcd_value = gcd(result_numerator, result_denominator);
    result_numerator /= gcd_value;
    result_denominator /= gcd_value;

    printf("%ub%u\n", result_numerator, result_denominator);

    char result_str[32];
    snprintf(result_str, sizeof(result_str), "%ub%u", result_numerator, result_denominator);

    char* formatted_result = strdup(result_str);

    return formatted_result;
}
    
char* valuef_sub(const char* a, const char* b) {
    // Parse the input fraction strings
    unsigned int num_1, denom_1;
    unsigned int num_2, denom_2;

    if (sscanf(a, "%ub%u", &num_1, &denom_1) != 2){
        if (sscanf(b, "%ub%u", &num_2, &denom_2) != 2){
            operations = 2;
            boolCont = -1;
            return NULL;
        }
        strcpy(oneParameter, b);
        operations = 2;
        boolCont = 0;
        return NULL;
    } else if (sscanf(b, "%ub%u", &num_2, &denom_2) != 2){
        strcpy(oneParameter, a);
        operations = 2;
        boolCont = 1;
        return NULL;
    }

    // Calculate the result fraction
    unsigned int result_numerator = num_1 * denom_2 - num_2 * denom_1;
    unsigned int result_denominator = denom_1 * denom_2;

    unsigned int gcd_value = gcd(result_numerator, result_denominator);
    result_numerator /= gcd_value;
    result_denominator /= gcd_value;

    printf("%ub%u\n", result_numerator, result_denominator);

    char result_str[32]; // Adjust size as needed
    snprintf(result_str, sizeof(result_str), "%ub%u", result_numerator, result_denominator);

    char* formatted_result = strdup(result_str);

    return formatted_result;
}
    
char* valuef_multiply(const char* a, const char* b) {
    // Parse the input fraction strings
    unsigned int num_1, denom_1;
    unsigned int num_2, denom_2;

    if (sscanf(a, "%ub%u", &num_1, &denom_1) != 2){
        if (sscanf(b, "%ub%u", &num_2, &denom_2) != 2){
            operations = 3;
            boolCont = -1;
            return NULL;
        }
        strcpy(oneParameter, b);
        operations = 3;
        boolCont = 0;
        return NULL;
    } else if (sscanf(b, "%ub%u", &num_2, &denom_2) != 2){
        strcpy(oneParameter, a);
        operations = 3;
        boolCont = 1;
        return NULL;
    }

    // Calculate the result fraction
    unsigned int result_numerator = num_1 * num_2;
    unsigned int result_denominator = denom_1 * denom_2;

    unsigned int gcd_value = gcd(result_numerator, result_denominator);
    result_numerator /= gcd_value;
    result_denominator /= gcd_value;

    printf("%ub%u\n", result_numerator, result_denominator);

    char result_str[32];
    snprintf(result_str, sizeof(result_str), "%ub%u", result_numerator, result_denominator);

    char* formatted_result = strdup(result_str);

    return formatted_result;
}
    
char* valuef_divide(const char* a, const char* b) {
    // Parse the input fraction strings
    unsigned int num_1, denom_1;
    unsigned int num_2, denom_2;

    if (sscanf(a, "%ub%u", &num_1, &denom_1) != 2){
        if (sscanf(b, "%ub%u", &num_2, &denom_2) != 2){
            operations = 4;
            boolCont = -1;
            return NULL;
        }
        strcpy(oneParameter, b);
        operations = 4;
        boolCont = 0;
        return NULL;
    } else if (sscanf(b, "%ub%u", &num_2, &denom_2) != 2){
        strcpy(oneParameter, a);
        operations = 4;
        boolCont = 1;
        return NULL;
    }

    // Calculate the result fraction
    unsigned int result_numerator = num_1 * denom_2;
    unsigned int result_denominator = denom_1 * num_2;

    unsigned int gcd_value = gcd(result_numerator, result_denominator);
    result_numerator /= gcd_value;
    result_denominator /= gcd_value;

    printf("%ub%u\n", result_numerator, result_denominator);

    char result_str[32];
    snprintf(result_str, sizeof(result_str), "%ub%u", result_numerator, result_denominator);

    char* formatted_result = strdup(result_str);

    return formatted_result;
}
    
unsigned int gcd(unsigned int a, unsigned int b) {
    while (b != 0) {
        unsigned int temp = b;
        b = a % b;
        a = temp;
    }
    return a;
}
    
void defun_function(char* identifier, char* explist){
    //control variable
    int control = 0;
    //control for is there a function that has same identifier
    for(int i=0;i< functions.size;i++){
        if(strcmp(functions.funcs[i].id,identifier)==0){
            control = 1;
            break;
        }
    }
    //if there is a function that has same identifier
    if(control == 1){
        printf("Error, %s Already defined..!\n",identifier);
        return;
    }
    //There is no function that has same identifier
    //There is no element in functions array
    //I needed this if statement to make room for functions with malloc.
    if(functions.size==0 || functions.funcs==NULL){
        functions.funcs=(Function*) malloc(1);
        strcpy(functions.funcs[0].id,identifier);
        strcpy(functions.funcs[0].explist,explist);
        functions.size=1;
    }
    //There is at least one element in functions array but no function that has same identifier in functions array
    else{
        functions.funcs=(Function*)realloc(functions.funcs,sizeof(Function)*(functions.size+1));
        strcpy(functions.funcs[functions.size].id,identifier);
        strcpy(functions.funcs[functions.size].explist,explist);
        functions.size++;
    }
}
    
void defun_function_2(char* identifier, char* explist){
    //control variable
    int control = 0;
    //control for is there a function that has same identifier
    for(int i=0;i< functions.size;i++){
        if(strcmp(functions.funcs[i].id,identifier)==0){
            control = 1;
            break;
        }
    }
    //if there is a function that has same identifier
    if(control == 1){
        printf("Error, %s Already defined..!\n",identifier);
        return;
    }
    //There is no function that has same identifier
    //There is no element in functions array
    //I needed this if statement to make room for functions with malloc.
    if(functions.size==0 || functions.funcs==NULL){
        functions.funcs=(Function*) malloc(1);
        strcpy(functions.funcs[0].id,identifier);
        strcpy(functions.funcs[0].explist,explist);
        strcpy(functions.funcs[0].valuefForOneParameter, oneParameter);
        functions.funcs[0].boolControl = boolCont;
        if (operations == 1){
            functions.funcs[0].operation = 1;
        } else if (operations == 2){
            functions.funcs[0].operation = 2;
        } else if (operations == 3){
            functions.funcs[0].operation = 3;
        } else{
            functions.funcs[0].operation = 4;
        }
        functions.size=1;
        printf("#function\n");
    }
    //There is at least one element in functions array but no function that has same identifier in functions array
    else{
        char temp[10];
        int tempInt = functions.funcs[0].operation;
        int tempBool = functions.funcs[0].boolControl;
        strcpy(temp,functions.funcs[0].valuefForOneParameter);
        functions.funcs=(Function*)realloc(functions.funcs,sizeof(Function)*(functions.size+1));
        strcpy(functions.funcs[0].valuefForOneParameter, temp);
        functions.funcs[0].operation = tempInt;
        functions.funcs[0].boolControl = tempBool;
        strcpy(functions.funcs[functions.size].id,identifier);
        strcpy(functions.funcs[functions.size].explist,explist);
        strcpy(functions.funcs[functions.size].valuefForOneParameter, oneParameter);
        functions.funcs[functions.size].boolControl = boolCont;
        if (operations == 1){
            functions.funcs[functions.size].operation = 1;
        } else if (operations == 2){
            functions.funcs[functions.size].operation = 2;
        } else if (operations == 3){
            functions.funcs[functions.size].operation = 3;
        } else{
            functions.funcs[functions.size].operation = 4;
        }
        functions.size++;
        printf("#function\n");
    }
}
    
void defun_function_3(char* identifier, char* explist){
    //control variable
    int control = 0;
    //control for is there a function that has same identifier
    for(int i=0;i< functions.size;i++){
        if(strcmp(functions.funcs[i].id,identifier)==0){
            control = 1;
            break;
        }
    }
    //if there is a function that has same identifier
    if(control == 1){
        printf("Error, %s Already defined..!\n",identifier);
        return;
    }
    
    //There is no function that has same identifier
    //There is no element in functions array
    //I needed this if statement to make room for functions with malloc.
    if(functions.size==0 || functions.funcs==NULL){
        functions.funcs=(Function*) malloc(1);
        strcpy(functions.funcs[0].id,identifier);
        strcpy(functions.funcs[0].explist,explist);
        if (operations == 1){
            functions.funcs[0].operation = 1;
        } else if (operations == 2){
            functions.funcs[0].operation = 2;
        } else if (operations == 3){
            functions.funcs[0].operation = 3;
        } else{
            functions.funcs[0].operation = 4;
        }
        functions.size=1;
        printf("#function\n");
    }
    //There is at least one element in functions array but no function that has same identifier in functions array
    else{
        int tempInt = functions.funcs[0].operation;
        functions.funcs=(Function*)realloc(functions.funcs,sizeof(Function)*(functions.size+1));
        functions.funcs[0].operation = tempInt;
        strcpy(functions.funcs[functions.size].id,identifier);
        strcpy(functions.funcs[functions.size].explist,explist);
        if (operations == 1){
            functions.funcs[functions.size].operation = 1;
        } else if (operations == 2){
            functions.funcs[functions.size].operation = 2;
        } else if (operations == 3){
            functions.funcs[functions.size].operation = 3;
        } else{
            functions.funcs[functions.size].operation = 4;
        }
        functions.size++;
        printf("#function\n");
    }
}
    
void displayOneParameterFunction(const char* ident, const char* exp){
    int index = -1;
    unsigned int num_1, denom_1;
    unsigned int num_2, denom_2;
    //control for is there a function that has same identifier
    for(int i=0;i< functions.size;i++){
        if(strcmp(functions.funcs[i].id,ident)==0){
            index = i;
            break;
        }
    }
    if (index == -1){
        printf("Error. There is no funciton in that name..!\n");
        return;
    }
    if (functions.funcs[index].boolControl == 0){
        if (sscanf(exp, "%ub%u", &num_1, &denom_1) != 2 ||
            sscanf(functions.funcs[index].valuefForOneParameter, "%ub%u", &num_2, &denom_2) != 2) {
            printf("Syntax Error\n");
            return;
        }
    } else if (functions.funcs[index].boolControl == 1){
        if (sscanf(functions.funcs[index].valuefForOneParameter, "%ub%u", &num_1, &denom_1) != 2 ||
            sscanf(exp, "%ub%u", &num_2, &denom_2) != 2) {
            printf("Syntax Error\n");
            return;
        }
    } else{
        printf("Syntax Error\n");
        return;
    }
    if (functions.funcs[index].operation == 1){
        unsigned int result_numerator = num_1 * denom_2 + num_2 * denom_1;
        unsigned int result_denominator = denom_1 * denom_2;

        unsigned int gcd_value = gcd(result_numerator, result_denominator);
        result_numerator /= gcd_value;
        result_denominator /= gcd_value;

        printf("%ub%u\n", result_numerator, result_denominator);
    } else if (functions.funcs[index].operation == 2){
        unsigned int result_numerator = num_1 * denom_2 - num_2 * denom_1;
        unsigned int result_denominator = denom_1 * denom_2;

        unsigned int gcd_value = gcd(result_numerator, result_denominator);
        result_numerator /= gcd_value;
        result_denominator /= gcd_value;

        printf("%ub%u\n", result_numerator, result_denominator);
    } else if (functions.funcs[index].operation == 3){
        unsigned int result_numerator = num_1 * num_2;
        unsigned int result_denominator = denom_1 * denom_2;

        unsigned int gcd_value = gcd(result_numerator, result_denominator);
        result_numerator /= gcd_value;
        result_denominator /= gcd_value;

        printf("%ub%u\n", result_numerator, result_denominator);
    } else if (functions.funcs[index].operation == 4){
        unsigned int result_numerator = num_1 * denom_2;
        unsigned int result_denominator = denom_1 * num_2;

        unsigned int gcd_value = gcd(result_numerator, result_denominator);
        result_numerator /= gcd_value;
        result_denominator /= gcd_value;

        printf("%ub%u\n", result_numerator, result_denominator);
    } else{
        printf("Syntax Error\n");
        return;
    }
}
    
void displayTwoParameterFunction(const char* ident, const char* exp, const char* exp2){
    
    int index = -1;
    unsigned int num_1, denom_1;
    unsigned int num_2, denom_2;
    //control for is there a function that has same identifier
    for(int i=0;i< functions.size;i++){
        if(strcmp(functions.funcs[i].id,ident)==0){
            index = i;
            break;
        }
    }
    if (index == -1){
        printf("Error. There is no funciton in that name..!\n");
        return;
    }
    if (sscanf(exp, "%ub%u", &num_1, &denom_1) != 2 ||
        sscanf(exp2, "%ub%u", &num_2, &denom_2) != 2) {
        printf("Syntax Error\n");
        return;
    }
    if (functions.funcs[index].operation == 1){
        unsigned int result_numerator = num_1 * denom_2 + num_2 * denom_1;
        unsigned int result_denominator = denom_1 * denom_2;

        unsigned int gcd_value = gcd(result_numerator, result_denominator);
        result_numerator /= gcd_value;
        result_denominator /= gcd_value;

        printf("%ub%u\n", result_numerator, result_denominator);
    } else if (functions.funcs[index].operation == 2){
        unsigned int result_numerator = num_1 * denom_2 - num_2 * denom_1;
        unsigned int result_denominator = denom_1 * denom_2;

        unsigned int gcd_value = gcd(result_numerator, result_denominator);
        result_numerator /= gcd_value;
        result_denominator /= gcd_value;

        printf("%ub%u\n", result_numerator, result_denominator);
    } else if (functions.funcs[index].operation == 3){
        unsigned int result_numerator = num_1 * num_2;
        unsigned int result_denominator = denom_1 * denom_2;

        unsigned int gcd_value = gcd(result_numerator, result_denominator);
        result_numerator /= gcd_value;
        result_denominator /= gcd_value;

        printf("%ub%u\n", result_numerator, result_denominator);
    } else if (functions.funcs[index].operation == 4){
        unsigned int result_numerator = num_1 * denom_2;
        unsigned int result_denominator = denom_1 * num_2;

        unsigned int gcd_value = gcd(result_numerator, result_denominator);
        result_numerator /= gcd_value;
        result_denominator /= gcd_value;

        printf("%ub%u\n", result_numerator, result_denominator);
    } else{
        printf("Syntax Error\n");
        return;
    }
}

int main(){
    functions.size=0;
    functions.funcs=NULL;

    printf("------------------------------\n");
    printf("   Welcome To GPP Language\n");
    printf("------------------------------\n");
    
    //loop until exit
    while(1)
        yyparse();
        
    return 0;
}

// error function
void yyerror(char* s) {
    printf("Syntax Error!!\n");
    exit(0);
}

