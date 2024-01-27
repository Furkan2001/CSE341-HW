%{
        // Definitions
%}

/* Definition of comment,values and idendifier */
/* a* -> 0 or more occurrences of a */
/* a+ -> 1 or more occurrences of a */

COMMENT [;]{2}.*
VALUEF [1-9][0-9]*+[b]+[1-9][0-9]*
IDENTIFIER [a-z_A-Z][a-z_A-Z0-9]*

/* END OF DEFINITION PART */

%%
    
    /* OPERATORS */
[+]     {printf("OP_PLUS\n");}
[-]     {printf("OP_MINUS\n");}
[/]     {printf("OP_DIV\n");}
[*]     {printf("OP_MULT\n");}
[(]     {printf("OP_OP\n");}
[)]     {printf("OP_CP\n");}
[,]     {printf("OP_COMMA\n");}


    /* Keywords */
and     {printf("KW_AND\n");}
or      {printf("KW_OR\n");}
not     {printf("KW_NOT\n");}
equal   {printf("KW_EQUAL\n");}
less    {printf("KW_LESS\n");}
nil     {printf("KW_NIL\n");}
list    {printf("KW_LIST\n");}
append  {printf("KW_APPEND\n");}
concat  {printf("KW_CONCAT\n");}
set     {printf("KW_SET\n");}
def     {printf("KW_DEF\n");}
for     {printf("KW_FOR\n");}
if      {printf("KW_IF\n");}
exit    {printf("KW_EXIT\n");}
load    {printf("KW_LOAD\n");}
display {printf("KW_DISPLAY\n");}
true    {printf("KW_TRUE\n");}
false   {printf("KW_FALSE\n");}

  /* if you want to out then write -exit- */
-exit- {printf("You want to out.\n"); return 0;}

 /* Rule for ignore \n,\t and space */
[\t]    {;}
[\n]    {;}
[ ]     {;}
[[:space:]]|[[:space:]] {;}


[0-9]+[a-zA-Z]*       {printf("INVALID EXPRESSION: '%s'\n", yytext);}



{COMMENT}       {printf("COMMENT\n");}
{VALUEF}        {printf("VALUEF\n");}
{IDENTIFIER}    {printf("IDENTIFIER\n");}

.                    {printf("INVALID EXPRESSION\n");}
    
%%

    int yywrap() {return 1;}
    
int main(int argc, char** argv) {
    
    
    if(argc == 1)
    {
           printf("Type -exit- to exit\n");
           printf("Lexer started:\n");
    }
    else if (argc == 2) /* Dosya okuma burada yapılıyor. */
    {
        FILE *fp;
        fp = fopen(argv[1], "r");
        yyin = fp;
    }
    else
    {
        printf("More than one command line arguments\n");
    }

    printf("Enter string: ");
    yylex();
    
    return 0;
}




   
