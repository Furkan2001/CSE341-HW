%{
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

%}

%union
{
    char *string;
    char *valuef;
}

%start START

%token OP_OP OP_CP OP_PLUS OP_MINUS OP_DIV OP_MULT OP_COMMA
%token KW_AND KW_OR KW_NOT KW_EQUAL KW_LESS KW_NIL KW_LIST KW_APPEND KW_CONCAT KW_SET KW_DEF KW_FOR KW_IF KW_LOAD KW_DISPLAY KW_EXIT KW_TRUE KW_FALSE

%token COMMENT
%token <valuef> VALUEF
%token <string> IDENTIFIER

%type <valuef>  EXP
%type <valuef> FUNCTION

%%

START:
    EXP START |
    FUNCTION START |
    OP_OP KW_EXIT OP_CP {printf("Program Terminated"); exit(0);}
;
    
EXP:
    OP_OP OP_PLUS EXP EXP OP_CP         {($$ = valuef_add($3,$4));}             |
    OP_OP OP_MINUS EXP EXP OP_CP        {($$ = valuef_sub($3,$4));}             |
    OP_OP OP_MULT EXP EXP OP_CP         {($$ = valuef_multiply($3,$4));}        |
    OP_OP OP_DIV EXP EXP OP_CP          {($$ = valuef_divide($3,$4));}          |
    OP_OP IDENTIFIER EXP OP_CP          {displayOneParameterFunction($2,$3);}   |
    OP_OP IDENTIFIER EXP EXP OP_CP      {displayTwoParameterFunction($2,$3,$4);} |
    OP_OP IDENTIFIER EXP EXP EXP OP_CP  {} |
    IDENTIFIER {$$=$1;} |
    VALUEF {$$=$1;}
;
   
FUNCTION:
    OP_OP KW_DEF IDENTIFIER EXP OP_CP {defun_function($3,$4);} |
    OP_OP KW_DEF IDENTIFIER IDENTIFIER EXP OP_CP {defun_function_2($3,$4);} |
    OP_OP KW_DEF IDENTIFIER IDENTIFIER IDENTIFIER EXP OP_CP {defun_function_3($3,$4);}
;
   
%%

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
