%option noyywrap

%{
#define TOKEN(token_id) { print_token(token_id, #token_id); return token_id; }
#define NUMBER(token_id, base) { print_int(token_id, base); return token_id; }
#define FLOAT(token_id, base) { print_float(token_id, base); return token_id; }
#define STRING() { print_string(); return T_STRING; }


#include <stdio.h>

// #define YY_DECL int yylex()

#include "bison.tab.h"
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <math.h>
#include <string.h>
#include "config.h"

int flex_errors = 0;
int maxErrors = 10;
static int lineno = 1;

char string_buffer[MAX_STR_SIZE];
char* string_buffer_ptr;

char* file_name;

char* yytext;
FILE *yyin;
int yylex();

void error(char* messageType, char* errMessage,int type);
void print_token(int tokenId, char* str);
void print_int(int token, int base);
void print_float(int token, int base);
void print_comment(char* comment); 
void print_string(int, char* str);

// void yyerror(char* message, int type);

int hex_to_int(char *str);
int bin_to_int(char *str);
float bin_to_real(char* str);
float hex_to_real(char*str);

/**
Transforms 
*/

%}

%option noyywrap case-insensitive
%option yylineno

%x S_STRING

LETTER          [a-zA-Z]
DIGIT           [0-9]
BIN             [01]
HEX             [0-9A-F]

/* Non Zero Digit */
NZ_NUMBER       [1-9]

EXPONENT        E[+-]?(0|{NZ_NUMBER}{DIGIT}*)
ALPHANUM        ({LETTER}|{DIGIT})

/** Or _ */
ALPHANUM_       ({ALPHANUM}|_)

ASCII           [ !"#$%&'()*+,-./:;<=>?@\[\\\]^_`{|}~]
WHITESPACE      [ \t]

%%

 /* Reserved Keywords */
"FUNCTION"                                               TOKEN(T_FUNCTION);
"SUBROUTINE"                                             TOKEN(T_SUBROUTINE);
"END"                                                    TOKEN(T_END);
"INTEGER"                                                TOKEN(T_INTEGER);                                                  
"REAL"                                                   TOKEN(T_REAL);
"LOGICAL"                                                TOKEN(T_LOGICAL);
"CHARACTER"                                              TOKEN(T_CHARACTER);
"RECORD"                                                 TOKEN(T_RECORD);
"ENDREC"                                                 TOKEN(T_ENDRECORD);
"DATA"                                                   TOKEN(T_DATA);
"CONTINUE"                                               TOKEN(T_CONTINUE);
"GOTO"                                                   TOKEN(T_GOTO);
"CALL"                                                   TOKEN(T_CALL);
"READ"                                                   TOKEN(T_READ);
"WRITE"                                                  TOKEN(T_WRITE);
"IF"                                                     TOKEN(T_IF);
"THEN"                                                   TOKEN(T_THEN);
"ELSE"                                                   TOKEN(T_ELSE);
"ENDIF"                                                  TOKEN(T_ENDIF);
"DO"                                                     TOKEN(T_DO);
"ENDDO"                                                  TOKEN(T_ENDDO);
"STOP"                                                   TOKEN(T_STOP);
"RETURN"                                                 TOKEN(T_RETURN);


 /* IDS */
({LETTER}{ALPHANUM}*)|(_{ALPHANUM}+(_{ALPHANUM}+)?_)        TOKEN(T_ID);


 /* Simple Constants */

 /* Int Dec */
(0|{NZ_NUMBER}{DIGIT}*)                                  NUMBER(T_ICONST, 10);

 /* Int Bin */
0B(0|1{BIN}*)                                            NUMBER(T_ICONST, 2);

 /* Int Hex */
0H(0|[1-9A-F]{HEX}*)                                     NUMBER(T_ICONST, 16);




 /* Real Dec */
(0|{NZ_NUMBER}{DIGIT}*)\.(0*{NZ_NUMBER}{DIGIT}*|0)       FLOAT(T_RCONST, 10);
(0|{NZ_NUMBER}{DIGIT}*)\.                                FLOAT(T_RCONST, 10);
\.(0*{NZ_NUMBER}{DIGIT}*|0)                              FLOAT(T_RCONST, 10);

 /* Real Bin */
0B(0|1{BIN}*)\.(0*[1]{BIN}*|0)                           FLOAT(T_RCONST, 2);
0B(0|1{BIN}*)\.                                          FLOAT(T_RCONST, 2);
 /* \.(0*[1]{BIN}*|0)                                        FLOAT(T_RCONST, 2); */

 /* Real Hex */
0H(0|[1-9A-F]{HEX}*)\.(0*[1-9A-F]{HEX}*|0)               FLOAT(T_RCONST, 16);
0H(0|[1-9A-F]{HEX}*)\.                                   FLOAT(T_RCONST, 16);
 /* \.(0*[1-9A-F]{HEX}*|0)                                   FLOAT(T_RCONST, 16); */


 /* Logical */

".TRUE."                                                TOKEN(T_LCONST);
".FALSE."                                               TOKEN(T_LCONST);

\'\\n\'                                                 TOKEN(T_CCONST);
\'\\f\'                                                 TOKEN(T_CCONST);
\'\\t\'                                                 TOKEN(T_CCONST);
\'\\r\'                                                 TOKEN(T_CCONST);
\'\\b\'                                                 TOKEN(T_CCONST);
\'\\v\'                                                 TOKEN(T_CCONST);   
    
\'{ASCII}\'                                             TOKEN(T_CCONST);
\'{ALPHANUM}\'                                          TOKEN(T_CCONST);

 /* Operators */
".OR."                                                  TOKEN(T_OROP);
".AND."                                                 TOKEN(T_ANDOP);
".NOT."                                                 TOKEN(T_NOTOP);

".GT."                                                  TOKEN(T_RELOP_GT);
".GE."                                                  TOKEN(T_RELOP_GE);
".LT."                                                  TOKEN(T_RELOP_LT);
".LE."                                                  TOKEN(T_RELOP_LE);
".EQ."                                                  TOKEN(T_RELOP_EQ);
".NE."                                                  TOKEN(T_RELOP_NE);

[+-]                                                    TOKEN(T_ADDOP);
"*"                                                     TOKEN(T_MULTOP);
"/"                                                     TOKEN(T_DIVOP);
"**"                                                    TOKEN(T_POWOP);

 /* Comments */
[$].*                                                   TOKEN(T_COMMENT);

"("                                                     TOKEN(T_LPAREN);
")"                                                     TOKEN(T_RPAREN);
","                                                     TOKEN(T_COMMA);
"="                                                     TOKEN(T_ASSIGN);
":"                                                     TOKEN(T_COLON);

 /* Strings */ 

 /* When a string is found */
\"                                                  { BEGIN(S_STRING); string_buffer_ptr = string_buffer; }

<S_STRING>\"                                        {   BEGIN(INITIAL); 
                                                        *string_buffer_ptr = '\0'; 
                                                        print_string(T_STRING, string_buffer);
                                                        // printf("STRING FOUND: %s | TOKEN NUMBER: %d | LINE: %d\n", string_buffer, T_STRING, yylineno); 
                                                        return T_STRING; }

 /* \n Without \ Existing */
<S_STRING>\n                                        {   error("Illegal input in string", string_buffer, ERR_ILL_STR); 
                                                        return 0;
                                                    }

<S_STRING>\\n                                       {*string_buffer_ptr++ = '\n';}
<S_STRING>\\f                                       {*string_buffer_ptr++ = '\f';}
<S_STRING>\\t                                       {*string_buffer_ptr++ = '\t';}
<S_STRING>\\r                                       {*string_buffer_ptr++ = '\r';}
<S_STRING>\\b                                       {*string_buffer_ptr++ = '\b';}
<S_STRING>\\v                                       {*string_buffer_ptr++ = '\v';}
<S_STRING>\\\\                                      {*string_buffer_ptr++ = '\\';}

<S_STRING>\\\"                                      {*string_buffer_ptr++ = '"';}

<S_STRING>\\\n                                      {lineno++;}

<S_STRING>\\{ALPHANUM_}?                            {*string_buffer_ptr++ = yytext[1];}

<S_STRING>[^\\\n\"]+                                {   char *yptr = yytext; 
                                                        while ( *yptr ) 
                                                            *string_buffer_ptr++ = *yptr++;
                                                    }

<S_STRING><<EOF>>                                   {   error("Unterminated string", string_buffer+0, ERR_UNTERMINATED_STR); 
                                                        return 0;
                                                    }

{WHITESPACE}                                        {}

\n                                                  {lineno++;}
                                                     
%%
#define RED "\033[0;31m"
#define YELLOW "\033[0;33m"
#define GREEN "\033[0;32m"
#define MAGENTA "\033[0;35m"
#define CYAN "\033[0;36m"
//#define RED(str) "\033[0;31m" str "\033[0m"
#define RESET "\033[0m"

void print_token(int token_id, char* str) {
    printf(""CYAN"line: " YELLOW"%d " RESET CYAN" type:"RESET" "MAGENTA"%s"RESET" "CYAN"id:"RESET" "RED"%d"RESET" "CYAN"text:"RESET" "GREEN"%s"RESET" "YELLOW""RESET"\n", lineno, str+2, token_id, yytext);
}

void print_string(int token_id, char* str) {
    printf(""CYAN"line: "YELLOW"%d "RESET" "CYAN"id:"RESET" "RED"%d"RESET" "CYAN"string:"RESET" "MAGENTA"%s"RESET"\n", lineno, token_id, str);
}

void print_int(int token_id, int base) {
    int num;
    if(base == 10) {
        num = atoi(yytext);
    }else if (base == 2){
        num = bin_to_int(yytext);
    }else if (base == 16){
        num = hex_to_int(yytext);
    }
    printf(""CYAN"line: "YELLOW"%d "RESET" "CYAN"Int:"RESET" "MAGENTA"%d"RESET" "CYAN"base:"RESET" "MAGENTA"%d"RESET" "CYAN"id:"RESET" "RED"%d"RESET" "CYAN"text:"RESET" "GREEN"%s"RESET"\n", lineno, num, base, token_id, yytext);
}

void print_float(int token_id, int base) {
    float num;
    if(base == 10){
        num = atoi(yytext);
    }else if (base == 2){
        num = bin_to_real(yytext);
    }else if (base == 16){
        num = hex_to_real(yytext);
    }
    printf(""CYAN"line: "YELLOW"%d "RESET" "CYAN"float:"RESET" "MAGENTA"%f"RESET" "CYAN"base:"RESET" "MAGENTA"%d"RESET" "CYAN"id:"RESET" "RED"%d"RESET" "CYAN"text:"RESET" "GREEN"%s"RESET"\n", lineno, num, base, token_id, yytext);
}

// extern int flex_errors;

int main(int argc, char** argv) {
    int token;
    if(argc > 1){
        yyin = fopen(argv[1], "r");
        if (yyin == NULL){
            perror ("Error opening file"); return -1;
        }
    }
    
    do{
		token = yylex();
    }while(token != T_EOF);

}

int hex_to_int(char *str){
    return (int) strtol(&str[2], NULL, 16);
}

int bin_to_int(char *str){
    return (int) strtol(&str[2], NULL, 2);
}

float bin_to_real(char* str){
    float number = 0.0;
    int i, length_real;
    char *character;
    character = strchr(str,'.');
    length_real = strlen(character) - 1;
    number = (int) strtol(&str[2], &character, 2);
    for(i = 1; i < length_real; i++){
        number += (character[i] - '0') * (1/pow(2,i));
    }
    return number;
}

float hex_to_real(char*str){
    float number = 0.0;
    int i, length_real;
    char *character;
    character = strchr(str,'.');
    length_real = strlen(character) - 1;
    number = (int) strtol(&str[2], &character, 16);
    for(i = 1; i <= length_real; i++){
        if(character[i] <= '9')
			number += (character[i] - '0') * (1 / pow(16, i));
		else
			number += (character[i] + 10 - 'A') * (1 / pow(16, i)); /** TODO: CASE SENSITIVE **/
	}
    return number;
}

void error(char* message_type, char* err_message, int type) {
    flex_errors++;

    printf(RED "ERROR: " RESET "at line " RED "%d" RESET ":\n", yylineno);
    printf(    "  |    " RESET "Error: " RED "%s\n" RESET, message_type);
    printf(    "  |    Text: " RED " %s\n" RESET, yytext);
    printf(    "  |    Error Message: " RED " %s\n" RESET, err_message);
    printf(    "  |           " RED "^~~~~~\n\n" RESET);

    if(flex_errors >= MAX_ERRORS) {
        printf("" RED "Max Errors Detected" RESET);
        exit(10);
    }

    // if(type == 0){
	// 	printf("-> "RED"ERROR at line %d caused by %s: %s"RESET"\n", lineno, yytext, err_message);
    // }else if(type == 1){
	// 	*str_buf_ptr = '\0'; // String or Comment Error. Cleanup old chars stored in buffer.
	// 	printf("-> "RED"ERROR at line %d near \"%s\": %s"RESET"\n", lineno, str_buf, err_message);
	// }

    // switch(type) {
    //     case ERR_UNTERMINATED_STR: {
            
    //         break;  
    //     }
    //     case ERR_ILL_STR: {
            
    //         break;
    //     }
    //     default: {
    //         break;
    //     }
    //         // printf("ERROR: Unterminated String found at line %d near '%s'")
    // }
}