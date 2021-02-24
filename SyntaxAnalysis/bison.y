%{
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>
#include <string.h>
#include "settings.h"

extern int lineno;

extern char str_buffer[MAX_STR_SIZE];
extern char *str_buffer_ptr;

extern int yylex();
extern char *yytext;
extern FILE *yyin;

extern void yyterminate();

int errors = 0;

enum ErrorType { 
    YYTEXT,
    STRBUF
};

void yyerror(const char* message);
%}

%define parse.error verbose

%union {
    int intvalue;
    double realvalue;
    char *strval;
}

%token <intvalue> T_ICONST "integer constant"
%token <realvalue> T_RCONST "real constant"

%token <strval> T_EOF "eof"
%token <strval> T_FUNCTION "function"
%token <strval> T_SUBROUTINE "subroutine"
%token <strval> T_END "end"
%token <strval> T_LOGICAL "logical"
%token <strval> T_CHARACTER "character"
%token <strval> T_RECORD "record"
%token <strval> T_ENDRECORD "endrecord"
%token <strval> T_DATA "data"
%token <strval> T_CONTINUE "continue"
%token <strval> T_GOTO "goto"
%token <strval> T_CALL "call"
%token <strval> T_READ "read"
%token <strval> T_WRITE "write"
%token <strval> T_IF "if"
%token <strval> T_THEN "then"
%token <strval> T_ELSE "else"
%token <strval> T_ENDIF "endif"
%token <strval> T_DO "do"
%token <strval> T_ENDDO "enddo"
%token <strval> T_STOP "stop"
%token <strval> T_RETURN "return"
%token <strval> T_CCONST "cconst"
%token <strval> T_ID "id"
%token <strval> T_OROP "orop"
%token <strval> T_ANDOP "andop"
%token <strval> T_NOTOP "notop"
%token <strval> T_RELOP_GT "relop_gt"
%token <strval> T_RELOP_GE "relop_ge"
%token <strval> T_RELOP_LT "relop_lt"
%token <strval> T_RELOP_LE "relop_le"
%token <strval> T_RELOP_EQ "relop_eq"
%token <strval> T_RELOP_NE "relop_ne"
%token <strval> T_ADDOP "addop"
%token <strval> T_MULTOP "multop"
%token <strval> T_DIVOP "divop"
%token <strval> T_POWOP "powop"
%token <strval> T_LPAREN "lparen"
%token <strval> T_RPAREN "rparen"
%token <strval> T_COMMA "comma"
%token <strval> T_ASSIGN "assign"
%token <strval> T_COLON "colon"
%token <strval> T_COMMENT "comment"
%token <strval> T_STRING "string"
%token <strval> T_RELOP ".GT. or .GE. or .LT. or .LE. or .EQ. or .NE."

%type <strval> program body declarations type vars undef_variable dims dim fields 
%type <strval> field vals value_list values value repeat constant statements labeled_statement label 
%type <strval> statement simple_statement assignment variable expressions expression goto_statement 
%type <strval> labels if_statement subroutine_call io_statement read_list read_item iter_space step 
%type <strval> write_list write_item compound_statement branch_statement tail loop_statement subprograms 
%type <strval> subprogram header formal_parameters

%left T_COMMA
%right T_ASSIGN
%left T_OROP
%left T_ANDOP
%left T_RELATIONOP
%left T_ADDOP
%left T_MULTOP
%left T_NOTOP T_RELOP
%left T_LPAREN T_RPAREN T_LBRACK T_RBRACK

%start program

%%

program: body T_END subprograms;

body: declarations statements;

declarations: declarations type vars
        | declarations T_RECORD fields T_ENDRECORD vars
        | declarations T_DATA vals
        | %empty;

type: T_ICONST         
        | T_RCONST
        | T_LOGICAL         
        | T_CHARACTER;

vars: vars T_COMMA undef_variable
        | undef_variable;

undef_variable: T_ID T_LPAREN dims T_RPAREN
        | T_ID;

dims: dims T_COMMA dim
        | dim;

dim: T_ICONST         
        | T_ID;

fields: fields field
        | field;

field: type vars
        | T_RECORD fields T_ENDRECORD vars;

vals: vals T_COMMA T_ID value_list
        | T_ID value_list;

value_list: T_DIVOP values T_DIVOP;

values: values T_COMMA value
        | value;

value: repeat T_MULTOP T_ADDOP constant
        | repeat T_MULTOP constant
        | repeat T_MULTOP T_STRING
        | T_ADDOP constant
        | constant
        | T_STRING;

repeat: T_ICONST         
        | %empty;
constant: T_ICONST         
        | T_RCONST         
        | T_LOGICAL         
        | T_CCONST;

statements: statements labeled_statement
        | labeled_statement;

labeled_statement: label statement
        | statement;

label: T_ICONST;

statement: simple_statement
        | compound_statement;

simple_statement: assignment
        | goto_statement
        | if_statement
        | subroutine_call
        | io_statement
        | T_CONTINUE
        | T_RETURN
        | T_STOP;

assignment: variable T_ASSIGN expression
        | variable T_ASSIGN T_STRING;

variable: variable T_COLON T_ID
        | variable T_LPAREN expressions T_RPAREN
        | T_ID;

expressions: expressions T_COMMA expression
        | expression;

expression: expression T_OROP expression
        | expression T_ANDOP expression
        | expression T_RELOP expression
        | expression T_ADDOP expression
        | expression T_MULTOP expression
        | expression T_DIVOP expression
        | expression T_POWOP expression
        | T_NOTOP expression
        | T_ADDOP expression
        | variable
        | constant
        | T_LPAREN expression T_RPAREN;

goto_statement: T_GOTO label
        | T_GOTO T_ID T_COMMA T_LPAREN labels T_RPAREN;

labels: labels T_COMMA label
        | label;

if_statement: T_IF T_LPAREN expression T_RPAREN label T_COMMA label T_COMMA label
        | T_IF T_LPAREN expression T_RPAREN simple_statement;

subroutine_call: T_CALL variable;

io_statement: T_READ read_list
        | T_WRITE write_list;

read_list: read_list T_COMMA read_item
        | read_item;

read_item: variable
        | T_LPAREN read_list T_COMMA T_ID T_ASSIGN iter_space T_RPAREN;

iter_space: expression T_COMMA expression step;

step: T_COMMA expression
        | %empty;

write_list: write_list T_COMMA write_item
        | write_item;

write_item: expression
        | T_LPAREN write_list T_COMMA T_ID T_ASSIGN iter_space T_RPAREN
        | T_STRING;

compound_statement: branch_statement
        | loop_statement;

branch_statement: T_IF T_LPAREN expression T_RPAREN T_THEN body tail;

tail: T_ELSE body T_ENDIF
        | T_ENDIF;

loop_statement: T_DO T_ID T_ASSIGN iter_space body T_ENDDO;

subprograms: subprograms subprogram
        | %empty;

subprogram: header body T_END;

header: type T_FUNCTION T_ID T_LPAREN formal_parameters T_RPAREN
        | T_SUBROUTINE T_ID T_LPAREN formal_parameters T_RPAREN
        | T_SUBROUTINE T_ID;

formal_parameters: type vars T_COMMA formal_parameters
        | type vars;

