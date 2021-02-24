all:
	bison -d bison.y && flex lexer.l && gcc lex.yy.c bison.tab.c -lfl -lm

run:
	bison -d bison.y && flex lexer.l && gcc lex.yy.c bison.tab.c -lfl -lm && ./a.out

debug:
	bison -d bison.y && flex lexer.l && gcc lex.yy.c bison.tab.c -lfl -lm && ./a.out

cleanup:
	rm bison.tab.* lex.yy.c