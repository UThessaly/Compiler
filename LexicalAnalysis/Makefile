all:
	flex lexer.l && gcc lex.yy.c -lfl -lm

run:
	flex lexer.l && gcc lex.yy.c -lfl -lm && ./a.out

debug:
	flex -d lexer.l && gcc lex.yy.c -lfl -lm && ./a.out