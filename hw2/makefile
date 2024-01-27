all: gpp_lexer.l
	@flex -o gpp_lexer.c gpp_lexer.l
	@gcc gpp_lexer.c -o g++
	@./g++
input:
	@flex -o gpp_lexer.c gpp_lexer.l
	@gcc gpp_lexer.c -o g++
	@./g++ example.txt
lisp:
	@sbcl --script gpp_lexer.lisp
