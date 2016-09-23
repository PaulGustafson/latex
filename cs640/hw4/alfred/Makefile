LEX = flex 
YACC = yacc
DEBUG = -ggdb3 -DYYDEBUG=1
WARN = -Wall -Wcast-align -Wstrict-prototypes \
       -Wmissing-prototypes -Wno-unused
WARN = -Wall -Wno-unused

CC = gcc $(WARN) $(DEBUG)
PROG = alfred

$(PROG): y.tab.o lex.yy.o sim.o 
	$(CC) -o $(PROG) sim.o y.tab.o lex.yy.o -lfl -lm

smallTestFile:  sim.o smallTestFile.o
	$(CC) -o smallTestFile smallTestFile.o sim.o -lm

smallTestFile.o: smallTestFile.c sim.h sim.c alfred_types.h
	$(CC) -c smallTestFile.c -o smallTestFile.o

homework: sim.o homework.o
	$(CC) -o homework homework.o sim.o -lm

homework.o: hw1.c sim.h sim.c alfred_types.h
	$(CC) -c hw1.c -o homework.o

lex.yy.o: lex.yy.c y.tab.h

y.tab.o: y.tab.c y.tab.h

sim.o: sim.c sim.h alfred_types.h
	$(CC) -c sim.c 

y.tab.c y.tab.h: $(PROG).y
	$(YACC) -d $(PROG).y

lex.yy.c: $(PROG).l
	$(LEX) $(PROG).l

clean:
	rm *.o y.tab.c y.tab.h lex.yy.c $(PROG)

