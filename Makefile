CC=ghc
CFLAGS=-O
LDFLAGS=-package parsec
PROJ=pretty

ALL: $(PROJ)

$(PROJ): Expression.o Simplify.o Parse.o Pretty.o main.o
	$(CC) -o $@ $^ $(LDFLAGS)

main.o: main.hs
	$(CC) $(CFLAGS) -c $<

Parse.o: Parse.hs
	$(CC) $(CFLAGS) -c $<

Pretty.o: Pretty.hs
	$(CC) $(CFLAGS) -c $<

Expression.o: Expression.hs
	$(CC) $(CFLAGS) -c $<

Simplify.o: Simplify.hs
	$(CC) $(CFLAGS) -c $<
	
clean:
	rm -f *.o *.hi

