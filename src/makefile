ERL_SRC = $(wildcard *.erl)
BEAM	= $(ERL_SRC:.erl=.beam)

all: $(BEAM)

test:
	./test.sh

clean:
	rm *.beam

%.beam: %.erl
	erlc +debug_info $*.erl
