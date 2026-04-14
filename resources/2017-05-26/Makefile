CFILES := $(wildcard *.c)
HFILES := $(wildcard *.h)
CC_FLAGS := -g -Wall -Werror

all: main

main: $(CFILES:.c=.o)
	gcc $(CC_FLAGS) *.o -o main

%.o: %.c $(HFILES)
	gcc $(CC_FLAGS) -c -o $@ $<

clean:
	rm -f *.o main
