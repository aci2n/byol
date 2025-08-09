CFLAGS += $(shell pkgconf --cflags readline) -Wall
LDFLAGS += $(shell pkgconf --libs readline)

run: parsing
	./parsing

main: main.o mpc.o
parsing: parsing.o mpc.o

