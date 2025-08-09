CFLAGS += $(shell pkgconf --cflags readline) -Wall -g
LDFLAGS += $(shell pkgconf --libs readline)

run: sexp
	'./$<'

main: main.o mpc.o
parsing: parsing.o mpc.o
sexp: sexp.o mpc.o
