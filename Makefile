CFLAGS += $(shell pkgconf --cflags readline) -Wall
LDFLAGS += $(shell pkgconf --libs readline)

all: main

mpc/build/libmpc.a:
	cd mpc && $(MAKE) build/libmpc.a

main: main.o mpc/build/libmpc.a

jsong: jsong.o mpc/build/libmpc.a
