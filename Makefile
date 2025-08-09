CFLAGS += $(shell pkgconf --cflags readline) -Wall
LDFLAGS += $(shell pkgconf --libs readline)

mpc/build/libmpc.so:
	cd mpc && $(MAKE) libs

libs/libmpc.so: mpc/build/libmpc.so
	ln -sf '$(CURDIR)/$<' '$(CURDIR)/$@'

main: main.o libs/libmpc.so
