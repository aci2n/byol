CFLAGS += $(shell pkgconf --cflags readline) -Wall -g
LDFLAGS += $(shell pkgconf --libs readline)

run: lispy
	'./$<'

lispy: lispy.o mpc.o
