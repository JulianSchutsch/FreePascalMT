.phony: all
.phony: clear

CFLAGS = -g -gl

TESTS= mainthread producerconsumer activeobject leaderfollower
APPS= $(TESTS:%=app%)

all: $(APPS)


$(APPS):%:%.pas
	fpc $@.pas $(CFLAGS)