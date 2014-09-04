
EI_HOME = $(shell erl -noshell -eval 'io:format("~s~n", [code:lib_dir(erl_interface)]).' -s init stop)
ERL_DRIVER_H = -I$(shell erl -noshell -eval 'io:format("~s~n", [filename:absname("usr/include", code:root_dir())]).' -s init stop)

LIBEI= -L$(EI_HOME)/lib -lei_st
EI_INCLUDE=-I$(EI_HOME)/include

TARGET=libyatce.so
OBJECTS=dispatcher.o tc_wrapper.o endecode.o easy_test.o \
tc_table_operation.o \
linked_list.o lock.o hash_map.o

CFLAGS += -fPIC -Wall -O2 #-O0 -DDEBUG_MSG #-DDEBUG_DONE

ifeq ($(shell uname),Linux)
	ARCH = linux
	SHAREFLAG = -shared
	LIBTC= -ltokyocabinet
	USR_LOCAL=/usr/local
else
	ARCH = Darwin
	CFLAGS += -bundle -undefined suppress -flat_namespace
	SHAREFLAG = -dynamic
	USR_LOCAL=/opt/local
endif

LIBTC= -L$(USR_LOCAL)/lib -ltokyocabinet
TC_INCLUDE=-I$(USR_LOCAL)/include

LIBRARY=$(LIBEI) $(LIBTC) -lz -lpthread -lm
INCLUDE=$(EI_INCLUDE) $(ERL_DRIVER_H) $(TC_INCLUDE)

all: $(TARGET) raw_performer 
	@cp $(TARGET) ../priv/lib

.c.o:
	$(CC) $(CFLAGS) $(INCLUDE) -c $< -o $@


$(TARGET): $(OBJECTS) 
	$(CC) $(CFLAGS) $(SHAREFLAG) $(INCLUDE) $(OBJECTS) -o $(TARGET) $(LDFLAGS) $(LIBRARY)

clean:
	-rm -rf $(OBJECTS) $(BEAMS) $(TARGET) *.o

raw_performer: raw_performer.o
	$(CC) $(INCLUDE) $< -o $@ $(LIBRARY) 
