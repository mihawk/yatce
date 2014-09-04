
include vsn.mk

TARGET2=ebin/erl_tc_driver.so
TARGET=c_src/libyatce.so
#BEAMS=tcadb.beam
ifndef ROOT
  ROOT=$(shell pwd)
endif

COMMON_TEST_LIB = $(shell erl -noshell -eval 'io:format("~s~n", [code:lib_dir(common_test)]).' -s init stop)
#TEST_SERVER_LIB = $(shell erl -noshell -eval 'io:format("~s~n", [code:lib_dir(test_server)]).' -s init stop)
RUN_TEST_CMD = $(COMMON_TEST_LIB)/priv/bin/run_test

ifeq ($(shell uname),Linux)
	ARCH = linux
#	LDFLAGS = $(LDFLAGS_COMMON) -shared
else
	ARCH = macosx
#	LDFLAGS = $(LDFLAGS_COMMON) -dynamic -bundle -undefined suppress -flat_namespace
endif



all: build
	@echo "this is yatce $(YATCE_VSN)."
	@echo "usage:"
	@echo "  make build (make b) - build all source code into *.beam/*.so"
	@echo "  make test  (make t) - basic automated test. all operations will be checked."
	@echo "  make perf  (make p) - performance test (not supported yet)"
	@echo "  make clean (make c) - clean all"

b: build
build: so beam app

so:
	cd c_src && make
beam:
	cd src && erl -make -smp -Wall

app:
	sed -e 's;%VSN%;$(YATCE_VSN);' ./src/yatce.app.src > ./ebin/yatce.app

##$(TARGET): # c_src/Makefile
##	cd c_src && make

t: test
test: build
	@echo "testing in ${ROOT} .."
	@mkdir -p test/log
	sh ${RUN_TEST_CMD} -dir ${ROOT}/test -logdir ${ROOT}/test/log \
	  -I ${ROOT}/include -I ${ROOT}/src  \
	  -cover ${ROOT}/test/yatce.coverspec  -pa ${ROOT}/ebin
	@echo "automated test done. see test/log/index.html."

c: clean
clean:
	cd c_src && make clean
	@rm -rf ebin/*.beam


NUM_THREAD=4
NUM_REPEAT=65536
p: perf
perf: build test/log
	@echo "performance test with ${NUM_THREAD} concurrency for ${NUM_REPEAT} reputation: "
#	@erl +Ktrue +A 8 -pa ${ROOT}/ebin -noshell -eval 'performer:start("c_src", ${NUM_THREAD}, ${NUM_REPEAT}).' -s init stop
	erl +Ktrue +A 8 -pa ${ROOT}/ebin -noshell -eval "supervisor:start_link({local,perf_sup},perf_sup, ['/tmp/sup_test.tch',  ${NUM_THREAD}])." #-s init stop
##    {ok,Pid}=supervisor:start({local, ?SERVER_NAME}, perf_sup,['/tmp/sup_test.tch', 1]),
