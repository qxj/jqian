#---------------------------------------------------------------------------
# export LC_ALL = zh_CN.UTF-8
# export LANG   = zh_CN.UTF-8

#---------------------------------------------------------------------------

VERSION_MAJOR  := 1
VERSION_MINOR  := 0
VERSION_PATCH  := 0
VERSION    :=   $(VERSION_MAJOR).$(VERSION_MINOR).$(VERSION_PATCH)

#---------------------------------------------------------------------------

CCCOLOR="\033[34m"
CXXCOLOR="\033[35m"
LINKCOLOR="\033[34;1m"
SRCCOLOR="\033[33m"
BINCOLOR="\033[37;1m"
MAKECOLOR="\033[32;1m"
ENDCOLOR="\033[0m"

#---------------------------------------------------------------------------
NOISY_CC    = printf  '    %b %b\n' $(CCCOLOR)CC$(ENDCOLOR) $(SRCCOLOR)$@$(ENDCOLOR);
NOISY_CXX   = printf  '    %b %b\n' $(CXXCOLOR)CXX$(ENDCOLOR) $(SRCCOLOR)$@$(ENDCOLOR);
QUIET_CC    = @printf '    %b %b\n' $(CCCOLOR)CC$(ENDCOLOR) $(SRCCOLOR)$@$(ENDCOLOR);
QUIET_CXX   = @printf '    %b %b\n' $(CXXCOLOR)CXX$(ENDCOLOR) $(SRCCOLOR)$@$(ENDCOLOR);
QUIET_LINK  = @printf '    %b %b\n' $(LINKCOLOR)LINK$(ENDCOLOR) $(BINCOLOR)$@$(ENDCOLOR);
CXXLINK     = $(QUIET_LINK)ar


NCC         = $(NOISY_CC)gcc
NCXX        = $(NOISY_CXX)g++
CC          = $(QUIET_CC)gcc
CXX         = $(QUIET_CXX)g++
CFLAGS      += -g -O2 -Wall -DVERSION=\"$(VERSION)\" -fstack-protector -fstack-protector-all -fno-strict-aliasing

#---------------------------------------------------------------------------
TOP_DIR     := $(word $(words $(MAKEFILE_LIST)),$(MAKEFILE_LIST))
PROJ_ROOT   := $(shell cd $(dir $(TOP_DIR));pwd)
CUR_DIR     =  $(shell pwd | awk -F \/ '{print $$NF;}' )
LOCAL_SRC   += $(sort $(wildcard *.cpp *.cc *.c))
OBJ_DIR     += $(PROJ_ROOT)/obj/$(CUR_DIR)
OBJ_DIR_SED += $(shell echo "$(PROJ_ROOT)/obj/$(CUR_DIR)/" | sed 's/\//\\\//g' )
LOCAL_OBJ   += $(patsubst %.cpp, $(OBJ_DIR)/%.o, $(patsubst %.cpp, $(OBJ_DIR)/%.o, $(patsubst %.c, $(OBJ_DIR)/%.o, $(LOCAL_SRC))))
LOCAL_DOBJ  += $(patsubst %.cpp, %.o, $(patsubst %.cpp, %.o, $(patsubst %.c, %.o, $(LOCAL_SRC))))
DEP_FILE    := $(foreach obj, $(LOCAL_DOBJ), $(dir $(obj)).$(basename $(notdir $(obj))).d)

$(shell mkdir -p $(PROJ_ROOT)/obj/$(CUR_DIR))

#---------------------------------------------------------------------------

BOOST_INC    := -I/usr/local/include
BOOST_LIB    := -lboost_thread -lboost_system -lboost_date_time -lboost_regex

PROTOBUF_INC :=  -I/usr/local/include
PROTOBUF_LIB :=  -lprotobuf

# COMM_INC := -I$(PROJ_ROOT)/common
# COMM_LIB := $(PROJ_ROOT)/common/common.a

INC := -I.
LIB := -L.

#---------------------------------------------------------------------------
# Usage:
# 1) makefile in root directory:

# .PHONY:all
# all:
# 	make -C common
# 	make -C test
# 	make -C execs
# clean:
# 	make clean -C common
# 	make clean -C test
# 	make clean -C execs
# cleanall:
# 	make cleanall -C common
# 	make cleanall -C test
# 	make cleanall -C execs

# 2.1) makefile for ar in subdirectories, such as ./common:
# include ../make.inc
#
# INC := $(INC) $(BOOST_INC)
# LIB := $(LIB) $(BOOST_LIB)
#
# TARGET := common.a
#
# include ../make.rules

# 2.2) makefile for execs in subdirectories, such as ./execs:
# include ../make.inc
#
# INC := $(INC) $(BOOST_INC)
# LIB := $(LIB) $(BOOST_LIB)
#
# TARGETS := exec1 exec2
#
# include ../make.rules
