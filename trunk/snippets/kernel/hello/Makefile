PWD = $(shell pwd)
KVER = $(shell uname -r)
KSRC = /lib/modules/$(KVER)/build
obj-m := hello.o
module-objs := hello.o
all:
	$(MAKE) -C $(KSRC) M=$(PWD) modules
clean:
	rm *.ko
	rm *.o
