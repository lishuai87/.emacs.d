SUBDIRS = clang-async distel

.PHONY: all clang distel clean

default: all

all:
	for i in $(SUBDIRS); do $(MAKE) -C $$i; done

clang:
	$(MAKE) -C clang-async

distel:
	$(MAKE) -C distel

distclean: clean

clean:
	for i in $(SUBDIRS); do	$(MAKE) -C $$i $(@); done
