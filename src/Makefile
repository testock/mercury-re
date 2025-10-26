include ../Make.options

.PHONY: default
default:
	$(MMC) --make libregex 

.PHONY: rebuild
rebuild:
	$(MMC) --rebuild libregex

.PHONY: install
install:
	$(MMC) --make libregex.install 

tags: $(wildcard *.m)
	mtags $^

.PHONY: clean
clean:
	$(MMC) --make regex.clean

.PHONY: realclean
realclean:
	$(MMC) --make regex.realclean
	/bin/rm -rf Mercury
	/bin/rm -f $(wildcard *.err) $(wildcard *.mh)
	/bin/rm -f tags