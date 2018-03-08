# emacs invocation
EMACS		:= emacs -Q -batch

# additional emacs loadpath
LOADPATH	:= -L .

# files to compile
EL			:= $(wildcard *.el)

# compiled files
ELC			:= $(EL:.el=.elc)

.PHONY: clean batch-compile

all: clean batch-compile

$(ELC): %.elc: %.el
	$(EMACS) $(LOADPATH) -f batch-byte-compile $<

# compile needed files
compile: $(ELC)

# compile all files at once
batch-compile:
	$(EMACS) $(LOADPATH) -f batch-byte-compile $(EL)

# remove all generated files
clean:
	rm -f $(ELC)
