
# Directories
ZEBIN = ../z_ebin
AEBIN = ../a_ebin

ERL_COMPILE_FLAGS += -W

# Modules
A = shell_default

Z = mnesia_t

TARGETS = $(A:%=$(AEBIN)/%.beam) $(Z:%=$(ZEBIN)/%.beam) 

all: $(TARGETS)


$(AEBIN)/%.beam: %.erl $(HEADER_FILES)
	erlc $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o$(AEBIN) $<

$(ZEBIN)/%.beam: %.erl $(HEADER_FILES)
	erlc $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o$(ZEBIN) $<

