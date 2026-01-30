# poscal-rs Runtime Library Makefile

CC = gcc
AR = ar
CFLAGS = -Wall -Wextra -O2 -fPIC -std=c11
LDFLAGS = -lm

# Detect OS
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
    # macOS
    SHARED_EXT = dylib
    SHARED_FLAGS = -dynamiclib -undefined dynamic_lookup
else ifeq ($(UNAME_S),Linux)
    # Linux
    SHARED_EXT = so
    SHARED_FLAGS = -shared
else
    # Windows (MinGW)
    SHARED_EXT = dll
    SHARED_FLAGS = -shared
endif

# Output files
STATIC_LIB = libposcal-rs_runtime.a
SHARED_LIB = libposcal-rs_runtime.$(SHARED_EXT)
OBJ = poscal-rs_runtime.o

# Targets
.PHONY: all clean install test

all: $(STATIC_LIB) $(SHARED_LIB)

$(OBJ): poscal-rs_runtime.c poscal-rs_runtime.h
	$(CC) $(CFLAGS) -c poscal-rs_runtime.c -o $(OBJ)

$(STATIC_LIB): $(OBJ)
	$(AR) rcs $(STATIC_LIB) $(OBJ)
	@echo "Built static library: $(STATIC_LIB)"

$(SHARED_LIB): $(OBJ)
	$(CC) $(SHARED_FLAGS) -o $(SHARED_LIB) $(OBJ) $(LDFLAGS)
	@echo "Built shared library: $(SHARED_LIB)"

clean:
	rm -f $(OBJ) $(STATIC_LIB) $(SHARED_LIB)
	rm -f test_runtime test_runtime.o
	@echo "Cleaned build artifacts"

install: all
	@echo "Installing runtime library..."
	mkdir -p ../lib
	cp $(STATIC_LIB) ../lib/
	cp $(SHARED_LIB) ../lib/
	cp poscal-rs_runtime.h ../lib/
	@echo "Installed to ../lib/"

# Test target
test: test_runtime
	./test_runtime

test_runtime: test_runtime.c $(STATIC_LIB)
	$(CC) $(CFLAGS) test_runtime.c -o test_runtime -L. -lposcal-rs_runtime $(LDFLAGS)

# Help target
help:
	@echo "poscal-rs Runtime Library Build System"
	@echo ""
	@echo "Targets:"
	@echo "  all      - Build both static and shared libraries (default)"
	@echo "  clean    - Remove build artifacts"
	@echo "  install  - Install libraries to ../lib/"
	@echo "  test     - Build and run tests"
	@echo "  help     - Show this help message"
	@echo ""
	@echo "Output:"
	@echo "  Static:  $(STATIC_LIB)"
	@echo "  Shared:  $(SHARED_LIB)"
