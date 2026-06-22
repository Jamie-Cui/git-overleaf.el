CC ?= cc
PKG_CONFIG ?= pkg-config
BUILD_DIR ?= build
TARGET := $(BUILD_DIR)/git-overleaf-cli

SRC := $(wildcard src/*.c)
OBJ := $(patsubst src/%.c,$(BUILD_DIR)/%.o,$(SRC))

CPPFLAGS += -D_DARWIN_C_SOURCE -Iinclude
CFLAGS ?= -std=c11 -Wall -Wextra -Wpedantic -O2
PKG_CFLAGS := $(shell $(PKG_CONFIG) --cflags libcurl jansson 2>/dev/null)
PKG_LIBS := $(shell $(PKG_CONFIG) --libs libcurl jansson 2>/dev/null)
LDLIBS += $(PKG_LIBS)

.PHONY: all clean check-deps

all: check-deps $(TARGET)

check-deps:
	@$(PKG_CONFIG) --exists libcurl || { echo "missing dependency: libcurl"; exit 1; }
	@$(PKG_CONFIG) --exists jansson || { echo "missing dependency: jansson"; exit 1; }

$(TARGET): $(OBJ)
	$(CC) $(CFLAGS) $(OBJ) $(LDLIBS) -o $@

$(BUILD_DIR)/%.o: src/%.c include/git-overleaf-cli/cli.h | $(BUILD_DIR)
	$(CC) $(CPPFLAGS) $(PKG_CFLAGS) $(CFLAGS) -c $< -o $@

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

clean:
	rm -rf $(BUILD_DIR)
