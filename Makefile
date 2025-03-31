# Directory structure
SRC_DIR     := src
OBJ_DIR     := build/obj
BIN_DIR     := build/bin
CFG_DIR     := config
ASSET_DIR   := assets

# Files
ROM_NAME    := snake
CFG_FILE    := $(CFG_DIR)/$(ROM_NAME).cfg
SOURCES     := $(wildcard $(SRC_DIR)/*.s)
OBJECTS     := $(SOURCES:$(SRC_DIR)/%.s=$(OBJ_DIR)/%.o)
ROM_FILE    := $(BIN_DIR)/$(ROM_NAME).nes

# Tools
CC65_HOME   := /path/to/cc65  # Adjust this to your cc65 installation path
CA65        := ca65
LD65        := ld65

# Flags
CAFLAGS     := -t nes -I $(SRC_DIR)/includes
LDFLAGS     := -C $(CFG_FILE) --mapfile $(BIN_DIR)/$(ROM_NAME).map

# Debug information
$(info Sources found: $(SOURCES))
$(info Objects to build: $(OBJECTS))

# Create directories if they don't exist
# $(shell mkdir -p $(OBJ_DIR) $(BIN_DIR))

# Main target
all: $(ROM_FILE)

# Link object files into the final ROM
$(ROM_FILE): $(OBJECTS) $(CFG_FILE) | $(BIN_DIR)
	$(LD65) $(LDFLAGS) -o $@ $(OBJECTS)
	@echo "ROM created: $@"

# Compile assembly files to object files
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.s | $(OBJ_DIR)
	$(CA65) $(CAFLAGS) -o $@ $<

# Compile nametable assets
nametables:
	@mkdir -p $(OBJ_DIR)/nametables
	@for nam in $(wildcard $(ASSET_DIR)/nametables/*.nam); do \
		base=$$(basename $$nam); \
		echo "Converting nametable: $$base"; \
		# If you need to process .nam files, add commands here \
	done

# Clean build artifacts
clean:
	rm -rf $(OBJ_DIR)/* $(BIN_DIR)/*

# Clean everything including built assets
distclean: clean
	rm -rf build

# Run the ROM in an emulator
run: $(ROM_FILE)
	fceux $(ROM_FILE)

# Phony target to list all source files found
list-sources:
	@echo "Source files found:"
	@for src in $(SOURCES); do echo "  $$src"; done
	@echo "Object files to create:"
	@for obj in $(OBJECTS); do echo "  $$obj"; done

# Create required directories
directories:
	@if not exist $(subst /,\,$(OBJ_DIR)) mkdir $(subst /,\,$(OBJ_DIR))
	@if not exist $(subst /,\,$(BIN_DIR)) mkdir $(subst /,\,$(BIN_DIR))

# Display help
help:
	@echo "NES Project Makefile"
	@echo "-------------------"
	@echo "Targets:"
	@echo "  all        - Build the ROM (default)"
	@echo "  clean      - Remove object files and ROM"
	@echo "  distclean  - Remove all generated files"
	@echo "  nametables - Process nametable files"
	@echo "  run        - Run the ROM in FCEUX emulator"

.PHONY: all clean distclean nametables run help