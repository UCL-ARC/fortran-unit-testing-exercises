# Top level variables
ROOT_DIR = $(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
FC ?= mpif90

#------------------------------------#
#      Targets for compiling src     #
#------------------------------------#
SRC_DIR = $(ROOT_DIR)/src
BUILD_DIR = $(ROOT_DIR)/build

# List source files
SRC_FILES = \
	cli.f90 \
	comms.f90 \
	game_of_life.f90 \
	io.f90 \
	main.f90

# Map src files to .o files
SRC_OBJS = $(patsubst %.f90, $(BUILD_DIR)/%.o, $(SRC_FILES))

# Build src .o files
$(BUILD_DIR)/%.o: $(SRC_DIR)/%.f90 | $(BUILD_DIR)
	$(FC) -c -J $(BUILD_DIR) -o $@ $<

$(BUILD_DIR)/game_of_life: $(SRC_OBJS)
	$(FC) -o $@ $(FC_FLAGS) $^ $(LIBS)

game_of_life: $(BUILD_DIR)/game_of_life

# Ensure the build dir exists
$(BUILD_DIR):
	mkdir -p $@

#------------------------------------#
#         Targets for testing        #
#------------------------------------#
TEST_DIR = $(ROOT_DIR)/test

# Include make command from test Makefiles
tests: $(SRC_OBJS)
	@echo "Building pFUnit test suite..."
	@$(MAKE) -C $(TEST_DIR) tests

#------------------------------------#
#        Targets for cleaning        #
#------------------------------------#
# Define target for cleaning objects built by pfunit
clean_pfunit:
	$(MAKE) -C $(TEST_DIR) clean

# Define target for cleaning all built objects
clean: clean_pfunit
	rm -rf build

.PHONY: clean

#--------------------------------------#
# Export variables for child Makefiles #
#--------------------------------------#
# Export variables for the other Makefiles to use
export BUILD_DIR
export ROOT_DIR
export SRC_OBJS
export SRC_DIR
export FC
export TEST_DIR
