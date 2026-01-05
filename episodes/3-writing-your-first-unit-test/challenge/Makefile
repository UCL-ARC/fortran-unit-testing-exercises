# Top level variables
ROOT_DIR = $(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
FC = gfortran

#------------------------------------#
#      Targets for compiling src     #
#------------------------------------#
SRC_DIR = $(ROOT_DIR)/src
BUILD_DIR = $(ROOT_DIR)/build

# List source files
SRC_FILES = temp_conversions.f90

# Map src files to .o files
SRC_OBJS = $(patsubst %.f90, $(BUILD_DIR)/%.o, $(SRC_FILES))

# Build src .o files
$(BUILD_DIR)/%.o: $(SRC_DIR)/%.f90 | $(BUILD_DIR)
	$(FC) -c -J $(BUILD_DIR) -o $@ $<

# Ensure the build dir exists
$(BUILD_DIR):
	mkdir -p $@

#------------------------------------#
#         Targets for testing        #
#------------------------------------#
TEST_DIR = $(ROOT_DIR)/test
STANDARD_FORTRAN_TEST_DIR = $(TEST_DIR)/standard_fortran
STANDARD_FORTRAN_TEST_EXE = $(BUILD_DIR)/standard_fortran_tests
PFUNIT_TEST_DIR = $(TEST_DIR)/pfunit

# Include make command from test Makefiles
standard_fortran_tests: $(SRC_OBJS)
	@echo "Building standard_fortran test suite..."
	@$(MAKE) -C $(STANDARD_FORTRAN_TEST_DIR) tests

pfunit_tests: $(SRC_OBJS)
	@echo "Building pFUnit test suite..."
	@$(MAKE) -C $(PFUNIT_TEST_DIR) tests

tests: standard_fortran_tests #| pfunit_tests

#------------------------------------#
#        Targets for cleaning        #
#------------------------------------#
# Define target for cleaning objects built by pfunit
clean_pfunit:
	$(MAKE) -C $(PFUNIT_TEST_DIR) clean

# Define target for cleaning all built objects
clean: #clean_pfunit
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
export STANDARD_FORTRAN_TEST_DIR
export STANDARD_FORTRAN_TEST_EXE
export PFUNIT_TEST_DIR
