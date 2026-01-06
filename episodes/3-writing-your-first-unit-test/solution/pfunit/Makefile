PFUNIT_SOURCE_DIR ?= $(ROOT_DIR)/../../../pfunit
PFUNIT_VERSION ?= 4.12
PFUNIT_INCLUDE_DIR = $(PFUNIT_SOURCE_DIR)/build/installed/PFUNIT-$(PFUNIT_VERSION)/include

# Don't try to include if we're cleaning as this doesn't depend on pFUnit
ifneq ($(MAKECMDGOALS),clean)
include $(PFUNIT_INCLUDE_DIR)/PFUNIT.mk
TEST_FLAGS = -I$(BUILD_DIR) $(PFUNIT_EXTRA_FFLAGS)
endif

# Define variables to be picked up by make_pfunit_test
tests_TESTS = \
  test_temp_conversions.pf
tests_OTHER_SOURCES = $(filter-out $(BUILD_DIR)/main.o, $(SRC_OBJS))
tests_OTHER_LIBRARIES = $(TEST_FLAGS)

# Triggers pre-processing and defines rule for building test executable
$(eval $(call make_pfunit_test,tests))

# Converts pre-processed test files into objects ready for building of the executable
%.o: %.F90
	$(FC) -c $(TEST_FLAGS) $<

# Cleanup generated files
clean:
	\rm -f *.o *.mod *.F90 *.inc tests
