PFUNIT_INCLUDE_DIR ?= /home/vscode/pfunit/build/installed/PFUNIT-4.12/include

# Don't try to include if we're cleaning as this doesn't depend on pFUnit
ifneq ($(MAKECMDGOALS),clean)
include $(PFUNIT_INCLUDE_DIR)/PFUNIT.mk
TEST_FLAGS = -I$(BUILD_DIR) $(PFUNIT_EXTRA_FFLAGS)
endif

check:
	@echo $(PFUNIT_EXTRA_FFLAGS)

# Define variables to be picked up by make_pfunit_test
tests_TESTS = \
  test_evolve_board.pf \
  test_find_steady_state.pf \
  test_read_model_from_file.pf \
  test_check_for_steady_state.pf \
  test_exchange_boundaries.pf \
  test_get_local_grid_info.pf
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
