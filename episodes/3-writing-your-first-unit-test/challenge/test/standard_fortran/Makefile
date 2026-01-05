TEST_FILES = test_temp_conversions.f90

TEST_OBJS = $(patsubst %.f90, $(BUILD_DIR)/%.o, $(TEST_FILES))

$(BUILD_DIR)/%.o: $(STANDARD_FORTRAN_TEST_DIR)/%.f90 | $(BUILD_DIR)
	$(FC) -c -J $(BUILD_DIR) -o $@ $<

# Build test executable
$(STANDARD_FORTRAN_TEST_EXE): $(TEST_OBJS) | $(SRC_OBJS)
	$(FC) -o $@ $^

tests: $(STANDARD_FORTRAN_TEST_EXE)
