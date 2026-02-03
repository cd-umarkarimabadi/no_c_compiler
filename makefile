# Compiler
CXX = g++

# Compiler flags
CXXFLAGS = -std=c++17 -Wall -Wextra -Iinclude -g -rdynamic

# Optimization flags - using safer O2 instead of O3
PROD_CXXFLAGS = -std=c++17 -Wall -Wextra -Iinclude -g -O2

# Output executable name
OUTPUT = main

# Build directory for object files
BUILD_DIR = build

# Find all .cpp files in the src/ directory
SOURCES = $(shell find src -name "*.cpp")

# Generate object files for each source file
OBJECTS = $(patsubst src/%.cpp, $(BUILD_DIR)/%.o, $(SOURCES))

# Default target
all: $(OUTPUT)

# Production build with optimization :: This causes everything to break 
prod: CXXFLAGS = $(PROD_CXXFLAGS)
prod: clean $(OUTPUT)


# Link all object files into the final executable
$(OUTPUT): $(OBJECTS)
	$(CXX) $(CXXFLAGS) -o $@ $(OBJECTS)

# Compile each .cpp file into an .o file in the build directory
$(BUILD_DIR)/%.o: src/%.cpp
	@mkdir -p $(dir $@) # Ensure the directory for the .o file exists
	$(CXX) $(CXXFLAGS) -c $< -o $@

# Clean up build files
clean:
	rm -rf $(OUTPUT) $(BUILD_DIR)

book:
	docker run --rm -v $(shell pwd):/documents/ -w /documents/ \
	  asciidoctor/docker-asciidoctor \
	  asciidoctor-pdf -r asciidoctor-diagram docs/compiler_doc.adoc
	  
# Phony targets
.PHONY: all clean prod
