# Koleidoscope
# Koleidoscope

# compile
clang++ -g toy.cpp `llvm-config --cxxflags --ldflags --system-libs --libs core orcjit native` -O3 -o toyJIT

# run (part 1&2)
./a.out


# run part 3
./toy
