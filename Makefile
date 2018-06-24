CC=clang++
SOURCES = $(wildcard src/*.cpp)
OBJ = $(src:.cpp:.o)

CFLAGS = -g -Og -rdynamic
LDFLAGS = `llvm-config --cxxflags --ldflags --system-libs --libs core mcjit native`

kaleidoscope: $(SOURCES)
	$(CC) -o $@ $^ $(CFLAGS) $(LDFLAGS)

.PHONY: clean
clean:
	rm -rf $(obj) kaleidoscope
