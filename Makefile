CC=clang++
CFLAGS = -g -Og -rdynamic `llvm-config --cxxflags --ldflags --system-libs --libs all`

kaleidoscope: src/kaleidoscope.cpp
	$(CC) -o $@ $^ $(CFLAGS)

.PHONY: clean
clean:
	rm -rf $(obj) kaleidoscope
