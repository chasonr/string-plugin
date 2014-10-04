OFILES = string-plugin.o format-plugin.o split.o utf8string.o

LIB = string-plugin.so

# Configuration for Kubuntu 14.04; your mileage may vary
CLANG_DIR = /usr/lib/llvm-3.4
CXX = /usr/bin/clang++

CXXFLAGS = -Wall -O2 -I${CLANG_DIR}/include -std=c++11 -fPIC -fno-rtti

${LIB} : ${OFILES}
	${CXX} -shared ${CXXFLAGS} -o ${LIB} ${OFILES}

clean:
	rm -f *.o ${LIB}
