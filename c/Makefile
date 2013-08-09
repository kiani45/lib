
DEFS=
CXX=g++
CXXFLAGS=-I. -Wall -rdynamic -g -DDEBUG $(DEFS)
EXE=out

# define myMacro
# 	echo "this is my macro"
# 	pwd
# endef

all: clean compile execute

clean:
	-@ rm *.o ${EXE}

compile: out
out: kai.cpp
	$(CXX) $(CXXFLAGS) -o ${EXE} $^

execute:
	@ printf "\nExecuting:\n"
	@ ./${EXE}

