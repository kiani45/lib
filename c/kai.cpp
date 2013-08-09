#include "kai.hpp"

#include <stdio.h>
#include <stdlib.h>
#include <cxxabi.h>
#include <execinfo.h>

// ###################
// ##    TESTING    ##
// ###################

void foo3()
{
    puts("\nPrintBT()");
    puts("===========");
    PrintBT();
    
    puts("\nPrintBT2()");
    puts("===========");
    PrintBT2();
}
void foo2()
{
    foo3();
}
void foo1()
{
    foo2();
}

int main (int argc, char*argv[])
{
    foo1();
    return EXIT_SUCCESS;
}
