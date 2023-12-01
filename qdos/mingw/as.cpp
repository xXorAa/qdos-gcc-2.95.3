/*
 * Copyright (c) 2023 Graeme Gregory <graeme@xora.org.uk>
 *
 * SPDX: GPL-2.0
 */

#include <iostream>
#include <cstdlib>
#include <string>
#include <unistd.h>
#include <vector>

const char *const opts="o:D:";

int main(int argc, char **argv)
{
    //std::cout << "XorA's as to as68 converter\n";

    std::string output;
    std::string input;
    std::vector<std::string> defines;

    int c;
    while(( c = getopt(argc, argv, opts)) != -1) {
        switch (c) {
        case 'o':
            if (optarg) {
                output = optarg;
            }
            break;
        case 'D':
            if (optarg) {
                //defines.push_back(optarg);
            }
            break;
        }
    }

    if (optind < argc) {
        input = argv[optind];
    } else {
        std::cerr << "Error no input file!" << std::endl;
        std::exit(1);
    }

    std::string cmdline = "/qdos-gcc/bin/as68.exe " + input + " -o " + output;

    //std::cout << "Running: " << cmdline << std::endl;

    system(cmdline.c_str()); 
}
