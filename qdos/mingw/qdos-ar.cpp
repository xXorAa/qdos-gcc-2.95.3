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
const std::string valid_flags1 = "-rc -rcv -r -c";
const std::string valid_flags2 = "-x -xv";

int main(int argc, char **argv)
{
    std::cout << "XorA's as to qdos-ar converter\n";

    if (argc < 4) {
        std::cerr << "Error invalid arguments!" << std::endl;
        std::exit(1);
    }

    std::string flags = argv[1];
    std::string library = argv[2];

    if (flags[0] != '-') {
        flags.insert(0, "-");
    }

    if (valid_flags1.find(flags)) {
        std::string cmdline = "/qdos/bin/slb.exe -ek " + library;

        for (int i = 3; i < argc ; i++) {
            cmdline.append(" ");
            cmdline.append(argv[i]);
        }

        std::cout << "Running: " << cmdline << std::endl;

        system(cmdline.c_str());
    } else if (valid_flags2.find(flags)) {
        std::string cmdline = "/qdos-gcc/bin/slb.exe " + flags + " " + library;

        for (int i = 3; i < argc ; i++) {
            cmdline.append(" ");
            cmdline.append(argv[i]);
        }

        std::cout << "Running: " << cmdline << std::endl;

        system(cmdline.c_str());
    }
}
