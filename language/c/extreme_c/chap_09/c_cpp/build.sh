#!/bin/bash

gcc -g -fsanitize=address -Wall -Wextra -Wno-unused-parameter -std=c11 ./rect.c -o rect.out
g++ -g -fsanitize=address -Wall -Wextra -Wno-unused-parameter ./rect.cpp -o rect.cpp.out

gcc -g -fsanitize=address -Wall -Wextra -Wno-unused-parameter -std=c11 ./inheritance.c -o inheritance.out
g++ -g -fsanitize=address -Wall -Wextra -Wno-unused-parameter ./inheritance.cpp -o inheritance.cpp.out

g++ -g -fsanitize=address -Wall -Wextra -Wno-unused-parameter ./abstract.cpp -o abstract.out
