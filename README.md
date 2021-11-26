# lox
This is an interpreter for Lox programming language. It's an expressive multi-paradigm, dynamically interpreted programming language as descibed in Crafting Interpreters text book written by Robert Nystrom (https://craftinginterpreters.com/). This version of interpreter is being implemented in C++17.

Currently it's still in active dovelopment.

Steps to install.
  1. git clone https://github.com/anippuleti/lox.git
  2. cd lox
  3. mkdir build
  4. cd build
  5. cmake -DCMAKE_BUILD_TPYE=Release ..
  6. make -f Makefile -j8
  
To run sample program
  1. cd lox
  2. build/lox test_scripts/sample1.lox
