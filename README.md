# lox
This is an interpreter for Lox programming language. It's an expressive multi-paradigm, dynamically interpreted programming language as descibed in Crafting Interpreters text book written by Robert Nystrom (https://craftinginterpreters.com/). This version of interpreter is being implemented in C++17.

Currently it's still in active dovelopment.

Steps to install.
  git clone https://github.com/anippuleti/lox.git
  cd lox
  mkdir build
  cd build
  cmake -DCMAKE_BUILD_TPYE=Release ..
  make -f Makefile -j8
  
To run sample program
  cd lox
  build/lox test_scripts/sample1.lox
