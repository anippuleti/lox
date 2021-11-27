cmake_minimum_required(VERSION 3.5)
project(lox VERSION 0.1.1 LANGUAGES CXX)
set(CMAKE_CXX_STANDARD 17)

add_compile_options(-Wall -Wextra -Wpedantic -Werror)
#add_subdirectory(lib/Catch2-v3/Catch2)

##Main lox executable
add_executable(lox
        src/main.cpp
        src/srcReader.cpp
        src/srcReader.h
        src/token.h
        src/scanner.cpp
        src/scanner.h
        src/errorHandler.cpp
        src/errorHandler.h
        src/exprNodes.cpp
        src/exprNodes.h
        src/varEnv.cpp
        src/varEnv.h
        src/vecWrapper.h
        src/stmtNodes.h
        src/stmtNodes.cpp
        src/parser.h
        src/parser.cpp
        src/interpreter.h
        src/interpreter.cpp)

target_compile_options(lox PRIVATE -ftemplate-backtrace-limit=0)

##Unittest version that linked to Catch2-v3 static library
#add_executable(utest_lox
#        src/srcReader.cpp
#        src/srcReader.h
#        src/token.h
#        src/scanner.cpp
#        src/scanner.h
#        src/errorHandler.cpp
#        src/errorHandler.h
#        src/exprNodes.cpp
#        src/exprNodes.h
#        src/varEnv.cpp
#        src/varEnv.h
#        src/vecWrapper.h
#        src/stmtNodes.h
#        src/stmtNodes.cpp
#        src/parser.h
#        src/parser.cpp
#        src/interpreter.h
#        src/interpreter.cpp
#        unit_tests/test_exprNodes.cpp)
#
#target_link_libraries(utest_lox
#        PRIVATE Catch2::Catch2WithMain
#)
#
#target_include_directories(utest_lox PRIVATE src)
#target_include_directories(utest_lox
#        PRIVATE lib/Catch2-v3/Catch2
#)
