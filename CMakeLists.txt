cmake_minimum_required(VERSION 3.11.4)
project(Kaleidoscope)

file(GLOB SOURCES "src/*.cpp")

add_executable(kaleidoscope ${SOURCES})
