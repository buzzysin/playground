cmake_minimum_required(VERSION 3.22)
project(c++-playground LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(LIB_HEADERS 
  src/quicksort.hpp
  src/ecs/ecs.hpp
)
set(LIB_SOURCES 
  src/ecs/ecs.cpp
)

set(APP_HEADERS)
set(APP_SOURCES 
  src/main.cpp
)

add_library(libc++-playground STATIC ${LIB_HEADERS} ${LIB_SOURCES})
target_include_directories(libc++-playground PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/src)

add_executable(c++-playground ${APP_SOURCES} ${APP_HEADERS})
target_link_libraries(c++-playground PRIVATE libc++-playground)
target_include_directories(c++-playground PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/src)