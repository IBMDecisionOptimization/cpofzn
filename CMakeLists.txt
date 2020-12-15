cmake_minimum_required(VERSION 3.5)
project(cpofzn VERSION 0.1)
set(CMAKE_CXX_STANDARD 11)

if (MSVC)
    add_compile_options(/W4)
else()
    add_compile_options(-Wall -Wextra -pedantic)
endif()

find_package(FLEX REQUIRED)
find_package(BISON REQUIRED)
flex_target(Scanner src/fznscanner.l src/fznscanner.cpp COMPILE_FLAGS --header-file=src/fznscanner.h DEFINES_FILE src/fznscanner.h)
bison_target(Parser src/fznparser.y src/fznparser.cpp COMPILE_FLAGS --defines=src/fznparser.h DEFINES_FILE src/fznparser.h)
ADD_FLEX_BISON_DEPENDENCY(Scanner Parser)

set(SOURCE_FILES src/cpofzn.cpp src/fznreader.cpp ${FLEX_Scanner_OUTPUTS} ${BISON_Parser_OUTPUTS})

add_executable(cpofzn ${SOURCE_FILES})

find_package(Threads)
target_link_libraries(cpofzn ${CMAKE_THREAD_LIBS_INIT})

find_path(CPLEX_STUDIO_DIR cpoptimizer/include/ilcp/cp.h REQUIRED)
find_path(CONCERT_DIR include/ilconcert/iloenv.h HINTS ${CPLEX_STUDIO_DIR}/concert REQUIRED)
find_path(CPOPTIMIZER_DIR include/ilcp/cp.h REQUIRED HINTS ${CPLEX_STUDIO_DIR}/cpoptimizer REQUIRED)
find_path(CPLEX_DIR include/ilcplex/cplex.h HINTS ${CPLEX_STUDIO_DIR}/cplex REQUIRED)
include_directories(${CONCERT_DIR}/include)
include_directories(${CPOPTIMIZER_DIR}/include)
find_library(CONCERT_LIB concert PATHS ${CONCERT_DIR}/lib REQUIRED PATH_SUFFIXES x86-64_linux/static_pic x86-64_osx/static_pic ppc64le_linux/static_pic s390x_linux/static_pic x64_windows_vs2017/stat_mda x64_windows_msvc14/stat_mda x64_windows_vs2015/stat_mda x64_windows_vs2013/stat_mda)
find_library(CPOPTIMIZER_LIB cp PATHS ${CPOPTIMIZER_DIR}/lib REQUIRED PATH_SUFFIXES x86-64_linux/static_pic x86-64_osx/static_pic ppc64le_linux/static_pic s390x_linux/static_pic x64_windows_vs2017/stat_mda x64_windows_msvc14/stat_mda x64_windows_vs2015/stat_mda x64_windows_vs2013/stat_mda)
find_library(CPLEX_LIB NAMES cplex cplex2010 PATHS ${CPLEX_DIR}/lib REQUIRED PATH_SUFFIXES x86-64_linux/static_pic x86-64_osx/static_pic ppc64le_linux/static_pic s390x_linux/static_pic x64_windows_vs2017/stat_mda x64_windows_msvc14/stat_mda x64_windows_vs2015/stat_mda x64_windows_vs2013/stat_mda)
target_link_libraries(cpofzn ${CPOPTIMIZER_LIB})
target_link_libraries(cpofzn ${CPLEX_LIB})
target_link_libraries(cpofzn ${CONCERT_LIB})

if(CMAKE_DL_LIBS)
    target_link_libraries(cpofzn ${CMAKE_DL_LIBS})
endif()

# Add targets "debug" and "release":
ADD_CUSTOM_TARGET(debug
  COMMAND ${CMAKE_COMMAND} -DCMAKE_BUILD_TYPE=Debug ${CMAKE_SOURCE_DIR}
  COMMAND ${CMAKE_COMMAND} --build ${CMAKE_BINARY_DIR} --target all
  COMMENT "Creating the executable in the debug mode.")

ADD_CUSTOM_TARGET(release
  COMMAND ${CMAKE_COMMAND} -DCMAKE_BUILD_TYPE=Release ${CMAKE_SOURCE_DIR}
  COMMAND ${CMAKE_COMMAND} --build ${CMAKE_BINARY_DIR} --target all
  COMMENT "Creating the executable in the release mode.")

# Generate solver config file for MiniZinc:
add_custom_command(TARGET cpofzn COMMAND cpofzn -c "${CMAKE_CURRENT_BINARY_DIR}/com.ibm.cpo.msc" $<TARGET_FILE:cpofzn> "${CMAKE_CURRENT_BINARY_DIR}")
