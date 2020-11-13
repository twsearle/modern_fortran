cmake_minimum_required (VERSION 2.8.11)
enable_language(Fortran)

SET(CMAKE_FC_COMPILER /usr/local/bin/caf)

# project name
project (stock_gain)

# make sure that the default is a RELEASE
if (NOT CMAKE_BUILD_TYPE)
  set (CMAKE_BUILD_TYPE RELEASE CACHE STRING
      "Choose the type of build, options are: None Debug Release."
      FORCE)
endif (NOT CMAKE_BUILD_TYPE)

# add executable
add_executable("stock_gain" "stock_gain.f90")

# add library
add_library(mod_utils "mod_utils.f90")
add_library(mod_stock_io "mod_stock_io.f90")

target_link_libraries(stock_gain PUBLIC mod_utils mod_stock_io)
target_link_libraries(mod_stock_io mod_utils)

install (TARGETS "stock_gain"
         RUNTIME DESTINATION "bin")


