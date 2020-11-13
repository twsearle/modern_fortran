cmake_minimum_required (VERSION 2.8.11)
enable_language(Fortran)

SET(CMAKE_FC_COMPILER /usr/local/bin/caf)

# project name
project (tsunami)

# make sure that the default is a RELEASE
if (NOT CMAKE_BUILD_TYPE)
  set (CMAKE_BUILD_TYPE RELEASE CACHE STRING
      "Choose the type of build, options are: None Debug Release."
      FORCE)
endif (NOT CMAKE_BUILD_TYPE)

# add executable
add_executable("tsunami" "main.f90")

# add library
add_library(mod_diff "mod_diff.f90")
add_library(mod_initial "mod_initial.f90")

target_link_libraries(tsunami PUBLIC mod_diff mod_initial)

install (TARGETS "tsunami"
         RUNTIME DESTINATION "bin")


