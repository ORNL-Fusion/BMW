cmake_minimum_required (VERSION 3.14)

project (bmw C Fortran)

find_package (OpenMP)

add_library (bmw STATIC)

target_link_libraries (bmw

                       PUBLIC

                       stell
                       $<$<BOOL:OpenMP_Fortran_FOUND>:OpenMP::OpenMP_Fortran>
)
target_include_directories (bmw

                            PUBLIC

                            $<TARGET_PROPERTY:bmw,BINARY_DIR>
)
add_executable (xbmw)
target_link_libraries (xbmw PUBLIC bmw)

add_subdirectory (Sources)

install (TARGETS xbmw)

################################################################################
#  Testing                                                                     #
################################################################################

add_subdirectory (Testing)
