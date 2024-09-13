#  Run BME. THis just tests if the code executes correctly.

#  Copy input file from the source to the build directory.
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/coils.test
                ${CMAKE_CURRENT_BINARY_DIR}/coils.test
                COPYONLY)
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/input.test.vmec
                ${CMAKE_CURRENT_BINARY_DIR}/input.test.vmec
                COPYONLY)

add_test (NAME    bmw_mgrid_test
          COMMAND $<TARGET_PROPERTY:mgrid,BINARY_DIR>/mgrid coils.test)
set_tests_properties (bmw_mgrid_test
                      PROPERTIES
                      PROCESSORS $<TARGET_PROPERTY:stell,MPIEXEC_MAX_NUMPROCS>)

add_test (NAME    bmw_vmec_test
          COMMAND $<TARGET_PROPERTY:stell,MPIEXEC_EXECUTABLE> $<TARGET_PROPERTY:stell,MPIEXEC_NUMPROC_FLAG> $<TARGET_PROPERTY:stell,MPIEXEC_MAX_NUMPROCS> $<TARGET_PROPERTY:vmec,BINARY_DIR>/xvmec input.test.vmec)
set_tests_properties (bmw_vmec_test
                      PROPERTIES
                      DEPENDS bmw_mgrid_test
                      PROCESSORS $<TARGET_PROPERTY:stell,MPIEXEC_MAX_NUMPROCS>)

add_test (NAME    bmw_test
          COMMAND $<TARGET_PROPERTY:bmw,BINARY_DIR>/xbmw -mgrid=mgrid_test.nc -woutf=wout_test.vmec.nc -outf=result.nc -para=-1)
set_tests_properties (bmw_test
                      PROPERTIES
                      DEPENDS bmw_vmec_test
                      PROCESSORS $<TARGET_PROPERTY:stell,MPIEXEC_MAX_NUMPROCS>)
