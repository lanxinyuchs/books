#
# CMake toolchain file for RCS i686
#
# Items marked with XXX require an action in order to use this toolchain
# file to build COM outside COM development environment.
#
set(CMAKE_SYSTEM_NAME Linux)
set(CMAKE_SYSTEM_VERSION 1)
set(TARGET_ARCHITECTURE "x86_64")

# Sysroot
# XXX If you are not a COM Developer, you will have to modify this with
#     the path where you store your sysroot
#set(SYSROOT /proj/ecomhud/com-deps/sysroot/rcs/R2A122/DISTRO_CXA11448/x86_64)
#set(SYSROOT /vobs/rcs/dev/RCP_CSX10179/OS_CRX901265/DISTRO/DISTRO_CXA11448/x86_64)
#set(SYSROOT /)
#set(SYSROOT_LD_LIBRARY_PATH "${SYSROOT}/lib64:${SYSROOT}/usr/lib64")

# WR5 Compilers
#set(CMAKE_C_COMPILER "/app/rbs/wrtools/5.0.1d2/bin/i686-wrs-linux-gnu-gcc")
#set(CMAKE_CXX_COMPILER "/app/rbs/wrtools/5.0.1d2/bin/i686-wrs-linux-gnu-g++")

# Set compiler options, the "CACHE STRING FORCE" is a workaround to make sure
# flags are used in CMake compiler tests.
# XXX Here you can add your own compiler and linker flags. If you tell COM about
#     the flags you want, they can probably add them to this file, so COM deliveries
#     are tested with your flags.
#set(CMAKE_C_FLAGS "-m64 --sysroot=${SYSROOT} -Wno-unused-local-typedefs" CACHE STRING "" FORCE)    # C Compiler flags
#set(CMAKE_CXX_FLAGS "-m64 --sysroot=${SYSROOT} -Wno-unused-local-typedefs" CACHE STRING "" FORCE)  # C++ Compiler flags
#set(CMAKE_EXE_LINKER_FLAGS "-m64 -Wl,--dynamic-linker=${SYSROOT}/lib64/ld-linux-x86-64.so.2" CACHE STRING "" FORCE) # Linker flags
set(CMAKE_EXE_LINKER_FLAGS "-m64 " CACHE STRING "" FORCE) # Linker flags

# We need to make sure LD_RUN_PATH does not affect the RPATH of the binary, so
# we set it ourselves. RCS will use LD_LIBRARY_PATH when running COM to point
# to their sysroot.
#
set(CMAKE_SKIP_BUILD_RPATH FALSE)
set(CMAKE_BUILD_WITH_INSTALL_RPATH TRUE)
set(CMAKE_INSTALL_RPATH "${SYSROOT_LD_LIBRARY_PATH}")

# Prefix search locations
set(CMAKE_FIND_ROOT_PATH
	${SYSROOT}
)
set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)


set(COM_INSTALL_PREFIX opt/com CACHE STRING "COM install prefix") 
set(FT_PSO_INSTALL_PREFIX opt/com)
set(RPM OFF CACHE BOOL "Set RPM OFF" FORCE)
set(FT OFF CACHE BOOL "Disable function tests")                          # Enable function tests
set(USEFILEM OFF CACHE BOOL "Do not use FileM component")              # Do not use FileM component
set(PM OFF CACHE BOOL "Do not use PM component")                       # Do not use PM component
set(PT OFF CACHE BOOL "Do not build performance tools")                # Do not build performance tools
set(UT OFF CACHE BOOL "Build UT")                                      # Cannot build UT with GCC4.8
set(ACCESSMGMT OFF CACHE BOOL "Do not use AccessManagement component") # Do not use AccessManagement component
set(TLSPROXY OFF CACHE BOOL "Do not use TLS proxy component")          # Do not use TLS proxy component
set(LDAP_MODULE OFF CACHE BOOL "Do not use LDAP Service module")       # Do not use LDAP Service module

# Add debug symbol flags "-g"
set(CMAKE_BUILD_TYPE Debug CACHE STRING "Enable debug symbol compiler flags") 

# Configuration of the FT framework. This will be written to com_test.properties
set(FT_IP_VERSION 4)           # Set IPv4 only for function tests
set(FT_NETSNMP_VERSION 30)     # Set net-snmp major version
set(FT_CHECK_ARCHITECTURE OFF) # Disable architecture checks

