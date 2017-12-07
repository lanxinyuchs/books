#
# Cmake toolchain file for VRCS64 x86_64
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
#set(SYSROOT "")

# Compilers
#set(CMAKE_C_COMPILER "")
#set(CMAKE_CXX_COMPILER "")

# Set compiler options, the "CACHE STRING FORCE" is a workaround to make sure
# flags are used in CMake compiler tests.
# XXX Here you can add your own compiler and linker flags. If you tell COM about
#     the flags you want, they can probably add them to this file, so COM deliveries
#     are tested with your flags.
#set(CMAKE_EXE_LINKER_FLAGS "") # Linker flags
set(CMAKE_C_FLAGS "${BUILDFLAGS}" CACHE STRING "" FORCE)    # C Compiler flags
set(CMAKE_CXX_FLAGS "${BUILDFLAGS}" CACHE STRING "" FORCE)  # C++ Compiler flags
set(CMAKE_EXE_LINKER_FLAGS "${BUILDLDFLAGS}" CACHE STRING "" FORCE)  # Linker flags

# Prefix search locations
set(CMAKE_FIND_ROOT_PATH
	${SYSROOT}
)
set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)

# Runtime bundle name


set(COM_INSTALL_PREFIX opt/com CACHE STRING "COM install prefix") 
set(FT_PSO_INSTALL_PREFIX opt/com)
set(RPM OFF CACHE BOOL "Set RPM OFF" FORCE)
set(FT OFF CACHE BOOL "Do not use function tests")                     # Do not use function tests
#set(FT ON CACHE BOOL "Enable function tests")                         # Enable function tests
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
set(FT_START_PORT 27000)             # Port span
set(FT_END_PORT 28000)
set(FT_NUM_PORTS 15)
set(FT_REMOTE_INSTALL_TYPE TAR)      # How COM will be installed in remote target (TAR / RPM)
set(FT_COM_USER_NAME root)           # Credentials of user of remote target, no need of root privileges
set(FT_COM_USER_PASSWORD root)
set(FT_ROOT_USER_NAME root)          # Credentials of user of remote target with root privileges
set(FT_ROOT_USER_PASSWORD root)
set(FT_NETSNMP_VERSION 30)           # Major version of SNMP installed in the target

