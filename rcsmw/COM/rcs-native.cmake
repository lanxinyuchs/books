#
# Cmake toolchain file for native builds with RCS specific configuration.
#
# NOTE: Toolchain files are intended for cross-compilation, however,
#       this one is convinient in order to set all the RCS speficic settings
#       with one single cmake flag.
#
# Items marked with XXX may require an action in order to use this toolchain
# file to build COM outside COM development environment.
#

# XXX Target architecture COM will be built for.
#     Supported values: x86_64, powerpc, arm, i686
set(TARGET_ARCHITECTURE "x86_64")

# Runtime bundle name
set(RUNTIME_BUNDLE_NAME COM_${TARGET_ARCHITECTURE}fRCS_RUNTIME)

set(COMFE_PACKAGING ON) # Enable COMfE packaging
set(FT ON)              # Enable function tests
set(USEFILEM OFF)       # Do not use FileM component
set(PM OFF)             # Do not use PM component
set(PT OFF)             # Do not build performance tools
set(ACCESSMGMT OFF)     # Do not use AccessManagement component
set(TLSPROXY OFF)       # Do not use TLS proxy component

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

