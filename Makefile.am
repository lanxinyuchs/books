SUBDIRS = src

ACLOCAL_AMFLAGS = -I m4

include_HEADERS =	include/libecp.h

noinst_HEADERS =

EXTRA_DIST = build-aux/git-version-gen \
	     .version

# Version string management.  There are two files to be aware of:
# .tarball-version - present only in a distribution tarball, and not in
#   a checked-out repository.  Created with contents that were learned at
#   the last time autoconf was run, and used by git-version-gen.  Must not
#   be present in either $(srcdir) or $(builddir) for git-version-gen to
#   give accurate answers during normal development with a checked out tree,
#   but must be present in a tarball when there is no version control system.
#   Therefore, it cannot be used in any dependencies.  GNUmakefile has
#   hooks to force a reconfigure at distribution time to get the value
#   correct, without penalizing normal development with extra reconfigures.
# .version - present in a checked-out repository and in a distribution
#   tarball.  At least as current as the most recent .tarball-version
#   creation.  Usable in dependencies, particularly for files that don't
#   want to depend on config.h but do want to track version changes.
BUILT_SOURCES = $(top_srcdir)/.version
$(top_srcdir)/.version:
	echo $(VERSION) > $@-t && mv $@-t $@

# Arrange so that .tarball-version appears only in distribution tarballs,
# never in a checked-out repository.
dist-hook:
	echo $(VERSION) > $(distdir)/.tarball-version

clean-local:
	-rm -rf lintresults
	-rm -rf coverityresults