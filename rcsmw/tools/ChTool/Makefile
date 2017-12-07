# **********************************************************************
# %CCaseFile:	Makefile %
# %CCaseRev:	/main/3 %
# %CCaseDate:	2012-10-11 %
# %CCaseDocNo:	267/190 04-LXA 119 334 Ux %
# Author:	etxbjca
#
# Short description: Syntax Check Tools
#
# **********************************************************************
# %CCaseCopyrightBegin%
# Copyright (c) Ericsson AB 2017 All rights reserved.
# 
# The information in this document is the property of Ericsson.
# 
# Except as specifically authorized in writing by Ericsson, the 
# receiver of this document shall keep the information contained 
# herein confidential and shall protect the same in whole or in 
# part from disclosure and dissemination to third parties.
# 
# Disclosure and disseminations to the receivers employees shall 
# only be made on a strict need to know basis.
# %CCaseCopyrightEnd%
#
# **********************************************************************
#
# Rev        Date       Name        What
# -----      -------    --------    --------------------------
# A          2017-05-16 etxbjca     Imported from CCase
# **********************************************************************
#

#******************************************************************************
#   Define macros.
#

BLOCKDIR = 	$(CURDIR)

PROGRAM :=	$(wildcard *.l)

INSTDIR =       ../bin

LDFLAGS =	-B static

CC =		gcc

#******************************************************************************
#   Define Targets.
#

default: 	$(PROGRAM:%.l=%)

clean:		
		rm -f $(PROGRAM:%.l=%)

install:	$(PROGRAM:%.l=$(INSTDIR)/%)

all:		default install clean

$(INSTDIR)/%:	%
		install -p -D $(@F) $(@D)

.l:
		$(RM) $*.c
		$(LEX.l) $< > $*.c
		$(LINK.c) -o $@ $*.c
		$(RM) $*.c

#******************************************************************************
#   End Makefile.
#
