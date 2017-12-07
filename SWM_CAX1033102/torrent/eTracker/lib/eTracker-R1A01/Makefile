# Makefile for eTorrent
# Copyright (c) Ericsson AB 2011 All rights reserved.
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
#
# ---------------------------------------------------------------------- 



SRC = src
EBIN = ebin
INC = include
MODULES = bencode.beam eTracker.beam eTrackerServer.beam eTrackerSuper.beam \
	  eTrackerWWW.beam

all: $(MODULES) eTracker.app

eTracker.app: $(MODULES)
	cp $(SRC)/eTracker.app $(EBIN) 
	cp *.beam $(EBIN)

bencode.beam:
	erlc -I $(INC)  $(SRC)/bencode.erl

eTracker.beam:
	erlc -I $(INC)  $(SRC)/eTracker.erl

eTrackerSuper.beam:
	erlc -I $(INC)  $(SRC)/eTrackerSuper.erl

eTrackerServer.beam:
	erlc -I $(INC)  $(SRC)/eTrackerServer.erl

eTrackerWWW.beam:
	erlc -I $(INC)  $(SRC)/eTrackerWWW.erl


clean:
	rm -f *.beam
	rm -rf *~ */*~ */*/*~ 


.PHONY: clean 
FORCE:
