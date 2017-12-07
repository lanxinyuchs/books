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
MODULES = bencode.beam eTorrent.beam eTorrentClient.beam eTorrentListen.beam eTorrentPeer.beam eTorrentSuper.beam 

all:	eTorrent.app $(MODULES)

eTorrent.app: $(MODULES)
	cp $(SRC)/eTorrent.app $(EBIN) 
	cp *.beam $(EBIN)

bencode.beam:
	erlc -I $(INC)  $(SRC)/bencode.erl

eTorrent.beam:
	erlc -I $(INC)  $(SRC)/eTorrent.erl

eTorrentClient.beam:
	erlc -I $(INC)  $(SRC)/eTorrentClient.erl

eTorrentListen.beam:
	erlc -I $(INC)  $(SRC)/eTorrentListen.erl

eTorrentPeer.beam:
	erlc -I $(INC)  $(SRC)/eTorrentPeer.erl

eTorrentSuper.beam:
	erlc -I $(INC)  $(SRC)/eTorrentSuper.erl


clean:
	rm -f *.beam
	rm -f $(EBIN)/*
	rm -rf *~ */*~ */*/*~ 


.PHONY: clean erlcomp
FORCE:
