ABOUT
-----
RCSMW source code repository.
Make support build and package all different targettypes and supported arch's at one call as default (see Makefile for more detailed  build alternatives)


PREREQUISITES
-------------
Ericsson, standard Linux HUB-host (with ARC/AFS)


HOWTO
-----

1. Clone Repo
  ExecProjName RCSDE "RCSMW/1.0" "mwclone"

2. Init environment
  cd <repo>
  ExecProjName RCSDE "RCSMW/1.0"

3. Build
  make package


Note! several tools, CRL's and "sourced" components are inserted to the build from binary caches, where version, fetch url, provisioning path (and more) is controlled by the global Baseline skeleton file (skeleton/CIdata.txt.skeleton).


CHANGE VERSION OF SOURCED COMPONENT
-----------------------------------

1. Update Component Version
  <editor> skeleton/CIdata.txt.skeleton

2. Build & Deploy
  cd <component>
  make release

3. Commit and push to gerrit


INTRODUCTION/REMOVAL OF ARCH TO BUILD
-------------------------------------

1. Update affected Makefile(s)

2. Build & Deploy affected Component(s)

3. Commit and push to gerrit


Component build order:

  tools/protobuf-c
  SAF
  OTP
  COM
