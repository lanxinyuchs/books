This directory contains a model project for tests of AVC and
persistent runtime attributes.

ClearCase symlinks are set up in the Output_Models/Notif directory
in such a way that files *_mp.xml, *_classes.xml and *_objects.xml
are written in the *CAX*/etc directory when artefacts are generated.

Be sure to use "IMM R3" or higher when generating artefacts.

The model was produced using DX ECIM Toolchain 2.7.0 running on
RSA 8.5.1. It has subsequently been revised with DXET 2.11.

Problems have been noticed when trying to use DX ECIM Toolchain 2.8.0
on RSA 8.0.4: an error prevented artefacts from being generated.

Before updating the model you typically need to check out some files
(some of them may stay unchanged, depending on what you modify). Run
the script `checkout.sh' to accomplish checkouts.


README.txt       This file
checkout.sh      Script to be run before modeling

.project         Model project, named 'notif'
ECIM_Top.emx     ECIM_Top model, cloned
notif.emx        The 'Notif' model


ClearCase symlinks to FAKE_CAX*/etc

Output_Models/Notif/ALKALI_mp.xml
Output_Models/Notif/ALKALI_immR3_classes.xml
Output_Models/Notif/ALKALIinstances_immR3_objects.xml

Output_Models/Notif/NOBLE_mp.xml
Output_Models/Notif/NOBLE_immR3_classes.xml
Output_Models/Notif/NOBLEinstances_immR3_objects.xml


View-private generated files

Output_Models/Notif/.intermediate/*
Output_Models/Notif/Notif_mp.xml
Output_Models/Notif/ALKALIinstances_mp.xml
Output_Models/Notif/NOBLEinstances_mp.xml
Output_Models/Notif/mp.dtd
