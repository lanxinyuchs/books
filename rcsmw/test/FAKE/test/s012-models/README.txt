A model project for maintaining MOMs used for
testing upgrade behaviors. ClearCase symlinks in this
file tree point to locations in FAKE_CAX*/etc and
../upgrade, with name changes in several cases.


README.txt                 This file

checkout.sh                To be run before modeling

.project                   DX ECIM Toolchain project

ECIM_Top.emx               ECIM_Top clone
MomFrom.emx                Model for the From-UP
MomTo.emx                  Model for the To-UP


Output_Models/MomFrom/.intermediate/*   View-private generated files

Output_Models/MomFrom/*                 Generated files (view-private and ClearCase symlinks)

Output_Models/MomTo/.intermediate/*     View-private generated files

Output_Models/MomTo/*                   Generated files (view-private and ClearCase symlinks)
