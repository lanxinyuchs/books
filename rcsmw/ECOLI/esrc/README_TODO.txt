Hej Lars!
Hoppas du haft en bra semester.

Things to do

OK make the std_eof_proxy build in csrc
OK implement the support for type:legacy
OK add an internal command "ls"
OK rename ecoli_DataInit to ecoli_datainit?
OK add introspection command /coli/info
OK clean up all the file-headers
OK get the src IP address (and port)
OK Put IWD in the block
OK Add to IWD info about restricted commands and path
OK make help work
OK make help work better
OK remove ? match in ecoli_ssh_channel
OK clean up all the debug printing
OKish clean up remove ecoli_debug
OK write primitive grep
OK remove hardcoded role/auth: "expert"/4 (in ecoli_ssh_channel)
OK fix the ssh-startup
OK write what should be in the app-src file
OK add symlinks into for CXC inclusion in delivery
OK remove/fix WELCOME message (MOTD)
OK seach of std_eof_proxy in ECOLI instead of SYS
OK fix empty ls
OKish add exception for lab usage
OK make help even more helpful
OK allow wildcarding if cmd registration type is 'external'
OK clean up pipe protocol
OK security loggging ala what it should be
OK re-init at upgrade, not needed
OK update IWD
OK run IWD inspection
OK Inform Erik about IWD
OK provide better xml example to the IWD
OK check if DTDs are needed (or whatever they are called)
OK clean up SYS (remove all coli + std_eof_proxy)

check for process leaks -check using ecoli_debug:process_leaks()
pg would be hard since we have no /dev/tty where to read the control...
