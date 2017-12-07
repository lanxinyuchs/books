#!/usr/bin/perl
## ----------------------------------------------------------
## Copyright (c) Ericsson AB 2012 All rights reserved.
## 
## The information in this document is the property of Ericsson.
## 
## Except as specifically authorized in writing by Ericsson, the 
## receiver of this document shall keep the information contained 
## herein confidential and shall protect the same in whole or in 
## part from disclosure and dissemination to third parties.
## 
## Disclosure and disseminations to the receivers employees shall 
## only be made on a strict need to know basis.
## ----------------------------------------------------------
## #1.    REVISION LOG
## ----------------------------------------------------------
## Rev        Date         Name        What
## --------   --------     --------    ------------------------
## R1A/1      2012-03-14   etxkols     Created
## R1A/2      2012-04-12   etxkols     Changed select timeout from 0.5 to 2.0
## R1A/3      2012-04-17   etxkols     Changed cycle sleep btwn off and on to 4 sec
## R1A/4      2012-04-26   etxkols     Parts rewritten since new console server
##                                     does not fail connect when port is busy
## R1A/5      2012-05-15   etxkols     Reverted back to old cons serv because of 
##                                     robustness problems in new cons serv
##                                     Sending every order 3 times because of unreliable
##                                     bus btwn processor and outlets in PDU.
## ----------------------------------------------------------
## Description:
## Perl module to power off/on/cycle PDU ports from shell
## Uses: $RCT_TOP/test/bin/getHwData.pm
##       /proj/rcs/host-book/
## Implements Svift protocol, see documents in Prim 1551-BMG980347, 
## 1551-BMG980348/2 and http://www.telamp.se/tekn/svift/docs/telamp03_104_e.pdf.
# ----------------------------------------------------------

use strict;
use lib '/proj/webdocs/rbs-rde/cgi-bin';
use getHwData;
use Socket;

my $booking_dir = "/proj/webdocs/rbs-rde/etc/host-book";
my $booking_page = "https://rbs-rde.rnd.ki.sw.ericsson.se/cgi-bin/hw-book.pl";

my ($remote, $port, $power_port, $action, $iaddr, $paddr);


if ($#ARGV == 3) {                                   # 4 arguments
    ($remote, $port, $power_port, $action) = @ARGV;
} elsif ($#ARGV == 1) {                              # 2 arguments, i.e. fetch data from RCS hw booking
    (my $hw, $action) = @ARGV;
    my ($column_headers, $sorted_hosts, $HoH) = get_booking_data();
    my @sorted_hosts = @$sorted_hosts;
    my %HoH = %$HoH;
    if (grep(/$hw/,@sorted_hosts)) {                 # Does hw exist?
	if ( open(BOOK, "$booking_dir/$hw") ) {      # Is hw booked?
	    my $file = <BOOK>;
	    close BOOK;
	    chomp $file;
	    if ( "$file" ne "$ENV{USER}" ) {         # Is hw booked by $USER?
		if ( warning("$hw booked by $file, do you want to continue (yes/no)? ") ) {
		    exit 0;
		}
	    }	    
	} else {
	    if ( warning("$hw not booked, do you want to continue (yes/no)? ") ) {
		exit 0;
	    }
	}	 
    } else {                                         # hw does not exist
	print "ERROR: $hw not configured, see $booking_page\n";
	exit 1;
    }
    if ( !($remote = $HoH{$hw}{power_cons_ip}) ) {   # Is power_cons_ip defined for hw?
	print "ERROR: $hw power_cons_ip not defined, see $booking_page\n";
	exit 1;
    }
    if ( !($port = $HoH{$hw}{power_cons_port}) ) {   # Is power_port defined for hw?
	    print "ERROR: $hw power_cons_port not defined, see $booking_page\n";
	exit 1;
    }
    if ( !($power_port = $HoH{$hw}{power_port}) ) {  # Is power_port defined for hw?
	if ( $power_port ne "0" ) {   # power_port can be 0?
	    print "ERROR: $hw power_port not defined, see $booking_page\n";
	    exit 1;
	}
    }
} else {                                             # Wrong number of arguments
    print "ERROR: Unsupported number of arguments\n";
    usage();
}

if ( $power_port > 6 || $power_port < 0 ) {          # Correct PDUport ?
    print "ERROR: Illegal PDUPort $power_port ( 0 >= PDUPort < 7 )\n";
    usage();
}

if ( $action eq "on" ) {                             # Correct Action ?
    print "Power on $remote $port $power_port (power_cons_ip power_cons_port power_port)\n";
} elsif ( $action eq "off" ) {
    print "Power off $remote $port $power_port (power_cons_ip power_cons_port power_port)\n";
} elsif ( $action eq "read" ) {
    print "Read power state $remote $port $power_port (power_cons_ip power_cons_port power_port)\n";
} elsif ( $action eq "cycle" ) {
    print "Power cycle $remote $port $power_port (power_cons_ip power_cons_port power_port)\n";
} else {                                             # Wrong action
    print "ERROR: Illegal Action $action\n";
    usage();
}

$iaddr = inet_aton($remote);
$paddr = sockaddr_in($port, $iaddr);
socket(RAW, AF_INET, SOCK_STREAM, getprotobyname('tcp')) || die $!;
my $count = 5;
while ( $count ) {
    if ( connect(RAW, $paddr) ) {
	$count = 0;
	recv_socket(); #flush
# The crappy PDU unit has a bus between processor unit and outlets.
# When ordering power off/on/read, the processor unit replies ok,
# but since the bus is not reliable, the outlet may not power off/on.
# The easiest way to overcome this is to order 3 power off/on, this
# should hopefully reduce the failed actions from 1/50 to 1/125000.
	if ( $action eq "cycle" ) {
	    $action = "off";
	    send (RAW, pack ("H22",make_power_cmd($power_port, $action)), 0);
	    recv_socket();
	    send (RAW, pack ("H22",make_power_cmd($power_port, $action)), 0);
	    recv_socket();
	    send (RAW, pack ("H22",make_power_cmd($power_port, $action)), 0);
	    check_recv($power_port, $action, recv_socket());
	    sleep 4;
	    $action = "on";
	    send (RAW, pack ("H22",make_power_cmd($power_port, $action)), 0);
	    recv_socket();
	    send (RAW, pack ("H22",make_power_cmd($power_port, $action)), 0);
	    recv_socket();
	    send (RAW, pack ("H22",make_power_cmd($power_port, $action)), 0);
	    check_recv($power_port, $action, recv_socket());
	} else {
	    send (RAW, pack ("H22",make_power_cmd($power_port, $action)), 0);
	    recv_socket();
	    send (RAW, pack ("H22",make_power_cmd($power_port, $action)), 0);
	    recv_socket();
	    send (RAW, pack ("H22",make_power_cmd($power_port, $action)), 0);
	    check_recv($power_port, $action, recv_socket());
	}
    } elsif ( $! =~ /Connection refused/ ) {
	$count--;
	print "$remote busy, will retry $count times with 5 seconds delay\n";
	sleep 5;
    } else {
	$count = 0;
	print "$! $remote}\n";
    }
}
shutdown (RAW, 2);

sub make_power_cmd {
    my ($power_port, $action) = @_;
    my @MCL2;
    if ( $action eq "read") {
	push @MCL2,"E9";          # MLC2: header 1, HI=1, MT=1, ME=1, SCE=0, FRLEN=9
    } else {
	push @MCL2,"EA";          # MLC2: header 1, HI=1, MT=1, ME=1, SCE=0, FRLEN=10
    }
    push @MCL2,"01";              # MLC2: header 2, Master expansion byte, 1=denib type protocol
    push @MCL2,"41";              # SVIFT: HFLG=4(HFLG_REQU), HPNR=1(always)
    push @MCL2,"21";              # SVIFT: DMOD=2(AMOD_RELP), DADR=1(Destination, always 1 for us)
    push @MCL2,"00";              # SVIFT: SMOD=2(AMOD_RELP), sADR=1(Source, always 0 for us)
    push @MCL2,"08";              # SVIFT: OTYP=8(OTYP_GROUP)
    push @MCL2,"${power_port}2";  # SVIFT: ONBR=our power_port, CODE=2(CODE_start)
    push @MCL2,"07";              # SVIFT: OTYP=7(OTYP_NSTCTL)
    if ( $action eq "read") {
	push @MCL2,"00";          # SVIFT: ONBR=0, CODE=0(CODE_read)
    } else {
	push @MCL2,"01";          # SVIFT: ONBR=0, CODE=1(CODE_write)
    }
    if ( $action eq "off") {
	push @MCL2,"00";          # SVIFT: Data=0(state off)
    } elsif ( $action eq "on") {
	push @MCL2,"01";          # SVIFT: Data=1(state on)
    }
    push @MCL2, make_csum(@MCL2); # MLC2: Checksum
    my $result = "";
    foreach my $item (@MCL2) {
	$result = "$result$item";
    }
    return $result;
}

sub make_csum {
    my @List = @_;
    my $sum = 0;
    foreach my $element (@List) {
	$sum = $sum + hex($element);
    }
    $sum = (255 ^ (255 & $sum));
    return sprintf("%02x", $sum);
}

sub recv_socket {
    my $rin;
    vec($rin, fileno(RAW), 1) = 1;
    my $msg = "";
    my $result = "";
    while (select($rin, undef, undef, 1.5)) {
	recv(RAW, $msg, 1, 0);# or die "recv $!";
	$msg = uc(unpack("H2", $msg));
	$result = "$result$msg";
    }
#    print "$result\n";
    return $result;
}

sub check_recv {
    my ($power_port, $action, $result) = @_;
    if ($action eq "off") {
	if ($result =~ /^EA0101002008${power_port}2070100../) {
	    print "Powered off\n";
	} else {
	    print "ERROR: Could not power off $power_port, received $result\n";
	}
    } elsif ($action eq "on") {
	if ($result =~ /^EA0101002008${power_port}2070101../) {
	    print "Powered on\n";
	} else {
	    print "ERROR: Could not power on $power_port, received $result\n";
	}
    } elsif ($action eq "read") {
	if ($result =~ /^EB0101002008${power_port}207000200../) {
	    print "Powered off\n";
	} elsif ($result =~ /^EB0101002008${power_port}207000201../) {
	    print "Powered on\n";
	} else {
	    print "ERROR: Could not read power on $power_port, received $result\n";
	}
    }
}

sub warning {
    my ($message) = @_;
    print "$message";
    my $userinput =  <STDIN>;
    chomp ($userinput);
    if ( $userinput=~ /yes/ ) {
	return 0;
    } else {
	return 1;
    }
}

sub usage {
    print "rcs_power.pl [ HWName | [ ConsoleIP, ConsolePort, PDUPort ] ] Action
HWName:        hw name on RCS HW booking page
ConsoleIP:     Console server IP address
ConsolePort:   Console server Port number
PDUPort:       Port in PDU ( 0 >= PDUPort < 7 )
Action:        on | off | cycle | read

Examples:
rcs_power.pl dus001 on
rcs_power.pl 137.58.180.20 1002 6 on\n";
    exit 1;
}


