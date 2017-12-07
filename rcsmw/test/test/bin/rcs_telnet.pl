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
## ----------------------------------------------------------
## Description:
## Perl module to telnet boards in lab, resolves hw into IPaddress and port.
## Uses: $RCT_TOP/test/bin/getHwData.pm
##       /proj/rcs/host-book/
## ----------------------------------------------------------

use strict;
use lib '/proj/webdocs/rbs-rde/cgi-bin';
use getHwData;

my $booking_dir = "/proj/webdocs/rbs-rde/etc/host-book";
my $booking_page = "https://rbs-rde.rnd.ki.sw.ericsson.se/cgi-bin/hw-book.pl";

my ($remote, $port);

if ($#ARGV == 0) {                              # 2 arguments, i.e. fetch data from RCS hw booking
    my ($hw) = @ARGV;
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
    if ( !($remote = $HoH{$hw}{console_ip}) ) {      # Is console_ip defined for hw?
	print "ERROR: $hw console_ip not defined, see $booking_page\n";
	exit 1;
    }
    if ( !($port = $HoH{$hw}{cons_port}) ) {         # Is cons_port defined for hw?
	    print "ERROR: $hw cons_port not defined, see $booking_page\n";
	exit 1;
    }
} else {                                             # Wrong number of arguments
    print "ERROR: Unsupported number of arguments\n";
    usage();
}

print "telnet $remote $port\n";
system("telnet $remote $port");

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
    print "rcs_telnet.pl HWName
HWName:        hw name on RCS HW booking page

Example:
rcs_telnet.pl dus001\n";
    exit 1;
}

