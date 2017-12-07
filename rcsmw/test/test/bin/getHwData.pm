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
## R1A/1      2012-02-25   etxkols     Created
## R1A/2      2012-06-13   qstetom     Filter out Meta tags
## ----------------------------------------------------------
## Read HW database (currently foswiki file) and returns data used by
## HW booking page (https://rbs-rde.rnd.ki.sw.ericsson.se/cgi-bin/hw-book.pl)
## and common test when grouping stp to hw (currently rct_add_stp.sh)
##
## Exports: get_booking_data which returns
## column_headers: array of all column headers
## sorted_hosts:   array of hw sorted in same order as spreadsheet file
## HoH:            hash of hashes hw,columnheader -> cell

package getHwData;

use strict;

use Exporter qw( import );
our @EXPORT = qw(get_booking_data);

my @column_headers;
my @sorted_hosts;
my %HoH;
my $row;
my @row;
my @file;
my $notcsv = "/proj/webdocs/rbs-rde-wiki/root/data/Main/LabConfig.txt";

sub get_booking_data {
    make_csv();  # csv returned in @file
##### Added ######
    $row = shift @file;
    chomp $row;
    @column_headers = split("[;]", $row);
##### End Added ######
#    @column_headers = split("[;]", shift @file);
    foreach $row (@file) {
##### Added ######
	chomp $row; 
##### End Added ######
	@row = split("[;]", $row);
	build_HoH();
    }
    close (FILE);
#    print_HoH();
    return ( \@column_headers, \@sorted_hosts, \%HoH );
}

# build hash of hashes
sub build_HoH {
#    @row = split("[;]", $row);
    if ( $row[0] ne "" ) {           # avoid rows without entry in name column
	push @sorted_hosts, $row[0]; # keyes sorted in same order as spreadsheet file, i.e. $csv
	for( my $i = 1; $i < ( $#column_headers + 1 ); $i++ ) {
	    $HoH{$row[0]}{$column_headers[$i]} = $row[$i];
	}
    }
}

# Writes ; separated csv file converted from foswiki spredsheet in @file
sub make_csv {
    open (FILE, $notcsv) || die("FAILED: to open $notcsv");
    while ( <FILE> ) {
	next if ( /%META:/ ) ;
	$row = $_;
	$row =~ s/^\| *\**//;       # Remove leading "| *" and "| "
	$row =~ s/\** *\|$//;       # Remove tailing "* |" and " |"
	$row =~ s/\** *\| *\**/;/g; # Replace "* | *" and " | " with ";"
	push(@file,$row);
    }
    close FILE;

}

# Call this for debug
sub print_HoH {
    foreach my $hw ( keys %HoH ) {
	print "hw=$hw\n";
        my $role;
        foreach $role ( keys %{ $HoH{$hw} } ) {
            print "$role=$HoH{$hw}{$role}\n";
        }
    }
}
