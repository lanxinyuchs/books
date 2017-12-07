#!/usr/bin/perl
## ----------------------------------------------------------
## Copyright (c) Ericsson AB 2013 All rights reserved.
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
## R2A/1      2012-11-01   etxkols     Created
## R2A/3      2012-11-02   etxkols     Added new arg to get different lables
## R2A/4      2012-11-29   etxkols     Fix for etxahar who wants latest stable prep4testInfo.txt
## R2A/5      2012-12-11   etxkols     Preparation for delivering only new stable labels to node CI
## R2A/6      2013-02-21   etxkols     Adaption to new Jenkins logic
## R2A/7      2013-02-27   etxkols     New Build label layout required new regexp
## R2A/8      2013-03-05   etxkols     Fixed regexp problem for sim
## R2A/9      2014-08-20   etxkols     Branch updates
## ----------------------------------------------------------
## Description:
## Perl module called from Jenkins stable_label job.
## This module scans and updates /proj/webdocs/rbs-rde-ci/root/ciresults/*
## with files marking the build stable or not stable
## Examples:
##./stable_label.pl /proj/webdocs/rbs-rde-ci/root/ciresults_branch/R2A test_activity_short_sim.state test_activity_long_sim.state test_activity_long.state
## ----------------------------------------------------------
use strict;

my $ciresults   = shift(@ARGV);
######### TO BE REMOVED WHEN JENKINS stable_label JOB IS UPDATED FOR BRANCHES ##########
if ( $ciresults eq "SimLabel" ) {
    return 0
} elsif ( $ciresults eq "TestCs" ) {
    $ciresults = "/proj/webdocs/rbs-rde-ci/root/ciresults";
    shift(@ARGV);
}
######### END REMOVAL #########
my @jobs     = @ARGV;

my @lsv_nos = get_lsv_numbers($ciresults);
my ($is_new_label, $get_stable_lsv_no) = get_stable_lsv_no();
my $stable_sim_label = get_stable_label("$ciresults/$get_stable_lsv_no/SimLabel");
write_file("$ciresults/sim_stable_label");
my $stable_lsv_label = get_stable_label("$ciresults/$get_stable_lsv_no/TestCs");
print "${is_new_label}:${get_stable_lsv_no}:${stable_lsv_label}";

# Returns LSV number or 0.
# Scans all LSV numbers (highest number first).
# If a LSV is already marked stable, the LSV number is returned,
# else all jobs are scanned for string color=red.
# If color=red is NOT found, LSV is marked stable and the LSV number is returned
sub get_stable_lsv_no {
    my $tmp;
    foreach my $lsv_no ( @lsv_nos ) {
	if (-e "$ciresults/$lsv_no/lsv_status") { # if lsv_no is already marked as stable, return lsv_no
	    return ("no",$lsv_no);                   
	} else {
	    my $stable = "true";
	    foreach my $job ( @jobs ) {
		if (-e "$ciresults/$lsv_no/$job") {
		    $tmp = read_file("$ciresults/$lsv_no/$job");
		    if ($tmp =~ m/color=red/ || $tmp =~ m/color=blue/ || $tmp =~ m/color=black/) {
			$stable = 0;
			last;
		    }
		} else {
		    $stable = 0;
		    last;
		}
	    }
	    if ($stable) {
		open(FILE, ">$ciresults/$lsv_no/lsv_status") || die("Failed to open $ciresults/$lsv_no/lsv_status");
		print FILE "stable";
		close(FILE);
		return ("yes",$lsv_no);
	    }
	}
    }
    return ("no",0);
}

# Masks out release label
sub get_stable_label {
    my ($file) = @_;
    my $tmp = read_file("$file");
    $tmp =~ s/> *(CXS[0-9]+_*[0-9]*-R[0-9]+.*[0-9]+)<//;  
    return $1;
}

# Reads a file content as scalar
sub read_file {
    my ($file) = @_;
    open(FILE, "$file") || die("Failed to open $file");
    my $tmp = do { local $/; <FILE> };
    close(FILE);
    return $tmp;
}

sub write_file {
    my ($file) = @_;
    open (FILE, ">$file") || die("Failed to open $file");
    print FILE $stable_sim_label;
    close (FILE);
}

# Returns a sorted array with all lsv numbers, latest first
sub get_lsv_numbers {
    my ($filepath) = @_;
    opendir(DIR, $filepath) || die("Cannot open directory $filepath");
    my @directories = grep /^[0-9]+$/, readdir(DIR);
    sub numerically { $a <=> $b; }
    @directories = sort numerically @directories;
    @directories = reverse(@directories);
    return @directories;
}
