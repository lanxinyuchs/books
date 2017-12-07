#!/usr/bin/perl
## ----------------------------------------------------------
## Copyright (c) Ericsson AB 2017 All rights reserved.
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
## R1A/1      2012-02-23   etxkols     Created
## R1A/2      2012-02-25   etxkols     Added write to stp.hws
## R1A/3      2012-02-27   etxkols     Changed perl module to getHwData
## R1A/4      2012-02-28   etxkols     Removed console usr/pwd from spreadsheet
## R1A/5      2012-03-22   etxkols     Removed power_unit since obsolete with new PDU
## R1A/6      2012-03-22   etxkols     Changes regarding new PDU
## R1A/7      2012-03-22   etxkols     Changes regarding new PDU
## R1A/8      2012-06-05   etxkols     Added snmp_reset_console and snmp_app
## R1A/9      2012-10-03   etxkols     Added board_type
## R2A/2      2012-12-07   etxkols     RU does not have power_cons_port
## R2A/3      2013-04-30   etxkols     Fix when tty not set
## R2A/4      2014-02-19   etxkols     Added entrys for autointegration
## R3A/1      2014-10-02   etxkols     Added entrys for product_no and serial_no
## R3A/2      2014-11-27   etxkols     Added secure_board
## R3A/3      2015-02-27   etxkols     Prepare for 2 labs
## R3A/4      2015-02-27   etxkols     Added NTP server
## R3A/5      2015-04-27   etxkols     Changed NTP server to 10.68.200.5
## R3A/7      2015-05-06   etxkols     New server addresses in Ki10
## R3A/8      2015-05-08   etxkols     HTTP server addresses in Ki10
## R3A/9      2015-05-08   etxkols     NTP server addresses in Ki10
## R3A/10     2015-05-18   etxkols     Changed username for syslog server
## R3A/12     2015-05-28   etxkols     Moving to Ki10
## R3A/13     2015-05-28   etxkols     moving back to .150
## R3A/14     2015-06-10   etxkols     removed sftp_server from individaul board
## R4A/1      2015-08-26   etxkols     Added TN_A IP address
## R4A/2      2015-09-15   etxkols     Added vlan to TN_A IP 
## R4A/3      2015-11-04   etxkols     Added netmask and gateway to TN_A IP 
## R4A/4      2016-01-20   etxkols     Added ipv6
## R4A/5      2016-01-22   etxkols     Added ipv6 NTP
## R4A/6      2016-02-11   etxkols     [] around sftp_server ipv6 address
## R4A/7      2016-02-26   etxkols     Cleaned up and added netem for dus3006
## R4A/8      2016-02-29   etxkols     Fixed brackets
## R4A/8      2016-02-29   etxkols     Fixed brackets
## R4A/9      2016-02-29   etxkols     5G
## R4A/10     2016-03-11   etxkols     more 5G
## R4A/11     2016-03_21   etxkols     New IP net in lab and cloud
## R4A/12     2016-04_07   etxkols     New IP net has new vlans
## R4A/14     2016-04_08   etxkols     Bugs
## R4A/15     2016-03-22   etxkols     Change ccs to rcf
## R4A/16     2016-05-25   etxkols     VRCS + BRCS
## R4A/17     2016-05-25   etxkols     Quick fix for Lava
## R4A/18     2016-07-05   etxkols     Handle _ in rcf name
## R4A/19     2016-07-05   etxkols     Handle rcs cloud
## R4A/20     2016-12-07   etxkols     Ki20
## R4A/21     2016-12-07   etxkols     stupido
## R9A/1      2017-02-09   etxkols     Rewritten for 5g
## R9A/2      2017-02-09   etxkols     Added os_auth_url and no_proxy to cloud_info
## R9A/3      2017-02-09   etxkols     Added image_name to cloud_info
## R9A/4      2017-02-13   etxkols     Added parameters
## R9A/5      2017-02-13   etxkols     Typo
## R9A/6      2017-02-23   etxkols     Fixed Typo
## R9A/7      2017-02-27   etxkols     Small fix
## R9A/7      2017-02-27   etxkols     Small fix
## R9A/8      2017-08-30   etxkols     Preparation for Youlab
## R9A/9      2017-09-01   etxkols     Bug in sub which_lab
## R9A/10     2017-10-17   etxivri     Add new subnet for EE
## R9A/11     2017-10-24   etxkols     Youlab preparations
## R9A/12     2017-10-27   etxkols     Added ftpes_server
## R9A/13     2017-10-27   etxkols     NTP server in youlab
## R9A/14     2017-11-02   etxkols     Handling youlab when concatenating several vrcs instances
## R9A/15     2017-11-20   etxkols     ipv6 for youlab sftp, ldap and ntp
## R9A/16     2017-11-20   etxkols     ftpes server for youlab
## R9A/17     2017-12-05   etxkols     ::1 as ipv6 default route in youlab
## ----------------------------------------------------------
## Calls /proj/webdocs/rbs-rde/cgi-bin/getHwData::get_booking_data() which returns
## column_headers: array of all column headers
## sorted_hosts:   array of hw sorted in same order as spreadsheet file
## HoH:            hash of hashes hw,columnheader -> cell
## Writes files stp.hws: contains hw names for stp
##              stp.cfg: contains common test config data for hws in stp
## ./rct_stp_config.pl /proj/rcs-tmp/stps dus5103 dus5103
## ./rct_stp_config.pl /proj/rcs-tmp/stps rcf001 single_rcf SSH_LMT_IPV4 10.68.102.33 0 25 10.68.102.1 PRAN_FRONTHAUL_IPV4 10.68.102.180 0 25 10.68.102.129
## ./rct_stp_config.pl /proj/rcs-tmp/stps dus5103_rcf001 dus5103 rcf001
use strict;
use lib '/proj/webdocs/rbs-rde/cgi-bin';
use getHwData;
use Switch;

(my $stpdir, my $stp, my @hws) = @ARGV;
my $path="${stpdir}/${stp}/config";
my ($column_headers,$sorted_hosts,$HoH ) = get_booking_data();
my %HoH = %$HoH;
my $i = 1;
my $test_hws;
my $which_lab = "house10";

my $single_rcf = 0;
my @rcf_args;
if ($hws[0] =~ /single_rcf/) {
    $single_rcf = 1;
    shift(@hws);
    @rcf_args = @hws;
    @hws = ($stp);
}

open (HWS, ">$path/stp.hws") || die("ERROR: failed to open $path/stp.hws");
foreach my $hw (@hws) {
    print HWS "$hw\n";                     # Writes hws for stp to stp.hws
    $test_hws = "$test_hws, {$i, '$hw'}";  
    $i++;
}
close HWS;

sub scan_for_secure_board {
    my $product_no = shift(@_);
    $product_no =~ s/\s+//g;
    my @secure_indexes = ("11", "31", "41");
    foreach my $index (@secure_indexes) {
	if ($product_no =~ /\/${index}[A-Z]/) {
	    return "yes";
	}
    }
    return "no"
}

sub which_lab {
    my $lab_info = shift(@_);
    if ($lab_info =~ /10\.86\.148\./)              { return "house30";
    } elsif ($lab_info =~ /10\.67\.224\./)         { return "house10"; # EE	
    } elsif ($lab_info =~ /10\.67\.17\./)          { return "house10"; # EE2       
    } elsif ($lab_info =~ /10\.67\.225\./)         { return "house10"; # MW1
    } elsif ($lab_info =~ /10\.67\.234\./)         { return "house10"; # MW2	
    } elsif ($lab_info =~ /redhat007-rcs-ci/)      { return "house10"; 	
    } elsif ($lab_info =~ /redhat007-rcs/)         { return "house10";
    } elsif ($lab_info =~ /10\.67\.140\./)         { return "youlab";  # EE	
    } elsif ($lab_info =~ /10\.67\.141\./)         { return "youlab";  # MW1	
    } elsif ($lab_info =~ /10\.67\.142\./)         { return "youlab";  # MW2	
    } elsif ($lab_info =~ /redhat017_XFT/)         { return "youlab";
    } elsif ($lab_info =~ /redhat017_XFT-vnfm/)    { return "youlab";
    } elsif ($lab_info =~ /redhat017_rcs-ci/)      { return "youlab";
    } elsif ($lab_info =~ /redhat017_sim-ci/)      { return "youlab";
    } elsif ($lab_info =~ /redhat017_rcs-ci-vnfm/) { return "youlab";
    } elsif ($lab_info =~ /redhat017_ee-ci/)       { return "youlab";
    } else {
	return "youlab";
    }
}

sub make_tn_a_ip {
    my $lmt_ipv4 = shift(@_);
#    tn_a_ipv4, tn_a_ipv4_vlan, tn_a_ipv4_netmask, tn_a_ipv4_gateway, tn_a_ipv4_alt, tn_a_ipv4_alt_vlan, tn_a_ipv4_alt_netmask, tn_a_ipv4_alt_gateway,
#    tn_a_ipv6, tn_a_ipv6_vlan, tn_a_ipv6_netmask, tn_a_ipv6_gateway, tn_a_ipv6_alt, tn_a_ipv6_alt_vlan, tn_a_ipv6_alt_netmask, tn_a_ipv6_alt_gateway,
#    tn_a_ipv4_ipsec_inner, tn_a_ipv4_ipsec_outer, tn_a_ipv4_ipsec_outer_netmask, tn_a_ipv4_ipsec_gateway
    if ($lmt_ipv4 =~ /10\.67\.224\.([0-9]+)/) { # EE
	("","0","","","","0","","",
	 "","0","","","","0","","",
	 "","","","");
    } elsif ($lmt_ipv4 =~ /10\.67\.17\.([0-9]+)/) { # EE2
	("","0","","","","0","","",
	 "","0","","","","0","","",
	 "","","","");
    } elsif ($lmt_ipv4 =~ /10\.67\.225\.([0-9]+)/) { # MW1
	("10.67.226.$1","2402","255.255.255.0","10.67.226.1","10.67.233.$1","2415","255.255.255.0","10.67.233.1",
	 "2001:1b70:6281:f100::$1","2402","64","2001:1b70:6281:f100::2","2001:1b70:6281:f480::$1","2415","64","2001:1b70:6281:f480::2",
	 "10.67.238.$1","192.168.226.$1","24","192.168.226.1");
    } elsif ($lmt_ipv4 =~ /10\.67\.234\.([0-9]+)/) { # MW2
	("10.67.235.$1","2417","255.255.255.0","10.67.235.1","10.67.236.$1","2418","255.255.255.0","10.67.236.1",
	 "2001:1b70:6281:f580::$1","2417","64","2001:1b70:6281:f580::2","2001:1b70:8292:600::$1","2418","64","2001:1b70:8292:600::2",
	 "","","","");
    } elsif ($lmt_ipv4 =~ /10\.67\.140\.([0-9]+)/) { # EE
	("","0","","","","0","","",
	 "","0","","","","0","","",
	 "","","","");
    } elsif ($lmt_ipv4 =~ /10\.67\.141\.([0-9]+)/) { # MW1
	("10.67.162.$1","176","255.255.255.0","10.67.162.1","10.67.170.$1","190","255.255.255.0","10.67.170.1",
	 "2001:1b70:8292:D100::$1","176","64","2001:1b70:8292:D100::1","2001:1b70:8292:D500::$1","190","64","2001:1b70:8292:D500::1",
	 "10.67.172.$1","192.168.226.$1","24","192.168.226.1");
    } elsif ($lmt_ipv4 =~ /10\.67\.142\.([0-9]+)/) { # MW2
	("10.67.169.$1","189","255.255.255.0","10.67.169.1","10.67.171.$1","191","255.255.255.0","10.67.171.1",
	 "2001:1b70:8292:D480::$1","189","64","2001:1b70:8292:D480::1","2001:1b70:8292:D580::$1","191","64","2001:1b70:8292:D580::1",
	 "","","","");
    } else {
	("","0","","","","0","","",
	 "","0","","","","0","","",
	 "","","","");
    }
}

sub one_hw_config {
    $which_lab = which_lab($HoH{$hws[0]}{ssh_lmt_ipv4});
    my $hw = $hws[0];
    my $power_cons_port;
    if (! ($power_cons_port = $HoH{$hw}{power_cons_port})) {
	$power_cons_port = 0
    }	
    my $cons_port;
    if (! ($cons_port = $HoH{$hw}{cons_port})) {
	$cons_port = 0
    }	
    my $secure_board = scan_for_secure_board($HoH{$hw}{product_no});
    my $tn_a_ipv4_vlan = 0;
    my $tn_a_ipv4_alt_vlan = 0;
    my $tn_a_ipv6_vlan = 0;
    my $tn_a_ipv6_alt_vlan = 0;
    (my $tn_a_ipv4, my $tn_a_ipv4_vlan, my $tn_a_ipv4_netmask, my $tn_a_ipv4_gateway, my $tn_a_ipv4_alt, my $tn_a_ipv4_alt_vlan, my $tn_a_ipv4_alt_netmask, my $tn_a_ipv4_alt_gateway,
     my $tn_a_ipv6, my $tn_a_ipv6_vlan, my $tn_a_ipv6_netmask, my $tn_a_ipv6_gateway, my $tn_a_ipv6_alt, my $tn_a_ipv6_alt_vlan, my $tn_a_ipv6_alt_netmask, my $tn_a_ipv6_alt_gateway,
     my $tn_a_ipv4_ipsec_inner, my $tn_a_ipv4_ipsec_outer, my $tn_a_ipv4_ipsec_outer_netmask, my $tn_a_ipv4_ipsec_gateway) = make_tn_a_ip($HoH{$hw}{ssh_lmt_ipv4});
    print CONFIG "{'$hw',\n";
    print CONFIG "    [{board_type, \"$HoH{$hw}{type}\"},\n";
    print CONFIG "     {product_no, \"$HoH{$hw}{product_no}\"},\n";
    print CONFIG "     {secure_board, \"$secure_board\"},\n";
    print CONFIG "     {serial_no, \"$HoH{$hw}{serial_no}\"},\n";
    print CONFIG "     {rs232, [{telnet, \"$HoH{$hw}{console_ip}\"}, {port, $cons_port}, {username, \"root\"}, {password, \"root\"}]},\n";
    print CONFIG "     {console_server, [{telnet, \"$HoH{$hw}{console_ip}\"}, {port, 23}, {username, \"root\"}, {password, \"dbps\"}, {tty, \"$HoH{$hw}{cons_tty}\"}]},\n";
    print CONFIG "     {power, [{power_cons_ip, \"$HoH{$hw}{power_cons_ip}\"}, {power_cons_port, $power_cons_port}, {power_port, \"$HoH{$hw}{power_port}\"}]},\n";
    print CONFIG "     {ssh_lmt_ipv4, [{ssh, \"$HoH{$hw}{ssh_lmt_ipv4}\"}, {port, 22}, {user, \"root\"}, {password, \"root\"}]},\n";
    print CONFIG "     {ssh_lmt_ipv6, [{ssh, \"$HoH{$hw}{ssh_lmt_ipv6}\"}, {port, 22}, {user, \"root\"}, {password, \"root\"}]},\n";
    print CONFIG "     {ssh_TN_A_ipv4, [{ssh, \"$tn_a_ipv4\"}, {vlan, $tn_a_ipv4_vlan}, {netmask, \"$tn_a_ipv4_netmask\"}, {gateway, \"$tn_a_ipv4_gateway\"}]},\n";
    print CONFIG "     {ssh_TN_A_ipv4_alt, [{ssh, \"$tn_a_ipv4_alt\"}, {vlan, $tn_a_ipv4_alt_vlan}, {netmask, \"$tn_a_ipv4_alt_netmask\"}, {gateway, \"$tn_a_ipv4_alt_gateway\"}]},\n";
    print CONFIG "     {ssh_TN_A_ipv6, [{ssh, \"$tn_a_ipv6\"}, {vlan, $tn_a_ipv6_vlan}, {netmask, \"$tn_a_ipv6_netmask\"}, {gateway, \"$tn_a_ipv6_gateway\"}]},\n";
    print CONFIG "     {ssh_TN_A_ipv6_alt, [{ssh, \"$tn_a_ipv6_alt\"}, {vlan, $tn_a_ipv4_alt_vlan}, {netmask, \"$tn_a_ipv6_alt_netmask\"}, {gateway, \"$tn_a_ipv6_alt_gateway\"}]},\n";
    print CONFIG "     {ssh_TN_A_ipv4_ipsec, [{ssh, \"$tn_a_ipv4_ipsec_inner\"}, {ssh_outer, \"$tn_a_ipv4_ipsec_outer\"}, {netmask_outer, \"$tn_a_ipv4_ipsec_outer_netmask\"}, {vlan, 4001}, {gateway_outer, \"$tn_a_ipv4_ipsec_gateway\"}]},\n";
    print CONFIG "     {erl_dist_ip, \"$HoH{$hw}{ssh_lmt_ipv4}\"},\n";
    print CONFIG "     {sftp_ai_install_dir, \"boards/$hw\"},\n";
    if ($hw eq "dus3006") {
	print CONFIG "     {ssh_netem, [{ssh, \"10.68.101.149\"}, {port, 22}, {user, \"root\"}, {password, \"labuser\"}]},\n";
    }
    print CONFIG "     {tftpboot, \"/proj/rcs-tmp/tftpboot/$hw\"}]}.\n";
}

sub make_networks_for_rcf {
    my $tenant = "";
    my $image_name = "";
    my $vim_url = "";
    my $vim_tenant_name = "";
    my $vim_username = "";
    my $vim_password = "";
    my $vim_cacert = "";
    my $os_auth_url = "";
    my $no_proxy = "";
    my ($ssh_lmt_ipv4,         $ssh_lmt_ipv4_vlan,         $ssh_lmt_ipv4_netmask,         $ssh_lmt_ipv4_gateway)         = ("","","",""); 
    my ($ssh_lmt_ipv6,         $ssh_lmt_ipv6_vlan,         $ssh_lmt_ipv6_netmask,         $ssh_lmt_ipv6_gateway)         = ("","","","");
    my ($om_ran_ipv4,          $om_ran_ipv4_vlan,          $om_ran_ipv4_netmask,          $om_ran_ipv4_gateway)          = ("","","","");
    my ($om_ran_ipv6,          $om_ran_ipv6_vlan,          $om_ran_ipv6_netmask,          $om_ran_ipv6_gateway)          = ("","","","");
    my ($pran_fronthaul_ipv4,  $pran_fronthaul_ipv4_vlan,  $pran_fronthaul_ipv4_netmask,  $pran_fronthaul_ipv4_gateway)  = ("","","","");
    my ($pran_fronthaul_ipv6,  $pran_fronthaul_ipv6_vlan,  $pran_fronthaul_ipv6_netmask,  $pran_fronthaul_ipv6_gateway)  = ("","","","");
    my ($pran_backhaul_ipv4,   $pran_backhaul_ipv4_vlan,   $pran_backhaul_ipv4_netmask,   $pran_backhaul_ipv4_gateway)   = ("","","","");
    my ($pran_backhaul_ipv6,   $pran_backhaul_ipv6_vlan,   $pran_backhaul_ipv6_netmask,   $pran_backhaul_ipv6_gateway)   = ("","","","");
    while(@rcf_args) {
	my $key = shift @rcf_args;
	switch ($key) {
	    case "PROJECT"                   { $tenant                   = shift @rcf_args; }
	    case "IMAGE_NAME"                { $image_name               = shift @rcf_args; }
	    case "VIM_URL"                   { $vim_url                  = shift @rcf_args; }
	    case "VIM_TENANT_NAME"           { $vim_tenant_name          = shift @rcf_args; }
	    case "VIM_USERNAME"              { $vim_username             = shift @rcf_args; }
	    case "VIM_PASSWORD"              { $vim_password             = shift @rcf_args; }
	    case "VIM_CACERT"                { $vim_cacert               = shift @rcf_args; }
	    case "OS_AUTH_URL"               { $os_auth_url              = shift @rcf_args; }
	    case "NO_PROXY"                  { $no_proxy                 = shift @rcf_args; }
	    case "SSH_LMT_IPV4"              { $ssh_lmt_ipv4             = shift @rcf_args; }
	    case "SSH_LMT_IPV6"              { $ssh_lmt_ipv6             = shift @rcf_args; }
	    case "OM_RAN_IPV4"               { $om_ran_ipv4              = shift @rcf_args; }
	    case "OM_RAN_IPV6"               { $om_ran_ipv6              = shift @rcf_args; }
	    case "PRAN_FRONTHAUL_IPV4"       { $pran_fronthaul_ipv4      = shift @rcf_args; }
	    case "PRAN_FRONTHAUL_IPV6"       { $pran_fronthaul_ipv6      = shift @rcf_args; }
	    case "PRAN_BACKHAUL_IPV4"        { $pran_backhaul_ipv4       = shift @rcf_args; }
	    case "PRAN_BACKHAUL_IPV6"        { $pran_backhaul_ipv6       = shift @rcf_args; }
	    case "SSH_LMT_IPV4_INFO"         { $ssh_lmt_ipv4_vlan        = shift @rcf_args; $ssh_lmt_ipv4_netmask        = shift @rcf_args;  $ssh_lmt_ipv4_gateway        = shift @rcf_args; }
	    case "SSH_LMT_IPV6_INFO"         { $ssh_lmt_ipv6_vlan        = shift @rcf_args; $ssh_lmt_ipv6_netmask        = shift @rcf_args;  $ssh_lmt_ipv6_gateway        = shift @rcf_args; }
	    case "OM_RAN_IPV4_INFO"          { $om_ran_ipv4_vlan         = shift @rcf_args; $om_ran_ipv4_netmask         = shift @rcf_args;  $om_ran_ipv4_gateway         = shift @rcf_args; }
	    case "OM_RAN_IPV6_INFO"          { $om_ran_ipv6_vlan         = shift @rcf_args; $om_ran_ipv6_netmask         = shift @rcf_args;  $om_ran_ipv6_gateway         = shift @rcf_args; }
	    case "PRAN_FRONTHAUL_IPV4_INFO"  { $pran_fronthaul_ipv4_vlan = shift @rcf_args; $pran_fronthaul_ipv4_netmask = shift @rcf_args;  $pran_fronthaul_ipv4_gateway = shift @rcf_args; }
	    case "PRAN_FRONTHAUL_IPV6_INFO"  { $pran_fronthaul_ipv6_vlan = shift @rcf_args; $pran_fronthaul_ipv6_netmask = shift @rcf_args;  $pran_fronthaul_ipv6_gateway = shift @rcf_args; }
	    case "PRAN_BACKHAUL_IPV4_INFO"   { $pran_backhaul_ipv4_vlan  = shift @rcf_args; $pran_backhaul_ipv4_netmask  = shift @rcf_args;  $pran_backhaul_ipv4_gateway  = shift @rcf_args; }
	    case "PRAN_BACKHAUL_IPV6_INFO"   { $pran_backhaul_ipv6_vlan  = shift @rcf_args; $pran_backhaul_ipv6_netmask  = shift @rcf_args;  $pran_backhaul_ipv6_gateway  = shift @rcf_args; }
	}
    }
    ($tenant,   
     $image_name,
     $vim_url,
     $vim_tenant_name,
     $vim_username,
     $vim_password,
     $vim_cacert,
     $os_auth_url,
     $no_proxy,
     $ssh_lmt_ipv4,         $ssh_lmt_ipv4_vlan,         $ssh_lmt_ipv4_netmask,         $ssh_lmt_ipv4_gateway, 
     $ssh_lmt_ipv6,         $ssh_lmt_ipv6_vlan,         $ssh_lmt_ipv6_netmask,         $ssh_lmt_ipv6_gateway,
     $om_ran_ipv4,          $om_ran_ipv4_vlan,          $om_ran_ipv4_netmask,          $om_ran_ipv4_gateway,
     $om_ran_ipv6,          $om_ran_ipv6_vlan,          $om_ran_ipv6_netmask,          $om_ran_ipv6_gateway,
     $pran_fronthaul_ipv4,  $pran_fronthaul_ipv4_vlan,  $pran_fronthaul_ipv4_netmask,  $pran_fronthaul_ipv4_gateway,
     $pran_fronthaul_ipv6,  $pran_fronthaul_ipv6_vlan,  $pran_fronthaul_ipv6_netmask,  $pran_fronthaul_ipv6_gateway,
     $pran_backhaul_ipv4,   $pran_backhaul_ipv4_vlan,   $pran_backhaul_ipv4_netmask,   $pran_backhaul_ipv4_gateway,
     $pran_backhaul_ipv6,   $pran_backhaul_ipv6_vlan,   $pran_backhaul_ipv6_netmask,   $pran_backhaul_ipv6_gateway)
}


sub one_rcf_config {
    my ($tenant,   
	$image_name,
	$vim_url,
	$vim_tenant_name,
	$vim_username,
	$vim_password,
	$vim_cacert,
	$os_auth_url,
	$no_proxy,
	$ssh_lmt_ipv4,         $ssh_lmt_ipv4_vlan,         $ssh_lmt_ipv4_netmask,         $ssh_lmt_ipv4_gateway, 
	$ssh_lmt_ipv6,         $ssh_lmt_ipv6_vlan,         $ssh_lmt_ipv6_netmask,         $ssh_lmt_ipv6_gateway,
	$om_ran_ipv4,          $om_ran_ipv4_vlan,          $om_ran_ipv4_netmask,          $om_ran_ipv4_gateway,
	$om_ran_ipv6,          $om_ran_ipv6_vlan,          $om_ran_ipv6_netmask,          $om_ran_ipv6_gateway,
	$pran_fronthaul_ipv4,  $pran_fronthaul_ipv4_vlan,  $pran_fronthaul_ipv4_netmask,  $pran_fronthaul_ipv4_gateway,
	$pran_fronthaul_ipv6,  $pran_fronthaul_ipv6_vlan,  $pran_fronthaul_ipv6_netmask,  $pran_fronthaul_ipv6_gateway,
	$pran_backhaul_ipv4,   $pran_backhaul_ipv4_vlan,   $pran_backhaul_ipv4_netmask,   $pran_backhaul_ipv4_gateway,
	$pran_backhaul_ipv6,   $pran_backhaul_ipv6_vlan,   $pran_backhaul_ipv6_netmask,   $pran_backhaul_ipv6_gateway) = make_networks_for_rcf();
    $which_lab = which_lab($tenant);
    my $hw = $hws[0];
    print CONFIG "{'$hw',\n";
    print CONFIG "    [{board_type, \"rcf\"},\n";
    print CONFIG "     {product_no, \"\"},\n";
    print CONFIG "     {secure_board, \"no\"},\n";
    print CONFIG "     {serial_no, \"\"},\n";
    print CONFIG "     {rs232, [{telnet, \"\"}, {port, 0}, {username, \"root\"}, {password, \"root\"}]},\n";
    print CONFIG "     {console_server, [{telnet, \"\"}, {port, 23}, {username, \"root\"}, {password, \"dbps\"}, {tty, \"\"}]},\n";
    print CONFIG "     {power, [{power_cons_ip, \"\"}, {power_cons_port, 0}, {power_port, \"\"}]},\n";
    print CONFIG "     {cloud_info, [{tenant, \"$tenant\"}, {image_name, \"$image_name\"}, {vim_url, \"$vim_url\"}, {os_auth_url, \"$os_auth_url\"}, {vim_tenant_name, \"$vim_tenant_name\"}, {vim_username, \"$vim_username\"}, {vim_password, \"$vim_password\"}, {vim_cacert, \"$vim_cacert\"}, {no_proxy, \"$no_proxy\"}]},\n";
    print CONFIG "     {ssh_lmt_ipv4, [{ssh, \"$ssh_lmt_ipv4\"}, {port, 22}, {user, \"root\"}, {password, \"root\"}]},\n";
    print CONFIG "     {ssh_lmt_ipv6, [{ssh, \"$ssh_lmt_ipv6\"}, {port, 22}, {user, \"root\"}, {password, \"root\"}]},\n";
    print CONFIG "     {ssh_TN_A_ipv4, [{ssh, \"\"}, {vlan, 0}, {netmask, \"\"}, {gateway, \"\"}]},\n";
    print CONFIG "     {ssh_TN_A_ipv4_alt, [{ssh, \"\"}, {vlan, 0}, {netmask, \"\"}, {gateway, \"\"}]},\n";
    print CONFIG "     {ssh_TN_A_ipv6, [{ssh, \"\"}, {vlan, 0}, {netmask, \"\"}, {gateway, \"\"}]},\n";
    print CONFIG "     {ssh_TN_A_ipv6_alt, [{ssh, \"\"}, {vlan, 0}, {netmask, \"\"}, {gateway, \"\"}]},\n";
    print CONFIG "     {ssh_TN_A_ipv4_ipsec, [{ssh, \"\"}, {ssh_outer, \"\"}, {netmask_outer, \"\"}, {vlan, 4001}, {gateway_outer, \"\"}]},\n";
    print CONFIG "     {om_ran_ipv4, [{ssh, \"$om_ran_ipv4\"}, {vlan, \"$om_ran_ipv4_vlan\"}, {netmask, \"$om_ran_ipv4_netmask\"}, {gateway, \"$om_ran_ipv4_gateway\"}]},\n";
    print CONFIG "     {om_ran_ipv6, [{ssh, \"$om_ran_ipv6\"}, {vlan, \"$om_ran_ipv6_vlan\"}, {netmask, \"$om_ran_ipv6_netmask\"}, {gateway, \"$om_ran_ipv6_gateway\"}]},\n";
    print CONFIG "     {pran_fronthaul_ipv4, [{ssh, \"$pran_fronthaul_ipv4\"}, {vlan, \"$pran_fronthaul_ipv4_vlan\"}, {netmask, \"$pran_fronthaul_ipv4_netmask\"}, {gateway, \"$pran_fronthaul_ipv4_gateway\"}]},\n";
    print CONFIG "     {pran_fronthaul_ipv6, [{ssh, \"$pran_fronthaul_ipv6\"}, {vlan, \"$pran_fronthaul_ipv6_vlan\"}, {netmask, \"$pran_fronthaul_ipv6_netmask\"}, {gateway, \"$pran_fronthaul_ipv6_gateway\"}]},\n";
    print CONFIG "     {pran_backhaul_ipv4, [{ssh, \"$pran_backhaul_ipv4\"}, {vlan, \"$pran_backhaul_ipv4_vlan\"}, {netmask, \"$pran_backhaul_ipv4_netmask\"}, {gateway, \"$pran_backhaul_ipv4_gateway\"}]},\n";
    print CONFIG "     {pran_backhaul_ipv6, [{ssh, \"$pran_backhaul_ipv6\"}, {vlan, \"$pran_backhaul_ipv6_vlan\"}, {netmask, \"$pran_backhaul_ipv6_netmask\"}, {gateway, \"$pran_backhaul_ipv6_gateway\"}]},\n";
    print CONFIG "     {erl_dist_ip, \"$ssh_lmt_ipv4\"},\n";
    print CONFIG "     {sftp_ai_install_dir, \"boards/$hw\"},\n";
    if ($hw eq "dus3006") {
	print CONFIG "     {ssh_netem, [{ssh, \"10.68.101.149\"}, {port, 22}, {user, \"root\"}, {password, \"labuser\"}]},\n";
    }
    print CONFIG "     {tftpboot, \"/proj/rcs-tmp/tftpboot/$hw\"},\n";
    print CONFIG "     {end_of_cloud_instance, []}]}.\n";
}

sub common_config {
    switch ($which_lab) {
	case "house30" { 
	    print CONFIG "{sftp_server,[{host, \"10.68.200.11\"},{username, \"mauve\"},{password, \"dilbert\"}]}.\n";
	    print CONFIG "{cmpv2_server,[{host,\"10.68.200.14\"}]}.\n";
	    print CONFIG "{syslog_udp_server,[{host, \"10.68.200.11\"},{username, \"mauve\"},{password, \"dilbert\"}]}.\n";
	    print CONFIG "{syslog_tls_server,[{host, \"10.68.200.9\"},{username, \"sniffer\"},{password, \"rcsrcs12\"}]}.\n";
	    print CONFIG "{http_server,[{host, \"10.68.200.9\"}]}.\n";
	    print CONFIG "{ldap_server,[{host, \"10.68.200.11\"}]}.\n";
	    print CONFIG "{ntp_server,[{host, \"10.68.200.5\"}]}.\n";
	    print CONFIG "{image_webserver,[{url, \"https://rbs-rde.rnd.ki.sw.ericsson.se\"}]}.\n";
	}
	case "house10" {
	    print CONFIG "{sftp_server,[{host, \"10.68.101.150\"},{username, \"dustest\"},{password, \"dustest\"}]}.\n";
	    print CONFIG "{ftpes_server,[{host, \"10.68.101.131\"},{username, \"labuser\"},{password, \"labuser\"}]}.\n";
	    print CONFIG "{cmpv2_server,[{host,\"10.99.99.99\"}]}.\n";
	    print CONFIG "{syslog_udp_server,[{host, \"10.68.101.153\"},{username, \"syslogtest\"},{password, \"readlog\"}]}.\n";
	    print CONFIG "{syslog_tls_server,[{host, \"10.68.101.153\"},{username, \"syslogtest\"},{password, \"readlog\"}]}.\n";
	    print CONFIG "{http_server,[{host, \"10.68.101.153\"}]}.\n";
	    print CONFIG "{ldap_server,[{host, \"10.68.101.150\"}]}.\n";
	    print CONFIG "{ntp_server,[{host, \"10.68.101.170\"}]}.\n";
	    print CONFIG "{sftp_server_ipv6,[{host, \"[2001:1b70:6282:b280::150]\"},{username, \"dustest\"},{password, \"dustest\"}]}.\n";
	    print CONFIG "{ftpes_server_ipv6,[{host, \"2001:1b70:6282:b280::131\"},{username, \"labuser\"},{password, \"labuser\"}]}.\n";
	    print CONFIG "{ldap_server_ipv6,[{host, \"2001:1b70:6282:b280::150\"}]}.\n";
	    print CONFIG "{ntp_server_ipv6,[{host, \"2001:1b70:6282:b280::170\"}]}.\n";
	    print CONFIG "{image_webserver,[{url, \"https://rbs-rde.rnd.ki.sw.ericsson.se\"}]}.\n";
	}
	case "youlab" {
	    print CONFIG "{sftp_server,[{host, \"10.67.136.24\"},{username, \"dustest\"},{password, \"dustest\"}]}.\n";
	    print CONFIG "{ftpes_server,[{host, \"10.67.136.26\"},{username, \"labuser\"},{password, \"labuser\"}]}.\n";
	    print CONFIG "{cmpv2_server,[{host,\"10.99.99.99\"}]}.\n";
	    print CONFIG "{syslog_udp_server,[{host, \"10.68.101.153\"},{username, \"syslogtest\"},{password, \"readlog\"}]}.\n";
	    print CONFIG "{syslog_tls_server,[{host, \"10.68.101.153\"},{username, \"syslogtest\"},{password, \"readlog\"}]}.\n";
	    print CONFIG "{http_server,[{host, \"10.67.136.26\"}]}.\n";
	    print CONFIG "{ldap_server,[{host, \"10.67.136.24\"}]}.\n";
	    print CONFIG "{ntp_server,[{host, \"10.67.136.24\"}]}.\n";
	    print CONFIG "{ntp_server_2,[{host, \"10.67.136.25\"}]}.\n";
	    print CONFIG "{sftp_server_ipv6,[{host, \"[2001:1b70:8292:C400::24]\"},{username, \"dustest\"},{password, \"dustest\"}]}.\n";
	    print CONFIG "{ftpes_server_ipv6,[{host, \"2001:1b70:8292:C400::26\"},{username, \"labuser\"},{password, \"labuser\"}]}.\n";
	    print CONFIG "{ldap_server_ipv6,[{host, \"2001:1b70:8292:C400::24\"}]}.\n";
	    print CONFIG "{ntp_server_ipv6,[{host, \"2001:1b70:8292:C400::24\"}]}.\n";
	    print CONFIG "{ntp_server_2_ipv6,[{host, \"2001:1b70:8292:C400::25\"}]}.\n";
	    print CONFIG "{image_webserver,[{url, \"https://rbs-rde.rnd.ki.sw.ericsson.se\"}]}.\n";
	    print CONFIG "{which_lab, youlab}.\n";
	}
    }
}

sub several_node_config {
    my $rcfdata;
    while (@hws) {
	my $rcfdata = "";
	my $hw = $hws[0];
	if ($hw =~ /^rcf/) {
	    my $path="${stpdir}/${hw}/config/stp.cfg";
	    open (FILE, '<', $path) or die "Could not open $path: $!";
	    while (<FILE>) {
		my $wholefile = $_;
		my $data = $wholefile if (/{'*$hw/ .. /{end_of_cloud_instance/);
		$rcfdata="${rcfdata}$data";
		if ($wholefile =~ m/youlab/) { 
		    $which_lab = "youlab";
		}
	    }
#	    while (<FILE>) {
#		my $data = $_ if (/{'*$hw/ .. /{end_of_cloud_instance/);
#		$rcfdata="${rcfdata}$data";
#	    }
	    close (FILE) or die "Could not close $path: $!";
	    print CONFIG "$rcfdata";
	} else {
	    one_hw_config();
	}
	shift @hws;
    }
}

$test_hws = (substr $test_hws, 2);
open (CONFIG, ">$path/stp.cfg") || die("ERROR: failed to open $path/stp.cfg");
print CONFIG "{test_nodes, [$test_hws]}.\n";
my $noof_nodes = @hws;
if ($noof_nodes == 1) {
    if ($single_rcf) {
	one_rcf_config();
    } else {
	one_hw_config();
    }
} else {
    several_node_config()
}
common_config();
close CONFIG;
print "OK\n";
