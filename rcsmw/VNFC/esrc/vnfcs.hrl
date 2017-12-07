%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile: vnfcs.hrl %
%%% Author:     etxaldu
%%% Description:
%%%
%%% ----------------------------------------------------------
-hrl_id('').
-hrl_vsn('').
-hrl_date('').
-hrl_author('').
%%% %CCaseTemplateFile: module.hrl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2016 All rights reserved.
%%%
%%% The information in this document is the property of Ericsson.
%%%
%%% Except as specifically authorized in writing by Ericsson, the
%%% receiver of this document shall keep the information contained
%%% herein confidential and shall protect the same in whole or in
%%% part from disclosure and dissemination to third parties.
%%%
%%% Disclosure and disseminations to the receivers employees shall
%%% only be made on a strict need to know basis.
%%% %CCaseCopyrightEnd%
%%%
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      -------    --------    ------------------------
%%%            2016-10-13 etxaldu     Created
%%%            2017-10-20 emariad     Added vnfTestLicense record
%%%            2017-11-30 emariad     Backed out vnfTestLicense record
%%%            2017-12-01 etxaldu     Added vnfc record
%%% ----------------------------------------------------------
%%%
%%% #2.    CODE
%%% #---------------------------------------------------------
%%% #2.1   DEFINITION OF CONSTANTS
%%% #---------------------------------------------------------
%%% ----------------------------------------------------------
%%% #           CONSTANT_NAME
%%% Description:
%%% ----------------------------------------------------------
%%% #---------------------------------------------------------
%%% #2.2   DEFINITION OF RECORDS
%%% #---------------------------------------------------------
%%% ----------------------------------------------------------
%%% #           record_name
%%% Description:
%%% ----------------------------------------------------------
-record(vnfcs, {key,
                value}).

-record(vnfm_data, {vnfm_http_ip,
                    vnfm_http_port,
                    vnf_instance_id}).

-record(vnfc, {key,
               value}).
