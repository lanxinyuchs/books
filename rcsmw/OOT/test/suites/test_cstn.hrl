%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	test_cstn.hrl %
%%% @author ecaiyan
%%% @copyright Ericsson AB 2017
%%% @version /main/R10A/3
%%%
%%% @doc ==Header file for cstn_SUITE.==
%%%
%%% @end

%% must match definition in master.h of ift_app
-define(CSTN, 25).

%% must match definitions in test_cstn.c
-define(CsTnInitialize, 1).
-define(CsTnInitialize2, 2).
-define(CsTnDispatch, 3).
-define(CsTnUpdate, 4).
-define(SetOamDNtoNsNameRsp, 5).
-define(CsTnInitialize3, 6).

%% must match values in cstnn.h
-define(CSTN_OK,           0).
-define(CSTN_FAIL,      -1).
