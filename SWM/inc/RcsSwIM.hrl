%%% --------------------------------------------------------
%%% Copyright (c) Ericsson AB 2017 All rights reserved.
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
%%% --------------------------------------------------------

-hrl_id({"RcsSwIM","1.3.1","/main/R1A/R2A/R3A/R4A/R5A/R6A/1"}).


%% -------------- CLASS SwInventory -------------------------

%% Description:
%% A singleton class, the purpose of which is only structural.

-record(swInventory, {swInventoryId,
                      userLabel,
                      active}).

-define(swInventory_types,
        [{swInventoryId, string},
         {userLabel, string},
         {active, {sequence,moRef}}]).

-define(SwInventory_restricted, [swInventoryId]).


%% -------------- CLASS SwVersion -------------------------

%% Description:
%% An MO of this class represents the software version of a domain of the software.

-record(swVersion, {swVersionId,
                    userLabel,
                    administrativeData,
                    timeOfInstallation,
                    timeOfActivation,
                    timeOfDeactivation,
                    consistsOf}).

-define(swVersion_types,
        [{swVersionId, string},
         {userLabel, string},
         {administrativeData, {struct,'ProductData'}},
         {timeOfInstallation, 'RcsSwIM.DateTime'},
         {timeOfActivation, 'RcsSwIM.DateTime'},
         {timeOfDeactivation, 'RcsSwIM.DateTime'},
         {consistsOf, {sequence,moRef}}]).

-define(SwVersion_restricted, [swVersionId]).


%% -------------- CLASS SwItem -------------------------

%% Description:
%% A software item holds information about an executable software product.

-record(swItem, {swItemId,
                 userLabel,
                 administrativeData,
                 additionalInfo,
                 consistsOf}).

-define(swItem_types,
        [{swItemId, string},
         {userLabel, string},
         {administrativeData, {struct,'ProductData'}},
         {additionalInfo, string},
         {consistsOf, {sequence,moRef}}]).

-define(SwItem_restricted, [swItemId]).


%% ------------------ STRUCT ProductData ----------------------
-ifndef(_PRODUCT_DATA).
-define(_PRODUCT_DATA, 1).

-record('ProductData', {productName,
                        productNumber,
                        productRevision,
                        productionDate,
                        description,
                        type}).

-define('ProductData_types',
        [{productName, string},
         {productNumber, string},
         {productRevision, string},
         {productionDate, 'RcsSwIM.DateTime'},
         {description, string},
         {type, string}]).


-endif. % _PRODUCT_DATA

