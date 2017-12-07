%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	ComsaEcimModelAdaptor.hrl %
%%% Author:	etxjotj
%%% Description:     
%%%
%%% ----------------------------------------------------------
-hrl_id('11/190 55-CNA 113 348 Ux').
-hrl_vsn('/main/R2A/R3A/1').
-hrl_date('2015-03-11').
-hrl_author('erarafo').
%%% %CCaseTemplateFile:	module.hrl %
%%% %CCaseTemplateId: 54/002 01-LXA 119 334 Ux, Rev: /main/6 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2015 All rights reserved.
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
%%% R2A/1      2013-12-19 etxjotj     Created
%%% R2A/3      2014-01-24 erarafo     Added type specs and comments
%%% R2A/4      2014-01-27 erarafo     Added record comsaEcimModelByRootRdn
%%% R2A/5      2014-01-29 erarafo     comsaEcimModelByRootRdn, record format
%%% R2A/6      2014-02-13 erarafo     Added record parsedItems
%%% R3A/1      2015-03-11 erarafo     Moved some type definitions here
%%% ----------------------------------------------------------

-include_lib("xmerl/include/xmerl.hrl").


-type access()::readWrite|readOnly.
-type basicType()::string|moRef|uint8|uint16|uint32|uint64|int8|int16|int32|
		   int64|boolean|enum|atom().
-type type()::basicType()|{sequence, basicType()}|{struct,atom()}|
	      {sequence,{struct,atom()}}.
-type notificationType()::notification|noNotification.
-type creationType()::systemCreated|userCreated|undefined.
-type classType()::{Attribute::atom(), Type::type(), Access::access(), NotificationType::notificationType()}.


%%% #2.    CODE
%%% #---------------------------------------------------------
%%% #2.1   DEFINITION OF CONSTANTS
%%% #---------------------------------------------------------
%%% ----------------------------------------------------------
%%% #           CONSTANT_NAME
%%% Description: 
%%% ----------------------------------------------------------
%-define(CONSTANT_NAME,               Value).

%%% #---------------------------------------------------------
%%% #2.2   DEFINITION OF RECORDS
%%% #---------------------------------------------------------
%%% ----------------------------------------------------------
%%% #           comsaEcimClassTypes
%%% Description: Type data for classes and their attributes.
%%% The 'class' field is atomic; the class name is prefixed by the
%%% model name (the MIM name).
%%%
%%% Each attribute is described by a 4-tuple of name, type,
%%% access mode and notification property. If the type is
%%% specified as {struct, Type} then it refers to a type
%%% in the comsaEcimTypes table.
%%% ----------------------------------------------------------
-record(comsaEcimClassTypes, {class           :: atom(),                      % 'ModelName.ClassName'
			      attributeTypes  :: [{atom(),                    % attribute name
						   atom() | {struct, atom()}, % type
						   readWrite|readOnly, 
						   notification|noNotification
						  }],
			      creationType    :: systemCreated | userCreated
			     }).

%%% ----------------------------------------------------------
%%% #           comsaEcimImplementationModel
%%% Description: The relation between ECIM and implementation (MIM)
%%% model names. The ECIM MOM name is encoded as
%%%   <domainExtension>
%%%     <extension name="ecimMomName" value="TESTMOMNAME"/>
%%%     ...
%%%   </domainExtension>
%%% which is a subelement of the <mim> element. Optionally there may also
%%% be a subelement
%%%   <implements name="TESTMOMNAME" release="..." version="..."/>
%%% at the same level.
%%%
%%% The MIM name is given by the 'name' attribute of the MIM element.
%%% ----------------------------------------------------------
-record(comsaEcimImplementationModel, {ecimName       :: string(),
				       mimName        :: string()
				      }).

%%% ----------------------------------------------------------
%%% #           comsaEcimRelations
%%% Description: The parent-child relations between classes. If the
%%% parent is given as a tuple the components represent MIM name
%%% and classname. The same goes for the child class.
%%% ----------------------------------------------------------
-record(comsaEcimRelations,
	{parent  :: '_' | {string() | '_' | root, string() | '_'},
	 child   :: '_' | {string() | '_', string() | '_'}
	}).


%%% ----------------------------------------------------------
%%% #           comsaEcimTypes
%%% Description: Relation between com base types and other derived types
%%% or enums.
%%%
%%% The 'type' is the name of a derived data type, formed by
%%% concatenating the ECIM MOM name, a period and the name specified in
%%% a <derivedDataType name="..." ...> element.
%%%
%%% The basicType is an atom such as string, int32 etc, or a
%%% "struct" type described by a list of {structElementName, type}
%%% tuples, where the 'type' is a basic type or a derived type.
%%%
%%% Example of a table entry defining a struct:
%%% ```
%%% {comsaEcimTypes,'RcsBrM.ProductData',
%%%                 [{productName,string},
%%%                  {productNumber,string},
%%%                  {productRevision,string},
%%%                  {productionDate,'RcsBrM.DateTime'},
%%%                  {description,string},
%%%                  {type,string}
%%%                 ]}
%%% '''
%%% Basic types are parsed from the MIM file; the set of valid types
%%% is likely to be defined by mp.dtd.
%%% ----------------------------------------------------------

-record(comsaEcimTypes, {type          :: atom(),
			 basicType     :: atom() | [{atom(), atom()}]
			}).

%%% ----------------------------------------------------------
%%% #           parsedItems
%%% Description: Intermediate results in parsing, valid in the
%%% scope of a parse_models/1 invocation.
%%% ----------------------------------------------------------

-record(parsedItems, {model       :: string(),
		      incDirs     :: [string()],
		      topElement  :: #xmlElement{},
		      mimElement  :: #xmlElement{},
		      mimName     :: string()
		     }). 


%%% ----------------------------------------------------------
%%% #           comsaEcimModelByRootRdn
%%% Description: Maps RDN name of the root class, in IMM format,
%%% to MIM info, consisting of model name (MIM name) and a flag.
%%% The flag is true if MIM name prefixing for IMM classes is
%%% in effect.
%%% ----------------------------------------------------------

-record(comsaEcimModelByRootRdn, {rootRdnName   :: string(),
			          mimInfo       :: {string(), boolean()}
			         }).
