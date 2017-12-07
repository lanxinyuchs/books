%% Revision History
%%
%% Date: 20150508
%% Name: uabhten, erasipe
%% Description: OTP-11851. 
%% From Erlang 18, it is no longer possible to refer to a record type with record(r); 
%% instead the usual record notation, #r{}, is to be used.
%%

%% COM CM Router Event types taken from MafOamSpiCmEvent_1.h

%% --------------------------------------------------------------------------
%% This type,indicates the source of the operation that led to the generation
%% of this notification. The values are aligned with 3GPP TS 32.662
%% It can have one of the following values:
%%
%% ResourceOperation:   The notification was generated in response to an
%%                      internal operation of the resource;
%%
%% ManagementOperation: The notification was generated in response to a
%%                      management operation applied across the managed
%%                      object boundary external to the managed object;
%%
%% SonOperation:        The notification was generated as result of a SON
%%                      (Self Organising Network, used in Radio networks)
%%                      process like self-configuration, self-optimization,
%%                      self-healing etc.
%%                          A system (MW) that has no support
%%                      for SON will not use this value
%%
%% Unknown:             It is not possible to determine the source
%%                      of the operation.
%% --------------------------------------------------------------------------
-define(ResourceOperation,   1).
-define(ManagementOperation, 2).
-define(SonOperation,        3).
-define(Unknown,             4).

-type cm_source_indicator() :: ?ResourceOperation
                             | ?ManagementOperation
                             | ?SonOperation
                             | ?Unknown.



%% --------------------------------------------------------------------------
%% This type, indicates an event type. It can have one of the following values:
%%
%% MoCreated:            An MO is created. All MOs created should report
%%                       create and delete notifications. There can be
%%                       zero or more named attributes in the event attribute
%%                       list.
%%
%% MoDeleted:            An MO is deleted. The event attribute list must
%%                       be empty.
%%
%% AttributeValueChange: One or more attributes have been updated
%%                       in an existing MO.
%%                       Note that attributes that are not marked as
%%                       notifiable in the MOM should not be reported.
%%
%% Overflow:             One or more notifications have been lost
%%                       due to a flooding situation. The event
%%                       attribute list must be empty.
%%
%% --------------------------------------------------------------------------
-define(MoCreated,            1).
-define(MoDeleted,            2).
-define(AttributeValueChange, 3).
-define(Overflow,             4).

-type cm_event_type() :: ?MoCreated
                       | ?MoDeleted
                       | ?AttributeValueChange
                       | ?Overflow.

%% Requires comte_types.hrl
-record(cm_event_1,
        {
          dn = <<"">>               :: comte_types:ecim_dn() | [binary()],
          event_type = ?MoDeleted   :: cm_event_type(),
          attributes = []           :: [comte_types:com_named_attribute()]
        }).


-record(cm_notification_1,
        {
          %% Default 0 -> not in scope of NBI transaction
          trans_id=0                    :: comte_types:transaction_id(),
          source=?ManagementOperation   :: cm_source_indicator(),
	  events=[#cm_event_1{}]        :: [#cm_event_1{}]
        }).


-type cm_notification() :: #cm_notification_1{}.

-type com_notification() :: cm_notification().

