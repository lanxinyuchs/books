


%% must match definitions in dnti.h
-define(DNTI_OK,                           0).
-define(DNTI_INVALID_PARAMETER_DIRECTION,  1).
-define(DNTI_INVALID_PARAMETER_TODNP,      2).
-define(DNTI_SEND_ERROR,                   3).
-define(DNTI_RECEIVE_ERROR,                4).
-define(DNTI_OBJECT_CLASS_NOT_FOUND,       5).

%% TODO; rename to match .h file
-define(MIM_TO_IMM, 1).
-define(IMM_TO_MIM, 2).



%% must match definition in ift_app: master.h
-define(DNTI, 5).


%% must match definitions in ift_app: test_dnti.c
-define(DNTI_TRANSFORM,                      1).
-define(DNTI_PASS_INVALID_TODNP_A,           2).
-define(DNTI_CODE_EXAMPLE,                   3).
-define(DNTI_INITIATE_SERVICE,               4).
-define(DNTI_TERMINATE_SERVICE,              5).
-define(DNTI_CLEANUP,                        6).
-define(DNTI_TERMINATE_SERVICE_KEEP_HANDLE,  7).
-define(DNTI_PASS_INVALID_TODNP_B,           8).


-define(ONESHOT, false).
-define(LONGLIVING, true).
-define(IGNORED_TRANSPORT, false).


-define(IGNORED_FUNCTION, 0).
-define(IGNORED_DIRECTION, 1).
-define(IGNORED_DN, "").


-define(REPEAT, 30).

-define(CLIENT_0, 0).
-define(CLIENT_1, 1).
-define(CLIENT_2, 2).


-define(DNTI_INTERACTIVE_HTTP_PORT, 8888).
