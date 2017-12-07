%% must match definition in master.h of ift_app
-define(LMHI, 9).

%% must match definitions in test_lmhi.c
-define(Lmhi_get_lms, 1).
-define(Lmhi_free_buffers, 2).
-define(Lmhi_start_pgm, 3).
-define(Lmhi_stop_pgm, 4).

%% must match values in appm_lmhi.h
-define(LMHI_OK, 0).
-define(LMHI_ERR_UNPACK_MSG, 1).
-define(LMHI_ERR_PEER_ERROR, 2).
-define(LMHI_ERR_NO_MEMORY, 3).
-define(LMHI_ERR_ZERO_LENGTH, 4).
