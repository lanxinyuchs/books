%%%-----------------------------------------------------------------------------
%%% @author enilsca
%%% @copyright Ericsson AB 2015
%%% @doc
%%%
%%% @end
%%% Created :  02 Apr 2015 by ENILCHR
%%%-----------------------------------------------------------------------------

{merge_tests, false}.
{cases, "../rct/suites/install", install_dus_SUITE, [all]}.
{cases, "../rct/suites/install", check_after_install_SUITE, [all]}.
