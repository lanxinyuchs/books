%%%-----------------------------------------------------------------------------
%%% @author enilsca
%%% @copyright Ericsson AB 2012-2014
%%% @doc
%%%
%%% @end
%%% Created :  26 Nov 2014 by ENILSCA
%%%-----------------------------------------------------------------------------

{merge_tests, false}.
{cases, "../rct/suites/install", install_dus_SUITE, [all]}.
{cases, "../rct/suites/install", check_after_install_SUITE, [all]}.
{cases, "../rct/suites/XL", xl_reboot_SUITE, [all]}.
