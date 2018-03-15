#include "test.h"

struct test test_domain = {
	"test-domain.cfg",
	{
	    0, { 0, }, /* Initialize stage and timespec to 0 */
		{
			{ stage_type_domain_start, { "test_domain" }, 0 },
			{ stage_type_child_launch, { "1" },           0 },
			{ stage_type_child_launch, { "2" },           0 },
			{ stage_type_child_launch, { "3" },           0 },
			{ stage_type_child_exit,   { "1" },           1000, 0 },
			{ stage_type_child_launch, { "1" },           1000 },
			{ stage_type_child_exit,   { "2" },           2000, 0 },
			{ stage_type_child_launch, { "2" },           2000 },
			{ stage_type_child_exit,   { "1" },           2000, 0 },
			{ stage_type_child_launch, { "1" },           2000 },
			{ stage_type_child_exit,   { "1" },           3000, 0 },
			{ stage_type_child_launch, { "1" },           3000 },
			{ stage_type_child_exit,   { "2" },           4000, 0 },
			{ stage_type_child_launch, { "2" },           4000 },
			{ stage_type_child_exit,   { "1" },           4000, 0 },
			{ stage_type_domain_stop,  { "test_domain" }, 4000 },
			{ stage_type_child_exit,   { "2" },           4100, 15 },
			{ stage_type_domain_start, { "test_domain" }, 4100 },
			{ stage_type_child_launch, { "1" },           4100 },
			{ stage_type_child_launch, { "2" },           4100 },
			{ stage_type_child_exit,   { "1" },           5100, 0 },
			{ stage_type_child_launch, { "1" },           5100 },
			{ stage_type_child_exit,   { "2" },           6100, 0 },
			{ stage_type_child_launch, { "2" },           6100 },
			{ stage_type_child_exit,   { "1" },           6100, 0 },
			{ stage_type_child_launch, { "1" },           6100 },
			{ stage_type_child_exit,   { "1" },           7100, 0 },
			{ stage_type_child_launch, { "1" },           7100 },
			{ stage_type_child_exit,   { "2" },           8100, 0 },
			{ stage_type_child_launch, { "2" },           8100 },
			{ stage_type_child_exit,   { "1" },           8100, 0 },
			{ stage_type_domain_stop,  { "test_domain" }, 8100 },
			{ stage_type_child_exit,   { "2" },           8100, 15 },
			{ stage_type_domain_start, { "test_domain" }, 8100 },
			{ stage_type_child_launch, { "1" },           8100 },
			{ stage_type_child_launch, { "2" },           8100 },
			{ stage_type_child_exit,   { "1" },           9000, 0 },
			{ stage_type_child_launch, { "1" },           9000 },
			{ stage_type_child_exit,   { "3" },           10000, 0 },
			{ stage_type_child_launch, { "3" },           10000 },
			{ stage_type_child_exit,   { "2" },           10000, 0 },
			{ stage_type_child_launch, { "2" },           10000 },
			{ stage_type_child_exit,   { "1" },           10000, 0 },
			{ stage_type_child_launch, { "1" },           10000 },
			{ stage_type_child_exit,   { "1" },           11000, 0 },
			{ stage_type_child_launch, { "1" },           11000 },
			{ stage_type_child_exit,   { "2" },           12000, 0 },
			{ stage_type_child_launch, { "2" },           12000 },
			{ stage_type_child_exit,   { "1" },           12000, 0 },
			{ stage_type_reboot,       {},                12000 },
		}
	}
};

struct test test_alive_timeout = {
	"test-alive-timeout.cfg",
	{
	    0, { 0, }, /* Initialize stage and timespec to 0 */
		{
			{ stage_type_child_launch, { "1" },           0 },
			{ stage_type_child_exit,   { "1" },           1000, 6 },
			{ stage_type_child_launch, { "1" },           1000 },
			{ stage_type_child_exit,   { "1" },           2000, 6 },
			{ stage_type_child_launch, { "1" },           2000 },
			{ stage_type_child_exit,   { "1" },           3000, 6 },
			{ stage_type_child_launch, { "1" },           3000 },
			{ stage_type_child_exit,   { "1" },           4000, 6 },
			{ stage_type_reboot,       {},                4000 },
		}
	}
};

struct test test_quit = {
	"test-quit.cfg",
	{
	    0, { 0, }, /* Initialize stage and timespec to 0 */
		{
			{ stage_type_child_launch, { "1" },           0 },
			{ stage_type_child_launch, { "2" },           0 },
			{ stage_type_child_exit,   { "1" },           2000, 0 },
			{ stage_type_child_launch, { "1" },           2000 },
			{ stage_type_child_exit,   { "2" },           3000, 0 },
			{ stage_type_child_launch, { "2" },           3000 },
			{ stage_type_child_exit,   { "1" },           4000, 0 },
			{ stage_type_child_launch, { "1" },           4000 },
			{ stage_type_child_exit,   { "2" },           6000, 0 },
			{ stage_type_child_launch, { "2" },           6000 },
			{ stage_type_child_exit,   { "1" },           6000, 0 },
			{ stage_type_child_launch, { "1" },           6000 },
			{ stage_type_child_exit,   { "1" },           8100, 0 },
			{ stage_type_reboot,       {},                8100 },
		}
	}
};

struct test test_fail_boot_launch = {
	"test-fail-boot-launch.cfg",
	{
	    0, { 0, }, /* Initialize stage and timespec to 0 */
		{
			{ stage_type_child_launch, { "1" },           0 },
			{ stage_type_child_launch, { "2" },           0 },
			{ stage_type_child_exit,   { "1" },           0, 0 },
			{ stage_type_reboot,       {},                0 },
		}
	}
};

struct test test_shutdown_fail = {
	"test-shutdown-fail.cfg",
	{
	    0, { 0, }, /* Initialize stage and timespec to 0 */
		{
			{ stage_type_child_launch, { "1" },           0 },
			{ stage_type_child_launch, { "2" },           0 },
			{ stage_type_child_launch, { "3" },           0 },
			{ stage_type_child_exit  , { "1" },           1000, 0 },
			{ stage_type_exit,
			  { .reason = MAMA_EXIT_REASON_TIMEOUT }, 3000 },
		}
	}
};

struct test test_shutdown_success = {
	"test-shutdown-success.cfg",
	{
	    0, { 0, }, /* Initialize stage and timespec to 0 */
		{
			{ stage_type_child_launch, { "1" },           0 },
			{ stage_type_child_launch, { "2" },           0 },
			{ stage_type_child_launch, { "3" },           0 },
			{ stage_type_child_exit  , { "1" },           1000, 0 },
			{ stage_type_child_exit  , { "3" },           2000, 15 },
			{ stage_type_child_exit  , { "2" },           2000, 15 },
			{ stage_type_exit,
			  { .reason = MAMA_EXIT_REASON_ORDERED }, 2000 },
		}
	}
};

struct test test_slow_domain_aborted = {
	"test-slow-domain-aborted.cfg",
	{
	    0, { 0, }, /* Initialize stage and timespec to 0 */
		{
			{ stage_type_domain_start, { "test_domain" }, 0 },
			{ stage_type_child_launch, { "1" },           0 },
			{ stage_type_child_launch, { "2" },           0 },
			{ stage_type_child_exit,   { "1" },           2000, 0 },
			{ stage_type_domain_stop,  { "test_domain" }, 2000 },
			{ stage_type_child_exit,   { "2" },           7000, 6 },
			{ stage_type_domain_start, { "test_domain" }, 7000 },
			{ stage_type_child_launch, { "1" },           7000 },
			{ stage_type_child_launch, { "2" },           7000 },
			{ stage_type_child_exit,   { "1" },           9000, 0 },
			{ stage_type_domain_stop,  { "test_domain" }, 9000 },
			{ stage_type_child_exit,   { "2" },           14100, 6 },
			{ stage_type_domain_start, { "test_domain" }, 14100 },
			{ stage_type_child_launch, { "1" },           14100 },
			{ stage_type_child_launch, { "2" },           14100 },
			{ stage_type_child_exit,   { "1" },           16100, 0 },
			{ stage_type_reboot,       {},                16100 },
		}
	}
};

struct test test_slow_domain_killed = {
	"test-slow-domain-killed.cfg",
	{
	    0, { 0, }, /* Initialize stage and timespec to 0 */
		{
			{ stage_type_domain_start, { "test_domain" }, 0 },
			{ stage_type_child_launch, { "1" },           0 },
			{ stage_type_child_launch, { "2" },           0 },
			{ stage_type_child_exit,   { "2" },           2000, 0 },
			{ stage_type_domain_stop,  { "test_domain" }, 2000 },
			{ stage_type_child_exit,   { "1" },           11000, 9 },
			{ stage_type_domain_start, { "test_domain" }, 11000 },
			{ stage_type_child_launch, { "1" },           11000 },
			{ stage_type_child_launch, { "2" },           11000 },
			{ stage_type_child_exit,   { "2" },           13000, 0 },
			{ stage_type_domain_stop,  { "test_domain" }, 13000 },
			{ stage_type_child_exit,   { "1" },           22100, 9 },
			{ stage_type_domain_start, { "test_domain" }, 22100 },
			{ stage_type_child_launch, { "1" },           22100 },
			{ stage_type_child_launch, { "2" },           22100 },
			{ stage_type_child_exit,   { "2" },           24100, 0 },
			{ stage_type_reboot,       {},                24100 },
		}
	}
};

struct test test_sync_timeout = {
	"test-sync-timeout.cfg",
	{
	    0, { 0, }, /* Initialize stage and timespec to 0 */
		{
			{ stage_type_child_launch, { "1" },           0 },
			{ stage_type_child_exit,   { "1" },           1000, 6 },
			{ stage_type_child_launch, { "1" },           1000 },
			{ stage_type_child_exit,   { "1" },           2000, 6 },
			{ stage_type_child_launch, { "1" },           2000 },
			{ stage_type_child_exit,   { "1" },           3000, 6 },
			{ stage_type_child_launch, { "1" },           3000 },
			{ stage_type_child_exit,   { "1" },           4000, 6 },
			{ stage_type_reboot,       {},                4000 },
		}
	}
};

struct test test_completion = {
	"test-completion.cfg",
	{
	    0, { 0, }, /* Initialize stage and timespec to 0 */
		{
			{ stage_type_child_launch, { "sh" },          0 },
			{ stage_type_child_launch, { "1" },           0 },
			{ stage_type_child_launch, { "2" },           0 },
			{ stage_type_child_launch, { "3" },           1000 },
			{ stage_type_child_exit,   { "3" },           2000, 0 },
			{ stage_type_reboot,       {},                2000 },
		}
	}
};

struct test test_completion_code = {
	"test-completion-code.cfg",
	{
	    0, { 0, }, /* Initialize stage and timespec to 0 */
		{
			{ stage_type_child_launch, { "1" },           0 },
			{ stage_type_child_launch, { "2" },           0 },
			{ stage_type_child_exit,   { "2" },           1000, 256 },
			{ stage_type_child_launch, { "2" },           1000 },
			{ stage_type_child_exit,   { "2" },           2000, 256 },
			{ stage_type_child_launch, { "2" },           2000 },
			{ stage_type_child_exit,   { "2" },           3000, 256 },
			{ stage_type_child_launch, { "2" },           3000 },
			{ stage_type_child_exit,   { "2" },           4000, 256 },
			{ stage_type_reboot,       {},                4000 },
		}
	}
};

struct test test_completion_crash = {
	"test-completion-crash.cfg",
	{
	    0, { 0, }, /* Initialize stage and timespec to 0 */
		{
			{ stage_type_child_launch, { "1" },           0 },
			{ stage_type_child_launch, { "2" },           0 },
			{ stage_type_child_exit,   { "2" },           1000, 11 },
			{ stage_type_child_launch, { "2" },           1000 },
			{ stage_type_child_exit,   { "2" },           2000, 11 },
			{ stage_type_child_launch, { "2" },           2000 },
			{ stage_type_child_exit,   { "2" },           3000, 11 },
			{ stage_type_child_launch, { "2" },           3000 },
			{ stage_type_child_exit,   { "2" },           4000, 11 },
			{ stage_type_reboot,       {},                4000 },
		}
	}
};

struct test test_crash = {
	"test-crash.cfg",
	{
	    0, { 0, }, /* Initialize stage and timespec to 0 */
		{
			{ stage_type_child_launch, { "1" },           0 },
			{ stage_type_child_exit,   { "1" },           1000,  6 },
			{ stage_type_child_launch, { "1" },           1000, },
			{ stage_type_child_exit,   { "1" },           2000,  6 },
			{ stage_type_child_launch, { "1" },           2000 },
			{ stage_type_child_exit,   { "1" },           3000,  6 },
			{ stage_type_child_launch, { "1" },           3000 },
			{ stage_type_child_exit,   { "1" },           4000,  6 },
			{ stage_type_reboot,       {},                4000 },
		}
	}
};

struct test test_escalation_killed = {
	"test-escalation-killed.cfg",
	{
	    0, { 0, }, /* Initialize stage and timespec to 0 */
		{
			{ stage_type_child_launch, { "1" },           0, },
			{ stage_type_child_exit,   { "1" },           3000,  9 },
			{ stage_type_child_launch, { "1" },           3000 },
			{ stage_type_child_exit,   { "1" },           6000,  9 },
			{ stage_type_reboot,       {},                6000 },
		}
	}
};

struct test test_cmd = {
	"test-cmd.cfg",
	{
	    0, { 0, }, /* Initialize stage and timespec to 0 */
		{
			{ stage_type_child_launch, { "1" },           0, },
			{ stage_type_domain_start, { "mydomain" },    0, },
			{ stage_type_child_launch, { "2" },           0, },
			{ stage_type_child_launch, { "3" },           0, },
			{ stage_type_child_exit,   { "1" },           2000,  15 },
			{ stage_type_child_launch, { "1" },           3000, },
			{ stage_type_child_launch, { "1" },           5000, },
			{ stage_type_child_exit,   { "1" },           7000,  0 },
			{ stage_type_reboot,       {},                7000 },
		}
	}
};

struct test test_affinity = {
	"test-affinity.cfg",
	{
	    0, { 0, }, /* Initialize stage and timespec to 0 */
		{
			{ stage_type_child_launch, { "1" },           0, },
			{ stage_type_child_launch, { "2" },           0, },
			{ stage_type_child_exit,   { "1" },           2000,  0 },
			{ stage_type_reboot,       {},                2000 },
		}
	}
};

struct test test_domain_shutdown = {
	"test-domain-shutdown.cfg",
	{
	    0, { 0, }, /* Initialize stage and timespec to 0 */
		{
			{ stage_type_domain_start, { "test_domain" }, 0, },
			{ stage_type_child_launch, { "1" },           0, },
			{ stage_type_child_launch, { "2" },           0, },
			{ stage_type_child_launch, { "3" },           0, },
			{ stage_type_child_launch, { "4" },           0, },
			{ stage_type_child_exit,   { "3" },           1000,  0 },
			{ stage_type_domain_stop,  { "test_domain" }, 1000 },
			{ stage_type_child_exit,   { "4" },           1000,  15 },
			{ stage_type_child_exit,   { "2" },           1000,  15 },
			{ stage_type_child_exit,   { "1" },           1000,  15 },
			{ stage_type_domain_start, { "test_domain" }, 1000, },
			{ stage_type_child_launch, { "1" },           1000, },
			{ stage_type_child_launch, { "2" },           1000, },
			{ stage_type_child_launch, { "3" },           1000, },
			{ stage_type_child_launch, { "4" },           1000, },
			{ stage_type_child_exit,   { "3" },           2000,  0 },
			{ stage_type_domain_stop,  { "test_domain" }, 2000 },
			{ stage_type_child_exit,   { "4" },           2000,  15 },
			{ stage_type_child_exit,   { "2" },           2000,  15 },
			{ stage_type_child_exit,   { "1" },           2000,  15 },
			{ stage_type_domain_start, { "test_domain" }, 2000, },
			{ stage_type_child_launch, { "1" },           2000, },
			{ stage_type_child_launch, { "2" },           2000, },
			{ stage_type_child_launch, { "3" },           2000, },
			{ stage_type_child_launch, { "4" },           2000, },
			{ stage_type_child_exit,   { "3" },           3000,  0 },
			{ stage_type_reboot,       {},                3000 },
		}
	}
};

struct test test_shutdown = {
	"test-shutdown.cfg",
	{
	    0, { 0, }, /* Initialize stage and timespec to 0 */
		{
			{ stage_type_child_launch, { "1" },           0, },
			{ stage_type_child_launch, { "2" },           0, },
			{ stage_type_child_launch, { "3" },           0, },
			{ stage_type_child_launch, { "4" },           0, },
			{ stage_type_child_exit,   { "4" },           2000,  0 },
			{ stage_type_child_exit,   { "3" },           3000,  15 },
			{ stage_type_child_exit,   { "2" },           5000,  15 },
			{ stage_type_child_exit,   { "1" },           5000,  15 },
			{ stage_type_exit,
			  { .reason = MAMA_EXIT_REASON_ORDERED }, 5000 },
		}
	}
};

struct test test_domain_shutdown_cmd = {
	"test-domain-shutdown-cmd.cfg",
	{
	    0, { 0, }, /* Initialize stage and timespec to 0 */
		{
			{ stage_type_domain_start, { "test_domain" }, 0, },
			{ stage_type_child_launch, { "1" },           0, },
			{ stage_type_child_launch, { "2" },           0, },
			{ stage_type_child_launch, { "3" },           0, },
			{ stage_type_child_launch, { "4" },           0, },
			{ stage_type_domain_stop,  { "test_domain" }, 1000, },
			{ stage_type_child_exit,   { "4" },           1000,  15 },
			{ stage_type_child_exit,   { "3" },           2000,  15 },
			{ stage_type_child_exit,   { "2" },           2000,  15 },
			{ stage_type_child_exit,   { "1" },           2000,  15 },
			{ stage_type_domain_start, { "test_domain" }, 3000, },
			{ stage_type_child_launch, { "1" },           3000, },
			{ stage_type_child_launch, { "2" },           3000, },
			{ stage_type_child_launch, { "3" },           3000, },
			{ stage_type_child_launch, { "4" },           3000, },
			{ stage_type_child_exit,   { "1" },           6000,  0 },
			{ stage_type_reboot,       {},                6000 },
		}
	}
};

struct test test_shutdown_cmd = {
	"test-shutdown-cmd.cfg",
	{
	    0, { 0, }, /* Initialize stage and timespec to 0 */
		{
			{ stage_type_child_launch, { "1" },           0, },
			{ stage_type_domain_start, { "mydomain" },    0, },
			{ stage_type_child_launch, { "2" },           0, },
			{ stage_type_child_launch, { "3" },           0, },
			{ stage_type_child_exit,   { "3" },           1000,  15 },
			{ stage_type_child_exit,   { "2" },           4000,  15 },
			{ stage_type_child_exit,   { "1" },           4000,  15 },
			{ stage_type_exit,
			  { .reason = MAMA_EXIT_REASON_COMMAND }, 4000 },
		}
	}
};
