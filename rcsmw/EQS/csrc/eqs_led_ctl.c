/*
 *
 * Copyright (c) Ericsson AB 2014-2015 All rights reserved.
 *
 * The information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson,the receiver of this
 * document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties. Disclosure and disseminations to the
 * receiver's employees shall only be made on a strict need to know basis.
 *
 */

#define __RHAI_MMI_13147__

#include <unistd.h>
#include <string.h>
#include <syslog.h>

#include "rhai-mmi.h"

#define SET_LED_STATE (1)
#define GET_LED_STATE (2)

typedef unsigned char byte;

static ssize_t read_cmd(byte *buf);
static ssize_t write_cmd(byte *buf, size_t count);
static ssize_t read_exact(byte *buf, size_t count);
static ssize_t write_exact(byte *buf, size_t count);

static void set_led_state(byte* buf);
static void get_led_state(byte* buf);

int
main()
{
  int fn;
  byte buf[100];

  openlog("eqs_led_ctl", LOG_PID, LOG_USER);

  while (read_cmd(buf) > 0) {
    fn = buf[0];

    switch (fn) {
    case SET_LED_STATE:
      set_led_state(buf+1);
      break;
    case GET_LED_STATE:
      get_led_state(buf+1);
      break;
    default:
      {
	int ret = -RHAI_MMIERR_INVAL;
	write_cmd((byte*)&ret, 4);
      }
    }
  }

  return 0;
}

static ssize_t
read_cmd(byte *buf)
{
  int len;

  if (read_exact(buf, 2) != 2)
    return -1;
  len = (buf[0] << 8) | buf[1];
  return read_exact(buf, len);
}

static ssize_t
write_cmd(byte *buf, size_t count)
{
  byte li;

  li = (count >> 8) & 0xff;
  write_exact(&li, 1);

  li = count & 0xff;
  write_exact(&li, 1);

  return write_exact(buf, count);
}

static ssize_t
read_exact(byte *buf, size_t count)
{
  size_t i, got = 0;

  do {
    if ((i = read(STDIN_FILENO, buf + got, count - got)) <= 0)
      return i;
    got += i;
  } while (got < count);

  return count;
}

static ssize_t
write_exact(byte *buf, size_t count)
{
  size_t i, wrote = 0;

  do {
    if ((i = write(STDOUT_FILENO, buf + wrote, count - wrote)) <= 0)
      return i;
    wrote += i;
  } while (wrote < count);

  return count;
}

static void
set_led_state(byte* buf)
{
  int ret;
  enum rhai_mmi_led_type type;
  enum rhai_mmi_led_state state;

  type = *(enum rhai_mmi_led_type*)buf;
  state = *(enum rhai_mmi_led_state*)(buf+4);

  ret = rhai_mmi_led_set_state(type, state);

  if (ret < 0)
    syslog(LOG_ERR, "rhai_mmi_led_set_state returned %i\n", ret);

  write_cmd((byte*)&ret, 4);
}

static void
get_led_state(byte* buf)
{
  int ret;
  enum rhai_mmi_led_type type;
  enum rhai_mmi_led_state state;
  byte cmd[8];

  type = *(enum rhai_mmi_led_type*)buf;
  ret = rhai_mmi_led_get_state(type, &state);

  if (ret < 0)
    syslog(LOG_ERR, "rhai_mmi_led_get_state returned %i\n", ret);

  memcpy(cmd, &ret, 4);
  memcpy(cmd+4, &state, 4);

  write_cmd(cmd, 8);
}
