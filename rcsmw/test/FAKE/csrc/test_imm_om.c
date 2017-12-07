#include <stdlib.h>
#include <stdio.h>
#include <saImmOm.h>

int test_imm_om_life_cycle();

int
test_imm_om()
{
  int rc = 0;

  printf("Running IMM OM functions\n");

  rc = test_imm_om_life_cycle();
  if (rc != 0)
    {
      return rc;
    }

  return rc;
}

int
test_imm_om_life_cycle()
{
  SaImmHandleT handle;
  SaImmCallbacksT_o2 *callbacks_o2 = NULL;
  SaImmCallbacksT *callbacks = NULL;
  SaVersionT version = {'A', 0x02, 0x0b};
  int rc = 0;

  rc = saImmOmInitialize_o2(&handle, callbacks_o2, &version);
  if (rc != SA_AIS_OK)
    {
      printf("Couldn't initialize Imm OM A.02.11 Reason:%u\n", rc);
      return 1;
   }

  rc = saImmOmFinalize(handle);
  if (rc != SA_AIS_OK)
    {
      printf("Couldn't finalize Imm OM A.02.11 Reason:%u\n", rc);
      return 1;
   }

  rc = saImmOmInitialize(&handle, callbacks, &version);
  if (rc != SA_AIS_OK)
    {
      printf("Couldn't initialize Imm OM A.02.xx Reason:%u\n", rc);
      return 1;
    }

  rc = saImmOmFinalize(handle);
  if (rc != SA_AIS_OK)
    {
      printf("Couldn't finalize Imm OM A.02.xx Reason:%u\n", rc);
      return 1;
    }

  return 0;
}
