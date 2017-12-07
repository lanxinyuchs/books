/*
 * %EricssonCopyright%
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2012-2017. All Rights Reserved.
 *
 * The program may be used and/or copied only with the written permission from
 * Ericsson AB, or in accordance with the terms and conditions stipulated in
 * the agreement/contract under which the program has been supplied.
 *
 * %CopyrightEnd%
 *
 * ----------------------------------------------------------------------
 *  Purpose : SAF IMM Common Functions
 * ----------------------------------------------------------------------
 *
 */
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <stdio.h>
#include <saAis.h>
#include <saImm.h>
#include <saImmOm.h>
#include "safc_trace.h"
#include "om.pb-c.h"
#include "safc_imm_lib.h"

void safc_free_safs_attr_values_2(SafsImmAttrValues2 *attr);
void safc_copy_from_cs_struct(SaImmCsStructT* from_struct, SafsImmAttrValue* aval);
SaImmAttrValueT safc_copy_to_cs_struct(SafsImmAttrValue *aval);

/*
 * Function: safc_imm_version_validate()
 * Description:
 * Validate that the requested version is supported in the library.
 */
SaAisErrorT safc_imm_version_validate(SaVersionT *version)
{
   SaAisErrorT rc = SA_AIS_OK;

   if ((version->releaseCode != IMM_RELEASE_CODE) || 
       (version->majorVersion != IMM_MAJOR_VERSION)) {
      TRACE1(("ERR_VERSION: Version %c %u not supported\n",
	      version->releaseCode,
	      version->majorVersion));
      rc = SA_AIS_ERR_VERSION;
   }

   version->releaseCode = IMM_RELEASE_CODE;
   version->majorVersion = IMM_MAJOR_VERSION;
   version->minorVersion = IMM_MINOR_VERSION;
   return rc;
}

/*
 * Function: safc_free_admop_params()
 * Description:
 * Free the memory used by admin operation parameters from protobuf.
 */
void safc_free_admop_params(int no_params, SafsImmAdminOperationParams2 **params)
{
   int i;

   for (i = 0; i < no_params; i++) {      
      switch(params[i]->paramtype) {
      case SA_IMM_ATTR_SANAMET:
	 free(params[i]->parambuffer->saname);
	 break;
      case SA_IMM_ATTR_SAANYT:
	 free(params[i]->parambuffer->saany.data);
	 break; 
      default:
	 break;
      }
      free(params[i]->parambuffer);
      free(params[i]);
   }

   free(params);

   return;
}

/*
 * Function: safc_free_imm_attr_value()
 * Description:
 * Free the memory used by an IMM attribute value.
 */
void safc_free_imm_attr_value(const SaImmValueTypeT attrValueType, SaImmAttrValueT attrValue)
{
   SaAnyT *saAnyTp = NULL;
   SaStringT *saStringTp = NULL;
   SaImmCsStructT *saCsStructTp = NULL;

   switch (attrValueType) {
   case SA_IMM_ATTR_SAINT32T:
   case SA_IMM_ATTR_SAUINT32T:
   case SA_IMM_ATTR_SAINT64T:
   case SA_IMM_ATTR_SAUINT64T:
   case SA_IMM_ATTR_SATIMET:
   case SA_IMM_ATTR_SAFLOATT:
   case SA_IMM_ATTR_SADOUBLET:
   case SA_IMM_ATTR_SANAMET:
      break;

   case SA_IMM_ATTR_SASTRINGT:
      saStringTp = (SaStringT *)attrValue;
      free(*saStringTp);
      break;
   case SA_IMM_ATTR_SAANYT:
      saAnyTp = (SaAnyT *)attrValue;
      if (saAnyTp->bufferSize) {
	 free(saAnyTp->bufferAddr);
      }
      break;
   case SA_IMM_ATTR_CSSTRUCTT: 
      saCsStructTp = (SaImmCsStructT *) attrValue;
      safc_free_imm_attributes(saCsStructTp->structMembers);
      free(saCsStructTp->structName);
      break;      
   default:
      TRACE1(("Incorrect value for SaImmValueTypeT:%u. "
	      "Is the attrValueType member set in "
	      "SaImmAttrValuesT_2 ?", attrValueType));
      abort();
   }

   free(attrValue);
}

/*
 * Function: safc_free_imm_attributes()
 * Description:
 * Free the memory used by an IMM attribute list.
 */
void safc_free_imm_attributes(SaImmAttrValuesT_2** attributes) 
{
   int i, j;
   
   for(i=0; attributes[i] != NULL; i++)
   {
      free(attributes[i]->attrName);

      for(j = 0; j < attributes[i]->attrValuesNumber; j++) 
	 safc_free_imm_attr_value(attributes[i]->attrValueType, attributes[i]->attrValues[j]);
      
      free(attributes[i]->attrValues);
      free(attributes[i]);
   } 
   free(attributes);

   return;
}

/*
 * Function: safc_free_imm_objects()
 * Description:
 * Free the memory used by an IMM object fetched by saImmOmSearchNextN_s2().
 */
void safc_free_imm_objects(SaImmSearchObjectsT_s2** objects) 
{
   int i, j, k;
   
   for(i=0; objects[i] != NULL; i++)
   {
      for(j=0;  objects[i]->attributes[j] != NULL; j++)
      {
	 free(objects[i]->attributes[j]->attrName);
      
	 for(k = 0; k < objects[i]->attributes[j]->attrValuesNumber; k++)
	    safc_free_imm_attr_value(objects[i]->attributes[j]->attrValueType, 
				     objects[i]->attributes[j]->attrValues[k]);
   
	 free(objects[i]->attributes[j]->attrValues);
	 free(objects[i]->attributes[j]);
      } 
      free(objects[i]->attributes);
      free(objects[i]);
   }

   free(objects);
   return;
}

/*
 * Function: safc_free_safs_struct()
 * Description:
 * Free the memory used by an RBS CS struct from protobuf
 */
static void safc_free_safs_struct(SafsImmCsStruct* cs_struct) 
{
   int i;
   for (i = 0; i < cs_struct->n_structmembers; i++)
      safc_free_safs_attr_values_2(cs_struct->structmembers[i]);
   free(cs_struct->structmembers);
   free(cs_struct);
}

/*
 * Function: safc_free_safs_attr_values_2()
 * Description:
 * Free the memory used by an attribute value list from protobuf.
 */
void safc_free_safs_attr_values_2(SafsImmAttrValues2 *attr) 
{
   int i;
   
   for (i = 0; i < attr->attrvaluesnumber; i++) {
	 switch(attr->attrvaluetype) {
	 case SA_IMM_ATTR_SANAMET:
	    free(attr->attrvalues[i]->saname);
	    break;
	 case SA_IMM_ATTR_SAANYT:
	    free(attr->attrvalues[i]->saany.data);
	    break;
	 case SA_IMM_ATTR_CSSTRUCTT:
	    safc_free_safs_struct(attr->attrvalues[i]->csstruct);
	    break;
	 default:
	    break;
	 }
	 free(attr->attrvalues[i]);	 
   }
   free(attr->attrvalues);
   free(attr);
}

/*
 * Function: safc_copy_from_imm_attr_value()
 * Description:
 * Copy the value from IMM to protobuf types.
 */
void safc_copy_from_imm_attr_value(SafsImmAttrValue* aval, 
				   const SaImmValueTypeT attrValueType,
				   const SaImmAttrValueT attrValue)
{
   /*
     Copies ONE attribute value.
     Multivalued attributes need to copy each value.
   */

   SaAnyT *saAnyTp = 0;
   SaNameT *saNameTp = 0;

   switch (attrValueType) {
   case SA_IMM_ATTR_SAINT32T:
      aval->has_saint32 = 1;
      aval->saint32 = *((SaInt32T *)attrValue);
      break;
   case SA_IMM_ATTR_SAUINT32T:
      aval->has_sauint32 = 1;
      aval->sauint32 = *((SaUint32T *)attrValue);
      break;
   case SA_IMM_ATTR_SAINT64T:
      aval->has_saint64 = 1;
      aval->saint64 = *((SaInt64T *)attrValue);
      break;
   case SA_IMM_ATTR_SAUINT64T:
      aval->has_sauint64 = 1;
      aval->sauint64 = *((SaUint64T *)attrValue);
      break;
   case SA_IMM_ATTR_SATIMET:
      aval->has_satime = 1;
      aval->satime = *((SaTimeT *)attrValue);
      break;
   case SA_IMM_ATTR_SAFLOATT:
      aval->has_safloat = 1;
      aval->safloat = *((SaFloatT *)attrValue);
      break;
   case SA_IMM_ATTR_SADOUBLET:
      aval->has_sadouble = 1;
      aval->sadouble = *((SaDoubleT *)attrValue);
      break;

   case SA_IMM_ATTR_SANAMET:
      saNameTp = (SaNameT *)attrValue;
      assert(saNameTp->length < SA_MAX_NAME_LENGTH);
      aval->saname = calloc(1, sizeof(SaUint8T) * (saNameTp->length + 1));
      (void)memcpy(aval->saname,  saNameTp->value, saNameTp->length);
      aval->saname[saNameTp->length] = '\0';
      break;
   case SA_IMM_ATTR_SASTRINGT:
      aval->sastring =  *((SaStringT *)attrValue);
      break;

   case SA_IMM_ATTR_SAANYT:
      saAnyTp = (SaAnyT *)attrValue;
      aval->saany.len = (saAnyTp) ? saAnyTp->bufferSize : 0;
      if (aval->saany.len > 0) {
	 aval->has_saany = 1;
	 aval->saany.data = calloc(1, sizeof(SaUint8T) * (aval->saany.len));
	 (void)memcpy(aval->saany.data, saAnyTp->bufferAddr, aval->saany.len);
      }
      break;
   default:
      break;
   }

}

/*
 * Function: safc_copy_to_imm_attr_value()
 * Description:
 * Copy the value from protobuf to IMM types.
 */
SaImmAttrValueT safc_copy_to_imm_attr_value(const SaImmValueTypeT attrValueType, 
					    SafsImmAttrValue *aval)
{
   /*
     Copies ONE attribute value. 
     Multivalued attributes need to copy each value.
     Allocates the root value on the heap. 
     WARNING!, for dynamic/large sized data (SaStringT, SaAnyT) the buffer
     is stolen from the aval.
   */
   size_t valueSize = 0;
   SaAnyT *saAnyTp = 0;
   SaNameT *saNameTp = 0;
   SaStringT *saStringTp = 0;
   SaImmAttrValueT retVal = NULL;

   switch (attrValueType) {
   case SA_IMM_ATTR_SAINT32T:
      valueSize = sizeof(SaInt32T);
      break;
   case SA_IMM_ATTR_SAUINT32T:
      valueSize = sizeof(SaUint32T);
      break;
   case SA_IMM_ATTR_SAINT64T:
      valueSize = sizeof(SaInt64T);
      break;
   case SA_IMM_ATTR_SAUINT64T:
      valueSize = sizeof(SaUint64T);
      break;
   case SA_IMM_ATTR_SATIMET:
      valueSize = sizeof(SaTimeT);
      break;
   case SA_IMM_ATTR_SAFLOATT:
      valueSize = sizeof(SaFloatT);
      break;
   case SA_IMM_ATTR_SADOUBLET:
      valueSize = sizeof(SaDoubleT);
      break;
   case SA_IMM_ATTR_SANAMET:
      valueSize = sizeof(SaNameT);
      break;
   case SA_IMM_ATTR_SASTRINGT:
      valueSize = sizeof(SaStringT);
      break;
   case SA_IMM_ATTR_SAANYT:
      valueSize = sizeof(SaAnyT);
      break;
   case SA_IMM_ATTR_CSSTRUCTT:
      TRACE1(("Illegal to have structs in structs"));
      abort();
   default:
      TRACE1(("Illegal value type: %u", attrValueType));
      abort();
   }

   retVal = calloc(1, valueSize);

   switch (attrValueType) {
   case SA_IMM_ATTR_SAINT32T:
      *((SaInt32T *)retVal) = aval->saint32;
      break;
   case SA_IMM_ATTR_SAUINT32T:
      *((SaUint32T *)retVal) = aval->sauint32;
      break;
   case SA_IMM_ATTR_SAINT64T:
      *((SaInt64T *)retVal) = aval->saint64;
      break;
   case SA_IMM_ATTR_SAUINT64T:
      *((SaUint64T *)retVal) = aval->sauint64;
      break;
   case SA_IMM_ATTR_SATIMET:
      *((SaTimeT *)retVal) = aval->satime;
      break;
   case SA_IMM_ATTR_SAFLOATT:
      *((SaFloatT *)retVal) = aval->safloat;
      break;
   case SA_IMM_ATTR_SADOUBLET:
      *((SaDoubleT *)retVal) = aval->sadouble;
      break;

   case SA_IMM_ATTR_SANAMET:
      saNameTp = (SaNameT *)retVal;
      saNameTp->length = strlen(aval->saname);
      assert(saNameTp->length <= SA_MAX_NAME_LENGTH);
      memcpy(saNameTp->value, aval->saname, saNameTp->length);
      break;

   case SA_IMM_ATTR_SASTRINGT:
      saStringTp = (SaStringT *)retVal; /* Pointer TO string-pointer. */
      /* Steal the buffer. */
      if (strlen(aval->sastring)) {
	 (*saStringTp) = aval->sastring;
	 aval->sastring = NULL;
      } else {
	 (*saStringTp) = NULL;
      }
      break;

   case SA_IMM_ATTR_SAANYT:
      saAnyTp = (SaAnyT *)retVal;
      /*Steal the value buffer. */
      saAnyTp->bufferSize = aval->saany.len;
      if (aval->saany.len) {
	 /*Steal the buffer. */
	 saAnyTp->bufferAddr = (SaUint8T *)aval->saany.data;
	 aval->saany.data = NULL;
	 aval->saany.len = 0;
      } else {
	 saAnyTp->bufferAddr = NULL;
      }
      break;

   default:
      abort();
   }
   return retVal;
}

/*
 * Function: safc_copy_from_imm_attributes()
 * Description:
 * Copy the attributes from IMM to protobuf types.
 */
void safc_copy_from_imm_attributes(const SaImmAttrValuesT_2* from_attr, SafsImmAttrValues2 **to_attr) 
{
   int i;
   const SaImmAttrValueT *attributeValues;

   attributeValues = from_attr->attrValues;

   *to_attr = calloc(1, sizeof (SafsImmAttrValues2));
   safs_imm_attr_values_2__init(*to_attr);

   (*to_attr)->attrname = (char*) from_attr->attrName;
   (*to_attr)->attrvaluetype = from_attr->attrValueType;
   (*to_attr)->attrvaluesnumber = from_attr->attrValuesNumber;

   (*to_attr)->attrvalues =
      calloc (1, sizeof (SafsImmAttrValue*) * from_attr->attrValuesNumber);
   (*to_attr)->n_attrvalues = from_attr->attrValuesNumber;

   for (i = 0; i < from_attr->attrValuesNumber; i++) {
      (*to_attr)->attrvalues[i] = calloc (1, sizeof (SafsImmAttrValue));
      safs_imm_attr_value__init((*to_attr)->attrvalues[i]);
      if(from_attr->attrValueType == SA_IMM_ATTR_CSSTRUCTT) {
	 safc_copy_from_cs_struct(attributeValues[i], (*to_attr)->attrvalues[i]);
      } else
	 safc_copy_from_imm_attr_value((*to_attr)->attrvalues[i], from_attr->attrValueType, attributeValues[i]);
   }
}

/*
 * Function: safc_copy_to_imm_attributes()
 * Description:
 * Copy the attributes from protobuf to IMM types.
 */
void safc_copy_to_imm_attributes(const SafsImmAttrValues2 *from_attr, SaImmAttrValuesT_2* to_attr) 
{
   int i, name_len;
   
   name_len = strlen(from_attr->attrname);
   to_attr->attrName = malloc(name_len + 1);
   strncpy(to_attr->attrName, from_attr->attrname, name_len + 1);
   /* attr[i]->attrName[q->attrName.size] = 0;	/\*redundant. *\/ */
   to_attr->attrValuesNumber = from_attr->attrvaluesnumber;
   to_attr->attrValueType = (SaImmValueTypeT) from_attr->attrvaluetype;

   if (from_attr->attrvaluesnumber) {
      to_attr->attrValues = calloc(1, to_attr->attrValuesNumber * sizeof(SaImmAttrValueT));	/*alloc-4 */

      for(i = 0; i < to_attr->attrValuesNumber; i++)
	 if((SaImmValueTypeT) from_attr->attrvaluetype == SA_IMM_ATTR_CSSTRUCTT) {
	    to_attr->attrValues[i] = safc_copy_to_cs_struct(from_attr->attrvalues[i]);
	 } else 
	    to_attr->attrValues[i] = safc_copy_to_imm_attr_value(from_attr->attrvaluetype, 
								 from_attr->attrvalues[i]);
   } else 
      to_attr->attrValues = NULL;
}

/*
 * Function: safc_copy_from_cs_struct()
 * Description:
 * Copy a CS struct from IMM to protobuf.
 */
void safc_copy_from_cs_struct(SaImmCsStructT* from_struct, SafsImmAttrValue* aval)
{
   int i; 

   aval->csstruct = calloc(1, sizeof (SafsImmCsStruct));
   safs_imm_cs_struct__init(aval->csstruct);

   aval->csstruct->structname = (char*) from_struct->structName;
   TRACE1(("Struct Name: %s\n", (char*) from_struct->structName));
   for (i = 0; from_struct->structMembers[i]; i++);
   aval->csstruct->n_structmembers = i;
   aval->csstruct->structmembers =
      calloc (1, sizeof (SafsImmAttrValues2*) * aval->csstruct->n_structmembers);
   for (i = 0; i < aval->csstruct->n_structmembers; i++) {
      safc_copy_from_imm_attributes(from_struct->structMembers[i], &(aval->csstruct->structmembers[i]));
   }
}

/*
 * Function: safc_copy_to_cs_struct()
 * Description:
 * Copy a CS struct from protobuf to IMM.
 */
SaImmAttrValueT safc_copy_to_cs_struct(SafsImmAttrValue *aval)
{
   int name_len, i;
   SafsImmCsStruct* from_struct = NULL;
   SaImmCsStructT* to_struct = NULL;
   SaImmAttrValuesT_2 **structMembers = NULL;

   from_struct = aval->csstruct;
   to_struct = calloc(1, sizeof(SaImmCsStructT));

   name_len = strlen(from_struct->structname);
   to_struct->structName = malloc(name_len + 1);
   strncpy(to_struct->structName, from_struct->structname, name_len + 1);
   TRACE1(("Struct Name: %s\n", (char*) to_struct->structName));
   structMembers = calloc(1, sizeof(SaImmAttrValuesT_2 *) * ( aval->csstruct->n_structmembers + 1));

   for (i = 0; i < aval->csstruct->n_structmembers; i++) {
      structMembers[i] = calloc(1, sizeof(SaImmAttrValuesT_2));
      safc_copy_to_imm_attributes(from_struct->structmembers[i], structMembers[i]);
   }

   structMembers[aval->csstruct->n_structmembers] = NULL; 
   to_struct->structMembers = structMembers;

   return (SaImmAttrValueT) to_struct;
}
