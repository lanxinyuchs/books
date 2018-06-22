/* >  CONTENTS
 ******************************************************************************
 ************************** Copyright ERICSSON CBC ****************************
 ******************************************************************************
 *
 * The copyright to the computer programs herein is the property of
 * ERICSSON China (CBC). The programs may be used and/or copied
 * only with the written permission from ERICSSON China (CBC) or
 * in accordance with the terms and conditions stipulated in the
 * agreement/contract under which the prgrams have been supplied.
 *
 ******************************************************************************
 CONTENTS
 --------
 1  Description
 2  Include files
 3  Declarations and Definitions
 4  Signal Definitions
 5  Function prototypes
 6  Functions
  ******************************************************************************
 */
/* >  1  DESCRIPTION
 *******************************************************************************
 *
 * General log file for AC data.
 *
 * Revision history
 *     Date                   Author             Description
 *  2016-11-04           edentao           First Revision
 *
 *******************************************************************************
 */


/* >  2  INCLUDE FILES
 *******************************************************************************
 */
/*******C libary header*******/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <fcntl.h>
#include <unistd.h>   
#include <dirent.h>   
#include <sys/stat.h>
#include <sys/types.h>

/*******   AC header  *******/
#include "logFile.h"

/* >  3  DECLARATIONS AND DEFINITIONS
 ******************************************************************************
 */
 
/* >  3.1  GLOBAL
 ******************************************************************************
 */

/* >  3.2  LOCAL
 ******************************************************************************
 */
 
/* >  4  SIGNAL DEFINITIONS
 ******************************************************************************
 */

/* >  5  FUNCTION PROTOTYPES
 ******************************************************************************
 */
 
/* >  6.1  GLOBAL
 ******************************************************************************
 */

/* >  6.2  LOCAL
 ******************************************************************************
 */
 
/* >  LogFile::LogFile
 *****************************************************************************
 *
 *  Description :  Construction function of LogFile Class.
 *
 *  Arguments   :  void
 *
 *  Return      :  void
 *
 *****************************************************************************
 */
LogFile::LogFile(void)
{
    //instanceDrvName = NULL;
}

LogFile::~LogFile()
{
    //::free(instanceDrvName);
}

LogFile* LogFile::getInstance()
{
    static LogFile instance;
    return &instance;
}

/* >  LogFile::init
 *****************************************************************************
 *
 *  Description :  The inialization of LogFile.
 *
 *  Arguments   :
 *     Type                     INPUT/OUTPUT          PARAMETER
 *    char *                       INPUT            instanceDbName
 *
 *  Return      :
 *  Type                                  Comments
 *  bool             Return true if success, return false if failed.
 *
 *****************************************************************************
 */
bool LogFile::init()
{
    getcwd(workPath,32);
    //printf("The current directoryis:%s\n",workPath);

    //getcwd(char *buf, size_t size);
    scan_dir(workPath, 0);
    strcpy(acPath, workPath);
    strcat(acPath,"/acLog");
    char folderName[] = "acLog";

    //find ac log folder.
    //if(!checkFolderExisted(workPath, "acLog"))
    if(!checkFolderExisted(workPath, folderName))
    {
        //acLog folder is not existed. create it.
        mkdir(acPath, S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH); 
    }
    chdir(acPath);    
    //printf("Changed the directory to:%s\n",acPath);

    return true;
}

void LogFile::scan_dir(char *dir, int depth)
{   
    DIR *dp;
    struct dirent *entry;
    struct stat statbuf;
    if((dp = opendir(dir)) == NULL) 
    {   
        puts("can't open dir.");   
        return;   
    }   
    chdir (dir);
    while((entry = readdir(dp)) != NULL)
    {   
        lstat(entry->d_name, &statbuf); 
        if(S_IFDIR &statbuf.st_mode)
        {   
            if (strcmp(".", entry->d_name) == 0 || strcmp("..", entry->d_name) == 0)   
              continue;   
   
            //printf("%*s%s/\n", depth, "", entry->d_name);
            //scan_dir(entry->d_name, depth+4);
        }   
        //else   
            //printf("%*s%s\n", depth, "", entry->d_name);
    }   
    chdir("..");
    closedir(dp);
}   

bool LogFile::checkFolderExisted(char *path, char *folderFind)
{   
    DIR *dp;
    struct dirent *entry;
    struct stat statbuf;
    bool ret = false;
    if((dp = opendir(path)) == NULL) 
    {
        puts("can't open dir.");   
        return ret;
    }   
    chdir (path);
    while((entry = readdir(dp)) != NULL)
    {   
        lstat(entry->d_name, &statbuf); 
        if(S_IFDIR &statbuf.st_mode)
        {   
            if (strcmp(".", entry->d_name) == 0 || strcmp("..", entry->d_name) == 0)   
              continue;
   
            if (strcmp(folderFind, entry->d_name) == 0)
            {
                ret = true;
                break;
            }
            //printf("%*s%s/\n", depth, "", entry->d_name);
        }   
        //else   
            //printf("%*s%s\n", depth, "", entry->d_name);
    }   
    //chdir("..");
    closedir(dp);
    return ret;
}   

bool LogFile::storeAcData(U16 carrierId, U16 antId, U16 receiveTransmit, S16 * data, U16 dataSize)
{
    //if ((antId>=MAX_ANTENNA_NUM) || (carrierId>=MAX_CARRIER_NUM))
    //   return false;      
    chdir (acPath);

    char filename[128];
    char direct[4];
    S32 ret_file = 0;
    mode_t attr_file;
    time_t now;  
    struct tm *curTime;
    FILE * pDataFile = NULL;
    char bufTemp[16];

    now = time(NULL);  
    curTime = localtime(&now);  
    if(receiveTransmit)
    {
        strcpy(direct,"TX");
    }
    else
    {
        strcpy(direct,"RX");
    }
		
    sprintf(filename,"C%dP%02d%s_%04d%02d%02d%02d%02d.log",
		carrierId,
		antId,
		direct,
		curTime->tm_year+1900,        
		curTime->tm_mon+1,
		curTime->tm_mday,
		curTime->tm_hour,
		curTime->tm_min);

    //printf("store AC data in the file %s\n", filename);

    attr_file = S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH;
    chdir(acPath);
/*
    ret_file = open(filename, O_RDONLY);
    if(ret_file == -1)
    {
        //printf("Ready to create AC log file %s/\n", filename);
        ret_file = open(filename, O_RDWR|O_CREAT, attr_file);
        if(ret_file != -1)
        {
            //printf("Create AC log file %s/\n", filename);
        }
    }
    else
    {
        printf("This AC log file is exsited %s/\n", filename);
    }
    close(ret_file);
*/

    //write data
    //printf("write data.\n");     
    if((pDataFile = fopen (filename, "ab")) == NULL)  
    {
        //printf("cant open the file %s\n", filename);     
    }
    memset(fileBuffer, 0, FILE_BUFFER_SIZE);
    for(int i=0; i<dataSize; i++)  
    {    
        sprintf(bufTemp, "%8d,", data[i]);
        if((i+1)%10 == 0)
        {
            strcat(bufTemp, "\n");
        }
        strcat(fileBuffer, bufTemp);  
    }  
    fwrite(fileBuffer, strlen(fileBuffer), 1, pDataFile);  
    fclose(pDataFile);

    return false;
}
