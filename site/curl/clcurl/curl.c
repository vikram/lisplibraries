/* ******************************************************** */
/*  file:        curl.c                                     */
/*  description: Glue functions for CL interface to         */
/*               libcurl.                                   */
/*  date:        Thu Jan 20 2005 - 15:26                    */
/*  author:      Liam M. Healy <cl@healy.washington.dc.us>  */
/*  modified:    Mon Aug 15 2005 - 23:12 */
/* ******************************************************** */
/* $Id: $ */

/* If you are not using ASDF, it is necessary to make a library:
 gcc -fPIC -shared curl.c -lcurl -Wl,-soname,libclcurl.so -o libclcurl.so 
or on Darwin (?):
 gcc -dynamiclib curl.c -lcurl -Wl,-soname,libclcurl.so -o libclcurl.so 
*/

#include <stdio.h>
#include <curl/curl.h>

struct MemoryStruct {
  char *memory;
  size_t size;
};

struct CurlTransaction {
  struct MemoryStruct chunk;
  CURL *handle;
};

/* Taken from /usr/share/doc/libcurl2-dev/examples/getinmemory.c */
size_t
WriteMemoryCallback(void *ptr, size_t size, size_t nmemb, void *data)
{
  register int realsize = size * nmemb;
  struct MemoryStruct *mem = (struct MemoryStruct *)data;
  
  mem->memory = (char *)(long)realloc(mem->memory, mem->size + realsize + 1);
  if (mem->memory) {
    memcpy(&(mem->memory[mem->size]), ptr, realsize);
    mem->size += realsize;
    mem->memory[mem->size] = 0;
  }
  return realsize;
}

struct CurlTransaction *curl_init_write_string()
{
  struct CurlTransaction *curltran;

  curltran = (struct CurlTransaction *)(long)malloc(sizeof(struct CurlTransaction));
  if (curltran != NULL) {

    curltran->chunk.memory=NULL; /* we expect realloc(NULL, size) to work */
    curltran->chunk.size = 0;    /* no data at this point */
    
    curltran->handle = curl_easy_init();
    if (curltran->handle) {
      /* send all data to this function  */
      curl_easy_setopt(curltran->handle, CURLOPT_WRITEFUNCTION, WriteMemoryCallback);
      /* we pass our 'chunk' struct to the callback function */
      curl_easy_setopt(curltran->handle, CURLOPT_WRITEDATA, (void *)&curltran->chunk);
      return curltran;
    }
    return (struct CurlTransaction *)NULL;
  }
  return (struct CurlTransaction *)NULL;
}  

size_t
ReadMemoryCallback(void *ptr, size_t size, size_t nmemb, void *data)
{
  size_t length = size*nmemb;
  strncpy(ptr,data,length);
  return length;
}

int curl_set_read_string(struct CurlTransaction *curltran, char *string)
/* Set a string to read from */
{
  curl_easy_setopt(curltran->handle, CURLOPT_READFUNCTION, ReadMemoryCallback);
  curl_easy_setopt(curltran->handle, CURLOPT_READDATA, string);
  return 0;
}  

int curl_set_option_string(struct CurlTransaction *curltran, int option, char *val)
{
  if (curltran->handle) {
    return curl_easy_setopt(curltran->handle, option, val);
  } else {
    return -1;
  }
}

int curl_set_option_long(struct CurlTransaction *curltran, int option, long val)
{
  if (curltran->handle) {
    return curl_easy_setopt(curltran->handle, option, val);
  } else {
    return -1;
  }
}

int curl_get_information_string(struct CurlTransaction *curltran, int option, char *val) 
{
  if (curltran->handle) {
    return curl_easy_getinfo(curltran->handle, option, val);
  } else {
    return -1;
  }
}

int curl_get_information_long(struct CurlTransaction *curltran, int option, long *val) 
{
  if (curltran->handle) {
    return curl_easy_getinfo(curltran->handle, option, val);
  } else {
    return -1;
  }
}

int curl_get_information_double(struct CurlTransaction *curltran, int option, double *val) 
{
  if (curltran->handle) {
    return curl_easy_getinfo(curltran->handle, option, val);
  } else {
    return -1;
  }
}

int curl_perform(struct CurlTransaction *curltran)
{
  return curl_easy_perform(curltran->handle);
}

char *curl_return_string(struct CurlTransaction *curltran)
{
  return curltran->chunk.memory;
}

void curl_free_string(struct CurlTransaction *curltran)
{
  free(curltran->chunk.memory);
}

void curl_finish(struct CurlTransaction *curltran)
{
  curl_easy_cleanup(curltran->handle);
  free(curltran);
}
