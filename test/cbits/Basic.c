#include <stddef.h>
#include <stdio.h>
#include <stdalign.h>
#include <stdlib.h>
#include "HsFFI.h"

typedef struct C1 {
//TYPE:    FIELD:  SIZE:  ALIGNMENT: OFFSET: 
  HsInt32  a;    //4      4          0
  HsInt8   b;    //1      1          4
  HsInt8   c;    //1      1          5
//padding - 2 bytes
} C1;

C1 * newC1(int a, char b, char c){
  C1 * ret = (C1*)malloc(sizeof(C1));
  ret->a = a;
  ret->b = b;
  ret->c = c;
  return ret;
}



//Checks whether the offsets exists. If not, output to stderr and return false.
inline int checkOffsets(unsigned int *offs, const char* function_name){
  if(offs == NULL){
     fprintf(stderr, "test/cbits/Basic.s: %s - offs is null.\n", function_name);
     return 0;
  }
  return 1;
}

// I am assuming that offs will always be non null.
int checkOffsetsC1(unsigned int *offs){
  //Check wthether the offs exist:
  if(!checkOffsets(offs, "offsetsC1(unsigned int)"))
    return 0;

  int field1 = offsetof(C1, a) == offs[0];
  int field2 = offsetof(C1, b) == offs[1];
  int field3 = offsetof(C1, c) == offs[2];
  return field1 && field2 && field3; 
}


int checkFieldsC1(C1* s1, C1* s2){
  int field1 = s1->a == s2->a;
  int field2 = s1->b == s2->b;
  int field3 = s1->c == s2->c;
  return field1 && field2 && field3;
}

int getSizeC1() {
  return sizeof(C1);
}

int getAlignmentC1() {
  return alignof(C1);
}

struct C2 {
//TYPE:  FIELD:  SIZE:  ALIGNMENT: OFFSET:
  long   a;    //8      8          0
  char   b;    //1      1          8
//padding - 3 bytes
  int    c;    //4      4          12
};


struct C3 {
//TYPE:  FIELD:  SIZE:  ALIGNMENT: OFFSET:
  short  a;    //2      2          0
  char   b;    //1      1          2
  char   c;    //1      1          3
  char   d;    //1      1          4
//padding - 2 bytes  
  short  e;    //2      2          6
//padding - 1 byte
};

