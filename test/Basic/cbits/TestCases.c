#include <stddef.h>
#include <stdio.h>
#include <stdalign.h>
#include <stdlib.h>
#include "HsFFI.h"
typedef struct C1{
    HsInt32 a;
    HsInt32 b;
} C1;

C1 * newC1(HsInt32 a, HsInt32 b){
    C1 * ret = (C1*) malloc(sizeof(C1));
    ret->a = a;
    ret->b = b;
    return ret;
}

void pokeC1(C1* val, HsInt32 a, HsInt32 b){
    val->a = a;
    val->b = b;
}

int checkOffsetsC1(HsInt16 *offs){
    int a = offsetof(C1, a) == offs[0];
    int b = offsetof(C1, b) == offs[1];
    return a && b;
}

int checkFieldsC1(C1* s1, C1* s2){
    int a = s1->a == s2->a;
    int b = s1->b == s2->b;
    return a && b;
}

HsInt16 getSizeC1() {
    return sizeof(C1);
}

HsInt16 getAlignmentC1() {
    return alignof(C1);
}

typedef struct C2{
    HsInt32 a;
    HsInt16 b;
    HsInt8 c;
} C2;

C2 * newC2(HsInt32 a, HsInt16 b, HsInt8 c){
    C2 * ret = (C2*) malloc(sizeof(C2));
    ret->a = a;
    ret->b = b;
    ret->c = c;
    return ret;
}

void pokeC2(C2* val, HsInt32 a, HsInt16 b, HsInt8 c){
    val->a = a;
    val->b = b;
    val->c = c;
}

int checkOffsetsC2(HsInt16 *offs){
    int a = offsetof(C2, a) == offs[0];
    int b = offsetof(C2, b) == offs[1];
    int c = offsetof(C2, c) == offs[2];
    return a && b && c;
}

int checkFieldsC2(C2* s1, C2* s2){
    int a = s1->a == s2->a;
    int b = s1->b == s2->b;
    int c = s1->c == s2->c;
    return a && b && c;
}

HsInt16 getSizeC2() {
    return sizeof(C2);
}

HsInt16 getAlignmentC2() {
    return alignof(C2);
}

typedef struct C3{
    HsInt32 a;
    HsInt8 b;
    HsInt16 c;
} C3;

C3 * newC3(HsInt32 a, HsInt8 b, HsInt16 c){
    C3 * ret = (C3*) malloc(sizeof(C3));
    ret->a = a;
    ret->b = b;
    ret->c = c;
    return ret;
}

void pokeC3(C3* val, HsInt32 a, HsInt8 b, HsInt16 c){
    val->a = a;
    val->b = b;
    val->c = c;
}

int checkOffsetsC3(HsInt16 *offs){
    int a = offsetof(C3, a) == offs[0];
    int b = offsetof(C3, b) == offs[1];
    int c = offsetof(C3, c) == offs[2];
    return a && b && c;
}

int checkFieldsC3(C3* s1, C3* s2){
    int a = s1->a == s2->a;
    int b = s1->b == s2->b;
    int c = s1->c == s2->c;
    return a && b && c;
}

HsInt16 getSizeC3() {
    return sizeof(C3);
}

HsInt16 getAlignmentC3() {
    return alignof(C3);
}

typedef struct C4{
    HsInt32 a;
    HsInt8 b;
    HsInt8 c;
} C4;

C4 * newC4(HsInt32 a, HsInt8 b, HsInt8 c){
    C4 * ret = (C4*) malloc(sizeof(C4));
    ret->a = a;
    ret->b = b;
    ret->c = c;
    return ret;
}

void pokeC4(C4* val, HsInt32 a, HsInt8 b, HsInt8 c){
    val->a = a;
    val->b = b;
    val->c = c;
}

int checkOffsetsC4(HsInt16 *offs){
    int a = offsetof(C4, a) == offs[0];
    int b = offsetof(C4, b) == offs[1];
    int c = offsetof(C4, c) == offs[2];
    return a && b && c;
}

int checkFieldsC4(C4* s1, C4* s2){
    int a = s1->a == s2->a;
    int b = s1->b == s2->b;
    int c = s1->c == s2->c;
    return a && b && c;
}

HsInt16 getSizeC4() {
    return sizeof(C4);
}

HsInt16 getAlignmentC4() {
    return alignof(C4);
}

typedef struct C5{
    HsInt32 a;
    HsInt16 b;
    HsInt8 c;
    HsInt8 d;
    HsInt8 e;
} C5;

C5 * newC5(HsInt32 a, HsInt16 b, HsInt8 c, HsInt8 d, HsInt8 e){
    C5 * ret = (C5*) malloc(sizeof(C5));
    ret->a = a;
    ret->b = b;
    ret->c = c;
    ret->d = d;
    ret->e = e;
    return ret;
}

void pokeC5(C5* val, HsInt32 a, HsInt16 b, HsInt8 c, HsInt8 d, HsInt8 e){
    val->a = a;
    val->b = b;
    val->c = c;
    val->d = d;
    val->e = e;
}

int checkOffsetsC5(HsInt16 *offs){
    int a = offsetof(C5, a) == offs[0];
    int b = offsetof(C5, b) == offs[1];
    int c = offsetof(C5, c) == offs[2];
    int d = offsetof(C5, d) == offs[3];
    int e = offsetof(C5, e) == offs[4];
    return a && b && c && d && e;
}

int checkFieldsC5(C5* s1, C5* s2){
    int a = s1->a == s2->a;
    int b = s1->b == s2->b;
    int c = s1->c == s2->c;
    int d = s1->d == s2->d;
    int e = s1->e == s2->e;
    return a && b && c && d && e;
}

HsInt16 getSizeC5() {
    return sizeof(C5);
}

HsInt16 getAlignmentC5() {
    return alignof(C5);
}

typedef struct C6{
    HsInt64 a;
    HsInt8 b;
    HsInt64 c;
} C6;

C6 * newC6(HsInt64 a, HsInt8 b, HsInt64 c){
    C6 * ret = (C6*) malloc(sizeof(C6));
    ret->a = a;
    ret->b = b;
    ret->c = c;
    return ret;
}

void pokeC6(C6* val, HsInt64 a, HsInt8 b, HsInt64 c){
    val->a = a;
    val->b = b;
    val->c = c;
}

int checkOffsetsC6(HsInt16 *offs){
    int a = offsetof(C6, a) == offs[0];
    int b = offsetof(C6, b) == offs[1];
    int c = offsetof(C6, c) == offs[2];
    return a && b && c;
}

int checkFieldsC6(C6* s1, C6* s2){
    int a = s1->a == s2->a;
    int b = s1->b == s2->b;
    int c = s1->c == s2->c;
    return a && b && c;
}

HsInt16 getSizeC6() {
    return sizeof(C6);
}

HsInt16 getAlignmentC6() {
    return alignof(C6);
}

typedef struct C17{
    HsFloat a;
    HsDouble b;
    HsFloat c;
} C17;

C17 * newC17(HsFloat a, HsDouble b, HsFloat c){
    C17 * ret = (C17*) malloc(sizeof(C17));
    ret->a = a;
    ret->b = b;
    ret->c = c;
    return ret;
}

void pokeC17(C17* val, HsFloat a, HsDouble b, HsFloat c){
    val->a = a;
    val->b = b;
    val->c = c;
}

int checkOffsetsC17(HsInt16 *offs){
    int a = offsetof(C17, a) == offs[0];
    int b = offsetof(C17, b) == offs[1];
    int c = offsetof(C17, c) == offs[2];
    return a && b && c;
}

int checkFieldsC17(C17* s1, C17* s2){
    int a = s1->a == s2->a;
    int b = s1->b == s2->b;
    int c = s1->c == s2->c;
    return a && b && c;
}

HsInt16 getSizeC17() {
    return sizeof(C17);
}

HsInt16 getAlignmentC17() {
    return alignof(C17);
}

typedef struct C18{
    HsFloat a;
    HsFloat b;
    HsDouble c;
    HsFloat d;
} C18;

C18 * newC18(HsFloat a, HsFloat b, HsDouble c, HsFloat d){
    C18 * ret = (C18*) malloc(sizeof(C18));
    ret->a = a;
    ret->b = b;
    ret->c = c;
    ret->d = d;
    return ret;
}

void pokeC18(C18* val, HsFloat a, HsFloat b, HsDouble c, HsFloat d){
    val->a = a;
    val->b = b;
    val->c = c;
    val->d = d;
}

int checkOffsetsC18(HsInt16 *offs){
    int a = offsetof(C18, a) == offs[0];
    int b = offsetof(C18, b) == offs[1];
    int c = offsetof(C18, c) == offs[2];
    int d = offsetof(C18, d) == offs[3];
    return a && b && c && d;
}

int checkFieldsC18(C18* s1, C18* s2){
    int a = s1->a == s2->a;
    int b = s1->b == s2->b;
    int c = s1->c == s2->c;
    int d = s1->d == s2->d;
    return a && b && c && d;
}

HsInt16 getSizeC18() {
    return sizeof(C18);
}

HsInt16 getAlignmentC18() {
    return alignof(C18);
}

