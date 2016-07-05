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

typedef struct C7{
    C1 a;
    HsInt32 b;
} C7;

C7 * newC7(C1* a, HsInt32 b){
    C7 * ret = (C7*) malloc(sizeof(C7));
    ret->a = *a;
    ret->b = b;
    return ret;
}

void pokeC7(C7* val, C1* a, HsInt32 b){
    val->a = *a;
    val->b = b;
}

int checkOffsetsC7(HsInt16 *offs){
    int a = offsetof(C7, a) == offs[0];
    int b = offsetof(C7, b) == offs[1];
    return a && b;
}

int checkFieldsC7(C7* s1, C7* s2){
    int a = checkFieldsC1(&(s1->a),&(s2->a));
    int b = s1->b == s2->b;
    return a && b;
}

HsInt16 getSizeC7() {
    return sizeof(C7);
}

HsInt16 getAlignmentC7() {
    return alignof(C7);
}

typedef struct C8{
    C2 a;
    HsInt8 b;
    C4 c;
} C8;

C8 * newC8(C2* a, HsInt8 b, C4* c){
    C8 * ret = (C8*) malloc(sizeof(C8));
    ret->a = *a;
    ret->b = b;
    ret->c = *c;
    return ret;
}

void pokeC8(C8* val, C2* a, HsInt8 b, C4* c){
    val->a = *a;
    val->b = b;
    val->c = *c;
}

int checkOffsetsC8(HsInt16 *offs){
    int a = offsetof(C8, a) == offs[0];
    int b = offsetof(C8, b) == offs[1];
    int c = offsetof(C8, c) == offs[2];
    return a && b && c;
}

int checkFieldsC8(C8* s1, C8* s2){
    int a = checkFieldsC2(&(s1->a),&(s2->a));
    int b = s1->b == s2->b;
    int c = checkFieldsC4(&(s1->c),&(s2->c));
    return a && b && c;
}

HsInt16 getSizeC8() {
    return sizeof(C8);
}

HsInt16 getAlignmentC8() {
    return alignof(C8);
}

typedef struct C9{
    C5 a;
    HsInt8 b;
    HsInt8 c;
    HsInt8 d;
} C9;

C9 * newC9(C5* a, HsInt8 b, HsInt8 c, HsInt8 d){
    C9 * ret = (C9*) malloc(sizeof(C9));
    ret->a = *a;
    ret->b = b;
    ret->c = c;
    ret->d = d;
    return ret;
}

void pokeC9(C9* val, C5* a, HsInt8 b, HsInt8 c, HsInt8 d){
    val->a = *a;
    val->b = b;
    val->c = c;
    val->d = d;
}

int checkOffsetsC9(HsInt16 *offs){
    int a = offsetof(C9, a) == offs[0];
    int b = offsetof(C9, b) == offs[1];
    int c = offsetof(C9, c) == offs[2];
    int d = offsetof(C9, d) == offs[3];
    return a && b && c && d;
}

int checkFieldsC9(C9* s1, C9* s2){
    int a = checkFieldsC5(&(s1->a),&(s2->a));
    int b = s1->b == s2->b;
    int c = s1->c == s2->c;
    int d = s1->d == s2->d;
    return a && b && c && d;
}

HsInt16 getSizeC9() {
    return sizeof(C9);
}

HsInt16 getAlignmentC9() {
    return alignof(C9);
}

typedef struct C10{
    C8 a;
    HsInt64 b;
    C1 c;
} C10;

C10 * newC10(C8* a, HsInt64 b, C1* c){
    C10 * ret = (C10*) malloc(sizeof(C10));
    ret->a = *a;
    ret->b = b;
    ret->c = *c;
    return ret;
}

void pokeC10(C10* val, C8* a, HsInt64 b, C1* c){
    val->a = *a;
    val->b = b;
    val->c = *c;
}

int checkOffsetsC10(HsInt16 *offs){
    int a = offsetof(C10, a) == offs[0];
    int b = offsetof(C10, b) == offs[1];
    int c = offsetof(C10, c) == offs[2];
    return a && b && c;
}

int checkFieldsC10(C10* s1, C10* s2){
    int a = checkFieldsC8(&(s1->a),&(s2->a));
    int b = s1->b == s2->b;
    int c = checkFieldsC1(&(s1->c),&(s2->c));
    return a && b && c;
}

HsInt16 getSizeC10() {
    return sizeof(C10);
}

HsInt16 getAlignmentC10() {
    return alignof(C10);
}

typedef struct C11{
    C10 a;
    C10 b;
} C11;

C11 * newC11(C10* a, C10* b){
    C11 * ret = (C11*) malloc(sizeof(C11));
    ret->a = *a;
    ret->b = *b;
    return ret;
}

void pokeC11(C11* val, C10* a, C10* b){
    val->a = *a;
    val->b = *b;
}

int checkOffsetsC11(HsInt16 *offs){
    int a = offsetof(C11, a) == offs[0];
    int b = offsetof(C11, b) == offs[1];
    return a && b;
}

int checkFieldsC11(C11* s1, C11* s2){
    int a = checkFieldsC10(&(s1->a),&(s2->a));
    int b = checkFieldsC10(&(s1->b),&(s2->b));
    return a && b;
}

HsInt16 getSizeC11() {
    return sizeof(C11);
}

HsInt16 getAlignmentC11() {
    return alignof(C11);
}

typedef struct C12{
    HsInt64 a;
    HsInt64 b;
    HsInt64 c;
    HsInt64 d;
} C12;

C12 * newC12(HsInt64 a, HsInt64 b, HsInt64 c, HsInt64 d){
    C12 * ret = (C12*) malloc(sizeof(C12));
    ret->a = a;
    ret->b = b;
    ret->c = c;
    ret->d = d;
    return ret;
}

void pokeC12(C12* val, HsInt64 a, HsInt64 b, HsInt64 c, HsInt64 d){
    val->a = a;
    val->b = b;
    val->c = c;
    val->d = d;
}

int checkOffsetsC12(HsInt16 *offs){
    int a = offsetof(C12, a) == offs[0];
    int b = offsetof(C12, b) == offs[1];
    int c = offsetof(C12, c) == offs[2];
    int d = offsetof(C12, d) == offs[3];
    return a && b && c && d;
}

int checkFieldsC12(C12* s1, C12* s2){
    int a = s1->a == s2->a;
    int b = s1->b == s2->b;
    int c = s1->c == s2->c;
    int d = s1->d == s2->d;
    return a && b && c && d;
}

HsInt16 getSizeC12() {
    return sizeof(C12);
}

HsInt16 getAlignmentC12() {
    return alignof(C12);
}

typedef struct C13{
    C12 a;
    C12 b;
    C12 c;
    C12 d;
} C13;

C13 * newC13(C12* a, C12* b, C12* c, C12* d){
    C13 * ret = (C13*) malloc(sizeof(C13));
    ret->a = *a;
    ret->b = *b;
    ret->c = *c;
    ret->d = *d;
    return ret;
}

void pokeC13(C13* val, C12* a, C12* b, C12* c, C12* d){
    val->a = *a;
    val->b = *b;
    val->c = *c;
    val->d = *d;
}

int checkOffsetsC13(HsInt16 *offs){
    int a = offsetof(C13, a) == offs[0];
    int b = offsetof(C13, b) == offs[1];
    int c = offsetof(C13, c) == offs[2];
    int d = offsetof(C13, d) == offs[3];
    return a && b && c && d;
}

int checkFieldsC13(C13* s1, C13* s2){
    int a = checkFieldsC12(&(s1->a),&(s2->a));
    int b = checkFieldsC12(&(s1->b),&(s2->b));
    int c = checkFieldsC12(&(s1->c),&(s2->c));
    int d = checkFieldsC12(&(s1->d),&(s2->d));
    return a && b && c && d;
}

HsInt16 getSizeC13() {
    return sizeof(C13);
}

HsInt16 getAlignmentC13() {
    return alignof(C13);
}

typedef struct C14{
    C13 a;
    C13 b;
    C13 c;
    C13 d;
    HsInt8 e;
} C14;

C14 * newC14(C13* a, C13* b, C13* c, C13* d, HsInt8 e){
    C14 * ret = (C14*) malloc(sizeof(C14));
    ret->a = *a;
    ret->b = *b;
    ret->c = *c;
    ret->d = *d;
    ret->e = e;
    return ret;
}

void pokeC14(C14* val, C13* a, C13* b, C13* c, C13* d, HsInt8 e){
    val->a = *a;
    val->b = *b;
    val->c = *c;
    val->d = *d;
    val->e = e;
}

int checkOffsetsC14(HsInt16 *offs){
    int a = offsetof(C14, a) == offs[0];
    int b = offsetof(C14, b) == offs[1];
    int c = offsetof(C14, c) == offs[2];
    int d = offsetof(C14, d) == offs[3];
    int e = offsetof(C14, e) == offs[4];
    return a && b && c && d && e;
}

int checkFieldsC14(C14* s1, C14* s2){
    int a = checkFieldsC13(&(s1->a),&(s2->a));
    int b = checkFieldsC13(&(s1->b),&(s2->b));
    int c = checkFieldsC13(&(s1->c),&(s2->c));
    int d = checkFieldsC13(&(s1->d),&(s2->d));
    int e = s1->e == s2->e;
    return a && b && c && d && e;
}

HsInt16 getSizeC14() {
    return sizeof(C14);
}

HsInt16 getAlignmentC14() {
    return alignof(C14);
}

typedef struct C15{
    C12 a;
    C14 b;
} C15;

C15 * newC15(C12* a, C14* b){
    C15 * ret = (C15*) malloc(sizeof(C15));
    ret->a = *a;
    ret->b = *b;
    return ret;
}

void pokeC15(C15* val, C12* a, C14* b){
    val->a = *a;
    val->b = *b;
}

int checkOffsetsC15(HsInt16 *offs){
    int a = offsetof(C15, a) == offs[0];
    int b = offsetof(C15, b) == offs[1];
    return a && b;
}

int checkFieldsC15(C15* s1, C15* s2){
    int a = checkFieldsC12(&(s1->a),&(s2->a));
    int b = checkFieldsC14(&(s1->b),&(s2->b));
    return a && b;
}

HsInt16 getSizeC15() {
    return sizeof(C15);
}

HsInt16 getAlignmentC15() {
    return alignof(C15);
}

typedef struct C16{
    C10 a;
    C15 b;
    C7 c;
} C16;

C16 * newC16(C10* a, C15* b, C7* c){
    C16 * ret = (C16*) malloc(sizeof(C16));
    ret->a = *a;
    ret->b = *b;
    ret->c = *c;
    return ret;
}

void pokeC16(C16* val, C10* a, C15* b, C7* c){
    val->a = *a;
    val->b = *b;
    val->c = *c;
}

int checkOffsetsC16(HsInt16 *offs){
    int a = offsetof(C16, a) == offs[0];
    int b = offsetof(C16, b) == offs[1];
    int c = offsetof(C16, c) == offs[2];
    return a && b && c;
}

int checkFieldsC16(C16* s1, C16* s2){
    int a = checkFieldsC10(&(s1->a),&(s2->a));
    int b = checkFieldsC15(&(s1->b),&(s2->b));
    int c = checkFieldsC7(&(s1->c),&(s2->c));
    return a && b && c;
}

HsInt16 getSizeC16() {
    return sizeof(C16);
}

HsInt16 getAlignmentC16() {
    return alignof(C16);
}

