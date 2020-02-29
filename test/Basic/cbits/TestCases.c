#include <stddef.h>
#include <stdio.h>
#include <stdalign.h>
#include <stdlib.h>
#include "HsFFI.h"
typedef struct C0{
} C0;

C0 * newC0(){
    C0 * ret = (C0*) malloc(sizeof(C0));
    return ret;
}

void pokeC0(C0* val){
}

int checkOffsetsC0(HsInt16 *offs){
    return 1;
}

int checkFieldsC0(C0* s1, C0* s2){
    return 1; 
}

HsInt16 getSizeC0() {
    return sizeof(C0);
}

HsInt16 getAlignmentC0() {
    return alignof(C0);
}

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
    C0 b;
    HsInt16 c;
    HsInt8 d;
} C2;

C2 * newC2(HsInt32 a, C0* b, HsInt16 c, HsInt8 d){
    C2 * ret = (C2*) malloc(sizeof(C2));
    ret->a = a;
    ret->b = *b;
    ret->c = c;
    ret->d = d;
    return ret;
}

void pokeC2(C2* val, HsInt32 a, C0* b, HsInt16 c, HsInt8 d){
    val->a = a;
    val->b = *b;
    val->c = c;
    val->d = d;
}

int checkOffsetsC2(HsInt16 *offs){
    int a = offsetof(C2, a) == offs[0];
    int b = offsetof(C2, b) == offs[1];
    int c = offsetof(C2, c) == offs[2];
    int d = offsetof(C2, d) == offs[3];
    return a && b && c && d;
}

int checkFieldsC2(C2* s1, C2* s2){
    int a = s1->a == s2->a;
    int b = checkFieldsC0(&(s1->b),&(s2->b));
    int c = s1->c == s2->c;
    int d = s1->d == s2->d;
    return a && b && c && d;
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
    C0 a;
    C0 b;
    HsInt32 c;
    HsInt16 d;
    HsInt8 e;
    HsInt8 f;
    HsInt8 g;
} C5;

C5 * newC5(C0* a, C0* b, HsInt32 c, HsInt16 d, HsInt8 e, HsInt8 f, HsInt8 g){
    C5 * ret = (C5*) malloc(sizeof(C5));
    ret->a = *a;
    ret->b = *b;
    ret->c = c;
    ret->d = d;
    ret->e = e;
    ret->f = f;
    ret->g = g;
    return ret;
}

void pokeC5(C5* val, C0* a, C0* b, HsInt32 c, HsInt16 d, HsInt8 e, HsInt8 f, HsInt8 g){
    val->a = *a;
    val->b = *b;
    val->c = c;
    val->d = d;
    val->e = e;
    val->f = f;
    val->g = g;
}

int checkOffsetsC5(HsInt16 *offs){
    int a = offsetof(C5, a) == offs[0];
    int b = offsetof(C5, b) == offs[1];
    int c = offsetof(C5, c) == offs[2];
    int d = offsetof(C5, d) == offs[3];
    int e = offsetof(C5, e) == offs[4];
    int f = offsetof(C5, f) == offs[5];
    int g = offsetof(C5, g) == offs[6];
    return a && b && c && d && e && f && g;
}

int checkFieldsC5(C5* s1, C5* s2){
    int a = checkFieldsC0(&(s1->a),&(s2->a));
    int b = checkFieldsC0(&(s1->b),&(s2->b));
    int c = s1->c == s2->c;
    int d = s1->d == s2->d;
    int e = s1->e == s2->e;
    int f = s1->f == s2->f;
    int g = s1->g == s2->g;
    return a && b && c && d && e && f && g;
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
    C0 c;
    C12 d;
    C12 e;
} C13;

C13 * newC13(C12* a, C12* b, C0* c, C12* d, C12* e){
    C13 * ret = (C13*) malloc(sizeof(C13));
    ret->a = *a;
    ret->b = *b;
    ret->c = *c;
    ret->d = *d;
    ret->e = *e;
    return ret;
}

void pokeC13(C13* val, C12* a, C12* b, C0* c, C12* d, C12* e){
    val->a = *a;
    val->b = *b;
    val->c = *c;
    val->d = *d;
    val->e = *e;
}

int checkOffsetsC13(HsInt16 *offs){
    int a = offsetof(C13, a) == offs[0];
    int b = offsetof(C13, b) == offs[1];
    int c = offsetof(C13, c) == offs[2];
    int d = offsetof(C13, d) == offs[3];
    int e = offsetof(C13, e) == offs[4];
    return a && b && c && d && e;
}

int checkFieldsC13(C13* s1, C13* s2){
    int a = checkFieldsC12(&(s1->a),&(s2->a));
    int b = checkFieldsC12(&(s1->b),&(s2->b));
    int c = checkFieldsC0(&(s1->c),&(s2->c));
    int d = checkFieldsC12(&(s1->d),&(s2->d));
    int e = checkFieldsC12(&(s1->e),&(s2->e));
    return a && b && c && d && e;
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
    C0 b;
    C15 c;
    C7 d;
} C16;

C16 * newC16(C10* a, C0* b, C15* c, C7* d){
    C16 * ret = (C16*) malloc(sizeof(C16));
    ret->a = *a;
    ret->b = *b;
    ret->c = *c;
    ret->d = *d;
    return ret;
}

void pokeC16(C16* val, C10* a, C0* b, C15* c, C7* d){
    val->a = *a;
    val->b = *b;
    val->c = *c;
    val->d = *d;
}

int checkOffsetsC16(HsInt16 *offs){
    int a = offsetof(C16, a) == offs[0];
    int b = offsetof(C16, b) == offs[1];
    int c = offsetof(C16, c) == offs[2];
    int d = offsetof(C16, d) == offs[3];
    return a && b && c && d;
}

int checkFieldsC16(C16* s1, C16* s2){
    int a = checkFieldsC10(&(s1->a),&(s2->a));
    int b = checkFieldsC0(&(s1->b),&(s2->b));
    int c = checkFieldsC15(&(s1->c),&(s2->c));
    int d = checkFieldsC7(&(s1->d),&(s2->d));
    return a && b && c && d;
}

HsInt16 getSizeC16() {
    return sizeof(C16);
}

HsInt16 getAlignmentC16() {
    return alignof(C16);
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

typedef struct C19{
    C17 a;
    HsFloat b;
    HsDouble c;
} C19;

C19 * newC19(C17* a, HsFloat b, HsDouble c){
    C19 * ret = (C19*) malloc(sizeof(C19));
    ret->a = *a;
    ret->b = b;
    ret->c = c;
    return ret;
}

void pokeC19(C19* val, C17* a, HsFloat b, HsDouble c){
    val->a = *a;
    val->b = b;
    val->c = c;
}

int checkOffsetsC19(HsInt16 *offs){
    int a = offsetof(C19, a) == offs[0];
    int b = offsetof(C19, b) == offs[1];
    int c = offsetof(C19, c) == offs[2];
    return a && b && c;
}

int checkFieldsC19(C19* s1, C19* s2){
    int a = checkFieldsC17(&(s1->a),&(s2->a));
    int b = s1->b == s2->b;
    int c = s1->c == s2->c;
    return a && b && c;
}

HsInt16 getSizeC19() {
    return sizeof(C19);
}

HsInt16 getAlignmentC19() {
    return alignof(C19);
}

typedef struct C20{
    HsDouble a;
    C18 b;
    HsDouble c;
    C19 d;
} C20;

C20 * newC20(HsDouble a, C18* b, HsDouble c, C19* d){
    C20 * ret = (C20*) malloc(sizeof(C20));
    ret->a = a;
    ret->b = *b;
    ret->c = c;
    ret->d = *d;
    return ret;
}

void pokeC20(C20* val, HsDouble a, C18* b, HsDouble c, C19* d){
    val->a = a;
    val->b = *b;
    val->c = c;
    val->d = *d;
}

int checkOffsetsC20(HsInt16 *offs){
    int a = offsetof(C20, a) == offs[0];
    int b = offsetof(C20, b) == offs[1];
    int c = offsetof(C20, c) == offs[2];
    int d = offsetof(C20, d) == offs[3];
    return a && b && c && d;
}

int checkFieldsC20(C20* s1, C20* s2){
    int a = s1->a == s2->a;
    int b = checkFieldsC18(&(s1->b),&(s2->b));
    int c = s1->c == s2->c;
    int d = checkFieldsC19(&(s1->d),&(s2->d));
    return a && b && c && d;
}

HsInt16 getSizeC20() {
    return sizeof(C20);
}

HsInt16 getAlignmentC20() {
    return alignof(C20);
}

typedef struct S0_1{
    HsInt8 a;
} S0_1;

typedef struct S0_2{
    HsInt16 a;
} S0_2;

typedef union S0_union{
    S0_1 a;
    S0_2 b;
} S0_union;

typedef struct S0 {
    HsWord8 tag;
    S0_union val;
} S0;

S0 * newS0_1(HsInt8 a){
    S0 * ret = (S0*) malloc(sizeof(S0));
    ret->tag = 0;
    ret->val.a.a = a;
    return ret;
}

S0 * newS0_2(HsInt16 a){
    S0 * ret = (S0*) malloc(sizeof(S0));
    ret->tag = 1;
    ret->val.b.a = a;
    return ret;
}

void pokeS0_1(S0* un, HsInt8 a){
    un->tag = 0;
    un->val.a.a = a;
}

void pokeS0_2(S0* un, HsInt16 a){
    un->tag = 1;
    un->val.b.a = a;
}

int checkOffsetsS0_1(HsInt16 *offs){
    int a = offsetof(S0_1, a) == offs[0];
    return a;
}

int checkOffsetsS0_2(HsInt16 *offs){
    int a = offsetof(S0_2, a) == offs[0];
    return a;
}

int checkOffsetsS0(HsInt16 *offs){
    int t = offsetof(S0, tag) == offs[0];
    int v = offsetof(S0, val) == offs[1];
    return t && v;
}

int checkFieldsS0_1(S0_1* s1, S0_1* s2){
    int a = s1->a == s2->a;
    return a;
}

int checkFieldsS0_2(S0_2* s1, S0_2* s2){
    int a = s1->a == s2->a;
    return a;
}

int checkFieldsS0(S0* s1, S0* s2){
    if (s1->tag != s2->tag) return 0;
    if (s1->tag == 0) return checkFieldsS0_1(&s1->val.a,&s2->val.a);
    if (s1->tag == 1) return checkFieldsS0_2(&s1->val.b,&s2->val.b);
    return 0;
}

HsInt16 getSizeS0() {
    return sizeof(S0);
}

HsInt16 getAlignmentS0() {
    return alignof(S0);
}

typedef struct S1_1{
    HsInt32 a;
    HsInt8 b;
} S1_1;

typedef struct S1_2{
    HsDouble a;
    HsInt32 b;
    HsInt8 c;
    HsInt8 d;
} S1_2;

typedef struct S1_3{
    HsFloat a;
} S1_3;

typedef union S1_union{
    S1_1 a;
    S1_2 b;
    S1_3 c;
} S1_union;

typedef struct S1 {
    HsWord8 tag;
    S1_union val;
} S1;

S1 * newS1_1(HsInt32 a, HsInt8 b){
    S1 * ret = (S1*) malloc(sizeof(S1));
    ret->tag = 0;
    ret->val.a.a = a;
    ret->val.a.b = b;
    return ret;
}

S1 * newS1_2(HsDouble a, HsInt32 b, HsInt8 c, HsInt8 d){
    S1 * ret = (S1*) malloc(sizeof(S1));
    ret->tag = 1;
    ret->val.b.a = a;
    ret->val.b.b = b;
    ret->val.b.c = c;
    ret->val.b.d = d;
    return ret;
}

S1 * newS1_3(HsFloat a){
    S1 * ret = (S1*) malloc(sizeof(S1));
    ret->tag = 2;
    ret->val.c.a = a;
    return ret;
}

void pokeS1_1(S1* un, HsInt32 a, HsInt8 b){
    un->tag = 0;
    un->val.a.a = a;
    un->val.a.b = b;
}

void pokeS1_2(S1* un, HsDouble a, HsInt32 b, HsInt8 c, HsInt8 d){
    un->tag = 1;
    un->val.b.a = a;
    un->val.b.b = b;
    un->val.b.c = c;
    un->val.b.d = d;
}

void pokeS1_3(S1* un, HsFloat a){
    un->tag = 2;
    un->val.c.a = a;
}

int checkOffsetsS1_1(HsInt16 *offs){
    int a = offsetof(S1_1, a) == offs[0];
    int b = offsetof(S1_1, b) == offs[1];
    return a && b;
}

int checkOffsetsS1_2(HsInt16 *offs){
    int a = offsetof(S1_2, a) == offs[0];
    int b = offsetof(S1_2, b) == offs[1];
    int c = offsetof(S1_2, c) == offs[2];
    int d = offsetof(S1_2, d) == offs[3];
    return a && b && c && d;
}

int checkOffsetsS1_3(HsInt16 *offs){
    int a = offsetof(S1_3, a) == offs[0];
    return a;
}

int checkOffsetsS1(HsInt16 *offs){
    int t = offsetof(S1, tag) == offs[0];
    int v = offsetof(S1, val) == offs[1];
    return t && v;
}

int checkFieldsS1_1(S1_1* s1, S1_1* s2){
    int a = s1->a == s2->a;
    int b = s1->b == s2->b;
    return a && b;
}

int checkFieldsS1_2(S1_2* s1, S1_2* s2){
    int a = s1->a == s2->a;
    int b = s1->b == s2->b;
    int c = s1->c == s2->c;
    int d = s1->d == s2->d;
    return a && b && c && d;
}

int checkFieldsS1_3(S1_3* s1, S1_3* s2){
    int a = s1->a == s2->a;
    return a;
}

int checkFieldsS1(S1* s1, S1* s2){
    if (s1->tag != s2->tag) return 0;
    if (s1->tag == 0) return checkFieldsS1_1(&s1->val.a,&s2->val.a);
    if (s1->tag == 1) return checkFieldsS1_2(&s1->val.b,&s2->val.b);
    if (s1->tag == 2) return checkFieldsS1_3(&s1->val.c,&s2->val.c);
    return 0;
}

HsInt16 getSizeS1() {
    return sizeof(S1);
}

HsInt16 getAlignmentS1() {
    return alignof(S1);
}

typedef struct S2_1{
    C13 a;
    HsInt8 b;
} S2_1;

typedef struct S2_2{
    C2 a;
    C1 b;
} S2_2;

typedef struct S2_3{
    C0 a;
} S2_3;

typedef union S2_union{
    S2_1 a;
    S2_2 b;
    S2_3 c;
} S2_union;

typedef struct S2 {
    HsWord8 tag;
    S2_union val;
} S2;

S2 * newS2_1(C13* a, HsInt8 b){
    S2 * ret = (S2*) malloc(sizeof(S2));
    ret->tag = 0;
    ret->val.a.a = *a;
    ret->val.a.b = b;
    return ret;
}

S2 * newS2_2(C2* a, C1* b){
    S2 * ret = (S2*) malloc(sizeof(S2));
    ret->tag = 1;
    ret->val.b.a = *a;
    ret->val.b.b = *b;
    return ret;
}

S2 * newS2_3(C0* a){
    S2 * ret = (S2*) malloc(sizeof(S2));
    ret->tag = 2;
    ret->val.c.a = *a;
    return ret;
}

void pokeS2_1(S2* un, C13* a, HsInt8 b){
    un->tag = 0;
    un->val.a.a = *a;
    un->val.a.b = b;
}

void pokeS2_2(S2* un, C2* a, C1* b){
    un->tag = 1;
    un->val.b.a = *a;
    un->val.b.b = *b;
}

void pokeS2_3(S2* un, C0* a){
    un->tag = 2;
    un->val.c.a = *a;
}

int checkOffsetsS2_1(HsInt16 *offs){
    int a = offsetof(S2_1, a) == offs[0];
    int b = offsetof(S2_1, b) == offs[1];
    return a && b;
}

int checkOffsetsS2_2(HsInt16 *offs){
    int a = offsetof(S2_2, a) == offs[0];
    int b = offsetof(S2_2, b) == offs[1];
    return a && b;
}

int checkOffsetsS2_3(HsInt16 *offs){
    int a = offsetof(S2_3, a) == offs[0];
    return a;
}

int checkOffsetsS2(HsInt16 *offs){
    int t = offsetof(S2, tag) == offs[0];
    int v = offsetof(S2, val) == offs[1];
    return t && v;
}

int checkFieldsS2_1(S2_1* s1, S2_1* s2){
    int a = checkFieldsC13(&(s1->a),&(s2->a));
    int b = s1->b == s2->b;
    return a && b;
}

int checkFieldsS2_2(S2_2* s1, S2_2* s2){
    int a = checkFieldsC2(&(s1->a),&(s2->a));
    int b = checkFieldsC1(&(s1->b),&(s2->b));
    return a && b;
}

int checkFieldsS2_3(S2_3* s1, S2_3* s2){
    int a = checkFieldsC0(&(s1->a),&(s2->a));
    return a;
}

int checkFieldsS2(S2* s1, S2* s2){
    if (s1->tag != s2->tag) return 0;
    if (s1->tag == 0) return checkFieldsS2_1(&s1->val.a,&s2->val.a);
    if (s1->tag == 1) return checkFieldsS2_2(&s1->val.b,&s2->val.b);
    if (s1->tag == 2) return checkFieldsS2_3(&s1->val.c,&s2->val.c);
    return 0;
}

HsInt16 getSizeS2() {
    return sizeof(S2);
}

HsInt16 getAlignmentS2() {
    return alignof(S2);
}

typedef struct S3_1{
    S1 a;
} S3_1;

typedef struct S3_2{
    S2 a;
} S3_2;

typedef struct S3_3{
    HsInt8 a;
} S3_3;

typedef union S3_union{
    S3_1 a;
    S3_2 b;
    S3_3 c;
} S3_union;

typedef struct S3 {
    HsWord8 tag;
    S3_union val;
} S3;

S3 * newS3_1(S1* a){
    S3 * ret = (S3*) malloc(sizeof(S3));
    ret->tag = 0;
    ret->val.a.a = *a;
    return ret;
}

S3 * newS3_2(S2* a){
    S3 * ret = (S3*) malloc(sizeof(S3));
    ret->tag = 1;
    ret->val.b.a = *a;
    return ret;
}

S3 * newS3_3(HsInt8 a){
    S3 * ret = (S3*) malloc(sizeof(S3));
    ret->tag = 2;
    ret->val.c.a = a;
    return ret;
}

void pokeS3_1(S3* un, S1* a){
    un->tag = 0;
    un->val.a.a = *a;
}

void pokeS3_2(S3* un, S2* a){
    un->tag = 1;
    un->val.b.a = *a;
}

void pokeS3_3(S3* un, HsInt8 a){
    un->tag = 2;
    un->val.c.a = a;
}

int checkOffsetsS3_1(HsInt16 *offs){
    int a = offsetof(S3_1, a) == offs[0];
    return a;
}

int checkOffsetsS3_2(HsInt16 *offs){
    int a = offsetof(S3_2, a) == offs[0];
    return a;
}

int checkOffsetsS3_3(HsInt16 *offs){
    int a = offsetof(S3_3, a) == offs[0];
    return a;
}

int checkOffsetsS3(HsInt16 *offs){
    int t = offsetof(S3, tag) == offs[0];
    int v = offsetof(S3, val) == offs[1];
    return t && v;
}

int checkFieldsS3_1(S3_1* s1, S3_1* s2){
    int a = checkFieldsS1(&(s1->a),&(s2->a));
    return a;
}

int checkFieldsS3_2(S3_2* s1, S3_2* s2){
    int a = checkFieldsS2(&(s1->a),&(s2->a));
    return a;
}

int checkFieldsS3_3(S3_3* s1, S3_3* s2){
    int a = s1->a == s2->a;
    return a;
}

int checkFieldsS3(S3* s1, S3* s2){
    if (s1->tag != s2->tag) return 0;
    if (s1->tag == 0) return checkFieldsS3_1(&s1->val.a,&s2->val.a);
    if (s1->tag == 1) return checkFieldsS3_2(&s1->val.b,&s2->val.b);
    if (s1->tag == 2) return checkFieldsS3_3(&s1->val.c,&s2->val.c);
    return 0;
}

HsInt16 getSizeS3() {
    return sizeof(S3);
}

HsInt16 getAlignmentS3() {
    return alignof(S3);
}

typedef struct S4_1{
    C0 a;
} S4_1;

typedef struct S4_2{
    C0 a;
} S4_2;

typedef struct S4_3{
    C0 a;
} S4_3;

typedef struct S4_4{
    C1 a;
} S4_4;

typedef union S4_union{
    S4_1 a;
    S4_2 b;
    S4_3 c;
    S4_4 d;
} S4_union;

typedef struct S4 {
    HsWord8 tag;
    S4_union val;
} S4;

S4 * newS4_1(C0* a){
    S4 * ret = (S4*) malloc(sizeof(S4));
    ret->tag = 0;
    ret->val.a.a = *a;
    return ret;
}

S4 * newS4_2(C0* a){
    S4 * ret = (S4*) malloc(sizeof(S4));
    ret->tag = 1;
    ret->val.b.a = *a;
    return ret;
}

S4 * newS4_3(C0* a){
    S4 * ret = (S4*) malloc(sizeof(S4));
    ret->tag = 2;
    ret->val.c.a = *a;
    return ret;
}

S4 * newS4_4(C1* a){
    S4 * ret = (S4*) malloc(sizeof(S4));
    ret->tag = 3;
    ret->val.d.a = *a;
    return ret;
}

void pokeS4_1(S4* un, C0* a){
    un->tag = 0;
    un->val.a.a = *a;
}

void pokeS4_2(S4* un, C0* a){
    un->tag = 1;
    un->val.b.a = *a;
}

void pokeS4_3(S4* un, C0* a){
    un->tag = 2;
    un->val.c.a = *a;
}

void pokeS4_4(S4* un, C1* a){
    un->tag = 3;
    un->val.d.a = *a;
}

int checkOffsetsS4_1(HsInt16 *offs){
    int a = offsetof(S4_1, a) == offs[0];
    return a;
}

int checkOffsetsS4_2(HsInt16 *offs){
    int a = offsetof(S4_2, a) == offs[0];
    return a;
}

int checkOffsetsS4_3(HsInt16 *offs){
    int a = offsetof(S4_3, a) == offs[0];
    return a;
}

int checkOffsetsS4_4(HsInt16 *offs){
    int a = offsetof(S4_4, a) == offs[0];
    return a;
}

int checkOffsetsS4(HsInt16 *offs){
    int t = offsetof(S4, tag) == offs[0];
    int v = offsetof(S4, val) == offs[1];
    return t && v;
}

int checkFieldsS4_1(S4_1* s1, S4_1* s2){
    int a = checkFieldsC0(&(s1->a),&(s2->a));
    return a;
}

int checkFieldsS4_2(S4_2* s1, S4_2* s2){
    int a = checkFieldsC0(&(s1->a),&(s2->a));
    return a;
}

int checkFieldsS4_3(S4_3* s1, S4_3* s2){
    int a = checkFieldsC0(&(s1->a),&(s2->a));
    return a;
}

int checkFieldsS4_4(S4_4* s1, S4_4* s2){
    int a = checkFieldsC1(&(s1->a),&(s2->a));
    return a;
}

int checkFieldsS4(S4* s1, S4* s2){
    if (s1->tag != s2->tag) return 0;
    if (s1->tag == 0) return checkFieldsS4_1(&s1->val.a,&s2->val.a);
    if (s1->tag == 1) return checkFieldsS4_2(&s1->val.b,&s2->val.b);
    if (s1->tag == 2) return checkFieldsS4_3(&s1->val.c,&s2->val.c);
    if (s1->tag == 3) return checkFieldsS4_4(&s1->val.d,&s2->val.d);
    return 0;
}

HsInt16 getSizeS4() {
    return sizeof(S4);
}

HsInt16 getAlignmentS4() {
    return alignof(S4);
}

