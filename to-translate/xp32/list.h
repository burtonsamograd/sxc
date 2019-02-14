/*
 * list.h
 *
 * Header file for list routines.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Burt Samograd
 * Copywrite 1995
 */

#ifndef _LIST_H
#define _LIST_H

/* Helpful when dealing with C++ compilers */
#ifdef __cplusplus
extern "C" {
#endif

/* Some handy typedefs */
typedef void * PVOID;

/* Keep data structures hidden */
typedef struct tagLIST* HLIST;
typedef struct tagELEMENT* HELEMENT;

/* Function pointer typedefs */
typedef void(*ElementFreeFunc)(HELEMENT);
typedef long (*ElementCompFunc)(HELEMENT, PVOID);

/* Size Limits for implimentation */
#define MAX_LIST_HEADS		16
#define MAX_LIST_ELEMENTS	128

/* List Function declerations */

/* Must be called before any routines are called */
long ListInit(void);

HLIST ListCreate(void);

long ListCount(HLIST hList);

PVOID ListFirst(HLIST hList);
PVOID ListLast(HLIST hList);
PVOID ListNext(HLIST hList);
PVOID ListPrev(HLIST hList);
PVOID ListCurr(HLIST hList);

long ListAdd(HLIST hList, PVOID Item);
long ListInsert(HLIST hList, PVOID Item);
long ListAppend(HLIST hList, PVOID Item);
long ListPrepend(HLIST hList, PVOID Item);

PVOID ListRemove(HLIST hList);
void ListConcat(HLIST hListOne, HLIST hListTwo);
void ListFree(HLIST hList, ElementFreeFunc pfFree);
PVOID ListTrim(HLIST hList);
PVOID ListSearch(HLIST hList, ElementCompFunc pfComp, PVOID CompData);

#ifdef __cplusplus
}
#endif

#endif /* _LIST_H */

/*
 * Burt Samograd
 * Student Number : 12216933
 * User ID : q4j1
 */




