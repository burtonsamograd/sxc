/*
 * list.c
 *
 * Function definitons for the declerations found in list.h
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

/* Helpful when dealing with C++ compilers */
#ifdef __cplusplus
extern "C" {
#endif

/* Standard C includes */
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdio.h>
  
#include "list.h"

/* Uncomment the following line for debug memory managment */
/* #define DEBUG_MM */

/* Uncommnet the following line to use system memory mananagment */
/* #define USE_MALLOC */

/* List Element structure */
typedef struct tagELEMENT {
	PVOID Data;
	struct tagELEMENT *Next;
	struct tagELEMENT *Prev;
#ifndef USE_MALLOC
	HELEMENT NextElement;	/*Next free element */
	
	#ifdef DEBUG_MM
		long Allocated;	/* Allocated flag */
	#endif /* DEBUG_MM */

#endif /* USE_MALLOC */
} ELEMENT;

/* Main list structure */
typedef struct tagLIST {
	HELEMENT Head;
	HELEMENT Tail;
	HELEMENT Current;
	long NumElements;	

#ifndef USE_MALLOC
	struct tagLIST *NextList; /* Next free list */

	#ifdef DEBUG_MM
		long Allocated;	/* Allocated flag */
	#endif /* DEBUG_MM */

#endif /* USE_MALLOC */
} LIST;			

/* Global Variables for memory managment */
#ifndef USE_MALLOC
static LIST Lists[MAX_LIST_HEADS];
static ELEMENT Elements[MAX_LIST_ELEMENTS];

static HLIST ListHead, ListTail;
static HELEMENT ElementHead, ElementTail;
#endif /* USE_MALLOC */

/* Module functions */
HELEMENT ListNewElement(void);
void ListFreeElement(HELEMENT hElement);
HLIST ListNewList(void);
void ListFreeList(HLIST hList);
long ListAddFirstElement(HLIST hList, PVOID Item);

/* 
	Function used to initialize the memory managment
	routines for list lists and elements 
	Must be called first if USE_MALLOC is not defined.

	Returns 0 if sucessful, or -1 if some error occurs.
*/
long ListInit(void)
{
#ifndef USE_MALLOC
	long i;

	/* 
		Initialize memory managment head and tail pointers
		for lists and elements
	*/
	ListHead = Lists;
	ListTail = Lists+(sizeof(Lists)/sizeof(LIST))-1;

	ElementHead = Elements;
	ElementTail = Elements+(sizeof(Elements)/sizeof(ELEMENT))-1;

	/* 
		Initialize the next pointers for the list and elements
		array.
	*/
	for(i=0;i<((sizeof(Lists)/sizeof(LIST))-1);i++)
		Lists[i].NextList = &Lists[i+1];
	Lists[i].NextList = NULL;

	for(i=0;i<((sizeof(Elements)/sizeof(ELEMENT))-1);i++)
		Elements[i].NextElement = &Elements[i+1];
	Elements[i].NextElement = NULL;
#else
	/* Init needed for malloc memory managment */
#endif
	return 0;
}

/* 
	Allocates a new element structure 
    Returns ponter to struct, else NULL
*/
HELEMENT ListNewElement(void)
{
	HELEMENT Temp;

#ifndef USE_MALLOC
	/* 
		Make temp the first available list, if there are no lists
		left, ListHead will be null. 
	*/
	Temp = ElementHead;

#ifdef DEBUG_MM
	assert(Temp->Allocated == 0);
#endif

	/* 
		If there were any lists left (ie ListHead is non-null, 
		move the head of the list to the next  available element 
	*/
	if(ElementHead)
		{
		/*
			Move the first available element to the next one in LL.
			If there are no available elements, then ElementHead will
			be made equal to null for the next allocation 
		*/
		ElementHead = ElementHead->NextElement;
		}
	else
		{
		/* 
			Set the tail to NULL, as there are no elements to be 
			allocated anyways, and its just safe to do so.
		*/
		ElementTail = NULL;
		}
	
#else
	/* Use malloc to allocate a new list structure */
	Temp = (HELEMENT)malloc(sizeof(ELEMENT));
#endif

	/* If allocation was successful... */
	if(Temp)
		{
		memset(Temp, 0, sizeof(ELEMENT));

#ifdef DEBUG_MM
		/* Set the allocated flag for the struct */
		Temp->Allocated = 1;
#endif
		}

	return Temp;
}

/*
	Frees an allocated list element.
*/
void ListFreeElement(HELEMENT hElement)
{
#ifndef USE_MALLOC

#ifdef DEBUG_MM
	assert(hElement->Allocated == 1);
#endif

	/* 
		If elementhead is non-null, all elements
		have not been allocated.
	*/
	if(ElementHead)
		{
		/* 
			Have the last available element point to
			to the freed element and have the element
			be the new tail.			
		*/
		ElementTail->NextElement = hElement;
		}
	else
		{
		/* 
			If all nodes were allocated, then make the
			head and the tail point to the node that was
			just freed
		*/
		ElementHead = hElement;
		}

	/* Set the tail to the element that is being freed */
	ElementTail = hElement;
	
	/* Just to make sure... */
	ElementTail->NextElement = NULL;

#ifdef DEBUG_MM
	/* Clear the allocated flag for the structure */
	hElement->Allocated = 0;
#endif

#else
	free(hElement);
#endif
}

/* Allocates a new list structure 
   Returns ponter to struct, else NULL
*/
HLIST ListNewList(void)
{
	HLIST Temp;

#ifndef USE_MALLOC 
	/* 
		Make temp the first available list, if there are no lists
		left, ListHead will be null. 
	*/
	Temp = ListHead;

#ifdef DEBUG_MM
	assert(Temp->Allocated == 0);
#endif

	/* 
		If there were any lists left (ie ListHead is non-null, 
		move the head of the list to the next  available List 
	*/
	if(ListHead)
		{
		/*
			Move the first available List to the next one in LL.
			If there are no available Lists, then ListHead will
			be made equal to null for the next allocation 
		*/
		ListHead = ListHead->NextList;
		}
	else
		{
		/* 
			Set the tail to NULL, as there are no Lists to be 
			allocated anyways, and its just safe to do so.
		*/
		ListTail = NULL;
		}

#ifdef DEBUG_MM
	/* Set the allocated flag for the structure */
	Temp->Allocated = 1;
#endif

#else
	/* Allocate a new list structure using dynamic MM */
	Temp = (HLIST)malloc(sizeof(LIST));
#endif
	
	/* If allocation was successful... */
	if(Temp)
		{
		memset(Temp, 0, sizeof(LIST));
		}
	
	return Temp;
}

void ListFreeList(HLIST hList)
{
#ifndef USE_MALLOC
	/* 
		If Listhead is non-null, all Lists
		have not been allocated.
	*/
	if(ListHead)
		{
		/* 
			Have the last available List point to
			to the freed List and have the List
			be the new tail.			
		*/
		ListTail->NextList = hList;
		ListTail = hList;
		}
	else
		{
		/* 
			If all nodes were allocated, then make the
			head and the tail point to the node that was
			just freed
		*/
		ListHead = hList;
		ListTail = ListHead;
		}
#else
	/* Use system MM to free structure */
	free(hList);
#endif
}

/* 
 * Creates a new list, if space is available.
 * If there are no more lists left in the pool,
 * NULL is returned, else a pointer to the list 
 * is returned 
 */
HLIST ListCreate(void)
{
	/* Allocate a new list */
	return ListNewList();
}

/*
	Returns the number of elements contained in
	hList, or else -1 on a null parameter 
*/
long ListCount(HLIST hList)
{
	assert(hList);
	
	/* Return the number of items in the list */
	return hList->NumElements;
}

/*
	Returns the first item in the given list and
	updates the current pointer in the list to 
	point to the first item.

	If the list pointer is null or there are 
	no items in the list, NULL is returned.
*/
PVOID ListFirst(HLIST hList)
{
	assert(hList);

	/* Set the current item to the head of the list */
	hList->Current = hList->Head;
	
	/* Return the contents of that item */
	return ListCurr(hList);
}					

/*
	Returns the last item in the given list and
	updates the current pointer in the list to 
	point to the last item.

	If the list pointer is null or there are 
	no items in the list, NULL is returned.
*/
PVOID ListLast(HLIST hList)
{
	assert(hList);

	/* Set the current item to the tail of the list */
	hList->Current = hList->Tail;

	/* Return the contents of that item */
	return ListCurr(hList);
}

/*
	Returns the next item in the given list and
	updates the current pointer in the list to 
	point to the next item.

	If the list pointer is null or there are 
	no items in the list, NULL is returned.
*/
PVOID ListNext(HLIST hList)
{
	assert(hList);
	
	/* If there is a current item */
	if(hList->Current)
		{
		/* Set the current item to the next item in the list */
		hList->Current = hList->Current->Next;
		
		/* return the data contained in the item */
		return ListCurr(hList);
		}

	/* No current item */
	return NULL;
}

/*
	Returns the previous item in the given list and
	updates the current pointer in the list to 
	point to the previous item.

	If the list pointer is null or there are 
	no items in the list, NULL is returned.
*/
PVOID ListPrev(HLIST hList)
{
	assert(hList);

	/* if there is a current item ... */
	if(hList->Current)
		{
		/* Set the current item to the previous item in the list */
		hList->Current = hList->Current->Prev;

		/* Return the data contained in the item */
		return ListCurr(hList);
		}

	return NULL;
}

/*
	Returns the current item in the given list.

	If the list pointer is null or there are 
	no items in the list, NULL is returned.
*/
PVOID ListCurr(HLIST hList)
{
	assert(hList);

	/* If there is a current element */
	if(hList->Current)
		{
		/* return the data contained in it */
		return hList->Current->Data;
		}

	/* No current element */
	return NULL;
}

/* Adds the first element to the list, updates head and
   tail pointers and sets the current pointer to the
   only element in the list.

   Returns 0 on success, or -1 otherwise...
*/
long ListAddFirstElement(HLIST hList, PVOID Item)
{
	HELEMENT Temp;

	assert(hList);

	/* Check that there are no elements in the list */
	if(hList->NumElements == 0)
		{
		/* Allocate a new element */
		Temp = ListNewElement();

		/* If the allocation is successful...*/
		if(Temp)
			{
			/* Update the list pointers to have one element */			
			Temp->Data = Item;

			hList->Head = Temp;
			hList->Tail = Temp;
			hList->Current = Temp;

			/* Set the number of elements to be one */
			hList->NumElements = 1;

			/* Return success */
			return 0;
			}
		}

	return -1;
}

/* 
	Adds an item to the end of the given list 
*/
long ListAppend(HLIST hList, PVOID Item)
{
	assert(hList);
	
	/* Point to the tail of the list */
	hList->Current = hList->Tail;
	
	/* Insert the item after the tail of the list */
	return ListAdd(hList, Item);	
}

long ListPrepend(HLIST hList, PVOID Item)
{
	assert(hList);

	/* Point to the head of the list */
	hList->Current = hList->Head;
	
	/* Insert the item before the head of the list */
	return ListInsert(hList, Item);	
}

/*
	Adds an item into the list after the position of the current
	ponter.  If there is no current pointer (ie. it is null), the
	item will be added at the tail of the list.

	The current pointer is set to the position of the new item.

	Returns 0 on sucess, or -1 on failure.
*/
long ListAdd(HLIST hList, PVOID Item)
{
	HELEMENT Temp;

	assert(hList);

	if((hList->Head == NULL) && (hList->Tail == NULL))
		{
		/* Should only be if there are no elements in the list */
		assert(hList->NumElements == 0);

		/* Add the item as the first item in the list and return*/
		return ListAddFirstElement(hList, Item);
		}
	
	if(hList->Current == NULL)
		{
		/* Set the current pointer to the tail of the list */
		hList->Current = hList->Tail;
		}
	
	/* Allocate a new element */
	Temp = ListNewElement();

	/* If allocation was successful...*/
	if(Temp)
		{
		/* Set the data for the item */
		Temp->Data = Item;

		/* Check if current is the tail of the list */
		if(hList->Current == hList->Tail)
			{
			/* The item will be the new tail for the list */
			//assert(hList->Current->Prev);

			/* Set the pointers for the new element */
			hList->Tail->Next = Temp;
			Temp->Prev = hList->Tail;
			Temp->Next = NULL;

			/* Set the new tail for the list */
			hList->Tail = Temp;
			}
		else
			{
			/* The item is not the tail of the list */
			
			/* 
				Update the pointers to inset the item into
				the list after the current item 
			*/
			Temp->Next = hList->Current->Next;
			Temp->Prev = hList->Current;
			Temp->Next->Prev = Temp;
			Temp->Prev->Next = Temp;
			}
		
		/* Set the current pointer for the list to the new element */
		hList->Current = Temp;

		/* Incriment the number of items in the list */
		hList->NumElements++;
		
		/* Return success */
		return 0;
		}

	return -1;
}

/*
	Adds an item into the list before the position of the current
	ponter.  If there is no current pointer (ie. it is null), the
	item will be added at the head of the list.

	The current pointer is set to the position of the new item.

	Returns 0 on sucess, or -1 on failure.
*/
long ListInsert(HLIST hList, PVOID Item)
{
	HELEMENT Temp;

	assert(hList);

	if((hList->Head == NULL) && (hList->Tail == NULL))
		{
		/* Should only be if there are no elements in the list */
		assert(hList->NumElements == 0);

		/* Add the item as the first item in the list and return*/
		return ListAddFirstElement(hList, Item);
		}
	
	if(hList->Current == NULL)
		{
		/* Set the current pointer to the head of the list */
		hList->Current = hList->Head;
		}
	
	/* Allocate a new element */
	Temp = ListNewElement();

	/* If allocation was successful...*/
	if(Temp)
		{
		/* Set the data for the item */
		Temp->Data = Item;

		/* Check if current is the head of the list */
		if(hList->Current == hList->Head)
			{
			/* The item will be the new head for the list */

			/* Set the pointers for the new element */
			hList->Head->Prev = Temp;
			Temp->Next = hList->Head;
			Temp->Prev = NULL;

			/* Set the new head for the list */
			hList->Head = Temp;
			}
		else
			{
			/* The item is not the head of the list */
			
			/* 
				Update the pointers to inset the item into
				the list before the current item 
			*/
			Temp->Next = hList->Current;
			Temp->Prev = hList->Current->Prev;
			Temp->Next->Prev = Temp;
			Temp->Prev->Next = Temp;
			}
		
		/* Set the current pointer for the list to the new element */
		hList->Current = Temp;
		
		/* Incriment the number of items in the list */
		hList->NumElements++;
		
		/* Return success */
		return 0;
		}

	return -1;
}

/* 
	Removes the current item from the list and returns a 
	pointer to it.

	Returns null if there is no current item (ie. it is null),
	or if the list pointer is null.
*/
PVOID ListRemove(HLIST hList)
{
	assert(hList);
	
	if(hList->Current)
		{
		HELEMENT Temp;
		PVOID TempData;

		/* Check that there are elements in the list */
		assert(hList->NumElements > 0);

		/* Get a pointer to the current item */
		Temp = hList->Current;

		/* The current element is in the middle of the list */
		if(Temp != hList->Head && Temp != hList->Tail)
		  {
			/* Make the current equal to the next element */
			assert(Temp->Next != NULL);
			hList->Current = Temp->Next;
		  }

		/* If current was at head of the list */
		if(Temp == hList->Head)
			{
			/* Make the head of the list point the 
			   next item in the list */
			hList->Head = Temp->Next;

			/* Make the current item the next item in 
			   the list */
			hList->Current = hList->Head;
			}
		
		/* If current was the last element in the list */
		if(Temp == hList->Tail)
			{
			/* Make the tail of the list point to the new 
			   last item in the list */
			hList->Tail= Temp->Prev;

			/* Make the current pointer null, to be consistent
			   with the rest of the remove code */
			hList->Current = NULL;
			}
		
		/* Remove the item from the list by updating the
		   items previous and next item's prev and next pointers */
		if(Temp->Next)
			{
			/* Updates pointers for old list head or element
			   in center of list */
			Temp->Next->Prev = Temp->Prev;
			}

		if(Temp->Prev)
			{
			/* Updates pointers for old list tail or element
			   in center of list */
			Temp->Prev->Next= Temp->Next;
			}
		
		/* Void the old pointers */
		Temp->Next = NULL;
		Temp->Prev = NULL;
		
		/* Decriment the number of elements */
		hList->NumElements--;
		
		/* Get the data from the item */
		TempData = Temp->Data;

		/* Free the element */
		ListFreeElement(Temp);
		
		/* return a pointer to the element data*/
		return TempData;
		}

	return NULL;
}

/*
	Returns and removes the last item from the list.

	Returns null on error
*/
PVOID ListTrim(HLIST hList)
{
	assert(hList);

	/* Set the current item to the tail of the list */
	hList->Current = hList->Tail;

	/* Remove and return the current(last) item in the list */
	return ListRemove(hList);
}


/* 
	Function to be used of no free function was 
	given for pfFree in the next routine 
*/
void DefaultFree(HELEMENT Temp)
{
	assert(Temp);	
}

/*
	Deletes all of the elements in a list, calling a 
	given free function for each item, and then
	frees the list structure 
	If null is given for the free function none 
	is called and the data is assumed not to be a
	pointer to allocated data
*/
void ListFree(HLIST hList, ElementFreeFunc pfFree)
{	
	HELEMENT hElement;

	assert(hList);
	
	/* Free all of the elements in the list */
	hElement = hList->Head;

	/* If no function was given, set to default */
	if(pfFree == NULL)
		pfFree = DefaultFree;
		
	/* For all of the items in the list */
	while(hElement)
		{
		/* Point to the next element in the list with current */
		hList->Current = hElement->Next;

		/* Call the free function for the element*/
		pfFree(hElement);

		/* Free the element structure */
		ListFreeElement(hElement);

		/* Point at the next element */
		hElement = hList->Current;

		/* Just decriment it just in case */
		hList->NumElements--;
		}

	/* Free the list structure */
	ListFreeList(hList);
}

void ListConcat(HLIST hListOne, HLIST hListTwo)
{
	assert(hListOne && hListTwo);

	/* Check if first list is empty */
	if(hListOne->NumElements > 0)
		{
		/* if it is not empty... */

		/* If the second list is not empty */
		if(hListTwo->NumElements >0)
			{
			/* Update the pointers */
			hListOne->Tail->Next = hListTwo->Head;
			hListTwo->Head->Prev = hListOne->Tail;

			/* Update the tail pointer to the end of the second list */
			hListOne->Tail = hListTwo->Tail;
			}
		/* Else do nothing */
		}
	else
		{
		/* If it is empty... */
	
		if(hListTwo->NumElements >0)
			{
			/* If the second list is not empty */
			hListOne->Head = hListTwo->Head;
			hListOne->Tail = hListTwo->Tail;
			}
		/* Else do nothing */
		}

	/* Free the structure for the second list */
	ListFreeList(hListTwo);
}

/*
	Default compare function for when people pass NULL as 
	pfComp in the next routine
*/
long DefaultComp(HELEMENT One, PVOID Data)
{
	if(One->Data == Data)
		return 1;
	else
		return 0;
}

/*
	Searches the list forward from the the current item in the list.
	Compares the contents of each item with the given data using the 
	compare function given.

	Returns the item that contained the data if it is found, else
	it returns null.
*/
PVOID ListSearch(HLIST hList, ElementCompFunc pfComp, PVOID CompData)
{
	assert(hList);

	/* Set the default function if none was given */
	if(pfComp == NULL)
		pfComp = DefaultComp;

	/* for all of the items in the list past the current... */
	while(hList->Current)
		{
		/* Compare the contents of the current item to the data */
		if(pfComp(hList->Current, CompData))
			{
			/* If matches, then return data */
			return ListCurr(hList);
			}
		else
			{
			/* Else go to the next item */
			ListNext(hList);
			}
		}

	return NULL;
}

#ifdef __cplusplus
}
#endif

/*
 * Returns the data pointer for the given element
 */
PVOID ElementGet(HELEMENT hElement)
{
  assert(hElement);

  /* Return the data pointer */
  return hElement->Data;
}
/*
 * Burt Samograd
 * Student Number : 12216933
 * User ID : q4j1
 */


