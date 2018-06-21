/*
 * window.c
 *
 * Overlapping window function definitions.
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
 * Copywrite 1997
 */

/* C includes */
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>

/* Local includes */
#include "window.h"

/* Window structure definition */

/* System global variables */
static WINDOW MainWindow;

/* Initialize the windowing system to use a rendering buffer */
int InitWindowSystem(unsigned char *Buffer, int Width, int Height)
{

  /* Allocate a window for the main window */
  MainWindow = NewWindow(Width, Height, 0, 0, NULL, NULL, 0);
  if(MainWindow == NULL)
	return -1;
  
  /* Set the main window surface (which is the buffer provided */
  MainWindow->Surface = Buffer;
  MainWindow->Pitch = MainWindow->Width;
  
  return 0;
}

/* Create a window structure */
WINDOW NewWindow(int Width, int Height, int PosX, int PosY, 
				  WINDOW Parent, void *Data, int Flags)
{
   WINDOW Win;

  /* Allocate the window structure */
  Win = (WINDOW)malloc(sizeof(*Win));
  if(Win == NULL) return NULL;
  memset(Win, 0, sizeof(*Win));

  /* Windows must always be a multiple of 4 in width */
  assert((Width % 4) == 0);

  /* Initialize the fields of the structure */
  Win->Width = Width;
  Win->Height = Height;
  Win->PosX = PosX;
  Win->PosY = PosY;
  Win->Parent = Parent;
  Win->Flags = Flags;
  Win->Data = Data;

  /* If the parent window was given */
  if(Parent)
	{
	  /* If the parent already has children */
	  if(Parent->Child)
		{
		  WINDOW Child = Parent->Child;

		  /* Traverse to the end of the next list for the children */
		  while(Child->Next)
			Child = Child->Next;

		  /* Put the new window at the end of the next list */
		  Child->Next = Win;
		  Win->Next = NULL;
		}
	  else
		{
		  /* Just make the window the sole child */
		  Parent->Child = Win;
		  Win->Next = NULL;
		}
	}

  /* Allocate or calculate the image buffer */
  if(Flags & WINDOW_OFFSCREEN)
	{
	  if(Flags & WINDOW_ON_PARENT)
		{
		  /* If the window is to be on the parent, it shares the 
			 buffer with it */
		  assert(Parent);
		  Win->Pitch = Parent->Pitch;
		  Win->Surface = Parent->Surface + PosX + PosY*Parent->Pitch;
		}
	  else
		{
		  /* Window is seperate, allocate a new buffer */
		  Win->Pitch = Win->Width;
		  Win->Surface = (unsigned char*)calloc(Width * Height, 1);
		  assert(Win->Surface);
		}
	}
  else if(Flags & WINDOW_ONSCREEN)
	{
	  Win->Pitch = MainWindow->Pitch;
	  Win->Surface = MainWindow->Surface + PosX + PosY*MainWindow->Pitch;
	}
  
  /* Allocate the stencil buffer if one is desired */
  if(Flags & WINDOW_STENCIL)
	{
	  Win->Stencil = (unsigned char*)calloc(Width*Height, 1);
	  assert(Win->Stencil);
	}

  return Win;
}

/* Free a window and all of its children */
void FreeWindow(WINDOW *Window)
{
  if((*Window)->Flags & WINDOW_OFFSCREEN)
	free((*Window)->Surface);

  if((*Window)->Flags & WINDOW_STENCIL)
	free((*Window)->Stencil);
		 
  free(*Window);
  *Window = NULL;

  return;
}

/* Clear the window its background color */
void ClearWindow(WINDOW Win)
{
  unsigned long Color;

  /* Make a long value of the background color */
  Color = (Win->BackgroundColor) |
	(Win->BackgroundColor<<8) |
	(Win->BackgroundColor<<16) |
	(Win->BackgroundColor<<24);

  /* Clear the surface to the color */
  if(Win->Flags & WINDOW_ONSCREEN || Win->Flags & WINDOW_ON_PARENT)
	{
	  int i;
	  unsigned char *Buffer = Win->Surface;
	  int WidthDivFour = Win->Width/4;

	  /* Window might be smaller than the main window, so need to clear it
		 for every scan line */
	  for(i=Win->Height;i>0;--i)
		{
#if 1		  
		  while(WidthDivFour--) {
		    *(unsigned long*)Buffer++ = Color;
		  }
#else
		  /* Inline asm to speed things up by copying longs*/
		  asm ("cld\n\t"
			   "rep\n\t"
			   "stosl"
			   : /* no output registers */
			   : "D" (Buffer),
			   "c" (WidthDivFour),
			   "a" (Color)
			   : "%edi", "%ecx", "%eax");
		  Buffer += Win->Pitch;
#endif
		}
	}
  else
	{
	  int Size = (Win->Width * Win->Height)/4;

	  /* Inline asm to speed things up by copying longs */
#if 1
	  unsigned long* x = (unsigned long*)Win->Surface;
	  while(Size--) {
	    *x++ = Color;
	  }

#else
	  asm ("cld\n\t"
		   "rep\n\t"
		   "stosl"
		   : /* no output registers */
		   : "D" (Win->Surface),
		   "c" (Size),
		   "a" (Color)
		   : "%edi", "%ecx", "%eax");
#endif
	}

  Win->CursorX = Win->StartCol;
  Win->CursorY = Win->StartRow;
}

/* Move a window and all of its child windows */
void MoveWindow(WINDOW Window, int x, int y)
{
  WINDOW Child = Window->Child;
  WINDOW Parent = Window->Parent;

  /* Update the current window */
  Window->PosX += x;
  Window->PosY += y;

  /* Update the surface pointer */
  Window->Surface = Parent->Surface + Window->PosX + 
	Window->PosY*Parent->Pitch;

  /* Go through all of the child windows */
  while(Child)
	{
	  MoveWindow(Child, x, y);
	  Child = Child->Next;
	}
}

/* Size a window to the given dimensions, updating buffers and
    such. Returns -1 on failure */
int SizeWindow(WINDOW Window, int w, int h)
{

  /* If this is an offscreen window */
  if(Window->Flags & WINDOW_OFFSCREEN)
	{
	  /* If the new required buffer is smaller for an offscreen window */
	  if(w*h <= Window->Width*Window->Height)
		{
		  /* Just use the old buffer and update some variables */
		  Window->Pitch = Window->Width;
		}
	  else
		{
		  /* Free the old buffer */
		  free(Window->Surface);
		  
		  /* Allocate a new surface buffer */
		  Window->Surface = (unsigned char *)malloc(w*h);
		  if(Window->Surface == NULL) return -1;
		  Window->Pitch = w;
		}
	}

  /* Don't need to do any buffer stuff with onscreen windows 
	 as they use the onscreen buffer */
    
  /* Set the width and height of the new window */
  Window->Width = w;
  Window->Height = h;

  return 0;
}

/* Scroll a window up by the given number of lines */
void ScrollWindowUp(WINDOW Win, int Lines)
{
	int i;
	unsigned char *SourceBuffer = Win->Surface + Lines*Win->Pitch;
	unsigned char *DestBuffer = Win->Surface;
	int WidthDivFour = Win->Width/4;

	for(i=Win->Height-Lines;i>0;--i)
		{
#if 1
		  while(WidthDivFour--) {
		    *(unsigned long*)DestBuffer++ = *(unsigned long*)SourceBuffer++;
		  }
#else
		  asm ("cld\n\t"
			   "rep\n\t"
			   "movsl"
			   : /* no output registers */
			   : "D" (DestBuffer),
			   "c" (WidthDivFour),
			   "S" (SourceBuffer)
			   : "%edi", "%ecx", "%esi");
		SourceBuffer += Win->Pitch;
		DestBuffer += Win->Pitch;
#endif		
		}
	DestBuffer -= Win->Pitch;

	for(i=Lines;i>0;--i)
		{
#if 1
		  while(WidthDivFour--) {
		    *(unsigned long*)DestBuffer++ = *(unsigned long*)SourceBuffer++;
		  }
#else
		  asm ("rep\n\t"
			   "stosl"
			   : /* No output registers */
			   : "D" (DestBuffer),
			   "c" (WidthDivFour)
			   : "%edi", "%ecx");
		  DestBuffer += Win->Pitch;
#endif
		}
}

/* Copy the contents of an offscreen window to the screen */
void CopyToScreen(WINDOW Win)
{
  int i;
  int WidthDivFour = Win->Width/4;
  unsigned char *SourceBuffer = Win->Surface;
  unsigned char *DestBuffer = MainWindow->Surface + 
	Win->PosX + Win->PosY*MainWindow->Pitch;

  for(i=Win->Height;i>0;--i)
	{
	  /* A little bit of inline assembly. The windows will always
		 be a multiple of 4 in width, so this will speed things
		 up quite a bit */
#if 1		  
		  while(WidthDivFour--) {
		    *(unsigned long*)DestBuffer++ = *(unsigned long*)SourceBuffer++;
		  }
#else
	  asm ("cld\n\t"
		   "rep\n\t"
		   "movsl"
		   : /* no output registers */
		   : "D" (DestBuffer),
		   "c" (WidthDivFour),
		   "S" (SourceBuffer)
		   : "%edi", "%ecx", "%esi");
#endif
	  SourceBuffer += Win->Pitch;
	  DestBuffer += MainWindow->Pitch;
	}
}

/* Copy the window to the screen using the window's masking stencil */
void StencilCopyToScreen(WINDOW Win)
{
  int i, j;
  int WidthDivFour = Win->Width/4;
  unsigned long Pixels, Mask;
  unsigned char *SourceBuffer = Win->Surface;
  unsigned char *StencilBuffer = Win->Stencil;
  unsigned char *DestBuffer = MainWindow->Surface + 
	Win->PosX + Win->PosY*MainWindow->Pitch;

  for(i=Win->Height;i>0;--i)
	{
	  for(j=WidthDivFour;j>=0;--j)
		{
		  Pixels = *((unsigned long*)SourceBuffer);
		  Mask = *((unsigned long*)StencilBuffer);

		  Pixels &= Mask;

		  *((unsigned long*)DestBuffer) = Pixels;

		  /* Update the buffer pointers */
		  SourceBuffer += 4;
		  StencilBuffer += 4;
		  DestBuffer += 4;
		}
	}
}

/* Window get functions */
int GetWinPosX(WINDOW Win)
{
  return Win->PosX;
}

int GetWinPosY(WINDOW Win)
{
  return Win->PosY;
}

int GetWinWidth(WINDOW Win)
{
  return Win->Width;
}

int GetWinHeight(WINDOW Win)
{
  return Win->Height;
}

int GetWinPitch(WINDOW Win)
{
  return Win->Pitch;
}

unsigned char* GetWinSurface(WINDOW Win)
{
  return Win->Surface;
}

unsigned char* GetWinStencil(WINDOW Win)
{
  return Win->Stencil;
}

WINDOW GetWinParent(WINDOW Win)
{
  return Win->Parent;
}

WINDOW GetWinChild(WINDOW Win)
{
  return Win->Child;
}

WINDOW GetWinNext(WINDOW Win)
{
  return Win->Next;
}

void *GetWinData(WINDOW Win)
{
  return Win->Data;
}

long GetWinFlags(WINDOW Win)
{
  return Win->Flags;
}

int GetWinCursorX(WINDOW Win)
{
  return Win->CursorX;
}

int GetWinCursorY(WINDOW Win)
{
  return Win->CursorY;
}

int GetWinStartCol(WINDOW Win)
{
  return Win->StartCol;
}

int GetWinStartRow(WINDOW Win)
{
  return Win->StartRow;
}

long GetWinFontFlags(WINDOW Win)
{
  return Win->FontFlags;
}

unsigned char GetWinBackgroundColor(WINDOW Win)
{
  return Win->BackgroundColor;
}

unsigned char GetWinFontColor(WINDOW Win)
{
  return Win->FontColor;
}

unsigned char GetWinFrameColor(WINDOW Win)
{
  return Win->FrameColor;
}

void SetWinPosX(WINDOW Win, int Arg)
{
  Win->PosX = Arg;
}

void SetWinPosY(WINDOW Win, int Arg)
{
  Win->PosY = Arg;
}

void SetWinWidth(WINDOW Win, int Arg)
{
  Win->Width = Arg;
}

void SetWinHeight(WINDOW Win, int Arg)
{
  Win->Height = Arg;
}

void SetWinPitch(WINDOW Win, int Arg)
{
  Win->Pitch = Arg;
}

void SetWinSurface(WINDOW Win, unsigned char* Arg)
{
  Win->Surface = Arg;
}

void SetWinStencil(WINDOW Win, unsigned char* Arg)
{
  Win->Stencil = Arg;
}

void SetWinParent(WINDOW Win, WINDOW Arg)
{
  Win->Parent = Arg;
}

void SetWinChild(WINDOW Win, WINDOW Arg)
{
  Win->Child = Arg;
}

void SetWinNext(WINDOW Win, WINDOW Arg)
{
  Win->Next = Arg;
}

void SetWinData(WINDOW Win, void *Arg)
{
  Win->Data = Arg;
}

void SetWinFlags(WINDOW Win, long Arg)
{
  Win->Flags = Arg;
}

void SetWinCursorX(WINDOW Win, int Arg)
{
  Win->CursorX = Arg;
}

void SetWinCursorY(WINDOW Win, int Arg)
{
  Win->CursorY = Arg;
}

void SetWinStartCol(WINDOW Win, int Arg)
{
  Win->StartCol = Arg;
}

void SetWinStartRow(WINDOW Win, int Arg)
{
  Win->StartRow = Arg;
}

void SetWinFontFlags(WINDOW Win, long Arg)
{
  Win->FontFlags = Arg;
}

void SetWinBackgroundColor(WINDOW Win, unsigned char Arg)
{
  Win->BackgroundColor = Arg;
}

void SetWinFontColor(WINDOW Win, unsigned char Arg)
{
  Win->FontColor = Arg;
}

void SetWinFrameColor(WINDOW Win, unsigned char Arg)
{
  Win->FrameColor = Arg;
}


WINDOW GetMainWindow(void)
{
  return MainWindow;
}

