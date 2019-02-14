/*
 * window.h
 *
 * Window routines for overlapping windows.
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

#ifndef _WINDOW_H
#define _WINDOW_H

/* Module typedef */
typedef struct tagWINDOW
{
  int PosX, PosY;
  int Width, Height, Pitch;
  unsigned char* Surface;
  unsigned char* Stencil;
  struct tagWINDOW *Parent, *Child, *Next;
  void *Data;
  long Flags;

  /* Font cursor information */
  int CursorX, CursorY;
  int StartCol, StartRow;
  long FontFlags;

  unsigned char BackgroundColor;
  unsigned char FontColor;
  unsigned char FrameColor;
} *WINDOW;

/* Window creation flags */
#define WINDOW_OFFSCREEN 1<<0
#define WINDOW_ONSCREEN 1<<1
#define WINDOW_STENCIL 1<<2
#define WINDOW_ON_PARENT 1<<3

/* Initialize the windowing system to use a rendering buffer */
int InitWindowSystem(unsigned char *Buffer, int Width, int Height);

/* Create a window structure */
WINDOW NewWindow(int Width, int Height, int PosX, int PosY,
				  WINDOW Parent, void *Data, int Flags);

/* Free a window and all of its children */ 
void FreeWindow(WINDOW *Window);

/* Utility functions */
void ClearWindow(WINDOW Win);
void MoveWindow(WINDOW Win, int x, int y);
int SizeWindow(WINDOW Win, int w, int h);

void ScrollWindowUp(WINDOW Win, int Lines);
void CopyToScreen(WINDOW Win);
void StencilCopyToScreen(WINDOW Win);

/* Get a pointer to the main window structure */
WINDOW GetMainWindow(void);

/* Window structure set/get functions */
int GetWinPosX(WINDOW Win);
int GetWinPosY(WINDOW Win);
int GetWinWidth(WINDOW Win);
int GetWinHeight(WINDOW Win);
int GetWinPitch(WINDOW Win);
unsigned char* GetWinSurface(WINDOW Win);
unsigned char* GetWinStencil(WINDOW Win);
WINDOW GetWinParent(WINDOW Win);
WINDOW GetWinChild(WINDOW Win);
WINDOW GetWinNext(WINDOW Win);
void *GetWinData(WINDOW Win);
long GetWinFlags(WINDOW Win);
int GetWinCursorX(WINDOW Win);
int GetWinCursorY(WINDOW Win);
int GetWinStartCol(WINDOW Win);
int GetWinStartRow(WINDOW Win);
long GetWinFontFlags(WINDOW Win);
unsigned char GetWinBackgroundColor(WINDOW Win);
unsigned char GetWinFontColor(WINDOW Win);
unsigned char GetWinFrameColor(WINDOW Win);

void SetWinPosX(WINDOW Win, int Arg);
void SetWinPosY(WINDOW Win, int Arg);
void SetWinWidth(WINDOW Win, int Arg);
void SetWinHeight(WINDOW Win, int Arg);
void SetWinPitch(WINDOW Win, int Arg);
void SetWinSurface(WINDOW Win, unsigned char* Arg);
void SetWinStencil(WINDOW Win, unsigned char* Arg);
void SetWinParent(WINDOW Win, WINDOW Arg);
void SetWinChild(WINDOW Win, WINDOW Arg);
void SetWinNext(WINDOW Win, WINDOW Arg);
void SetWinData(WINDOW Win, void *Arg);
void SetWinFlags(WINDOW Win, long Arg);
void SetWinCursorX(WINDOW Win, int Arg);
void SetWinCursorY(WINDOW Win, int Arg);
void SetWinStartCol(WINDOW Win, int Arg);
void SetWinStartRow(WINDOW Win, int Arg);
void SetWinFontFlags(WINDOW Win, long Arg);
void SetWinBackgroundColor(WINDOW Win, unsigned char Arg);
void SetWinFontColor(WINDOW Win, unsigned char Arg);
void SetWinFrameColor(WINDOW Win, unsigned char Arg);

#endif





