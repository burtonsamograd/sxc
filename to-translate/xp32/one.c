/*
 * one.c
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
 * Copyright 1997
 */

/* C Includes */
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>

/* Local includes */
#include "seq.h"
#include "goo.h"
#include "font.h"
#include "window.h"
#include "fbanim.h"
#include "screen.h"

/* Module global variables */
static int SequenceFinished;
static WINDOW OffWin;

/* Local functions */
static void InitOne(WINDOW Win);
static void CreateStencil(WINDOW Win);
static void UpdateOne(unsigned long Arg);
static void UpdateStencil(WINDOW Win);
static void RunNextSequence(void);

/* Call to run this sequence */
void RunOne(void)
{
  WINDOW MainWindow = GetMainWindow();

  /* Create an offscreen window */
  OffWin = NewWindow(GetWinWidth(MainWindow), GetWinHeight(MainWindow),
					 0, 0, NULL, NULL, WINDOW_OFFSCREEN | WINDOW_STENCIL);

  /* Initialize the sequence */
  InitOne(OffWin);

  /* Start the sequence processes */
  DynamicInsertProc(-1, 1, MED, UpdateOne, (unsigned long)OffWin);
  InsertProc(GetFrameCounter()+70, -1, 1, HIGH, FadePalette, 0);
  InsertProc(GetFrameCounter()+140, -1, 2, HIGH, 
			 UpdateStencil, (unsigned long)OffWin);
}
  
/* Initialize the first sequence */
void InitOne(WINDOW Win)
{
  InitGoo(Win);
  CreateStencil(Win);
}

/* Create the stencil for the window */
void CreateStencil(WINDOW Win)
{
  unsigned char *Stencil;
  unsigned char *Source;
  int i, j;
  char StringOne[] =
	"D   E   M   O";
  char StringTwo[] =
	"X   P   3   2";
  WINDOW TempWin;

  /* Create a temp window for printing fonts into */
  TempWin = NewWindow(GetWinWidth(Win)/2, GetWinHeight(Win)/2, 0, 0, 
						 NULL, NULL, WINDOW_OFFSCREEN);
  SetWinFontColor(TempWin, 255);

  /* Print the string into the window */
  SetWinCursorX(TempWin, GetWinWidth(TempWin)/2 - 7*GetFontWidth());
  SetWinCursorY(TempWin, GetWinHeight(TempWin)/2 - GetFontHeight() - GetFontHeight()/2);
  PrintString(TempWin, StringOne);
  SetWinCursorX(TempWin, GetWinWidth(TempWin)/2 - 7*GetFontWidth());
  SetWinCursorY(TempWin, GetWinHeight(TempWin)/2 + GetFontHeight()/2);
  PrintString(TempWin, StringTwo);

  /* Stretch copy the temp window surface into the Stencil */
  Stencil = GetWinStencil(Win);
  Source = GetWinSurface(TempWin);
  for(i=0;i<GetWinHeight(TempWin);i++)
	{
	  for(j=0;j<GetWinWidth(TempWin);j++)
		{
		  unsigned long Temp;
		  
		  /* Create the destination pixel block */
		  Temp = *Source;
		  Temp |= Temp<<8;/* | Temp<<16 | Temp<<24; */
		  
		  /* Put the pixel on the stencil */
		  *((unsigned short*)Stencil) = Temp;

		  /*  Update the source and destination pointers */		  
		  Source++;
		  Stencil += 2;
		}
	  /* Copy the previous scan line */
	  memcpy(Stencil, Stencil-GetWinWidth(Win), GetWinWidth(Win));
	  Stencil += GetWinWidth(Win);
	}

  /* Filter the window so that the font is a bit less chunky */
  Source = GetWinSurface(Win);
  SetWinSurface(Win, GetWinStencil(Win));
  FilterWindow(Win, 0);
  SetWinSurface(Win, Source);
  for(i=0;i<GetWinWidth(Win)*GetWinHeight(Win);i++)
	if(GetWinStencil(Win)[i] != 0)
	  {
		if(GetWinStencil(Win)[i] > 127)
		  GetWinStencil(Win)[i] = 255;
		else
		  GetWinStencil(Win)[i] = 0;
	  }  

  FreeWindow(&TempWin);
}

/* Render the screen shot for the first sequence */
void UpdateOne(unsigned long Arg)
{
  WINDOW Win = (WINDOW)Arg;

  /* Update the goo screen */
  UpdateGoo(Win);

  /* Copy the offscreen window to screen */
  WaitForVerticalBlank(0);
  StencilCopyToScreen(Win);

  /* If the sequence is finished, go to the next */
  if(SequenceFinished && FadePaletteFinished())
	RunNextSequence();
}

/* Update the main window's stencil */
void UpdateStencil(WINDOW Win)
{
  static int Line = 0;
  static int Width = 0;
  unsigned char* Stencil;

#define FRAME_DELTA 10
  
  Stencil = GetWinStencil(Win)+(GetWinWidth(Win)*GetWinHeight(Win)/2) + FRAME_DELTA;

  memset(Stencil - GetWinWidth(Win)*Line, 255, Width);
  memset(Stencil + GetWinWidth(Win)*Line, 255, Width);
  Line += 1;

  if(Width < GetWinWidth(Win) - FRAME_DELTA*2)
	{
	  Width += 4;
	  Line = 0;
	}
  else if(!(Line < (GetWinHeight(Win)/2)-FRAME_DELTA))
	{
	  KillCurProc();
	  FadePaletteFinished();
	  InsertProc(GetFrameCounter()+70, -1, 1, HIGH, FadeToBlack, 0);
	  SequenceFinished = 1;
	}
}

/* Start the next sequnce and clean up the stuff for this sequence */
static void RunNextSequence(void)
{
  /* Stop the update process */
  KillAllProcs();

  /* De-allocate the offscreen window */
  FreeWindow(&OffWin);
  
  /* Start the next sequence */
  RunTwo();
}


