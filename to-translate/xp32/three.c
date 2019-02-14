/*
 * three.c
 *
 * Third sequence of the demo.  Smoke and fire.
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
#include "stdlib.h"
#include "string.h"

/* Local includes */
#include "fbanim.h"
#include "screen.h"
#include "window.h"
#include "smoke.h"
#include "font.h"
#include "fire.h"
#include "goo.h"

/* Module global variables */
static WINDOW OffWin;
static unsigned char* BufferStencil;

/* Local function declerations */
static void InitThree(void);
static void CleanupThree(void);
static void UpdateThree(unsigned long Arg);
static void StartFire(unsigned long Arg);
static void FinishSequence(unsigned long Arg);
static void MoveMatch(unsigned long Arg);
static void CreateStencil(WINDOW Win);
/* static void UpdateStencil(unsigned long Arg); Defined but not used */
static void AfterFire(unsigned long Arg);
static void WipeOnFinish(unsigned long Arg);

/* Run the third seqence */
void RunThree(void)
{
  WINDOW MainWindow = GetMainWindow();

  OffWin = NewWindow(GetWinWidth(MainWindow), GetWinHeight(MainWindow), 0, 0,
					 NULL, NULL, WINDOW_OFFSCREEN | WINDOW_STENCIL);
  InitThree();

  UpdatePhysicalPalette();
  memset(GetWinStencil(OffWin), 0xff, GetWinWidth(OffWin)*GetWinHeight(OffWin));

  InsertProc(GetFrameCounter()+70*5, -1, 2, HIGH, MoveMatch, 0);
  InsertProc(GetFrameCounter()+70*5, -1, 2, HIGH, FadeToBlack, 0);
  
  DynamicInsertProc(-1, 1, LOW, UpdateThree, 0);
}

/* Iniitailze the third sequence */
void InitThree(void)
{
  InitSmoke((unsigned long)OffWin);
  CreateStencil(OffWin);
}

void CleanupThree(void)
{
  free(BufferStencil);
}

void UpdateThree(unsigned long Arg)
{
  UpdateSmoke((unsigned long)OffWin);
  SeedSmoke((unsigned long)OffWin);
  WaitForVerticalBlank(0);
  StencilCopyToScreen(OffWin);
}

void StartFire(unsigned long Arg)
{
  KillAllProcs();
  ClearWindow(OffWin);
  InitFire((unsigned long)OffWin);

  DynamicInsertProc(-1, 1, HIGH, FadePalette, 0);
  DynamicInsertProc(-1, 1, HIGH, UpdateFire, (unsigned long)OffWin);
  InsertProc(GetFrameCounter()+5*70, -1, 1, HIGH, AfterFire, 0);
  
  KillCurProc();
}

void MoveMatch(unsigned long Arg)
{
  static float dy;

  dy += 0.1;
  
  if(MoveSmoke(OffWin, 0, (int)(dy+0.5)))
	DynamicInsertProc(-1,  1, HIGH, StartFire, 0);
}

/* Defined but not used.
static void UpdateStencil(unsigned long Arg)
{
}
*/

/* Create the stencil for the window */
void CreateStencil(WINDOW Win)
{
  unsigned char *Stencil;
  unsigned char *Source;
  int i, j;
  WINDOW TempWin;

  /* Create a temp window for printing fonts into */
  TempWin = NewWindow(GetWinWidth(Win)/8, GetWinHeight(Win)/8, 0, 0, 
						 NULL, NULL, WINDOW_OFFSCREEN);
  SetWinFontColor(TempWin, 255);
  ClearWindow(TempWin);

  /* Allocate the buffer stencil thatp the window will use later */
  BufferStencil = (unsigned char *)malloc(GetWinWidth(Win)*GetWinHeight(Win));
  
  /* Print the string into the window */
  SetWinCursorX(TempWin, 5);
  SetWinCursorY(TempWin, GetFontHeight()-2);
  PrintChar(TempWin, 'f');

  SetWinCursorX(TempWin, GetWinWidth(TempWin)/2 - GetFontWidth()/2);
  PrintChar(TempWin, 'i');

  SetWinCursorX(TempWin, GetWinWidth(TempWin) - GetFontWidth() - 4);
  PrintChar(TempWin, 'n');

  /* Stretch copy the temp window surface into the Stencil */
  Stencil = BufferStencil;
  Source = GetWinSurface(TempWin);
  for(i=0;i<GetWinHeight(TempWin);i++)
	{
	  for(j=0;j<GetWinWidth(TempWin);j++)
		{
		  unsigned long Temp;
		  
		  /* Create the destination pixel block */
		  Temp = *Source;
		  Temp |= Temp<<8 | Temp<<16 | Temp<<24;
		  
		  /* Put the pixel on the stencil */
		  *((unsigned long*)Stencil) = Temp;
 		  Stencil += 4;
		  *((unsigned long*)Stencil) = Temp;
 		  Stencil += 4;

		  /*  Update the source and destination pointers */		  
		  Source++;
		}
	  /* Copy the previous scan line */
	  memcpy(Stencil, Stencil-GetWinWidth(Win), GetWinWidth(Win));
	  Stencil += GetWinWidth(Win);
	  /* Copy the previous scan line */
	  memcpy(Stencil, Stencil-GetWinWidth(Win), GetWinWidth(Win));
	  Stencil += GetWinWidth(Win);
	  /* Copy the previous scan line */
	  memcpy(Stencil, Stencil-GetWinWidth(Win), GetWinWidth(Win));
	  Stencil += GetWinWidth(Win);
	  /* Copy the previous scan line */
	  memcpy(Stencil, Stencil-GetWinWidth(Win), GetWinWidth(Win));
	  Stencil += GetWinWidth(Win);
	  /* Copy the previous scan line */
	  memcpy(Stencil, Stencil-GetWinWidth(Win), GetWinWidth(Win));
	  Stencil += GetWinWidth(Win);
	  /* Copy the previous scan line */
	  memcpy(Stencil, Stencil-GetWinWidth(Win), GetWinWidth(Win));
	  Stencil += GetWinWidth(Win);
	  /* Copy the previous scan line */
	  memcpy(Stencil, Stencil-GetWinWidth(Win), GetWinWidth(Win));
	  Stencil += GetWinWidth(Win);
	}

  /* Filter the window so that the font is a bit less chunky */
  Source = GetWinSurface(Win);
  SetWinSurface(Win, BufferStencil);
  FilterWindow(Win, 0);
  FilterWindow(Win, 0);
  SetWinSurface(Win, Source);
  for(i=0;i<GetWinWidth(Win)*GetWinHeight(Win);i++)
	if (BufferStencil[i] != 0)
	  {
		if(BufferStencil[i] > 98)
		  BufferStencil[i] = 255;
		else
		  BufferStencil[i] = 0;
	  }  
  FreeWindow(&TempWin);
}

static void AfterFire(unsigned long Arg)
{
  InsertProc(GetFrameCounter()+3*35, -1, 1, HIGH, WipeOnFinish, 0);
  KillCurProc();
}

static void WipeOnFinish(unsigned long Arg)
{
  int i;
  static int Count;
  int Offset;

  Offset = Count;

  for(i=0;i<GetWinHeight(OffWin);i++)
	{
	  GetWinStencil(OffWin)[Offset] = BufferStencil[Offset];
	  Offset += GetWinWidth(OffWin);
	}

  Offset = GetWinWidth(OffWin)-1-Count;
  for(i=0;i<GetWinHeight(OffWin);i++)
	{
	  GetWinStencil(OffWin)[Offset] = BufferStencil[Offset];
	  Offset += GetWinWidth(OffWin);
	}


  if(Count < GetWinWidth(OffWin)/2)
	{
	  Count++;
	}
  else
	{
	  KillCurProc();
	  InsertProc(GetFrameCounter()+70*5, -1, 1, HIGH, FinishSequence, 0);
	}
}

static void FinishSequence(unsigned long Arg)
{
  static int Flag;

  if(!Flag)
	{
	  FadePaletteFinished();
	  DynamicInsertProc(-1, 1, HIGH, FadeToBlack, 0);
	  Flag = 1;
	}

  else if(FadePaletteFinished())
	KillAllProcs();

  CleanupThree();
}



