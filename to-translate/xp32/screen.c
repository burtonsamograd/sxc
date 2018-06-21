/*
 * screen.c
 * 
 * ChangeLog:
 *
 * - removed DOS dependent code behind DOS #ifdef
 * - preparing for new graphics terminal driver.
 *
 * Screen (vga) utility functions
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
 * Copyright Burt Samograd
 * Sept 1997
 */

/* C Includes */
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* DJGPP Includes */
#ifdef DOS
#include <dpmi.h>
#include <sys/nearptr.h>
#include <pc.h>
#endif /* DOS */

/* Local includes */
#include "screen.h"
#include "fbanim.h"

/* Screen global variables */
unsigned char *Screen;
long ScreenWidth;
long ScreenHeight;
long ScreenPitch;
long BufferSize;
RGBCOLOR PhysicalPalette[256], CurPalette[256];

/* Module global variables */
static unsigned char *ScreenPointer;
static unsigned char GammaTable[] = {
  0, 10, 14, 17, 19, 21, 23, 24, 26, 27, 28, 29, 31, 32, 33, 34,
  35, 36, 37, 37, 38, 39, 40, 41, 41, 42, 43, 44, 44, 45, 46, 46,
  47, 48, 48, 49, 49, 50, 51, 51, 52, 52, 53, 53, 54, 54, 55, 55,
  56, 56, 57, 57, 58, 58, 59, 59, 60, 60, 61, 61, 62, 62, 63, 63};

/* VGA Register constants */
#define VGA_STATUS_REGISTER   0x3da
#define VGA_PAL_READ          0x3c7
#define VGA_PAL_WRITE         0x3c8
#define VGA_PAL_DATA          0x3c9

void SetMode320x200(void)
{
  int i;
  RGBCOLOR Color;				   

  ScreenWidth = 320;
  ScreenHeight = 200;
  ScreenPitch = 320;

#if DOS
  __dpmi_regs regs;

  /* Set the video mode to 13h through BIOS call */
  memset(&regs, 0, sizeof(regs));
  regs.x.ax = 0x13;
  __dpmi_int(0x10, &regs);

  /* Set up the screen pointer */
  __djgpp_nearptr_enable();

  /* Initialize some variables */
  Screen = (unsigned char *)0xa0000 + __djgpp_conventional_base;
#else /* DOS */
  Screen = (unsigned char*)calloc(ScreenWidth, ScreenHeight);
#endif /* DOS */
  ScreenPointer = Screen;

  Color.r = 0;
  Color.g = 0;
  Color.b = 0;
  for(i=0;i<256;i++)
	{
	  SetPhysicalPalColor(i, CurPalette);
	}
  BufferSize = ScreenWidth*ScreenHeight;
}

void SetModeText(void)
{
#if DOS
  __dpmi_regs regs;

  /* Set the video mode to 13h through BIOS call */
  memset(&regs, 0, sizeof(regs));
  regs.x.ax = 0x03;
  __dpmi_int(0x10, &regs);

  /* Diable the screen pointer */
  __djgpp_nearptr_disable();
#else /* DOS */
  fprintf(stderr, "(set-mode 'text)\n");
#endif
}

/* Wait for the end of the vertical retrace interval */
void WaitForVerticalBlank(unsigned long Arg)
{
#if DOS
  unsigned char Reg;

  /* Fall through only during the retrace interval */
  /* See if we are in the display */
  do
	{
	  Reg = inp(VGA_STATUS_REGISTER);
	}
  while(!(Reg & 0x08));
	
  /* See if we are in the retrace */
  do
	{
	  Reg = inp(VGA_STATUS_REGISTER);
	}
  while(Reg & 0x08);
#else /* DOS */
  fprintf(stderr, "(wait-for-vertical-blank)\n");
#endif /* DOS */
}

void SetCurPalColorWithGamma(unsigned char Index, RGBCOLOR *Color)
{
  /* save the gamma corrected palette color in the local palette */
  CurPalette[Index].r = GammaTable[Color->r];
  CurPalette[Index].g = GammaTable[Color->g];
  CurPalette[Index].b = GammaTable[Color->b];
#if DOS
#else /* DOS */
  fprintf(stderr, "(set-current-pallette-color-with-gamma %d %d %d %d)\n",
	  Index, 
	  GammaTable[Color->r], 
	  GammaTable[Color->g], 
	  GammaTable[Color->b]);
#endif /* DOS */

}

/* Set a raw palette color value without using gamma correction */
void SetCurPalColor(unsigned char Index, RGBCOLOR *Color)
{
  /* Save the palette color in the local palette */
  CurPalette[Index] = *Color;
#if DOS
#else /* DOS */
  fprintf(stderr, "(set-current-pallette-color %d %d %d %d)\n",
          Index, Color->r, Color->g, Color->b);
#endif /* DOS */
}

void SetPhysicalPalColor(unsigned char Index, RGBCOLOR *Color)
{
#ifdef DOS
  /* Output color index to write port */
  outp(VGA_PAL_WRITE, Index);
  outp(VGA_PAL_DATA, Color->r);
  outp(VGA_PAL_DATA, Color->g);
  outp(VGA_PAL_DATA, Color->b);
  
  /* Save the cur palette in the physical palette */
#else /* DOS */
  fprintf(stderr, "(set-physical-palette-color %d %d %d %d)\n",
          Index, Color->r, Color->g, Color->b);
#endif /* DOS */
  PhysicalPalette[Index] = *Color;
}

/* Get the color from the current palette, which may be different from
 the physical palette if the palette hasn't been updated */
void GetCurPalColor(RGBCOLOR *Color, unsigned char Index)
{
  *Color = CurPalette[Index];
}

/* Get the actual color from the hardware palette */
void GetPhysicalPalColor(RGBCOLOR *Color, unsigned char Index)
{
#ifdef DOS
  /* Output color index to read port */
  outp(VGA_PAL_READ, Index);

  /* Read RGB components into struct */
  Color->r = inp(VGA_PAL_DATA);
  Color->g = inp(VGA_PAL_DATA);
  Color->b = inp(VGA_PAL_DATA);  
#else /* DOS */
  fprintf(stderr, "(get-physical-palette-color %d)\n", Index);
  // FIXME: how to read output of graphics terminal easily when in a pipe
  // fscanf(1, "(%d %d %d)", &Color->r, &Color->g, &Color->b); 
#endif

  PhysicalPalette[Index] = *Color;
}

/* Update the physical palette */
void UpdatePhysicalPalette(void)
{
  int i;

  /*  WaitForVerticalBlank(0); */
  for(i=0;i<256;i++)
	{
	  SetPhysicalPalColor(i, CurPalette+i);
	}
}

static int FadeFinished;
void FadePalette(unsigned long Arg)
{
  int NumFinished = 0;
  int i;
  int Flag;

  FadeFinished = 0;

  for(i=0;i<256;i++)
	{
	  Flag = 0;
	  if(PhysicalPalette[i].r < CurPalette[i].r)
		{
		  Flag = 1;
		  PhysicalPalette[i].r++;
		}
	  else if (PhysicalPalette[i].r > CurPalette[i].r)
		{
		  Flag = 1;
		  PhysicalPalette[i].r--;
		}
	  else
		{
		  NumFinished++;
		}
	  
	  if(PhysicalPalette[i].g < CurPalette[i].g)
		{
		  Flag = 1;
		  PhysicalPalette[i].g++;
		}
	  else if (PhysicalPalette[i].g > CurPalette[i].g)
		{
		  Flag = 1;
		  PhysicalPalette[i].g--;
		}
	  else
		{
		  NumFinished++;
		}

	  if(PhysicalPalette[i].b < CurPalette[i].b)
		{
		  Flag = 1;
		  PhysicalPalette[i].b++;
		}
	  else if (PhysicalPalette[i].b > CurPalette[i].b)
		{
		  Flag = 1;
		  PhysicalPalette[i].b--;
		}
	  else
		{
		  NumFinished++;
		}

	  if(Flag)
		SetPhysicalPalColor(i, PhysicalPalette+i);
	}

  if(NumFinished == 256*3)
	{
	  FadeFinished = 1;
	  KillCurProc();
	}
}

/* Return non-zero if the current palette fade is finished.
 Reset the fade flag */
int FadePaletteFinished(void)
{
  int Temp;

  Temp = FadeFinished;
  FadeFinished = 0;
  return Temp;
}

/* Fade the current palette to black */
void FadeToBlack(unsigned long Arg)
{
  int NumFinished = 0;
  int i;
  int Flag;

  FadeFinished = 0;

  for(i=0;i<256;i++)
	{
	  Flag = 0;

	  if(PhysicalPalette[i].r != 0)
		{
		  PhysicalPalette[i].r--;
		  CurPalette[i].r = PhysicalPalette[i].r;
		  Flag = 1;
		}
	  else
		NumFinished++;
	  
	  if(PhysicalPalette[i].g != 0)
		{
		  PhysicalPalette[i].g--;
		  CurPalette[i].g = PhysicalPalette[i].g;
		  Flag = 1;
		}
	  else
		NumFinished++;
	  
	  if(PhysicalPalette[i].b != 0)
		{
		  PhysicalPalette[i].b--;
		  CurPalette[i].b = PhysicalPalette[i].b;
		  Flag = 1;
		}
	  else
		NumFinished++;

	  if(Flag)
		SetPhysicalPalColor(i, PhysicalPalette+i);
	}

  if(NumFinished == 256*3)
	{
	  FadeFinished = 1;
	  KillCurProc();
	}
}







