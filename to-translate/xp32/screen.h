/*
 * screen.h
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

#ifndef _SCREEN_H
#define _SCREEN_H

/* Color structure */
typedef struct tagRGBCOLOR
{
  unsigned char r,g,b;
} RGBCOLOR;

/* Screen global variables */
extern unsigned char *Screen;
extern unsigned char *Offscreen;
extern long ScreenWidth;
extern long ScreenHeight;
extern long ScreenPitch;
extern long BufferSize;

/* Utility functions */
void SetModeText(void);
void SetMode320x200(void);

void WaitForVerticalBlank(unsigned long Arg);

/* Palette setting getting functions */
void SetCurPalColor(unsigned char Index, RGBCOLOR *Color);
void SetCurPalColorWithGamma(unsigned char Index, RGBCOLOR *Color);
void SetPhysicalPalColor(unsigned char Index, RGBCOLOR *Color);
void GetCurPalColor(RGBCOLOR *Color, unsigned char Index);
void GetPhysicalPalColor(RGBCOLOR *Color, unsigned char Index);

/* The above functions only set the internal palette. The following functions
   can be used to set the physical (screen) palette */
void UpdatePhysicalPalette(void);
void FadePalette(unsigned long NumFrames);
void FadeToBlack(unsigned long NumFrames);
int FadePaletteFinished(void);

#endif /* _SCREEN_H */








