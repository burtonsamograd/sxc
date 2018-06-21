/*
 * main.c
 *
 * Main routine for demo.
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
#include <values.h>

#ifdef DOS
#include <conio.h>
#include <pc.h>
#include <dos.h>
#endif /* DOS */

/* Local includes */
#include "screen.h"
#include "line.h"
#include "font.h"
#include "window.h"
#include "fbanim.h"
#include "list.h"
#include "random.h"
#include "seq.h"

int main(void)
{
  /* Initialize the subsystems */
  ListInit();
  InitFrameBasedAnimSystem();

  SetMode320x200();
  InitWindowSystem(Screen, ScreenWidth, ScreenHeight);

  InitLine();
  InitFont();
  InitRandom(1024, 0);

  /* Clear the main window */
  ClearWindow(GetMainWindow());
  
  /* Start the first seqence of the demo */
  RunOne();

  RunFrameBasedAnimSystem();
  SetModeText();

  printf("Demo XP32 - 1997\n"
		 "Programming, Art and Design - Burt Samograd\n"
		 "Story Boarding - Ryan Slemko\n\n");
  return 0;
}








