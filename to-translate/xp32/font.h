/*
 * font.h
 *
 * Character printing functions using font tables.
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

/* Local includes */
#include "window.h"
#include "fbanim.h"

#ifndef _FONT_H
#define _FONT_H

/* Font rendering flags */
#define FONT_SHOW_CURSOR 1<<0
#define FONT_AUTOFILL    1<<1
#define FONT_AUTOSCROLL  1<<2
#define FONT_BLINK_CURSOR 1<<3

/* Structure to save the state a given stutter print */
typedef struct tagPRINT_STRUCT
{
  WINDOW Win;    /* Window to print in */
  char *String;   /* String to print */
  int Count;      /* Number of characters printed in string */
  int Strlen;     /* The length of the string */
  PROCESS Proc;   /* Procedure to call after the string is finished printing */
  int EndDelay;   /* Number of character times to delay before Proc or Next */
  int CharDelay;  /* Number of frames to delay after each character */
  struct tagPRINT_STRUCT *Next; /* Next string to print after this one */
} STUTTER_PARAM;

/* Initialize the font table using the standard vga font */
int InitFont(void);

/* Print a character or string at the current cursor location */
int PrintChar(WINDOW Window, char c);
int PrintString(WINDOW Window, char *s);
int PrintStutterString(STUTTER_PARAM *Param);

/* Print a character to the screen using the current font table
   at location (x,y) on the screen */
int PrintCharAt(WINDOW Window, int c, int x, int y);
int PrintStringAt(WINDOW Window, char *s, int x, int y);

/* Get font characteristics */
int GetFontHeight(void);
int GetFontWidth(void);

void ShowFontCursor(WINDOW Window, int  Flag);

void HomeCursor(WINDOW Window);

/* Start or stop the cursor from blinking */
void BlinkCursor(WINDOW Win, int Flag);
#endif


