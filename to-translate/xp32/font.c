/*
 * font.c
 *
 * Font rendering using vga fonts.
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
#include <string.h>
#include <ctype.h>
#include <assert.h>

/* Local inclues */
#include "screen.h"
#include "font.h"
#include "window.h"

/* Local defines */
#define VGA_FONT_HEIGHT 8
#define VGA_FONT_WIDTH 8
#define FONT_ROWS 3

/* Static local variables */
static char LowerCase[]=
"abcdefghijklmnopqrstuvwxyz\n";
static char UpperCase[]=
"ABCDEFGHIJKLMNOPQRSTUVWXYZ\n";
static char Numbers[]=
"0123456789\n";

/* Module global variables */
/* Font buffer information */
static char *FontBuffer;
static int FontBufferSize;
static int FontBufferWidth;
static int StopBlinking;

/* Font information variables */
static int FontHeight;
static int FontWidth;

/* Local function prototypes */
static void DrawFontCursor(WINDOW Window);
static void EraseFontCursor(WINDOW Window);
static void StutterString(unsigned long Arg);
static void BlinkCursorProc(unsigned long Arg);

int InitFont(void)
{
  
  RGBCOLOR OldColor, NewColor;
  unsigned char *TempScreen;
  unsigned char *TempFont;
  int i, j;

  /* Initialize some variables */
  FontWidth = VGA_FONT_WIDTH;
  FontHeight = VGA_FONT_HEIGHT;
  
  /* Set the font color to black and save the old color */
  GetPhysicalPalColor(&OldColor, 7);
  memset(&NewColor, 0, sizeof(RGBCOLOR));
  SetPhysicalPalColor(7, &NewColor);
  
  /* Print the font to the screen */
  printf(UpperCase);
  printf(LowerCase);
  printf(Numbers);
  
  /* Copy the font lines to the font buffer */
  FontBufferWidth = FontWidth*26;
  FontBufferSize = FontHeight*FONT_ROWS*FontBufferWidth;
  FontBuffer = (unsigned char*)malloc(FontBufferSize);
  
  /* Copy the screen section to the font buffer */
  TempScreen = Screen;
  TempFont = FontBuffer;
  for(i=0;i<FontHeight*FONT_ROWS;i++)
	{
	  memcpy(TempFont, TempScreen, FontBufferWidth);
	  TempFont += FontBufferWidth;
	  TempScreen += ScreenWidth;
	}

  /* Change the default color index to 1 */
  for(i=0;i<FontHeight;i++)
	for(j=0;j<FontWidth;j++)
	  {
		long Temp = j+i*FontBufferWidth;

		if(FontBuffer[Temp])
		  FontBuffer[Temp] = 1;
	  }
		  
  /* Set the old font color */
  WaitForVerticalBlank(0);
  SetPhysicalPalColor(7, &OldColor);
  
  return 0;
}

int PrintString(WINDOW Window, char *String)
{
	while(*String)
	  {
		PrintChar(Window, *String);
		String++;
	  }	

	return 0;
}

int PrintStutterString(STUTTER_PARAM* Param)
{
  return DynamicInsertProc(-1, Param->CharDelay, HIGH, 
						   StutterString, (unsigned long)Param);
}
					   
/* Print a character to the screen at the current cursor position */
int PrintChar(WINDOW Window, char c)
{

  EraseFontCursor(Window);

  /* Draw the character */
  PrintCharAt(Window, c, GetWinCursorX(Window), GetWinCursorY(Window));

  switch(c)
	{
	  case '\n':
		SetWinCursorX(Window, GetWinStartCol(Window));
		SetWinCursorY(Window, GetWinCursorY(Window)+FontHeight);
		break;

	  case '.':
	  case ':':
		SetWinCursorX(Window, GetWinCursorX(Window)+5);
		break;

	  default:
		SetWinCursorX(Window, GetWinCursorX(Window)+FontWidth);
	}


  /* If the next character to be drawn will fall outside of the window */
  if(GetWinCursorX(Window) + FontWidth >= GetWinWidth(Window))
	{
	  /* If autofill (linewrap) was requested */
	  if(GetWinFontFlags(Window) & FONT_AUTOFILL)
		 {
		   /* Print out a newline to go to the next line */
		   PrintChar(Window, '\n');
		 }
	  else
		{
		  /* Move the cursor back so that the next
			 character won't fall outside the window */
		  SetWinCursorX(Window, GetWinCursorX(Window)-FontWidth);
		}
	}

  /* Scroll the window for if autoscrolling is desired */
  if(GetWinCursorY(Window) + FontHeight >= GetWinHeight(Window))
	{
	if(GetWinFontFlags(Window) & FONT_AUTOSCROLL)
		{
		ScrollWindowUp(Window, FontHeight);
		}
	SetWinCursorY(Window, GetWinCursorY(Window)-FontHeight);
	}

  /* Draw the cursor */
  if(GetWinFontFlags(Window) & FONT_SHOW_CURSOR)
	{
	  DrawFontCursor(Window);
	}

  return 0;
}

/* Print a font character at the given location */
int PrintCharAt(WINDOW Window, int c, int x, int y)
{
  unsigned char *TempScreen;
  unsigned char *TempFont;
  int i, j;


  /* Calculate the proper offset into the font table */
  if(isupper(c))
	{
	  c -= 'A';
	  c *= 8;
	}
  else if(islower(c))
	{
	  c -= 'a';
	  c *= 8;
	  c += FontBufferWidth*FontHeight;
	}
  else if(isdigit(c))
	{
	  c -= '0';
	  c *= 8;
	  c += FontBufferWidth*FontHeight*2;
	}
  else if(c == '.')
	{
	  /* Draw a period */
	  TempScreen = GetWinSurface(Window) + x + y*GetWinPitch(Window);
	  TempScreen += GetWinPitch(Window)*7 + 1;

	  *TempScreen = GetWinFontColor(Window);
	  *(TempScreen+1) = GetWinFontColor(Window);

	  return 0;
	}
  else if(c == ':')
	{

	  /* Draw a colon */
	  TempScreen = GetWinSurface(Window) + x + y*GetWinPitch(Window);
	  TempScreen += 1 + GetWinPitch(Window);

	  *TempScreen = GetWinFontColor(Window);
	  *(TempScreen+1) = GetWinFontColor(Window);
	  TempScreen += GetWinPitch(Window);
	  *TempScreen = GetWinFontColor(Window);
	  *(TempScreen+1) = GetWinFontColor(Window);

	  TempScreen += GetWinPitch(Window);
	  TempScreen += GetWinPitch(Window)<<1;
	  *TempScreen = GetWinFontColor(Window);
	  *(TempScreen+1) = GetWinFontColor(Window);
	  TempScreen += GetWinPitch(Window);
	  *TempScreen = GetWinFontColor(Window);
	  *(TempScreen+1) = GetWinFontColor(Window);

	  return 0;
	}
  else
	{
	  c = FontBufferWidth*FontHeight*2 + FontWidth*11;
	}

  TempScreen = GetWinSurface(Window) + x + y*GetWinPitch(Window);
  TempFont = FontBuffer + c;

  /* Copy the character to the screen */
  for(i=0;i<FontHeight;i++)
	{
	  for(j=0;j<FontWidth;j++)
		{
		  if(TempFont[j])
			TempScreen[j] = GetWinFontColor(Window);
		  else
			TempScreen[j] = GetWinBackgroundColor(Window);
		}

	  TempScreen += GetWinPitch(Window);
	  TempFont += FontBufferWidth;
	}


  return 0;
}

void ShowFontCursor(WINDOW Window, int Flag)
{
  if(Flag)
	{
	  SetWinFontFlags(Window, GetWinFontFlags(Window)|FONT_SHOW_CURSOR);
	  DrawFontCursor(Window);
	}
  else
	{
	  SetWinFontFlags(Window, GetWinFontFlags(Window)&~FONT_SHOW_CURSOR);
	  EraseFontCursor(Window);
	}
}

void DrawFontCursor(WINDOW Window)
{
  unsigned char *TempScreen = GetWinSurface(Window);

  TempScreen += GetWinCursorX(Window) +
	GetWinPitch(Window)*(GetWinCursorY(Window) + FontHeight -  2);

  memset(TempScreen, GetWinFontColor(Window), FontWidth);
}

void EraseFontCursor(WINDOW Window)
{
  unsigned char *TempScreen = GetWinSurface(Window);

  TempScreen += GetWinCursorX(Window) +
	GetWinPitch(Window)*(GetWinCursorY(Window) + FontHeight - 2);

  memset(TempScreen, 0, FontWidth);
}

int GetFontWidth(void)
{
  return FontWidth;
}

int GetFontHeight(void)
{
  return FontHeight;
}
void HomeCursor(WINDOW Win)
{
	SetWinCursorX(Win, GetWinStartCol(Win));
	SetWinCursorY(Win, GetWinStartRow(Win));
}

/* Print a string with delays between the characters */
static void StutterString(unsigned long Arg)
{
  static int Flag = 1;
  STUTTER_PARAM *ps = (STUTTER_PARAM*)Arg;

  /* If we are finished printing the string */
  if(ps->Count == ps->Strlen)
	{
	  /* Decriment and check the delay value */
	  ps->EndDelay--;
	  if(ps->EndDelay <= 0)
		{
		  /* Stop the cursor from blinking */
		  StopBlinking = 1;

		  /* This proc is finished so set the flag again */
		  Flag = 1;

		  /* If the is another string to be printed after this one */
		  if(ps->Next)
			/* Print the next string */
			PrintStutterString(ps->Next);

		  /* Stop this from running */
		  KillCurProc();

		  /* Call the requested procedure */
		  if(ps->Proc)
			ps->Proc(0);
		}
	  /* Start the cursor blinking while we wait */
	  else if(Flag)
		{
		  /* Insert the cursor proc */
		  BlinkCursor(ps->Win, 1);

		  /* Unset the flag so this does not happen again */
		  Flag = 0;
		}

	  /* Return here */
	  return;
	}

  /* Print the char to the window */
  PrintChar(ps->Win, ps->String[ps->Count]);

  /* Increment the current character count */
  ps->Count++;
}

/* Start or stop the cursor from blinking */
void BlinkCursor(WINDOW Win, int Flag)
{
  if(Flag && !(GetWinFontFlags(Win) & FONT_BLINK_CURSOR))
	{
	  SetWinFontFlags(Win, GetWinFontFlags(Win)|FONT_BLINK_CURSOR);
	  DynamicInsertProc(-1, 8, HIGH, BlinkCursorProc, (unsigned long)Win);
	}
  else
	SetWinFontFlags(Win, GetWinFontFlags(Win)&~FONT_BLINK_CURSOR);
}

/* A process which causes the cursor to blink on and off */
static void BlinkCursorProc(unsigned long Arg)
{
  static int CursorFlag = 0;
  WINDOW Win = (WINDOW)Arg;
 
  if(GetWinFontFlags(Win) & FONT_BLINK_CURSOR)
	{
	  if(CursorFlag)
		{
		  ShowFontCursor(Win, CursorFlag);
		  CursorFlag=!CursorFlag;
		}
	}
  else
	{
	  ShowFontCursor(Win, GetWinFontFlags(Win) & FONT_SHOW_CURSOR);
	  KillCurProc();
	}
}








