/*
 * fbanim.h
 *
 * Frame based animation system.
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

#ifndef _FBANIM_H
#define _FBANIM_H

typedef enum {NULLPROC=0, LOW=16, MED=32, HIGH=64} PRIORITY;
typedef void(*PROCESS)(unsigned long Arg);

/* Initialize the animation system */
int InitFrameBasedAnimSystem(void);

/* Run the system based on the current proc queue */
int RunFrameBasedAnimSystem(void);

/* Stop the system at any time and return */
int StopFrameBasedAnimSystem(void);

/* Frame counter routines.  Each returns the current
   value of the frame counter */
unsigned long ResetFrameCounter(void);
unsigned long IncFrameCounter(void);
unsigned long GetFrameCounter(void);
void SetFrameCounter(unsigned long New);

/* Process insertion functions */
int InsertProc(unsigned long StartFrame, unsigned long EndFrame,
			   unsigned long Period, PRIORITY Priority, 
			   void *Func, unsigned long Arg);

/* Insert a proc while the system is running */
int DynamicInsertProc(unsigned long EndFrame,
					  unsigned long Period, PRIORITY Priority, 
					  void *Func, unsigned long Arg);

/* Process information routines for the currently running process */
unsigned long GetStartFrame(void);
unsigned long GetEndFrame(void);
unsigned long GetNextFrame(void);
unsigned long GetPeriod(void);
void SetPeriod(unsigned long Arg);
void SetCurProcArg(unsigned long Arg);

void KillCurProc(void);
void KillAllProcs(void);

#endif


