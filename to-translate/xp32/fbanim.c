/*
 * fbanim.c
 *
 * Frame based animation system.  This is a basic run until yield
 * multitasking system that uses the frame rate as the cycle timer.
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
 *
 * Burt Samograd
 * Copywrite 1997
 *
 * TODO:
 * Need to add support for passing parameters to procs.
 */

/* C Includes */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <stdarg.h>
#include <values.h>

/* DJGPP Includes */
#ifdef DOS
#include <pc.h>
#endif /* DOS */

/* Local includes */
#include "fbanim.h"
#include "list.h"
#include "screen.h"

/* Proc structure definition */
typedef struct tagPROC
{ 
  PROCESS Func;              /* Pointer to the function to
								be called to exec the process */
  unsigned long StartFrame;  /* The frame that the process 
							 	will begin execution       */ 
  unsigned long EndFrame;    /* The last frame that the process
								will execute in */
  unsigned long NextFrame;   /* The next frame the process will
								execute in */
  unsigned long Period;      /* The number of frames between
								process execution */
  PRIORITY Priority;         /* Governs how the process is placed
								on the queue */
  unsigned long Arg;         /* Argument to be passed to the proc
								when called */
} PROC;

/* Module global variables */
static unsigned long FrameCounter;
static HLIST ReadyQueue;
static HLIST ExecutionQueue;
static int RunFlag;
static PROC *CurProc;


/* Module function prototypes */
static int AddProcToQueue(HLIST List, PROC *Proc);
static void RunProc(PROC *Proc);
static int Reschedual(void);

/* Initialize the animation system */
int InitFrameBasedAnimSystem(void)
{
  /* Allocate the lists for the module */
  ReadyQueue = ListCreate();
  ExecutionQueue = ListCreate();
  if(ReadyQueue == NULL || ExecutionQueue == NULL) return -1;

  /* Initialize some varaibles */
  RunFlag = 0;

  return 0;
}

/* Run the system based on the contents of the current queue */
int RunFrameBasedAnimSystem(void)
{
  /* Set the run flag */
  RunFlag = 1;

  /* Perform a proc list update */
  if(Reschedual()) return -1;

  /* Do until break */
  while(RunFlag)
	{
	  CurProc = (PROC*)ListFirst(ExecutionQueue);
	  while(CurProc)
		{
		  /* Run the procedure */
		  RunProc(CurProc);

		  /* If we have gone too long, end this sequence of procs and
			 go to the next frame */
#if 0
		   if(inp(0x3da /* VGA_STATUS_REGISTER*/ ) & 0x08) assert(0);
#endif
		  /* Get the next proc to be run */
		  CurProc = ListNext(ExecutionQueue);
		}
	  IncFrameCounter();
	  /*	  WaitForVerticalBlank(0);*/
	  if(Reschedual()) break;
	} 

  return 0;
}

/* Stop the frame based anim system on the next context switch */
int StopFrameBasedAnimSystem(void)
{
  RunFlag = 0;

  return 0;
}

/* Frame counter routines */
unsigned long ResetFrameCounter(void)
{
  return FrameCounter=0;
}
unsigned long IncFrameCounter(void)
{
  return ++FrameCounter;
}
unsigned long GetFrameCounter(void)
{
  return FrameCounter;
}
void SetFrameCounter(unsigned long New)
{
  FrameCounter = New;
}

/* Insert a procedure that takes no arguments into the proc queue */
int InsertProc(unsigned long StartFrame, unsigned long EndFrame,
			   unsigned long Period, PRIORITY Priority, 
			   void *Func, unsigned long Arg)
{
  PROC *NewProc;

  /* FIX ME!! The system will block if a process is added *while running*
	 and the start frame is less than or equal to the current frame */

  /* Create the new proc structure */
  NewProc = (PROC*)malloc(sizeof(*NewProc));
  if(NewProc == NULL) return -1;

  NewProc->StartFrame = StartFrame;
  NewProc->EndFrame = EndFrame;
  NewProc->NextFrame = StartFrame;
  NewProc->Period = Period;
  NewProc->Priority = Priority;
  NewProc->Func = Func;
  NewProc->Arg = Arg;
  
  return AddProcToQueue(ReadyQueue, NewProc);
}

/* Insert a process into the ready queue when the system is running.
   This function is not required, but gives more robust interface.
   If the start frame is not correct when inserting a process, the system
   can block (which will be fixed later).  This function will insert a
   procedure 1 quantum after the current frame */
int DynamicInsertProc(unsigned long EndFrame,
					  unsigned long Period, PRIORITY Priority, 
					  void *Func, unsigned long Arg)
{
  return InsertProc(FrameCounter+1, EndFrame, Period, Priority, Func, Arg);
}

/* Add the given procedure to the proc queue */
int AddProcToQueue(HLIST List, PROC *Proc)
{
  PROC *TempProc;

  /* Get the first procedure on the proc queue */
  TempProc = ListFirst(List);

  /* Sort by starting frame.  Stop sorting as soon as a proc
     with the same next frame number is reached */
  while((TempProc != NULL) && (TempProc->NextFrame < Proc->NextFrame))
	{
	  TempProc = ListNext(List);
	}
	  
  /* Sort by priority.  If there are any proc's with the same priority
     as this one, place them in sequential in the order they were
	 inserted.   Stop as soon as a proc with a lower priority is
	 found.  */
  if((TempProc != NULL) && (TempProc->NextFrame == Proc->NextFrame))
	{
	  while((TempProc != NULL) && (TempProc->Priority >= Proc->Priority)
			&& (TempProc->NextFrame == Proc->NextFrame))
		{
		  TempProc = ListNext(List);
		}
	}

  /* Add the new procedure to the proc queue */
  /* If TempProc is NULL, this means that we ran off the end of
	 the list.  In this case the proc is added to the end.  If the
	 end of the list was not reached, the proc is inserted before
	 the last item that it was compared to (the one which caused the
	 conditional to fail). */
  if(TempProc == NULL)
	return ListAppend(List, Proc);
  else
	return ListInsert(List, Proc);
}

/* Run the given procedure based on its procedure definition structure */
static void RunProc(PROC *Proc)
{
  Proc->Func(Proc->Arg);
  Proc->NextFrame = FrameCounter+Proc->Period;
}

/* Update the execution list for the next frame */
static int Reschedual(void)
{
  PROC *ExecProc;
  PROC *ReadyProc;
  
  /* Prune the execution list by removing any procedures that
	 are not to be run on the next frame */
  ExecProc = (PROC*)ListFirst(ExecutionQueue);
  while(ExecProc != NULL)
	{

	  if(ExecProc->NextFrame != FrameCounter ||
		 ExecProc->EndFrame < FrameCounter)
		{
		  ListRemove(ExecutionQueue);
		  AddProcToQueue(ReadyQueue, ExecProc);
		  ExecProc = ListCurr(ExecutionQueue);
		}
	  else
		{
		  ExecProc = ListNext(ExecutionQueue);
		}
	}

  /* Add the procs in the queue to the proc list */
  ReadyProc = (PROC*)ListFirst(ReadyQueue);
  while(ReadyProc != NULL)
	{
	  /* If this proc has expired, remove it from the ready queue */
	  if(ReadyProc->EndFrame < FrameCounter)
		{
		  PROC *Temp = ListRemove(ReadyQueue);
		  free(Temp);
		}
	  /* If this proc is to be run, add it to the execution queue */
	  else if(ReadyProc->NextFrame == FrameCounter)
		{
		  ListRemove(ReadyQueue);
		  AddProcToQueue(ExecutionQueue, ReadyProc);
		}
	  /* If this frame has a next frame greater than the current frame,
		 diregard the rest of the list, since all procs are sorted by
		 their next frame */
	  else if(ReadyProc->NextFrame > FrameCounter)
		{
		  break;
		}
	  ReadyProc = ListCurr(ReadyQueue);
	}

  /* If the proc list is empty, then return a non-zero value */
  return ((ListCount(ExecutionQueue) == 0) &&
		  (ListCount(ReadyQueue) == 0));
}

/* Stop running the current proceduree */
void KillCurProc(void)
{
  CurProc->EndFrame = FrameCounter;
}

/* Set the value of the current processes arg value */
void SetCurProcArg(unsigned long Arg)
{
  CurProc->Arg = Arg;
}

/* Set the period of the current proc */
void SetPeriod(unsigned long Arg)
{
  CurProc->Period = Arg;
					   
}

/* Kill all of the procedures in the execution and ready queues */
void KillAllProcs(void)
{
  PROC *Proc;

  Proc = ListFirst(ExecutionQueue);
  while(Proc)
	{
	Proc->EndFrame = FrameCounter;
	Proc = ListNext(ExecutionQueue);
	}

  Proc = ListFirst(ReadyQueue);
  while(Proc)
	{
	  Proc->EndFrame = FrameCounter;
	  Proc = ListNext(ReadyQueue);
	}
  Reschedual();
}

