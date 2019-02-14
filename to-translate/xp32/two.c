/*
 * intro.c
 *
 * Intro code for demo.
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
#include <string.h>
#include <assert.h>

/* DJGPP includes */
#ifdef DOS
#include <pc.h>
#endif /* DOS */

/* Local includes */
#include "seq.h"
#include "fbanim.h"
#include "screen.h"
#include "font.h"
#include "matrix.h"
#include "camera.h"
#include "terrain.h"
#include "line.h"
#include "random.h"

/* Module defines and macros */
/* Macro to find the color of a pixel in the 3d window */
#define POINT_COLOR(dz)  (64 + 40 - (vz))

/* Standard color index definition */
#define WINDOW_BORDER_INDEX 44+64
#define WINDOW_DROP_BORDER_INDEX ((44-15)+64)
#define WINDOW_TOP_BORDER_INDEX ((44+15)+64)
#define WINDOW_BACKGROUND_COLOR 0
#define FONT_INDEX       (44+64)

/* Background color flashing bounds */
#define LOWER_BG_BOUND 4
#define UPPER_BG_BOUND 6

/* Planetary model constant defines */
#define NUM_RINGS 32
#define RING_RESOLUTION 32

/* Terrain patch constant defines */
#define TERRAIN_WIDTH 32
#define TERRAIN_HEIGHT 32

/* Model size definition */
#define MODEL_SIZE (NUM_RINGS*RING_RESOLUTION)

#define SetPrintParam(ps, w, s, p, d, n)  \
memset(&ps, 0, sizeof(ps)), \
ps.Win = w; ps.String = s; ps.Strlen = strlen(s); \
ps.Count = 0; ps.Proc = p; ps.EndDelay = d; ps.Next = n; \
ps.CharDelay = LETTER_DELAY;


/* Number of frames to delay between stutter chars */
#define LETTER_DELAY 16

/* Structure for fuel bars */
typedef struct tagBARPARAM
{
  int XPos, YPos, Width, Height, NextHeight;
} BARPARAM;

/* Module global variables */
static char CollectingString[] =
"Collecting planetary data...\n";
static char LandingString[] =
"Locating landing site...\n";
static char TerrainString[] =
"Generating map...\n";
static char FinishedString[] =
"Complete\n";
static WINDOW OffWin, View, Output, Fuel, Radar, Logo;
static RGBCOLOR Color;
static STUTTER_PARAM CollectingStringParam;
static STUTTER_PARAM FinishedStringParam;
static STUTTER_PARAM TerrainStringParam;
static STUTTER_PARAM LandingStringParam;
static int ModelPoints;
static MATRIX ModelMatrix;
static VECTOR *Model, *TModel;
static long NumRings;
static long NumPoints;
static TERRAINDATA *TData;
static ANGLE XRot, YRot, ZRot;
static ANGLE XInc, YInc, ZInc;
static int Fading;

/* Local function definitions */
void CleanupTwo(void);
void DrawWindowBorder(WINDOW Win);
void AfterBootString(unsigned long Arg);
void AfterFinishedString(unsigned long Arg);
void DrawPlanetView(unsigned long Arg);
void DrawFuel(unsigned long Arg);
void DrawRadar(unsigned long Arg);
void DrawLogo(unsigned long Arg);
void GenPointArc(VECTOR *Out, SCALAR r, ANGLE ang, long Resolution, float z);
void GenerateSphereModel(unsigned long Arg);
void GenerateCylinderModel(unsigned long Arg);
void GenerateTerrainMap(unsigned long Arg);
void RenderPoints(WINDOW Win, VECTOR *Points, long NumPoints, MATRIX *Matrix);
void RotateModelInit(unsigned long Arg);
void RotateModelOne(unsigned long Arg);
void RotateModelTwo(unsigned long Arg);
void RotateModelThree(unsigned long Arg);
void RotateModelFour(unsigned long Arg);
void RotateModelFive(unsigned long Arg);
void MorphPlanetModel(unsigned long Arg);
void MorphTerrainModel(unsigned long Arg);
void MorphLastModel(unsigned long Arg);
void DrawBar(WINDOW Win, BARPARAM *Bar);
void InitDrawView(unsigned long Arg);
void InitDrawFuel(unsigned long Arg);
void FlashScreen(unsigned long Arg);
void UpdateIntro(unsigned long Arg);
void ExpandDrawView(unsigned long Arg);
void UpdateStencil(unsigned long Arg);
void StartNextSequence(unsigned long Arg);
void GravityWell(unsigned long Arg);
void EndOfSequence(unsigned long Arg);
int MorphModel(VECTOR *DestModel, VECTOR *SourceModel, long NumPoints);
int MorphTerrain(VECTOR *Model, TERRAINDATA* TData, int ModelPoints);
void UpdateFuelBars(BARPARAM *Bar);

/* Inititalize the intro sequence */
void RunTwo(void)
{
  int i;
  WINDOW MainWindow = GetMainWindow();

  /* Initialize the green color palette for the 3d window */
  memset(&Color, 0, sizeof(Color));
  for(i=1;i<64;i++)
	{
	  Color.g = i-1;
	  SetCurPalColor(i, &Color);
	}
  /* Duplicate the palette for the other windows */
  for(;i<127;i++)
	{
	  Color.g = i-64;
	  SetCurPalColor(i, &Color);
	}
  Color.g = 0;
  SetPhysicalPalColor(127, &Color);

  /* Clear the main window */
  ClearWindow(MainWindow);

  /* This is used later, so it should be set to zero */
  Color.g = 0;

  /* Allocate the other windows for the system */
  OffWin = NewWindow(GetWinWidth(MainWindow), GetWinHeight(MainWindow), 0, 0,
					 NULL, NULL, WINDOW_OFFSCREEN | WINDOW_STENCIL);
  View = NewWindow(240, 150, 80, 0, OffWin, NULL, 
				   WINDOW_OFFSCREEN | WINDOW_ON_PARENT);
  Output = NewWindow(240, 50, 80, 150, OffWin, NULL, 
					 WINDOW_OFFSCREEN | WINDOW_ON_PARENT);
  Logo = NewWindow(80, 50, 0, 0, OffWin, NULL, 
				   WINDOW_OFFSCREEN | WINDOW_ON_PARENT);
  Radar = NewWindow(80, 80, 0, 50, OffWin, NULL, 
					WINDOW_OFFSCREEN | WINDOW_ON_PARENT);
  Fuel = NewWindow(80, 70, 0, 130, OffWin, NULL, 
				   WINDOW_OFFSCREEN | WINDOW_ON_PARENT);
  assert(OffWin && View && Output && Logo && Radar && Fuel);

  /* Initialize the some window flags */
  SetWinFontColor(MainWindow, FONT_INDEX);
  SetWinFontFlags(MainWindow, FONT_SHOW_CURSOR);

  SetWinFontColor(Output, FONT_INDEX | FONT_BLINK_CURSOR);
  SetWinFontFlags(Output, FONT_SHOW_CURSOR | FONT_BLINK_CURSOR);

  SetWinBackgroundColor(OffWin, 127);
  SetWinBackgroundColor(View, 127);
  SetWinBackgroundColor(Output, 127);
  SetWinBackgroundColor(Logo, 127);
  SetWinBackgroundColor(Fuel, 127);
  
  /* Set up the printing parameter structures */
  SetPrintParam(CollectingStringParam, Output, CollectingString, 
				NULL, 8, NULL);
  SetPrintParam(FinishedStringParam, Output, FinishedString, 
				AfterFinishedString, 8, NULL);
  SetPrintParam(LandingStringParam, Output, LandingString, 
				NULL, 8, NULL);
  SetPrintParam(TerrainStringParam, Output, TerrainString, 
				NULL, 8, NULL);

  /* Clear the offscreen window and set its stencil to fully off */
  ClearWindow(OffWin);
  memset(GetWinStencil(OffWin), 0, GetWinWidth(OffWin)*GetWinHeight(OffWin));
	
  /* After this process is done, it will start the rest of the demo */
  AfterBootString(0);

  return;
}

/* Update the intro screen */
void UpdateIntro(unsigned long Arg)
{
  WINDOW Win = (WINDOW)Arg;

  /* Copy the back buffer to the screen */
  WaitForVerticalBlank(0);
  StencilCopyToScreen(Win);
}

/* Clean up any allocated memory used by the intro sequence */
void CleanupTwo(void)
{
  /* Free the windows */
  FreeWindow(&OffWin);
  FreeWindow(&View);
  FreeWindow(&Output);
  FreeWindow(&Fuel);
  FreeWindow(&Radar);
  FreeWindow(&Logo);

  /* Free model memory */
  if(Model != NULL)
	free(Model);
  if(TModel != NULL)
	free(TModel);
}


/* Procedure which is called after the bootstring has finished printing.  This
   gets the rest of the demo going */
void AfterBootString(unsigned long Arg)
{

  InitDrawView((unsigned long)View);
  InitDrawFuel((unsigned long)Fuel);

  /* Insert the processes for the intro */
  DynamicInsertProc(-1, 2, HIGH, UpdateStencil, (unsigned long)OffWin);
  DynamicInsertProc(-1, 2, HIGH, DrawPlanetView, (unsigned long)View);
  DynamicInsertProc(-1, 1, LOW, FadePalette, 0);
  DynamicInsertProc(-1, 2, LOW, FlashScreen, 0);
  DynamicInsertProc(-1, 2, LOW, UpdateIntro, (unsigned long)OffWin);

  InsertProc(GetFrameCounter()+2,-1, 8, HIGH, DrawLogo, (unsigned long)Logo);
  InsertProc(GetFrameCounter()+2,-1, 2, HIGH, DrawFuel, (unsigned long)Fuel);
  InsertProc(GetFrameCounter()+2,-1, 2, HIGH, DrawRadar, (unsigned long)Radar);
  
  /* Setup the parameters for the output window */
  SetWinFontColor(Output, FONT_INDEX);
  SetWinCursorX(Output, 2);
  SetWinCursorY(Output, 2);
  SetWinStartCol(Output, 2);
  SetWinStartRow(Output, 2);
  ShowFontCursor(Output, 1);
  SetWinFontFlags(Output, GetWinFontFlags(Output)|FONT_AUTOFILL|FONT_AUTOSCROLL);

  SetWinBackgroundColor(Logo, WINDOW_BORDER_INDEX);
  ClearWindow(Logo);

  DrawWindowBorder(Output);

  DrawWindowBorder(View);

  MoveWindow(View, 1, 1);
  SetWinWidth(View, GetWinWidth(View)-2);
  SetWinHeight(View, GetWinHeight(View)-2);

  SetWinBackgroundColor(Logo, WINDOW_BORDER_INDEX);
  ClearWindow(Logo);
}

/* Update the offscreen window stencil in a radial wipe pattern */
void UpdateStencil(unsigned long Arg)
{
  unsigned char *Buffer;
  static COOR OriginX, OriginY;
  static COOR LeftX, LeftY;
  static COOR BottomX, BottomY;
  static COOR RightX, RightY;
  static COOR TopX, TopY;
  static int Flag;
  int Count = 0; 

  if(Flag == 0)
	{
	  OriginX = GetWinWidth(OffWin)/2;
	  OriginY = GetWinHeight(OffWin)/2;
	  LeftX = GetWinWidth(OffWin)-1;
	  LeftY = OriginY;
	  BottomX = OriginX;
	  BottomY = GetWinHeight(OffWin)-1;
	  RightX = 0;
	  RightY = OriginY;
	  TopX = OriginX;
	  TopY = 0;

	  Flag = 1;
	}
  else
	{
	  Buffer = GetWinSurface(OffWin);
	  SetWinSurface(OffWin, GetWinStencil(OffWin));
	  
	  SetLineColor(255);

	  Line(OffWin, LeftX, LeftY, OriginX, OriginY);
	  Line(OffWin, BottomX, BottomY, OriginX, OriginY);
	  Line(OffWin, RightX, RightY, OriginX, OriginY);
	  Line(OffWin, TopX, TopY, OriginX, OriginY);

	  if(LeftY < GetWinHeight(OffWin)-1)
		LeftY++;
	  else if(LeftX > OriginX+1)
		LeftX--;
	  else
		Count++;

	  if(RightY > 0)
		RightY--;
	  else if(RightX < OriginX-1)
		RightX++;
	  else
		Count++;
		
	  if(TopX < GetWinWidth(OffWin)-1)
		TopX++;
	  else if(TopY < OriginY)
		TopY++;
	  else
		Count++;

	  if(BottomX > 0)
		BottomX--;
	  else if(BottomY > OriginY)
		BottomY--;
	  else
		Count++;

	  SetWinSurface(OffWin, Buffer);

	  if(Count == 4)
		KillCurProc();
	}
  
}


/* A process which changes the background color of the screen
   (color 0) on a periodic basis */
void FlashScreen(unsigned long Arg)
{
  static int Inc = 1;

  /* If the palette isn't fading out */
  if(!Fading)
	{
	  /* Flicker the color */
	  Color.g += Inc;
	  if(Color.g == UPPER_BG_BOUND)
		Inc = -1;
	  else if(Color.g == LOWER_BG_BOUND)
		Inc = 1;
	  else if(Color.g < LOWER_BG_BOUND)
		Color.g++;
	}
  else
	{
	  /* Change the period and fade the color to black */
	  SetPeriod(10);
	  Color.g--;
	  if(Color.g == 0)
		KillCurProc();
	}

  /* Set the color */
  SetPhysicalPalColor(127, &Color);  

}

/* Draws a 1 pixel border around a window */
void DrawWindowBorder(WINDOW Win)
{
  int i;
  unsigned char *Buffer;
  unsigned long Color;
  int WidthDivFour;

  Buffer = GetWinSurface(Win);
  
  Color = WINDOW_TOP_BORDER_INDEX |
	WINDOW_TOP_BORDER_INDEX << 8 | 
	WINDOW_TOP_BORDER_INDEX << 16 | 
	WINDOW_TOP_BORDER_INDEX << 24;
  WidthDivFour = GetWinWidth(Win)/4;

#if 1
  while(WidthDivFour--) {
    (*(unsigned long*)Buffer++) = Color;
  }
#else
  asm("cld\n\t"
	  "rep\n\t"
	  "stosl"
      : "a" (Color)
	  : "a" (Color),
	  "c" (WidthDivFour),
	  "D" (Buffer)
	  : "%eax", "%ecx", "%edi");
#endif
  Buffer += GetWinPitch(Win);

  for(i=GetWinHeight(Win)-2;i>0;--i)
	{
	  Buffer[0] = WINDOW_TOP_BORDER_INDEX;
	  Buffer[GetWinWidth(Win)-1] = WINDOW_DROP_BORDER_INDEX;
	  Buffer += GetWinPitch(Win);
	}

  Color = WINDOW_DROP_BORDER_INDEX |
	WINDOW_DROP_BORDER_INDEX << 8 | 
	WINDOW_DROP_BORDER_INDEX << 16 | 
	WINDOW_DROP_BORDER_INDEX << 24;

#if 1
  while(WidthDivFour--) {
    (*(unsigned long*)Buffer++) = Color;
  }
#else
  asm("cld\n\t"
	  "rep\n\t"
	  "stosl"
	  : /* No output registers */
	  : "a" (Color),
	  "c" (WidthDivFour),
	  "D" (Buffer)
	  : "%eax", "%ecx", "%edi");
#endif
  Buffer += GetWinPitch(Win);
}

/***********************************************/
/****** Bottom left hand window         ********/
/***********************************************/

#define NUM_BARS 7
int NumBars;
BARPARAM Bars[NUM_BARS];
void InitDrawFuel(unsigned long Arg)
{
  WINDOW Win = (WINDOW)Arg;
  int i;

  memset(Bars, 0, sizeof(Bars));

  for(i=NUM_BARS;i>=0;--i)
	{
	  Bars[i].XPos = 6+10*i;
	  Bars[i].YPos = GetWinHeight(Win)-3;
	  Bars[i].Width = 8;
	  Bars[i].NextHeight = 64;
	}

  NumBars = NUM_BARS;
}

void UpdateFuelBars(BARPARAM *Bar)
{
  if(Bar->Height < Bar->NextHeight)
	Bar->Height++;
  else if(Bar->Height > Bar->NextHeight)
	Bar->Height--;
  else
	Bar->NextHeight = Random()%GetWinHeight(Fuel)-6;
}

void DrawBar(WINDOW Win, BARPARAM *Bar)
{
  unsigned char *Cur;
  int i;

  Cur = GetWinSurface(Win) + Bar->XPos + (Bar->YPos-Bar->Height)*GetWinPitch(Win);
  memset(Cur, WINDOW_TOP_BORDER_INDEX, Bar->Width);
  for(i=Bar->Height;i>=0;--i)
	{
	  Cur += GetWinPitch(Win);
	  *(Cur) = WINDOW_TOP_BORDER_INDEX;
	  *(Cur + Bar->Width-1) = WINDOW_DROP_BORDER_INDEX;

	  memset(Cur+1, WINDOW_BORDER_INDEX, Bar->Width-2);
	}
  memset(Cur, WINDOW_DROP_BORDER_INDEX, Bar->Width);
}


/* Redraw the fuel window */
void DrawFuel(unsigned long Arg)
{

  WINDOW Win = (WINDOW)Arg;
  int i;

  for(i=0;i<NumBars;i++)
	{
	  UpdateFuelBars(Bars+i);
  
	  DrawBar(Win, Bars+i);
	}
  DrawWindowBorder(Win);
}

/***********************************************/
/****** Middle left hand window         ********/
/***********************************************/

/* Redraw the fuel window */
void DrawRadar(unsigned long Arg)
{
  WINDOW Win = (WINDOW)Arg;
  static unsigned char Inc = 0;
  unsigned char *Buffer;
  int j;
  int Color;

  Color = 128;

  /* Top to Bottom */
  Buffer = GetWinSurface(Win) + Inc*GetWinPitch(Win);
  
  for(j=Inc;j>=0 && Color>64;--j)
	{
	  memset(Buffer, --Color, GetWinWidth(Win));
	  Buffer -= GetWinPitch(Win);

	}
  Buffer = GetWinSurface(Win) + (GetWinHeight(Win)-1)*GetWinPitch(Win);
  for(j=GetWinHeight(Win)-1;Color>64;j--)
	{
	  memset(Buffer, --Color, GetWinWidth(Win));
	  Buffer -= GetWinPitch(Win);
	}
  
  Inc++;
  Inc %= GetWinHeight(Win);

  DrawWindowBorder(Win);
}

/***********************************************/
/****** Top left hand window            ********/
/***********************************************/
void DrawLogo(unsigned long Arg)
{
  WINDOW Win = (WINDOW)Arg;

  /* Draw a pyramid like thing in the window for now */
  if(GetWinWidth(Win) >= 4 && GetWinHeight(Win) >= 4)
	{
	  DrawWindowBorder(Win);

	  SetWinSurface(Win, GetWinSurface(Win)-GetWinPitch(Win)*2+2);
	  
	  SetWinWidth(Win, GetWinWidth(Win)-4);
	  SetWinHeight(Win, GetWinHeight(Win)-4);
	}
}

/***********************************************/
/****** 3d window rendering             ********/
/***********************************************/
void InitDrawView(unsigned long Arg)
{
  VECTOR v = {0, 0, -75, 1};
  int i;

  UnitMatrix(&ModelMatrix);

  /* Initialize the camera matrix */
  SetCameraNearZ(1);
  SetCameraFarZ(100);
  SetCameraOrigin(&v);

  v.z = 1;
  SetCameraDirection(&v);

  v.z = -1;
  SetCameraUp(&v);

  SetCameraFocalLength(4);
  SetCameraWindow((WINDOW)Arg);
  UpdateCameraVariables();
  CreateCameraMatrix();

  /* Allocate model data */
  Model = (VECTOR*)malloc(sizeof(VECTOR)*MODEL_SIZE);
  TModel = (VECTOR*)malloc(sizeof(VECTOR)*MODEL_SIZE);
  assert(Model && TModel);

  /* Zero all of the vectors in the model */
  for(i=MODEL_SIZE-1;i>=0;--i)
	  {
		ZeroVector(Model+i);
		ZeroVector(TModel+i);
	  }

  /* initailize model variables */
  NumPoints = RING_RESOLUTION;
  
  /* Start the model building task */
  PrintStutterString(&CollectingStringParam);
  DynamicInsertProc(-1, 32, HIGH, GenerateSphereModel, 0);

  TData = NewTerrainData(TERRAIN_WIDTH, TERRAIN_HEIGHT, 0);

  XInc = ZInc = DEG_TO_ANGLE(1.0);
  YInc = DEG_TO_ANGLE(1.5);
}

/* Incrementally generate the model data for the planet */
void GenerateSphereModel(unsigned long Arg)
{
  float Height;

  /* Generate the model for drawing */
  if(NumRings<NUM_RINGS)
	{
	  Height = -20.0+40.0/((float)NUM_RINGS-1.0)*(float)NumRings;
	  GenPointArc(Model+NumRings*RING_RESOLUTION, 
				  20.0, DEG_TO_ANGLE(360), 
				  NumPoints, Height);
	  NumRings++;
	  ModelPoints += NumPoints;
	}
  else
	{
	  KillCurProc();
	  DynamicInsertProc(-1, 2, HIGH, RotateModelInit, 0);
	  PrintStutterString(&LandingStringParam);
	}
}

/* Rotate the planetary model in a cool way */
void RotateModelInit(unsigned long Arg)
{
  XInc = DEG_TO_ANGLE(1.0);
  YInc = ZInc = DEG_TO_ANGLE(0.0);
  
  KillCurProc();
  DynamicInsertProc(-1, 2, HIGH, RotateModelOne, 0);
}

void RotateModelOne(unsigned long Arg)
{
  if(XRot > DEG_TO_ANGLE(-24.5))
	{
	  XInc = DEG_TO_ANGLE(-1.0);
	}
  else if(XRot < DEG_TO_ANGLE(-25.5))
	{
	  XInc = DEG_TO_ANGLE(1.0);
	}
  else
	XInc = DEG_TO_ANGLE(0.0);
  
  if(YRot > DEG_TO_ANGLE(0.5))
	{
	  YInc = DEG_TO_ANGLE(-1.0);
	}
  else if(YRot < DEG_TO_ANGLE(-0.5))
	{
	  YInc = DEG_TO_ANGLE(1.0);
	}
  else
	YInc = DEG_TO_ANGLE(0.0);
  
  if(ZRot > DEG_TO_ANGLE(0.5))
	{
	  ZInc = DEG_TO_ANGLE(-1.0);
	}
  else if(ZRot < DEG_TO_ANGLE(-0.5))
	{
	  ZInc = DEG_TO_ANGLE(1.0);
	}
  else
	ZInc = DEG_TO_ANGLE(0.0);

  if(XInc == DEG_TO_ANGLE(0.0) && 
	 YInc == DEG_TO_ANGLE(0.0) &&
	 ZInc == DEG_TO_ANGLE(0.0))
	{
	  KillCurProc();
	  DynamicInsertProc(-1, 2, HIGH, RotateModelTwo, 0);
	  DynamicInsertProc(-1, 1, HIGH, GenerateTerrainMap, 0);
	}
}

/* Second rotation sequence */
void RotateModelTwo(unsigned long Arg)
{
  /* Update the angles for the terrain model */
  YInc = DEG_TO_ANGLE(1.0);
  XRot = DEG_TO_ANGLE(-25.0);

  KillCurProc();
}

/* Third rotation sequence */
void RotateModelThree(unsigned long Arg)
{
  int i;

  if(XRot >= DEG_TO_ANGLE(-90.0))
	XRot += DEG_TO_ANGLE(-0.25);

  if(Camera.Origin.z <= -55)
	{
	  Camera.Origin.z += 0.125;
	  CreateCameraMatrix();
	}

  if(XRot <= DEG_TO_ANGLE(-90.0) && Camera.Origin.z > -55) 
	{
	  RGBCOLOR Color;
	  int i;

	  Color.r = 63;
	  Color.g = 0;
	  Color.b = 0;
	  for(i=1;i<128;i++)
		SetCurPalColor(i, &Color);

	  SetWinBackgroundColor(View, 0);
	  DynamicInsertProc(-1, 1, HIGH, FadePalette, 0);
	  DynamicInsertProc(-1, 2, HIGH, RotateModelFour, 0);
	  KillCurProc();
	}

  for(i=ModelPoints;i>=0;--i)
	Model[i].y -= 0.125;
}

void RotateModelFour(unsigned long Arg)
{
  if(YInc < DEG_TO_ANGLE(60))
	{
	  YInc += DEG_TO_ANGLE(.025);
	}

 if(YInc > DEG_TO_ANGLE(45))
	{
	  DynamicInsertProc(-1, 4, HIGH, RotateModelFive, 0);
	  InsertProc(GetFrameCounter()+140, -1, 4, HIGH, GravityWell, 0);
	  KillCurProc();
	}
}

void RotateModelFive(unsigned long Arg)
{
  if(YInc >= DEG_TO_ANGLE(5))
	{
	  YInc -= DEG_TO_ANGLE(.125);
	}

 if(YInc < DEG_TO_ANGLE(5))
	{
	  KillCurProc();
	}
}

/* Generate the terrain map for the morph */
void GenerateTerrainMap(unsigned long Arg)
{
  int i, j;
  int Offset;

  BuildTerrain(TData);
  
  ModelPoints = MODEL_SIZE;

  for(i=TData->Height-1;i>=0;i--)
	for(j=TData->Width-1;j>=0;j--)
	  {
		Offset = i*TData->Width + j;
		
		TModel[Offset].x = -23.0 + (SCALAR)j*46.0/(SCALAR)TData->Width;
		TModel[Offset].z = -23.0 + (SCALAR)i*46.0/(SCALAR)TData->Height;
		TModel[Offset].y = 0;
		
		TData->Data[Offset] >>= 3;
	  } 

  PrintStutterString(&TerrainStringParam);

  KillCurProc();
  DynamicInsertProc(-1, 8, HIGH, MorphPlanetModel, 0);
  
}

/* Morph the planet model to a plane */
void MorphPlanetModel(unsigned long Arg)
{
  if(MorphModel(Model, TModel, ModelPoints))
	{
	  KillCurProc();
	  DynamicInsertProc(-1, 8, HIGH, MorphTerrainModel, 0);
	}
}

/* Morph the plane to the terrain model */
void MorphTerrainModel(unsigned long Arg)
{
  if(MorphTerrain(Model, TData, ModelPoints))
	{
	  PrintStutterString(&FinishedStringParam);
	  KillCurProc();
	}
}

/* Draw the 3d planet in the view window */
void DrawPlanetView(unsigned long Arg)
{
  WINDOW Win = (WINDOW)Arg;
  VECTOR PreTemp, Temp;
  MATRIX XRotMat, YRotMat, ZRotMat, PreFinal, Final;

  /* Clear the main window */
  ClearWindow(Win);
  
  /* Initialize the required fields of the vector */
  ZeroVector(&Temp);
  ZeroVector(&PreTemp);

  UnitMatrix(&XRotMat);
  UnitMatrix(&YRotMat);
  UnitMatrix(&ZRotMat);

  XRotMatrix(&XRotMat, XRot);
  XRot += XInc;
  YRotMatrix(&YRotMat, YRot);
  YRot += YInc;
  ZRotMatrix(&ZRotMat, ZRot);
  ZRot += ZInc;

  /* Create the final transform matrix */
  UnitMatrix(&Final);
  UnitMatrix(&PreFinal);
  MultMatrix(&PreFinal, &XRotMat, &YRotMat);
  MultMatrix(&ModelMatrix, &PreFinal, &ZRotMat);
  MultMatrix(&Final, GetCameraMatrix(), &ModelMatrix);

  RenderPoints(Win, Model, ModelPoints, &Final);
}

/* Generate a point arc of the given resolution and radiuos, of the
   said number of degrees */
void GenPointArc(VECTOR *Out, SCALAR r, ANGLE ang, long Resolution, float z)
{
  ANGLE IncAng = ang/(SCALAR)Resolution;
  MATRIX Rot;
  VECTOR v;
  int i;

  ZeroVector(&v);
  v.x = sqrt(r*r - z*z);
  v.y = z;


  UnitMatrix(&Rot);
  YRotMatrix(&Rot, IncAng);

  *Out = v;

  for(i=1;i<Resolution;i++)
	{
	  MatrixMultVec(Out+i, &Rot, &v);
	  v = *(Out+i);
	}
}

/* Render a point model array to the given window using the given
   transformation matrix */
void RenderPoints(WINDOW Win, VECTOR *Points, long NumPoints, MATRIX *Matrix)
{
  VECTOR *CurPoint, CurResult;
  int i;
  SCALAR fTemp;
  long lTemp;
  long vz;

  CurPoint = Points;

  for(i=NumPoints;i>0;--i)
	  {
		/* Transform the point */
		MatrixMultVec(&CurResult, Matrix, CurPoint);
		CurPoint++;

		/* Perform near z clipping */
		if(CurResult.z < 0.01)
		  continue;
		
		/* Perspective project the point to the view plane */
		fTemp = Camera.ProjectionRatio/CurResult.z;
		CurResult.x = (fTemp*CurResult.x) + Camera.HalfScreenWidth;
		CurResult.y = (fTemp*CurResult.y) + Camera.HalfScreenHeight;
		
		/* Generate the color and set the pixel...big hack here... */
 		vz = (long)CurResult.z;
		lTemp = (long)CurResult.x + (long)CurResult.y*GetWinPitch(Win);

		/* 2d clip the point to the window */
		if(CurResult.x >= 0 && CurResult.y >= 0 && 
		   CurResult.x < GetWinWidth(Win) && CurResult.y < GetWinHeight(Win))
		  /* If in the window, do some z-buffering */
			if(*(GetWinSurface(Win) + lTemp) < POINT_COLOR(vz) ||
			   *(GetWinSurface(Win) + lTemp)==127)
			  /* Set the color of the pixel */
			  *(GetWinSurface(Win) + lTemp) = POINT_COLOR(vz);
	  }
}

/* Morph the dest vector model to be the same as the first, returning -1
   when finshed, and 0 if not */
int MorphModel(VECTOR *DestModel, VECTOR *SourceModel, long NumPoints)
{
  int i;
  SCALAR DeltaX, DeltaY, DeltaZ;
  long NumDone = 0;

  
#define MORPH_DELTA (.25)
#define SCALAR_HALF (MORPH_DELTA/2.0)
					 
  for(i=NumPoints-1;i>=0;i--)
	{
	  
	  DeltaX = TModel[i].x - Model[i].x;
	  DeltaY = TModel[i].y - Model[i].y;
	  DeltaZ = TModel[i].z - Model[i].z;
	  
	  if(fabs(DeltaX) > MORPH_DELTA)
		{
		  DeltaX *= SCALAR_HALF;
		  Model[i].x += DeltaX;
		}
	  else
		{
		Model[i].x = TModel[i].x;
		NumDone++;
		}
	  
	  if(fabs(DeltaY) > MORPH_DELTA)
		{
		  DeltaY *= SCALAR_HALF;
		  Model[i].y += DeltaY;
		}
	  else
		{
		  Model[i].y = TModel[i].y;
		  NumDone++;
		}
	  
	  if(fabs(DeltaZ) > MORPH_DELTA)
		{
		  DeltaZ *= SCALAR_HALF;
		  Model[i].z += DeltaZ;
		}
	  else
		{
		  NumDone++;
		  Model[i].z = TModel[i].z;
		}
	}

  if(NumDone >= ModelPoints*3)
	return -1;
  else
	return 0;
}
void GravityWell(unsigned long Arg)
{
  int NumFinished = 0;
  int i;
  
#define GRAVITY 0.25
  
  for(i=0; i<ModelPoints;i++)
	{
	  if(Model[i].x > GRAVITY*2)
		{
		  Model[i].x -= GRAVITY;
		}
	  else
		{
		  if(Model[i].x < GRAVITY)
			{
			  Model[i].x += GRAVITY;
			}
		  else
			{
			  NumFinished++;
			}
		}

	  if(Model[i].y > GRAVITY*2)
		{
		  Model[i].y -= GRAVITY;
		}
	  else
		{
		  if(Model[i].y < GRAVITY)
			{
			  Model[i].y += GRAVITY;
			}
		  else
			{
			  NumFinished++;
			}
		}

	  if(Model[i].z > GRAVITY*2)
		{
		  Model[i].z -= GRAVITY;
		}
	  else
		{
		  if(Model[i].z < GRAVITY)
			{
			  Model[i].z += GRAVITY;
			}
		  else
			{
			  NumFinished++;
			}
		}
	}

  if(NumFinished == 3*ModelPoints)
	{
	  KillCurProc();
	  DynamicInsertProc(-1, 2, HIGH, FadeToBlack, 0);
	  DynamicInsertProc(-1, 2, HIGH, EndOfSequence, 0);
	}
}

/* Morph the given model to the terrain map that is given */
int MorphTerrain(VECTOR *Model, TERRAINDATA* TData, int ModelPoints)
{
  int i, j;
  int Count = 0;
  int Offset;

  for(i=TData->Height-1;i>=0;i--)
	for(j=TData->Width-1;j>=0;j--)
	  {
		Offset = i*TData->Width + j;
		
		if(Model[Offset].y < (SCALAR) (TData->Data[Offset]))
		  {
			Model[Offset].y += 0.125;
		  }
		else
		  {
			Model[Offset].y = TData->Data[Offset];
			Count++;
		  }
	  } 
  if(Count == ModelPoints)
	return -1;
  else
	return 0;
  
}

/* Process that is run after the finshed string is done printing */
void AfterFinishedString(unsigned long Arg)
{
  int i;
  RGBCOLOR Color;

  memset(&Color, 0, sizeof(Color));
  for(i=64;i<128;i++)
	SetCurPalColor(i, &Color);

  Fading = 1;
  KillCurProc();

  DynamicInsertProc(-1, 2, HIGH, FadePalette, 0);
  InsertProc(GetFrameCounter()+70*3, -1, 2, HIGH, StartNextSequence, 0);
}

/* Start the next sequence of events happening */
void StartNextSequence(unsigned long Arg)
{
  if(FadePaletteFinished())
	{
	  KillAllProcs();
	  DynamicInsertProc(-1, 2, HIGH, DrawPlanetView, (unsigned long)View);
	  DynamicInsertProc(-1, 2, HIGH, ExpandDrawView, 0);
	  DynamicInsertProc(-1, 2, LOW, UpdateIntro, (unsigned long)OffWin);
	}
}

/* Expand the 3d view window to fill the whole screen */
void ExpandDrawView(unsigned long Arg)
{
  if(GetWinWidth(View) < GetWinWidth(OffWin)-1)
	{
	  SetWinWidth(View, GetWinWidth(View)+1);
	  MoveWindow(View, -1, 0);
	}

  if(GetWinHeight(View) < GetWinHeight(OffWin)-1)
	{
	  SetWinHeight(View, GetWinHeight(View)+1);
	}

  UpdateCameraVariables();

  if(GetWinHeight(View) == GetWinHeight(OffWin)-1 &&
	 GetWinWidth(View) == GetWinWidth(OffWin)-1)
	{
	  
	  MoveWindow(View, 0, -1);
	  SetWinHeight(View, GetWinHeight(View)+1);
	  SetWinWidth(View, GetWinWidth(View)+1);

	  UpdateCameraVariables();

	  KillCurProc();
	  
	  DynamicInsertProc(-1, 2, HIGH, RotateModelThree, 0);
	}
}

/* Last process to be run in this sequence */
void EndOfSequence(unsigned long Arg)
{
  if(!FadePaletteFinished())
	return;

  KillAllProcs();

  CleanupTwo();

  RunThree();
}






