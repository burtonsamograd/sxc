/*
 * random.c
 *
 * Fast random numbers.
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

/* Module global varaibles */
static int *Table;
static int Cur;
static int Size;

/* Initialize the random number table */
void InitRandom(int Num, int Seed)
{
  int i;

  /* Allocate the random number table */
  Table = (int*)malloc(sizeof(int)*Num);
  Size = Num;

  /* See the random number generator */
  srand(Seed);

  /* Fill in the table with random numbers */
  for(i=0;i<Num;i++)
	Table[i] = rand();
}

void CleanupRandom(void)
{
  if(Table)
	free(Table);

  Table = NULL;
}

/* Get a random number from the table */
int Random(void)
{
  int Return;

  Return = Table[Cur];

  Cur--;

  if(Cur < 0)
	Cur = Size;

  return Return;
}

int SeedRandom(unsigned long Arg)
{
  if(Arg != 0  && Arg >= Size)
	Cur = Arg%Size;
  else
	Cur = Arg;

  return Cur;
}





