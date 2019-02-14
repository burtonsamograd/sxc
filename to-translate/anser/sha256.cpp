// sha256.cpp
//
// An implementation of SHA256 in anser.
//
// Burton Samograd
// License: AGPL
// 2018


#include "anser.h"

#ifndef __ANSER_SHA256_H__
#define __ANSER_SHA256_H__

static Bus& ror32(Bus& a, int n) {
  return a >>= n;
}

static inline Bus& Ch(Bus& x, Bus& y, Bus& z)
{
  return z ^ (x & (y ^ z));
}

static inline Bus& Maj(Bus& x, Bus& y, Bus& z)
{
  return (x & y) | (z & (x | y));
}

#define e0(x)       (ror32(x, 2) ^ ror32(x,13) ^ ror32(x,22))
#define e1(x)       (ror32(x, 6) ^ ror32(x,11) ^ ror32(x,25))
#define s0(x)       (ror32(x, 7) ^ ror32(x,18) ^ (x >> 3))
#define s1(x)       (ror32(x,17) ^ ror32(x,19) ^ (x >> 10))

static inline void BLEND_OP(int I, Bus W[])
{
  W[I] = s1(W[I-2]) + W[I-7] + s0(W[I-15]) + W[I-16];
}

static Bus* sha256_transform(Bus state[8], const Bus input[16])
{
  Bus a, b, c, d, e, f, g, h, t1, t2;
  Bus W[64];
  int i;

  /* load the input */
  for (i = 0; i < 16; i++)
    W[i] = input[i];
  
  /* now blend */
  for (i = 16; i < 64; i++)
    BLEND_OP(i, W);

  /* load the state into our registers */
  a=state[0];  b=state[1];  c=state[2];  d=state[3];
  e=state[4];  f=state[5];  g=state[6];  h=state[7];

  /* now iterate */
  t1 = h + e1(e) + Ch(e,f,g) + 0x428a2f98 + W[ 0];
  t2 = e0(a) + Maj(a,b,c);    d=d+t1;    h=t1+t2;
  t1 = g + e1(d) + Ch(d,e,f) + 0x71374491 + W[ 1];
  t2 = e0(h) + Maj(h,a,b);    c=c+t1;    g=t1+t2;
  t1 = f + e1(c) + Ch(c,d,e) + 0xb5c0fbcf + W[ 2];
  t2 = e0(g) + Maj(g,h,a);    b=b+t1;    f=t1+t2;
  t1 = e + e1(b) + Ch(b,c,d) + 0xe9b5dba5 + W[ 3];
  t2 = e0(f) + Maj(f,g,h);    a=a+t1;    e=t1+t2;
  t1 = d + e1(a) + Ch(a,b,c) + 0x3956c25b + W[ 4];
  t2 = e0(e) + Maj(e,f,g);    h=h+t1;    d=t1+t2;
  t1 = c + e1(h) + Ch(h,a,b) + 0x59f111f1 + W[ 5];
  t2 = e0(d) + Maj(d,e,f);    g=g+t1;    c=t1+t2;
  t1 = b + e1(g) + Ch(g,h,a) + 0x923f82a4 + W[ 6];
  t2 = e0(c) + Maj(c,d,e);    f=f+t1;    b=t1+t2;
  t1 = a + e1(f) + Ch(f,g,h) + 0xab1c5ed5 + W[ 7];
  t2 = e0(b) + Maj(b,c,d);    e=e+t1;    a=t1+t2;

  t1 = h + e1(e) + Ch(e,f,g) + 0xd807aa98 + W[ 8];
  t2 = e0(a) + Maj(a,b,c);    d=d+t1;    h=t1+t2;
  t1 = g + e1(d) + Ch(d,e,f) + 0x12835b01 + W[ 9];
  t2 = e0(h) + Maj(h,a,b);    c=c+t1;    g=t1+t2;
  t1 = f + e1(c) + Ch(c,d,e) + 0x243185be + W[10];
  t2 = e0(g) + Maj(g,h,a);    b=b+t1;    f=t1+t2;
  t1 = e + e1(b) + Ch(b,c,d) + 0x550c7dc3 + W[11];
  t2 = e0(f) + Maj(f,g,h);    a=a+t1;    e=t1+t2;
  t1 = d + e1(a) + Ch(a,b,c) + 0x72be5d74 + W[12];
  t2 = e0(e) + Maj(e,f,g);    h=h+t1;    d=t1+t2;
  t1 = c + e1(h) + Ch(h,a,b) + 0x80deb1fe + W[13];
  t2 = e0(d) + Maj(d,e,f);    g=g+t1;    c=t1+t2;
  t1 = b + e1(g) + Ch(g,h,a) + 0x9bdc06a7 + W[14];
  t2 = e0(c) + Maj(c,d,e);    f=f+t1;    b=t1+t2;
  t1 = a + e1(f) + Ch(f,g,h) + 0xc19bf174 + W[15];
  t2 = e0(b) + Maj(b,c,d);    e=e+t1;    a=t1+t2;

  t1 = h + e1(e) + Ch(e,f,g) + 0xe49b69c1 + W[16];
  t2 = e0(a) + Maj(a,b,c);    d=d+t1;    h=t1+t2;
  t1 = g + e1(d) + Ch(d,e,f) + 0xefbe4786 + W[17];
  t2 = e0(h) + Maj(h,a,b);    c=c+t1;    g=t1+t2;
  t1 = f + e1(c) + Ch(c,d,e) + 0x0fc19dc6 + W[18];
  t2 = e0(g) + Maj(g,h,a);    b=b+t1;    f=t1+t2;
  t1 = e + e1(b) + Ch(b,c,d) + 0x240ca1cc + W[19];
  t2 = e0(f) + Maj(f,g,h);    a=a+t1;    e=t1+t2;
  t1 = d + e1(a) + Ch(a,b,c) + 0x2de92c6f + W[20];
  t2 = e0(e) + Maj(e,f,g);    h=h+t1;    d=t1+t2;
  t1 = c + e1(h) + Ch(h,a,b) + 0x4a7484aa + W[21];
  t2 = e0(d) + Maj(d,e,f);    g=g+t1;    c=t1+t2;
  t1 = b + e1(g) + Ch(g,h,a) + 0x5cb0a9dc + W[22];
  t2 = e0(c) + Maj(c,d,e);    f=f+t1;    b=t1+t2;
  t1 = a + e1(f) + Ch(f,g,h) + 0x76f988da + W[23];
  t2 = e0(b) + Maj(b,c,d);    e=e+t1;    a=t1+t2;

  t1 = h + e1(e) + Ch(e,f,g) + 0x983e5152 + W[24];
  t2 = e0(a) + Maj(a,b,c);    d=d+t1;    h=t1+t2;
  t1 = g + e1(d) + Ch(d,e,f) + 0xa831c66d + W[25];
  t2 = e0(h) + Maj(h,a,b);    c=c+t1;    g=t1+t2;
  t1 = f + e1(c) + Ch(c,d,e) + 0xb00327c8 + W[26];
  t2 = e0(g) + Maj(g,h,a);    b=b+t1;    f=t1+t2;
  t1 = e + e1(b) + Ch(b,c,d) + 0xbf597fc7 + W[27];
  t2 = e0(f) + Maj(f,g,h);    a=a+t1;    e=t1+t2;
  t1 = d + e1(a) + Ch(a,b,c) + 0xc6e00bf3 + W[28];
  t2 = e0(e) + Maj(e,f,g);    h=h+t1;    d=t1+t2;
  t1 = c + e1(h) + Ch(h,a,b) + 0xd5a79147 + W[29];
  t2 = e0(d) + Maj(d,e,f);    g=g+t1;    c=t1+t2;
  t1 = b + e1(g) + Ch(g,h,a) + 0x06ca6351 + W[30];
  t2 = e0(c) + Maj(c,d,e);    f=f+t1;    b=t1+t2;
  t1 = a + e1(f) + Ch(f,g,h) + 0x14292967 + W[31];
  t2 = e0(b) + Maj(b,c,d);    e=e+t1;    a=t1+t2;

  t1 = h + e1(e) + Ch(e,f,g) + 0x27b70a85 + W[32];
  t2 = e0(a) + Maj(a,b,c);    d=d+t1;    h=t1+t2;
  t1 = g + e1(d) + Ch(d,e,f) + 0x2e1b2138 + W[33];
  t2 = e0(h) + Maj(h,a,b);    c=c+t1;    g=t1+t2;
  t1 = f + e1(c) + Ch(c,d,e) + 0x4d2c6dfc + W[34];
  t2 = e0(g) + Maj(g,h,a);    b=b+t1;    f=t1+t2;
  t1 = e + e1(b) + Ch(b,c,d) + 0x53380d13 + W[35];
  t2 = e0(f) + Maj(f,g,h);    a=a+t1;    e=t1+t2;
  t1 = d + e1(a) + Ch(a,b,c) + 0x650a7354 + W[36];
  t2 = e0(e) + Maj(e,f,g);    h=h+t1;    d=t1+t2;
  t1 = c + e1(h) + Ch(h,a,b) + 0x766a0abb + W[37];
  t2 = e0(d) + Maj(d,e,f);    g=g+t1;    c=t1+t2;
  t1 = b + e1(g) + Ch(g,h,a) + 0x81c2c92e + W[38];
  t2 = e0(c) + Maj(c,d,e);    f=f+t1;    b=t1+t2;
  t1 = a + e1(f) + Ch(f,g,h) + 0x92722c85 + W[39];
  t2 = e0(b) + Maj(b,c,d);    e=e+t1;    a=t1+t2;

  t1 = h + e1(e) + Ch(e,f,g) + 0xa2bfe8a1 + W[40];
  t2 = e0(a) + Maj(a,b,c);    d=d+t1;    h=t1+t2;
  t1 = g + e1(d) + Ch(d,e,f) + 0xa81a664b + W[41];
  t2 = e0(h) + Maj(h,a,b);    c=c+t1;    g=t1+t2;
  t1 = f + e1(c) + Ch(c,d,e) + 0xc24b8b70 + W[42];
  t2 = e0(g) + Maj(g,h,a);    b=b+t1;    f=t1+t2;
  t1 = e + e1(b) + Ch(b,c,d) + 0xc76c51a3 + W[43];
  t2 = e0(f) + Maj(f,g,h);    a=a+t1;    e=t1+t2;
  t1 = d + e1(a) + Ch(a,b,c) + 0xd192e819 + W[44];
  t2 = e0(e) + Maj(e,f,g);    h=h+t1;    d=t1+t2;
  t1 = c + e1(h) + Ch(h,a,b) + 0xd6990624 + W[45];
  t2 = e0(d) + Maj(d,e,f);    g=g+t1;    c=t1+t2;
  t1 = b + e1(g) + Ch(g,h,a) + 0xf40e3585 + W[46];
  t2 = e0(c) + Maj(c,d,e);    f=f+t1;    b=t1+t2;
  t1 = a + e1(f) + Ch(f,g,h) + 0x106aa070 + W[47];
  t2 = e0(b) + Maj(b,c,d);    e=e+t1;    a=t1+t2;

  t1 = h + e1(e) + Ch(e,f,g) + 0x19a4c116 + W[48];
  t2 = e0(a) + Maj(a,b,c);    d=d+t1;    h=t1+t2;
  t1 = g + e1(d) + Ch(d,e,f) + 0x1e376c08 + W[49];
  t2 = e0(h) + Maj(h,a,b);    c=c+t1;    g=t1+t2;
  t1 = f + e1(c) + Ch(c,d,e) + 0x2748774c + W[50];
  t2 = e0(g) + Maj(g,h,a);    b=b+t1;    f=t1+t2;
  t1 = e + e1(b) + Ch(b,c,d) + 0x34b0bcb5 + W[51];
  t2 = e0(f) + Maj(f,g,h);    a=a+t1;    e=t1+t2;
  t1 = d + e1(a) + Ch(a,b,c) + 0x391c0cb3 + W[52];
  t2 = e0(e) + Maj(e,f,g);    h=h+t1;    d=t1+t2;
  t1 = c + e1(h) + Ch(h,a,b) + 0x4ed8aa4a + W[53];
  t2 = e0(d) + Maj(d,e,f);    g=g+t1;    c=t1+t2;
  t1 = b + e1(g) + Ch(g,h,a) + 0x5b9cca4f + W[54];
  t2 = e0(c) + Maj(c,d,e);    f=f+t1;    b=t1+t2;
  t1 = a + e1(f) + Ch(f,g,h) + 0x682e6ff3 + W[55];
  t2 = e0(b) + Maj(b,c,d);    e=e+t1;    a=t1+t2;

  t1 = h + e1(e) + Ch(e,f,g) + 0x748f82ee + W[56];
  t2 = e0(a) + Maj(a,b,c);    d=d+t1;    h=t1+t2;
  t1 = g + e1(d) + Ch(d,e,f) + 0x78a5636f + W[57];
  t2 = e0(h) + Maj(h,a,b);    c=c+t1;    g=t1+t2;
  t1 = f + e1(c) + Ch(c,d,e) + 0x84c87814 + W[58];
  t2 = e0(g) + Maj(g,h,a);    b=b+t1;    f=t1+t2;
  t1 = e + e1(b) + Ch(b,c,d) + 0x8cc70208 + W[59];
  t2 = e0(f) + Maj(f,g,h);    a=a+t1;    e=t1+t2;
  t1 = d + e1(a) + Ch(a,b,c) + 0x90befffa + W[60];
  t2 = e0(e) + Maj(e,f,g);    h=h+t1;    d=t1+t2;
  t1 = c + e1(h) + Ch(h,a,b) + 0xa4506ceb + W[61];
  t2 = e0(d) + Maj(d,e,f);    g=g+t1;    c=t1+t2;
  t1 = b + e1(g) + Ch(g,h,a) + 0xbef9a3f7 + W[62];
  t2 = e0(c) + Maj(c,d,e);    f=f+t1;    b=t1+t2;
  t1 = a + e1(f) + Ch(f,g,h) + 0xc67178f2 + W[63];
  t2 = e0(b) + Maj(b,c,d);    e=e+t1;    a=t1+t2;

  Bus *retval = new Bus[8];
  retval[0] = state[0] + a; 
  retval[1] = state[1] + b; 
  retval[2] = state[2] + c; 
  retval[3] = state[3] + d;
  retval[4] = state[4] + e; 
  retval[5] = state[5] + f; 
  retval[6] = state[6] + g; 
  retval[7] = state[7] + h;

  return retval;
}

const uint32_t sha256_init_state[8] = {
	0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a,
	0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19
};

#ifdef __MAIN__
int main(int argc, char** argv) {
  Bus input[16];
  Bus initstate[8];

  for(int i = 0; i < 8; i++) {
    initstate[i].set(sha256_init_state[i]);
  }

  Bus* hash = sha256_transform(initstate, input);

  // SHA256 the null string
  input[0].set(1 << 31);
  for(int i = 1; i < 16; i++) {
   input[i].set(0);
  }

  for(int i = 0; i < 8; i++) {
    std::cerr << std::hex << hash[i].get();
  }
  std::cerr << std::endl;
}
#else /* __MAIN__ */
/* void test() { #DFW to remove warning
  Bus state[8];
  const Bus input[16];
  sha256_transform(state, input);
}
*/
#endif // __MAIN__

#endif /* __ANSER_SHA256_H__ */
 
