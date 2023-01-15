#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#define ROTL(a, s) ((a << s) | a >> (32 - s))
#define ROTR(a, s) ((a >> s) | a << (32 - s))

uint32_t xrand(void) {
  uint32_t a = ((uint32_t)random()) << 8;
  uint32_t b = ((uint32_t)random()) << 1;
  uint32_t c = ((uint32_t)random());
  return a ^ b ^ c;
}

int main(void) {
  // 2の補数は、負の数を表すとき、その正の値をビット反転し、1を足す。
  // 2の補数表現の32ビット符号付整数の場合、最下位ビットを1ビットめと数えると、31ビットめまでを使う。
  const int32_t NUM_OF_SAMPLES = 128;
  uint32_t a, b, c;

  printf("\"xor\" should \"a ^ b = c\" in {\n");
  printf("// xor\n");
  for (int32_t i = 0; i < NUM_OF_SAMPLES; ++i) {
    a = xrand();
    b = xrand();
    c = a ^ b;
    printf("assert(arith.xor(0x%08x, 0x%08x) === 0x%08x)\n", a, b, c);
  }
  printf("}\n\n");

  printf("\"rotl\" should \"left rotation\" in {\n");
  printf("// left rotation\n");
  for (int32_t i = 0; i < NUM_OF_SAMPLES; ++i) {
    a = xrand();
    b = 1 + xrand() % 31;
    c = ROTL(a, b);
    printf("assert(arith.rotl(0x%08x, %2u) === 0x%08x)\n", a, b, c);
  }
  printf("}\n\n");

  printf("\"rotr\" should \"right rotation\" in {\n");
  printf("// right rotation\n");
  for (int32_t i = 0; i < NUM_OF_SAMPLES; ++i) {
    a = xrand();
    b = 1 + xrand() % 31;
    c = ROTR(a, b);
    printf("assert(arith.rotr(0x%08x, %2u) === 0x%08x)\n", a, b, c);
  }
  printf("}\n\n");

  printf("\"usadd\" should \"unsigned addition\" in {\n");
  printf("// unsigned addtion mod 2^32\n");
  for (int32_t i = 0; i < NUM_OF_SAMPLES; ++i) {
    a = xrand();
    b = xrand();
    c = a + b;
    printf("assert(arith.usadd(0x%08x, 0x%08x) === 0x%08x)\n", a, b, c);
  }
  printf("}\n\n");

  printf("\"ussubb\" should \"unsigned subtraction\" in {\n");
  printf("// unsigned subtraction mod 2^32\n");
  for (int32_t i = 0; i < NUM_OF_SAMPLES; ++i) {
    a = xrand();
    b = xrand();
    c = a - b;
    printf("assert(arith.ussub(0x%08x, 0x%08x) === 0x%08x)\n", a, b, c);
  }
  printf("}\n\n");

  return 0;
}