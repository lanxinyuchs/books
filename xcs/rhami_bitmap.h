#ifndef BITMAP_H_
#define BITMAP_H_

#include <stdint.h>

void rhami_bm_set(uint32_t index, uint8_t *vector, uint32_t size);
uint8_t rhami_bm_isset(uint32_t index, uint8_t *vector, uint32_t size);
void rhami_bm_clear(uint32_t index, uint8_t *vector, uint32_t size);
void rhami_bm_clearall(uint8_t *vector, uint32_t size);
uint8_t rhami_bm_is_empty(uint8_t *vector, uint32_t size);

#endif //BITMAP_H_

