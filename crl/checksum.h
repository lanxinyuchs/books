#ifndef CHECKSUM_H
#define CHECKSUM_H

#ifdef __cplusplus
extern "C" {
#endif

int ose_base64_encode(unsigned char *dest, const void *src, int length);

int ose_base64_decode(void *dest, const unsigned char *src, int length);

#ifdef __cplusplus
}
#endif

#endif /* CHECKSUM_H */
