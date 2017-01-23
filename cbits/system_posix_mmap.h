#ifndef SYSTEM_POSIX_MMAP_H
#define SYSTEM_POSIX_MMAP_H

#include <stddef.h>

int   system_posix_mmap_PROT_NONE();
int   system_posix_mmap_PROT_READ();
int   system_posix_mmap_PROT_WRITE();
int   system_posix_mmap_PROT_EXEC();

int   system_posix_mmap_MAP_SHARED();
int   system_posix_mmap_MAP_PRIVATE();

void* system_posix_mmap_mmap(size_t len, int prot, int flags, int fd, long long offset);
void  system_posix_mmap_munmap(void* addr, size_t len);

#endif
