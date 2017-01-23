#include "system_posix_mmap.h"

#include <sys/mman.h>

int   system_posix_mmap_PROT_NONE() { return PROT_NONE; }
int   system_posix_mmap_PROT_READ() { return PROT_READ; }
int   system_posix_mmap_PROT_WRITE() { return PROT_WRITE; }
int   system_posix_mmap_PROT_EXEC() { return PROT_EXEC; }

int   system_posix_mmap_MAP_SHARED() { return MAP_SHARED; }
int   system_posix_mmap_MAP_PRIVATE() { return MAP_PRIVATE; }

void* system_posix_mmap_mmap(size_t len, int prot, int flags, int fd, long long offset) {
    void* result = mmap(NULL, len, prot, flags, fd, offset);
    if (MAP_FAILED == result) result = NULL;
    return result;
}

void  system_posix_mmap_munmap(void* addr, size_t len) {
    munmap(addr, len);
}
