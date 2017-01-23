#include "raspberrypi_mailbox.h"
#include <sys/ioctl.h>

int   raspberrypi_mailbox_ioctlReq() {
    return _IOWR(100, 0, char*);
}
