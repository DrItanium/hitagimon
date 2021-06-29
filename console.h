//
// Created by jwscoggins on 5/2/21.
//

#ifndef I960SXCHIPSET_CONSOLE_H
#define I960SXCHIPSET_CONSOLE_H

#include <stdlib.h>
/**
 * @brief Provide system services console io. This routine will be entered from 'calls 0'
 * in the supervisor table, thus allowing an application to execute and do I/O to
 * the serial device of this monitor through run-time binding
 * @param type
 * @param chr
 */
extern "C" int console_io(int type, int chr);
extern "C" int sys_write(int fd, const void *buf, size_t sz, int& nwrite);
extern "C" int sys_read(int fd, void *buf, size_t sz, int& nread);

#endif //I960SXCHIPSET_CONSOLE_H
