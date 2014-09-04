#ifndef DISPATCHER_H
#define DISPATCHER_H

//#include <ei.h>
#include <erl_driver.h>
#include "tokyocabinet.h"

static ErlDrvData yatce_drv_start(ErlDrvPort port, char *buff);
static void yatce_drv_stop(ErlDrvData handle);

// inspired by http://rakuto.blogspot.com/2008/07/manipulate-erlang-binary-term-format-in.html
static int yatce_drv_control(ErlDrvData handle, unsigned int command, 
			     char * buf, int len, char ** rbuf, int rlen );

void init_tcadb_(void * TCADB);
void fini_tcadb_(void * TCADB);

// for details, see http://erlang.org/doc/man/driver_entry.html
ErlDrvEntry yatce_driver_entry = {
  .start    = yatce_drv_start,          /* L_PTR start, called when port is opened */
  .stop     = yatce_drv_stop,           /* F_PTR stop, called when port is closed */
  .driver_name  = DYLIB_NAME,
  // char *driver_name, the argument to open_port, must be same as dynamic library file name without suffix
  .control= yatce_drv_control,             /* control, F_PTR control, port_command callback */
  .event = NULL,      // 			  Called when an event selected by driver_event() has occurred  */
  .extended_marker = ERL_DRV_EXTENDED_MARKER,  //       int extended_marker;        // ERL_DRV_EXTENDED_MARKER
  .major_version   = ERL_DRV_EXTENDED_MAJOR_VERSION, //    int major_version;          // ERL_DRV_EXTENDED_MAJOR_VERSION
  .minor_version   = ERL_DRV_EXTENDED_MINOR_VERSION, //    int minor_version;          // ERL_DRV_EXTENDED_MINOR_VERSION
  .driver_flags    = ERL_DRV_FLAG_USE_PORT_LOCKING,//    int driver_flags;           // ERL_DRV_FLAGs 
};

//DRIVER_INIT(example_drv) <- though wrong argument allowed, duplicate of DRIVER_INIT is not allowed.
DRIVER_INIT( DYLIB_NAME ) /* must match name in driver_entry */
{
  return &yatce_driver_entry;
}
#endif
