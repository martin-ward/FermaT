#include <assem.h>

/* EQUates table */



/* Files */



static struct {
/*           --> CSECT: FMT001A7_ <-- */
  BYTE  __FILLER_fmt001a7__0001[56];
  DECIMAL(4, __wglobal);

/* CSECT #defines */
#define FILLER_fmt001a7__0001 local.__FILLER_fmt001a7__0001
#define wglobal local.__wglobal
} __attribute__((packed)) local;


/*           --> EXPORTED DATA <-- */
int fmt501(regs_t * p_regs);
FWORD V_FMT501 = (FWORD)&fmt501;
int fmt502(regs_t * p_regs);
FWORD V_FMT502 = (FWORD)&fmt502;

