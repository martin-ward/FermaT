#include <assem.h>

                              /* EQUates table */
#define FILLER_fmt001a7__0001 local.__FILLER_fmt001a7__0001
#define wglobal local.__wglobal



                              /* Files */



static struct {
                              /* --> CSECT: FMT001A7_ <-- */
  BYTE  __FILLER__0001[56];
  DECIMAL(4, __wglobal);

                              /* CSECT #defines */
} __attribute__((packed)) local;


                              /* --> EXPORTED DATA <-- */
int fmt501(regs_t * p_regs);
FWORD V_FMT501 = (FWORD)&fmt501;
int fmt502(regs_t * p_regs);
FWORD V_FMT502 = (FWORD)&fmt502;


/* External data (V constants) */

extern FWORD V_FMT601;
extern FWORD V_FMT602;

