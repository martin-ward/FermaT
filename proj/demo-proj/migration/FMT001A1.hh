#include <assem.h>

/* EQUates table */



/* Files */



static struct {
/*           --> CSECT: CSECT001 <-- */
  BYTE  __FILLER_csect001_0001[276];
/*SAVE AREA*/
  FWORD __wsave[18];
/*INPUT RECORD AREA*/
  BYTE  __wrec[80];
/*OUTPUT PRINT AREA*/
  BYTE  __wprt[80];
/*LAST ITEM*/
  BYTE  __wlast[20];

/* CSECT #defines */
#define FILLER_csect001_0001 local.__FILLER_csect001_0001
#define wsave local.__wsave
#define wrec local.__wrec
#define wprt local.__wprt
#define wlast local.__wlast
} __attribute__((packed)) local;


/*           --> EXPORTED DATA <-- */
int fmt001a1(regs_t * p_regs);
FWORD V_FMT001A1 = (FWORD)&fmt001a1;

