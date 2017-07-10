#include <assem.h>

/* EQUates table */

#define STAR__0001 wdtot1
#define wtable   STAR__0001


/* Files */



static struct {
/*           --> CSECT: CSECT001 <-- */
  BYTE  __FILLER_csect001_0001[10];
  BYTE  __wfield;
  struct { /* __wreport */
    BYTE  wf1[10];
    BYTE  FILLER_csect001_0002[5];
    struct { /* wf2 */
      BYTE  wf2a[5];
      BYTE  wf2b[5];
      BYTE  wf2c[5];
      struct { /* wf2d */
        BYTE  wf2d1;
        BYTE  wf2d2;
        BYTE  wf2d3;
        BYTE  wf2d4;
        BYTE  wf2d5;
      } __attribute__((packed)) wf2d;
    } __attribute__((packed)) wf2;
    DECIMAL_p(15, wf3, 3);
  } __attribute__((packed)) __wreport;
/*AMOUNT*/
  DECIMAL(4, __wdtot1);
/*RATE*/
  DECIMAL(4, __wdtot2);
/*CREDIT*/
  DECIMAL(4, __wdtot3);
/*AMOUNT*/
  DECIMAL(4, __wdtot4);
  DECIMAL(4, __wdtot5);
/*AMOUNT*/
  DECIMAL(4, __wetot1);
/*RATE*/
  DECIMAL(4, __wetot2);
/*CREDIT*/
  DECIMAL(4, __wetot3);
/*AMOUNT*/
  DECIMAL(4, __wetot4);
  DECIMAL(4, __wetot5);
/*AMOUNT*/
  DECIMAL(4, __wftot1);
/*RATE*/
  DECIMAL(4, __wftot2);
/*CREDIT*/
  DECIMAL(4, __wftot3);
/*AMOUNT*/
  DECIMAL(4, __wftot4);
  DECIMAL(4, __wftot5);

/* CSECT #defines */
#define FILLER_csect001_0001 local.__FILLER_csect001_0001
#define wfield local.__wfield
#define wreport local.__wreport
#define wdtot1 local.__wdtot1
#define wdtot2 local.__wdtot2
#define wdtot3 local.__wdtot3
#define wdtot4 local.__wdtot4
#define wdtot5 local.__wdtot5
#define wetot1 local.__wetot1
#define wetot2 local.__wetot2
#define wetot3 local.__wetot3
#define wetot4 local.__wetot4
#define wetot5 local.__wetot5
#define wftot1 local.__wftot1
#define wftot2 local.__wftot2
#define wftot3 local.__wftot3
#define wftot4 local.__wftot4
#define wftot5 local.__wftot5
} __attribute__((packed)) local;


/*           --> EXPORTED DATA <-- */
int fmt200(regs_t * p_regs);
FWORD V_FMT200 = (FWORD)&fmt200;

