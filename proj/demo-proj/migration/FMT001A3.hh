#include <assem.h>

/* EQUates table */

#define STAR__0001 wktot1
#define wdata    STAR__0001


/* Files */



static struct {
/*           --> CSECT: CSECT001 <-- */
  BYTE  __FILLER_csect001_0001[58];
  HWORD __UNNAMED__0001;
  BYTE  __FILLER_csect001_0002[26];
  HWORD __UNNAMED__0002;
  BYTE  __FILLER_csect001_0003[26];
  HWORD __UNNAMED__0003;
  BYTE  __FILLER_csect001_0004[74];
  HWORD __UNNAMED__0004;
  BYTE  __FILLER_csect001_0005[100];
  FWORD __wsave[18];
  DECIMAL(5, __wtotal);
  BYTE  __wprt[80];
  BYTE  __wspaces[80];
  BYTE  __wrep1[14];
  BYTE  __wrep2;
  BYTE  __wrep3[20];
  struct { /* __wrep4 */
/*DATE 1*/
    BYTE  wxdat1[6];
    BYTE  FILLER_csect001_0006[2];
/*ENTRY A*/
    struct { /* wxent2 */
/*DATE 2*/
      BYTE  wxdat2[6];
      BYTE  FILLER_csect001_0007[2];
    } __attribute__((packed)) wxent2;
/*DATE 3*/
    BYTE  wxdat3[6];
    BYTE  FILLER_csect001_0008[2];
/*DATE 4*/
    BYTE  wxdat4[6];
    BYTE  FILLER_csect001_0009[2];
    BYTE  wxdat5[6];
    BYTE  FILLER_csect001_0010[2];
    BYTE  wxdat6[6];
    BYTE  FILLER_csect001_0011[2];
    BYTE  wxdat7[6];
    BYTE  FILLER_csect001_0012[2];
    BYTE  wxdat8[6];
    BYTE  FILLER_csect001_0013[2];
/*INTEREST*/
    DECIMAL(4, wyamt1);
/*INTEREST RATE*/
    DECIMAL(3, wyrate);
/*AMOUNT*/
    DECIMAL(4, wyamt2);
    BYTE  FILLER_csect001_0014[5];
  } __attribute__((packed)) __wrep4;
/*AMOUNT*/
  DECIMAL(4, __wktot1);
/*RATE*/
  DECIMAL(4, __wktot2);
/*CREDIT*/
  DECIMAL(4, __wktot3);
/*AMOUNT*/
  DECIMAL(4, __wktot4);
  DECIMAL(4, __wktot5);
/*AMOUNT*/
  DECIMAL(4, __wjtot1);
/*RATE*/
  DECIMAL(4, __wjtot2);
/*CREDIT*/
  DECIMAL(4, __wjtot3);
/*AMOUNT*/
  DECIMAL(4, __wjtot4);
  DECIMAL(4, __wjtot5);
/*AMOUNT*/
  DECIMAL(4, __wltot1);
/*RATE*/
  DECIMAL(4, __wltot2);
/*CREDIT*/
  DECIMAL(4, __wltot3);
/*AMOUNT*/
  DECIMAL(4, __wltot4);
  DECIMAL(4, __wltot5);
  DECIMAL(4, __wnew1);
  DECIMAL(4, __wnew9);
/*AMOUNT*/
  DECIMAL(4, __kktot1);
/*RATE*/
  DECIMAL(4, __kktot2);
/*CREDIT*/
  DECIMAL(4, __kktot3);
/*AMOUNT*/
  DECIMAL(4, __kktot4);
  DECIMAL(4, __kktot5);
  DECIMAL(1, __wkg);

/* CSECT #defines */
#define FILLER_csect001_0001 local.__FILLER_csect001_0001
#define UNNAMED__0001 local.__UNNAMED__0001
#define FILLER_csect001_0002 local.__FILLER_csect001_0002
#define UNNAMED__0002 local.__UNNAMED__0002
#define FILLER_csect001_0003 local.__FILLER_csect001_0003
#define UNNAMED__0003 local.__UNNAMED__0003
#define FILLER_csect001_0004 local.__FILLER_csect001_0004
#define UNNAMED__0004 local.__UNNAMED__0004
#define FILLER_csect001_0005 local.__FILLER_csect001_0005
#define wsave local.__wsave
#define wtotal local.__wtotal
#define wprt local.__wprt
#define wspaces local.__wspaces
#define wrep1 local.__wrep1
#define wrep2 local.__wrep2
#define wrep3 local.__wrep3
#define wrep4 local.__wrep4
#define wktot1 local.__wktot1
#define wktot2 local.__wktot2
#define wktot3 local.__wktot3
#define wktot4 local.__wktot4
#define wktot5 local.__wktot5
#define wjtot1 local.__wjtot1
#define wjtot2 local.__wjtot2
#define wjtot3 local.__wjtot3
#define wjtot4 local.__wjtot4
#define wjtot5 local.__wjtot5
#define wltot1 local.__wltot1
#define wltot2 local.__wltot2
#define wltot3 local.__wltot3
#define wltot4 local.__wltot4
#define wltot5 local.__wltot5
#define wnew1 local.__wnew1
#define wnew9 local.__wnew9
#define kktot1 local.__kktot1
#define kktot2 local.__kktot2
#define kktot3 local.__kktot3
#define kktot4 local.__kktot4
#define kktot5 local.__kktot5
#define wkg local.__wkg
} __attribute__((packed)) local;


/*           --> EXPORTED DATA <-- */
int fmt001a3(regs_t * p_regs);
FWORD V_FMT001A3 = (FWORD)&fmt001a3;

