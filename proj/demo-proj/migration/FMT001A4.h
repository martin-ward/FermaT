#include <assem.h>

                              /* EQUates table */
#define mfi0071  STAR__0001
#define mfi0073  STAR__0002
#define modstore amod113
#define STAR__0003 wdtot1
#define wdata    STAR__0003
#define FILLER_csect001_0001 local.__FILLER_csect001_0001
#define UNNAMED__0001 local.__UNNAMED__0001
#define UNNAMED__0002 local.__UNNAMED__0002
#define a0002 local.__a0002
#define FILLER_csect001_0002 local.__FILLER_csect001_0002
#define UNNAMED__0003 local.__UNNAMED__0003
#define FILLER_csect001_0003 local.__FILLER_csect001_0003
#define b0024 local.__b0024
#define UNNAMED__0004 local.__UNNAMED__0004
#define FILLER_csect001_0004 local.__FILLER_csect001_0004
#define b0025 local.__b0025
#define UNNAMED__0005 local.__UNNAMED__0005
#define FILLER_csect001_0005 local.__FILLER_csect001_0005
#define UNNAMED__0006 local.__UNNAMED__0006
#define FILLER_csect001_0006 local.__FILLER_csect001_0006
#define UNNAMED__0007 local.__UNNAMED__0007
#define FILLER_csect001_0007 local.__FILLER_csect001_0007
#define b0068 local.__b0068
#define UNNAMED__0008 local.__UNNAMED__0008
#define FILLER_csect001_0008 local.__FILLER_csect001_0008
#define b0069 local.__b0069
#define UNNAMED__0009 local.__UNNAMED__0009
#define FILLER_csect001_0009 local.__FILLER_csect001_0009
#define STAR__0001 local.__STAR__0001
#define FILLER_csect001_0010 local.__FILLER_csect001_0010
#define UNNAMED__0011 local.__UNNAMED__0011
#define FILLER_csect001_0011 local.__FILLER_csect001_0011
#define STAR__0002 local.__STAR__0002
#define UNNAMED__0013 local.__UNNAMED__0013
#define FILLER_csect001_0012 local.__FILLER_csect001_0012
#define UNNAMED__0014 local.__UNNAMED__0014
#define FILLER_csect001_0013 local.__FILLER_csect001_0013
#define wstack local.__wstack
#define vmod1 local.__vmod1
#define amod3 local.__amod3
#define wmod112 local.__wmod112
#define amod113 local.__amod113
#define amod114 local.__amod114
#define amod115 local.__amod115
#define wprog local.__wprog
#define whw local.__whw
#define typecont local.__typecont
#define wrec local.__wrec
#define wtype local.__wtype
#define wcalc local.__wcalc
#define wtotal local.__wtotal
#define whead1 local.__whead1
#define whead2 local.__whead2
#define wprt local.__wprt
#define wtot1 local.__wtot1
#define wtot2 local.__wtot2
#define wtot3 local.__wtot3
#define wktot local.__wktot
#define wztot1 local.__wztot1
#define wp1 local.__wp1
#define wp2 local.__wp2
#define wxdat1 local.__wxdat1
#define FILLER_csect001_0020 local.__FILLER_csect001_0020
#define wxent2 local.__wxent2
#define wxdat3 local.__wxdat3
#define FILLER_csect001_0022 local.__FILLER_csect001_0022
#define wxdat4 local.__wxdat4
#define FILLER_csect001_0023 local.__FILLER_csect001_0023
#define wxdat5 local.__wxdat5
#define FILLER_csect001_0024 local.__FILLER_csect001_0024
#define wxdat6 local.__wxdat6
#define FILLER_csect001_0025 local.__FILLER_csect001_0025
#define wxdat7 local.__wxdat7
#define FILLER_csect001_0026 local.__FILLER_csect001_0026
#define wxdat8 local.__wxdat8
#define FILLER_csect001_0027 local.__FILLER_csect001_0027
#define wyamt1 local.__wyamt1
#define wyrate local.__wyrate
#define wyamt2 local.__wyamt2
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



                              /* Files */



static struct {
                              /* --> CSECT: CSECT001 <-- */
  BYTE  __FILLER__0001[34];
  BYTE  __UNNAMED__0001[8];
  BYTE  __UNNAMED__0002[10];
  FWORD __a0002[18];
  BYTE  __FILLER__0002[284];
  FWORD __UNNAMED__0003;
  BYTE  __FILLER__0003[16];
  BYTE  __b0024[4];
  FWORD __UNNAMED__0004;
  BYTE  __FILLER__0004[20];
  BYTE  __b0025[4];
  FWORD __UNNAMED__0005;
  BYTE  __FILLER__0005[192];
  FWORD __UNNAMED__0006;
  BYTE  __FILLER__0006[12];
  FWORD __UNNAMED__0007;
  BYTE  __FILLER__0007[624];
  BYTE  __b0068[4];
  FWORD __UNNAMED__0008;
  BYTE  __FILLER__0008[16];
  BYTE  __b0069[4];
  FWORD __UNNAMED__0009;
  BYTE  __FILLER__0009[16];
  FWORD __STAR__0001;
  BYTE  __FILLER__0010[8];
  FWORD __UNNAMED__0011;
  BYTE  __FILLER__0011[16];
  FWORD __STAR__0002;
  FWORD __UNNAMED__0013;
  BYTE  __FILLER__0012[8];
  FWORD __UNNAMED__0014;
  BYTE  __FILLER__0013[44];
  FWORD __wstack[16];
  FWORD __vmod1;
  FWORD __amod3;
  FWORD __wmod112;
  FWORD __amod113;
  FWORD __amod114;
  FWORD __amod115;
  FWORD __wprog;
  HWORD __whw;
  BYTE  __typecont[80];
  struct { /* __wrec */
    BYTE  wrid[7];
    BYTE  wrind;
                              /*VALIDATION PROGRAM ADDRESS*/
    BYTE  wrprog[4];
    DECIMAL(4, wrdata);
    DECIMAL_p(16, wrtot, 4);
    struct { /* wrfdate */
      BYTE  wrfdd[2];
      BYTE  FILLER__0014;
      struct { /* wrfmm */
        BYTE  wrfm1;
        BYTE  wrfm2;
      } __attribute__((packed)) wrfmm;
      BYTE  FILLER__0015;
      struct { /* wrfyyyy */
        BYTE  wrfcen[2];
        struct { /* wrfyy */
          BYTE  wrfy1;
          BYTE  wrfy2;
        } __attribute__((packed)) wrfyy;
      } __attribute__((packed)) wrfyyyy;
    } __attribute__((packed)) wrfdate;
    BYTE  FILLER__0016[38];
  } __attribute__((packed)) __wrec;
  BYTE  __wtype;
  BYTE  __wcalc[8];
                              /*CREDIT AMOUNT*/
  DECIMAL(4, __wtotal);
  BYTE  __whead1[133];
  BYTE  __whead2[133];
  struct { /* __wprt */
    BYTE  wpcc;
    BYTE  UNNAMED__0019[4];
    BYTE  wpf1[12];
    BYTE  UNNAMED__0020[4];
    BYTE  wpftot1[10];
    BYTE  UNNAMED__0021[5];
    BYTE  wpftot2[10];
    BYTE  UNNAMED__0022[5];
    BYTE  wpftot3[10];
    BYTE  UNNAMED__0023[5];
    struct { /* wpfdate */
      BYTE  wpfdd[2];
      BYTE  FILLER__0017;
      struct { /* wpfmm */
        BYTE  wpfm1;
        BYTE  wpfm2;
      } __attribute__((packed)) wpfmm;
      BYTE  FILLER__0018;
      struct { /* wpfyyyy */
        BYTE  wpfcen[2];
        struct { /* wpfyy */
          BYTE  wpfy1;
          BYTE  wpfy2;
        } __attribute__((packed)) wpfyy;
      } __attribute__((packed)) wpfyyyy;
    } __attribute__((packed)) wpfdate;
    BYTE  FILLER__0019[57];
  } __attribute__((packed)) __wprt;
                              /*INTEREST*/
  DECIMAL(4, __wtot1);
                              /*RATE*/
  DECIMAL(4, __wtot2);
                              /*DEBIT*/
  DECIMAL(4, __wtot3);
  DECIMAL(4, __wktot);
                              /*INTEREST RATE*/
  DECIMAL(2, __wztot1);
  BYTE  __wp1[8];
  BYTE  __wp2[8];
                              /*DATE 1*/
  BYTE  __wxdat1[6];
  BYTE  __FILLER__0020[2];
                              /*ENTRY A*/
  struct { /* __wxent2 */
                              /*DATE 2*/
    BYTE  wxdat2[6];
    BYTE  FILLER__0021[2];
  } __attribute__((packed)) __wxent2;
                              /*DATE 3*/
  BYTE  __wxdat3[6];
  BYTE  __FILLER__0022[2];
                              /*DATE 4*/
  BYTE  __wxdat4[6];
  BYTE  __FILLER__0023[2];
  BYTE  __wxdat5[6];
  BYTE  __FILLER__0024[2];
  BYTE  __wxdat6[6];
  BYTE  __FILLER__0025[2];
  BYTE  __wxdat7[6];
  BYTE  __FILLER__0026[2];
  BYTE  __wxdat8[6];
  BYTE  __FILLER__0027[2];
                              /*INTEREST*/
  DECIMAL(4, __wyamt1);
                              /*INTEREST RATE*/
  DECIMAL(3, __wyrate);
                              /*AMOUNT*/
  DECIMAL(4, __wyamt2);
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
} __attribute__((packed)) local;


                              /* --> EXPORTED DATA <-- */


/* External data (V constants) */

extern FWORD V_FMT001A5;
extern FWORD V_FMT101;
extern FWORD V_FMT102;
extern FWORD V_FMT103;
extern FWORD V_FMT104;
extern FWORD V_FMT105;
extern FWORD V_FMT106;
extern FWORD V_FMT107;
extern FWORD V_FMT108;
extern FWORD V_FMT109;
extern FWORD V_FMT110;
extern FWORD V_FMT111;
extern FWORD V_FMT112;
extern FWORD V_FMT300;
extern FWORD V_FMT301;
extern FWORD V_FMT302;
extern FWORD V_FMT303;
extern FWORD V_FMT501;
extern FWORD V_FMT502;
extern FWORD V_FMTAAA1;
extern FWORD V_FMTBBB2;
extern FWORD V_MF370EFO;
extern FWORD V_R15;

