
#ifdef USE_VPP
/* $::SYSECT = uc("FMT001A4"); ""; */
@@ $::SYSECT = uc("FMT001A4");"";@@
#endif
#include "FMT001A4.h"
/* Entry points are: fmt001a4 */
  int
fmt001a4(regs_t * p_regs);
void
_ENTER_1_FMT001A4_a();
void
DISPATCH_FMT001A4_a();
void
proc11_FMT001A4_p();
void
a_000090_FMT001A4_p();
void
P_1_FMT001A4_p();
void
a_0002a4_FMT001A4_p();
void
a_000578_FMT001A4_p();
void
a_000294_FMT001A4_p();
void
a_000554_FMT001A4_p();
void
a_00008c_FMT001A4_p();
void
a_00019c_FMT001A4_p();
void
a_000088_FMT001A4_p();

FILE *
  ddin;
FILE *
  rdsout;
#define rdsout_status V_RDSOUT_STATUS
extern FWORD
  rdsout_status;
                            /* FIXME: This variable was not in the ll file */

/* End of declarations */

#include "FMT001A4_init.h"

int
fmt001a4(regs_t * p_regs)
{
  regs = *p_regs;
  exit_flag = 0;
/*  <ENTRY POINT>  */
/*  <NAME=FMT001A4>  */
/* ********************************************************************* */
  regs.r3 = regs.r15;
  regs.r12 = (regs.r15 + 4096);
  regs.r13 = (FWORD) & a0002;
/* */
  wstack[1] = 132;
  wstack[2] = (FWORD) & wstack;
  regs.r10 = ((FWORD) & wstack + 8);
  regs.r1 = 0;
  regs.r15 = V_FMT104;
  destination = V_FMT104;
  result_code = fmt104(&regs);
  OPEN(ddin, "INPUT", &result_code);
  OPEN(rdsout, "OUTPUT", &result_code);
  regs.r1 = 4;
  destination = V_FMT104;
  result_code = fmt104(&regs);
  regs.r15 = result_code;
  regs.r10 = wstack[2];
  regs.r14 = *(FWORD *) (wstack[2] + 4);
  destination = *(FWORD *) (wstack[2] + 4);
  result_code = CALL_VIA_PTR(*(FWORD *) (wstack[2] + 4), &regs);
  regs.r15 = result_code;
  destination = 0;
  exit(0);
  exit_flag = 0;
  return (regs.r15);
}

/* ************************************************************ */

void
_ENTER_1_FMT001A4_a()
{
  return;
}

/* ************************************************************ */

void
DISPATCH_FMT001A4_a()
{
  switch (destination) {
   case 0:
     exit(0);
     break;
   case 136:
     a_000088_FMT001A4_p();
     if (exit_flag == 1) {
       exit(0);
     } else {
       DISPATCH_FMT001A4_a();
     }
     break;
   case 136:
     a_000088_FMT001A4_p();
     if (exit_flag == 1) {
       exit(0);
     } else {
       DISPATCH_FMT001A4_a();
     }
     break;
   case 140:
     a_00008c_FMT001A4_p();
     DISPATCH_FMT001A4_a();
     break;
   case 140:
     a_00008c_FMT001A4_p();
     DISPATCH_FMT001A4_a();
     break;
   case 144:
     a_000090_FMT001A4_p();
     exit(0);
     break;
   case 144:
     a_000090_FMT001A4_p();
     exit(0);
     break;
   case 412:
     a_00019c_FMT001A4_p();
     DISPATCH_FMT001A4_a();
     break;
   case 660:
     a_000294_FMT001A4_p();
     DISPATCH_FMT001A4_a();
     break;
   case 676:
     a_0002a4_FMT001A4_p();
     DISPATCH_FMT001A4_a();
     break;
   case 1364:
     a_000554_FMT001A4_p();
     DISPATCH_FMT001A4_a();
     break;
   case 1400:
     a_000578_FMT001A4_p();
     DISPATCH_FMT001A4_a();
     break;
   default:
                                             /* FIXME: Unknown destination */
     exit(0);
     break;
  }
  return;
}

/* ************************************************************ */

void
proc11_FMT001A4_p()
{
  *(FWORD *) (regs.r10 + 4) = regs.r14;
  *(FWORD *) (regs.r10 + 8) = regs.r10;
  regs.r10 += 8;
  zap(wtotal, 4, p_dec("\x1C", 1), 1);
  switch (wtype) {
   case '1':
     ap(wtotal, 4, p_dec("\x10\x0C", 100), 2);
     ap(wdtot1, 4, wtotal, 4);
     ap(wetot1, 4, wdtot1, 4);
     sp(wftot1, 4, wdtot1, 4);
     break;
   case '2':
     ap(wtotal, 4, p_dec("\x20\x0C", 200), 2);
     ap(wdtot2, 4, wtotal, 4);
     ap(wetot2, 4, wdtot2, 4);
     sp(wftot2, 4, wdtot2, 4);
     break;
   case '3':
     ap(wtotal, 4, p_dec("\x30\x0C", 300), 2);
     ap(wdtot3, 4, wtotal, 4);
     ap(wetot3, 4, wdtot3, 4);
     sp(wftot3, 4, wdtot3, 4);
     break;
   case '4':
     ap(wtotal, 4, p_dec("\x40\x0C", 400), 2);
     ap(wdtot4, 4, wtotal, 4);
     ap(wetot4, 4, wdtot4, 4);
     sp(wftot4, 4, wdtot4, 4);
     break;
   case '5':
     ap(wtotal, 4, p_dec("\x50\x0C", 500), 2);
     break;
   case '6':
     ap(wtotal, 4, p_dec("\x60\x0C", 600), 2);
     break;
   case '7':
     ap(wtotal, 4, p_dec("\x70\x0C", 700), 2);
     break;
   case '8':
     ap(wtotal, 4, p_dec("\x80\x0C", 800), 2);
     break;
   default:
     break;
  }
  ap(wtotal, 4, p_dec("\x1C", 1), 1);
  memmove(whead1,
"REPORT HEADING 1                                                \
                                                                 \
    ",
          133);
/* FIXME: Unknown File max for RDSOUT  */
/* FIXME: Unknown File max for RDSOUT  */
/* FIXME: Unknown File min for RDSOUT  */
/* FIXME: Unknown File max for RDSOUT  */
/* FIXME: Unknown File max for RDSOUT  */
/* FIXME: Unknown File min for RDSOUT  */
  PUT_FIXED(rdsout, whead1, &result_code);
  memmove(whead2,
"REPORT HEADING 2                                                \
                                                                 \
    ",
          133);
/* FIXME: Unknown File max for RDSOUT  */
/* FIXME: Unknown File max for RDSOUT  */
/* FIXME: Unknown File min for RDSOUT  */
/* FIXME: Unknown File max for RDSOUT  */
/* FIXME: Unknown File max for RDSOUT  */
/* FIXME: Unknown File min for RDSOUT  */
  PUT_FIXED(rdsout, whead2, &result_code);
  wprt.wpcc = *(FWORD *) ' ';
  memmove(wprt.wpf1, "FIELD1      ", 12);
/*        EDIT WTOT1 TOTAL FIELD */
  ed(wprt.wpftot1, 10, &cc1, &wedit_addr, wtot1, 4,
     "\x40\x20\x6B\x20\x20\x20\x6B\x20\x21\x20", 10);
  ed(wprt.wpftot2, 10, &cc1, &wedit_addr, wtot2, 4,
     "\x40\x20\x6B\x20\x20\x20\x6B\x20\x21\x20", 10);
  ed(wprt.wpftot3, 10, &cc1, &wedit_addr, wtot3, 4,
     "\x40\x20\x6B\x20\x20\x20\x6B\x20\x21\x20", 10);
/* FIXME: Unknown File max for RDSOUT  */
/* FIXME: Unknown File max for RDSOUT  */
/* FIXME: Unknown File min for RDSOUT  */
/* FIXME: Unknown File max for RDSOUT  */
/* FIXME: Unknown File max for RDSOUT  */
/* FIXME: Unknown File min for RDSOUT  */
  PUT_FIXED(rdsout, (BYTE *) & wprt, &result_code);
  regs.r15 = rdsout_status;
  regs.r10 = *(FWORD *) regs.r10;
  regs.r14 = *(FWORD *) (regs.r10 + 4);
  exit_flag = 0;
  return;
}

/* ************************************************************ */

void
a_000090_FMT001A4_p()
{
  regs.r13 = *(FWORD *) (regs.r13 + 4);
  regs.r15 = 0;
  destination = regs.r14;
  return;
}

/* ************************************************************ */

void
P_1_FMT001A4_p()
{
  memmove(wcalc, "END     ", 8);
  wtype = '9';
  proc11_FMT001A4_p();
  regs.r10 = *(FWORD *) regs.r10;
  regs.r14 = *(FWORD *) (regs.r10 + 4);
  exit_flag = 0;
  return;
}

/* ************************************************************ */

void
a_0002a4_FMT001A4_p()
{
  regs.r15 = *(FWORD *) regs.r15;
  destination = regs.r15;
  result_code = CALL_VIA_PTR(regs.r15, &regs);
  regs.r15 = result_code;
  regs.r10 = *(FWORD *) regs.r10;
  regs.r14 = *(FWORD *) (regs.r10 + 4);
  destination = regs.r14;
  return;
}

/* ************************************************************ */

void
a_000578_FMT001A4_p()
{
  regs.r15 = *(FWORD *) regs.r15;
  destination = regs.r15;
  result_code = CALL_VIA_PTR(regs.r15, &regs);
  regs.r15 = result_code;
  memmove(wrec.wrtot, "WKTOT", 4);
  ap(wtotal, 4, wrec.wrtot, 4);
  ap(wrec.wrtot, 4, p_dec("\x1C", 1), 1);
  zap(wktot, 4, wrec.wrtot, 4);
  regs.r10 = *(FWORD *) regs.r10;
  regs.r14 = *(FWORD *) (regs.r10 + 4);
  destination = regs.r14;
  return;
}

/* ************************************************************ */

void
a_000294_FMT001A4_p()
{
  regs.r15 = *(FWORD *) regs.r15;
  destination = regs.r15;
  result_code = CALL_VIA_PTR(regs.r15, &regs);
/* MULTIPLE ENTRY FUNCTION */
/*  DATA MACRO: CNOP 0,4  */
  regs.r15 = 676;
  a_0002a4_FMT001A4_p();
  return;
}

/* ************************************************************ */

void
a_000554_FMT001A4_p()
{
  regs.r15 = *(FWORD *) regs.r15;
  destination = regs.r15;
  result_code = CALL_VIA_PTR(regs.r15, &regs);
  regs.r1 = (FWORD) & mfi0073;
/*  DATA MACRO: CNOP 0,4  */
  regs.r0 = 2;
/*  DATA MACRO: CNOP 0,4  */
  regs.r15 = 1400;
  a_000578_FMT001A4_p();
  return;
}

/* ************************************************************ */

void
a_00008c_FMT001A4_p()
{
/* SAMPLE ONLINE DATA TRACKER */
  *(FWORD *) (regs.r10 + 4) = 144;
  *(FWORD *) (regs.r10 + 8) = regs.r10;
  regs.r10 += 8;
/*        TEST COMMENT FOR WKTOT */
  zap(wktot, 4, wrec.wrdata, 4);
/*        ANOTHER COMMENT FOR WKTOT TO BE IGNORED */
  mp(wktot, 4, p_dec("\x2C", 2), 1);
  sp(wktot, 4, p_dec("\x5C", 5), 1);
  zap(wrec.wrtot, 4, wktot, 4);
  if (wrec.wrind == '1') {
    memmove(whead1,
"DATA HEADING 1                                                  \
                                                                 \
    ",
            133);
                                     /* FIXME: Unknown File max for RDSOUT */
                                     /* FIXME: Unknown File max for RDSOUT */
                                     /* FIXME: Unknown File min for RDSOUT */
                                     /* FIXME: Unknown File max for RDSOUT */
                                     /* FIXME: Unknown File max for RDSOUT */
                                     /* FIXME: Unknown File min for RDSOUT */
    PUT_FIXED(rdsout, whead1, &result_code);
    memmove(wprt.wpf1, "FIELD1      ", 12);
    ed(wprt.wpftot1, 10, &cc1, &wedit_addr, wrec.wrtot, 4,
       "\x40\x20\x6B\x20\x20\x20\x6B\x20\x21\x20", 10);
  } else {
    ap(wktot, 4, p_dec("\x10\x0C", 100), 2);
    memmove(whead1,
"DATA HEADING 2                                                  \
                                                                 \
    ",
            133);
                                     /* FIXME: Unknown File max for RDSOUT */
                                     /* FIXME: Unknown File max for RDSOUT */
                                     /* FIXME: Unknown File min for RDSOUT */
                                     /* FIXME: Unknown File max for RDSOUT */
                                     /* FIXME: Unknown File max for RDSOUT */
                                     /* FIXME: Unknown File min for RDSOUT */
    PUT_FIXED(rdsout, whead1, &result_code);
    memmove(wprt.wpf1, "FIELD2      ", 12);
    ed(wprt.wpftot1, 10, &cc1, &wedit_addr, wktot, 4,
       "\x40\x20\x6B\x20\x20\x20\x6B\x20\x21\x20", 10);
  }
  regs.r15 = V_FMT300;
  regs.r1 = (FWORD) b0068;
  destination = V_FMT300;
  result_code = fmt300(&regs);
  regs.r15 = V_FMT301;
  regs.r1 = (FWORD) b0069;
  destination = V_FMT301;
  result_code = fmt301(&regs);
  regs.r1 = (FWORD) & mfi0071;
/*  DATA MACRO: CNOP 0,4  */
  regs.r0 = 1;
/*  DATA MACRO: CNOP 0,4  */
  regs.r15 = 1364;
  a_000554_FMT001A4_p();
  return;
}

/* ************************************************************ */

void
a_00019c_FMT001A4_p()
{
  regs.r15 = *(FWORD *) regs.r15;
  destination = regs.r15;
  result_code = CALL_VIA_PTR(regs.r15, &regs);
  regs.r15 = V_FMT108;
  regs.r1 = (FWORD) b0024;
  destination = V_FMT108;
  result_code = fmt108(&regs);
  regs.r15 = V_FMT109;
  regs.r1 = (FWORD) b0025;
  destination = V_FMT109;
  result_code = fmt109(&regs);
/* FIXME: Unknown File max for DDIN  */
/* FIXME: Unknown File max for DDIN  */
/* FIXME: Unknown File max for DDIN  */
/* FIXME: Unknown File min for DDIN  */
/* FIXME: Unknown File max for DDIN  */
/* FIXME: Unknown File max for DDIN  */
/* FIXME: Unknown File max for DDIN  */
/* FIXME: Unknown File min for DDIN  */
/* FIXME: Unknown File max for DDIN  */
/* FIXME: Unknown File min for DDIN  */
  GET_FIXED(ddin, (BYTE *) & wrec, &result_code);
  if (end_of_file(ddin)) {
/* */
    exit_flag = 0;
  }
/*        UNKNOWN CALL */
  regs.r15 = wprog;
/* FIXME: entry point is a register for CALL */
  result_code = CALL_VIA_PTR_PARS(wprog);
  regs.r15 = result_code;
/*        LOAD PROCESSING */
/* STANDARD */
  result_code = load("FMT110 EP=FMT110", &regs.r0, &regs.r1);
  regs.r0 = V_FMT110;
  regs.r15 = V_FMT110;
  destination = V_FMT110;
  result_code = fmt110(&regs);
  regs.r15 = result_code;
  result_code = load("FMT111 EP=FMT111", &regs.r0, &regs.r1);
  regs.r0 = V_FMT111;
  regs.r4 = V_FMT111;
  wtype = *(FWORD *) '1';
  regs.r15 = V_FMT111;
/* SAVED ADDRESS IN REG */
  destination = V_FMT111;
  result_code = fmt111(&regs);
  regs.r15 = result_code;
  result_code = load("FMT112 EP=FMT112", &regs.r0, &regs.r1);
  regs.r0 = V_FMT112;
  wmod112 = V_FMT112;
  wtype = *(FWORD *) '2';
  regs.r15 = wmod112;
/* SAVED ADDRESS IN STORAGE */
  destination = V_FMT112;
  result_code = fmt112(&regs);
  regs.r15 = amod113;
/* EXTERNAL SAVED AREA */
/* FIXME: entry point is a register for CALL */
  result_code = CALL_VIA_PTR_PARS(amod113, typecont);
  regs.r15 = result_code;
  regs.r4 = amod114;
/* FIXME: entry point is a register for CALL */
  result_code = CALL_VIA_PTR_PARS(amod114, typecont);
  regs.r15 = result_code;
  regs.r5 = amod115;
/* FIXME: entry point is a register for CALL */
  result_code = CALL_VIA_PTR_PARS(amod115, typecont);
/* MULTI ENTRY MODULE */
  regs.r15 = V_FMTAAA1;
  destination = V_FMTAAA1;
  result_code = fmtaaa1(&regs);
  regs.r15 = V_FMTBBB2;
  destination = V_FMTBBB2;
  result_code = fmtbbb2(&regs);
/* MULTIPLE ENTRY FUNCTION */
/*  DATA MACRO: CNOP 0,4  */
  regs.r15 = 660;
  a_000294_FMT001A4_p();
  return;
}

/* ************************************************************ */

void
a_000088_FMT001A4_p()
{
/* JUMP TABLE */
  *(FWORD *) (regs.r10 + 8) = regs.r10;
  regs.r10 += 8;
/*        JUMP TABLE 1 */
  regs.r4 = (4 * whw);
/* Jump table successfully processed. */
  if ((whw == 2 || whw == 3)) {
    if (whw == 2) {
      memmove(wcalc, "READ    ", 8);
      wtype = '2';
    } else {
      memmove(wcalc, "CLOSE   ", 8);
      wtype = '3';
    }
    proc11_FMT001A4_p();
    P_1_FMT001A4_p();
  } else if (((4 * whw) != 4 && (4 * whw) != 16)) {
    exit_flag = 1;
  } else {
    memmove(wcalc, "OPEN    ", 8);
    wtype = '1';
    proc11_FMT001A4_p();
    P_1_FMT001A4_p();
  }
  if (exit_flag != 1) {
    a_00008c_FMT001A4_p();
  }
  return;
}

/* ************************************************************ */
