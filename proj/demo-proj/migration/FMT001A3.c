
#ifdef USE_VPP
/* $::SYSECT = uc("FMT001A3"); ""; */
@@ $::SYSECT = uc("FMT001A3");"";@@
#endif
#include "FMT001A3.h"
  FILE * print2;

/* End of declarations */

#include "FMT001A3_init.h"

int
fmt001a3(regs_t * p_regs)
{
  regs = *p_regs;
  exit_flag = 0;
/*  <ENTRY POINT>  */
/*  <NAME=FMT001A3>  */
/* ********************************************************************* */
/* */
  OPEN(print2, "OUTPUT", &result_code);
/* MVS VERSION */
/* MVS VERSION */
/* GET MF370EFO QSAM VECTOR TABLE ADDRESS */
/* GET=+0, PUT=+4, PUTX=+8, PUTX OUT=+12 */
/* FIXME: No jump table found: assuming this jumps over data */
/* FIXME: was: r14 := 74; destination := r15 + 4; CALL dispatch; */
  memmove(wprt, wspaces, 80);
  *wprt = wrep2;
/* MVS VERSION */
/* MVS VERSION */
/* GET MF370EFO QSAM VECTOR TABLE ADDRESS */
/* GET=+0, PUT=+4, PUTX=+8, PUTX OUT=+12 */
/* FIXME: No jump table found: assuming this jumps over data */
/* FIXME: was: r14 := 102; destination := r15 + 4; CALL dispatch; */
/* MVS VERSION */
/* MVS VERSION */
/* GET MF370EFO QSAM VECTOR TABLE ADDRESS */
/* GET=+0, PUT=+4, PUTX=+8, PUTX OUT=+12 */
/* FIXME: No jump table found: assuming this jumps over data */
/* FIXME: was: r14 := 130; destination := r15 + 4; CALL dispatch; */
  memmove(wrep4.wxdat1, "AAAAAA", 6);
  memmove(wrep4.wxent2.wxdat2, "BBBBBB", 6);
  memmove(wrep4.wxdat3, "CCCCCC", 6);
  memmove(wrep4.wxdat4, "DDDDDD", 6);
  memmove(wrep4.wxdat5, "EEEEEE", 6);
  memmove(wrep4.wxdat6, "FFFFFF", 6);
  memmove(wrep4.wxdat7, "GGGGGG", 6);
  memmove(wrep4.wxdat8, "HHHHHH", 6);
  memmove(wprt, (BYTE *) & wrep4, 80);
/* MVS VERSION */
/* MVS VERSION */
  regs.r0 = (FWORD) wprt;
/* GET MF370EFO QSAM VECTOR TABLE ADDRESS */
/* GET=+0, PUT=+4, PUTX=+8, PUTX OUT=+12 */
/* FIXME: No jump table found: assuming this jumps over data */
/* FIXME: was: r14 := 206; destination := r15 + 4; CALL dispatch; */
  memmove(wprt, wspaces, 80);
  zap(wtotal, 5, wrep4.wyamt1, 4);
  ap(wtotal, 5, wrep4.wyamt2, 4);
  mp(wtotal, 5, wrep4.wyrate, 3);
  div_modp(wtotal, 3, (wtotal + 3), 2, wtotal, 5,
           p_dec("\x10\x0C", 100), 2);
  CLOSE(print2, &result_code);
/* */
  result_code = fmt001a4();
/* */
  regs.r15 = 0;
  exit_flag = 0;
  return (regs.r15);
}

/* ************************************************************ */
