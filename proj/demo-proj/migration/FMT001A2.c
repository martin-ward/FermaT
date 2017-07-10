
#ifdef USE_VPP
/* $::SYSECT = uc("FMT001A2"); ""; */
@@ $::SYSECT = uc("FMT001A2");"";@@
#endif
#include "FMT001A2.h"
  FILE * print1;

/* End of declarations */

#include "FMT001A2_init.h"

int
fmt001a2(regs_t * p_regs)
{
  regs = *p_regs;
  exit_flag = 0;
/*  <ENTRY POINT>  */
/*  <NAME=FMT001A2>  */
/* ********************************************************************* */
/* */
  OPEN(print1, "OUTPUT", &result_code);
  memmove(wprt, "CALL TO FUNCTION FMT001A2", 25);
/* FIXME: Unknown File max for PRINT1  */
/* FIXME: Unknown File max for PRINT1  */
/* FIXME: Unknown File min for PRINT1  */
/* FIXME: Unknown File max for PRINT1  */
/* FIXME: Unknown File max for PRINT1  */
/* FIXME: Unknown File min for PRINT1  */
  PUT_FIXED(print1, wprt, &result_code);
  CLOSE(print1, &result_code);
/* */
  zap(wtotal, 4, p_dec("\x0C", 0), 1);
  ap(wct, 4, p_dec("\x1C", 1), 1);
  ap(wnum, 4, p_dec("\x1C", 1), 1);
/* */
  regs.r15 = 0;
  exit_flag = 0;
  return (regs.r15);
}

/* ************************************************************ */
