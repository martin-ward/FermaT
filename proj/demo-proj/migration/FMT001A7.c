
#ifdef USE_VPP
/* $::SYSECT = uc("FMT001A7"); ""; */
@@ $::SYSECT = uc("FMT001A7");"";@@
#endif
#include "FMT001A7.h"
/* Entry points are: fmt502, fmt501, fmt001a7 */
  int
fmt001a7(regs_t * p_regs);
int
fmt501(regs_t * p_regs);
int
fmt502(regs_t * p_regs);
void
fmt501_FMT001A7_p();

/* End of declarations */

#include "FMT001A7_init.h"

int
fmt001a7(regs_t * p_regs)
{
  regs = *p_regs;
  exit_flag = 0;
/*  <ENTRY POINT>  */
/*  <NAME=FMT001A7>  */
/*   SAMPLE PROGRAM WITH MULTIPLE ENTRY POINTS */
  fmt501_FMT001A7_p();
  exit_flag = 0;
  return (regs.r15);
}

/* ************************************************************ */

int
fmt501(regs_t * p_regs)
{
  regs = *p_regs;
  exit_flag = 0;
/*  <ENTRY POINT>  */
  fmt501_FMT001A7_p();
  exit_flag = 0;
  return (regs.r15);
}

/* ************************************************************ */

int
fmt502(regs_t * p_regs)
{
  regs = *p_regs;
  exit_flag = 0;
/*  <ENTRY POINT>  */
/* SAMPLE FUNCTION 2 */
  regs.r4 = *(FWORD *) regs.r1;
  memmove((BYTE *) regs.r4, "BBBB", 4);
  result_code = fmt602();
  regs.r15 = result_code;
  exit_flag = 0;
  return (regs.r15);
}

/* ************************************************************ */

void
fmt501_FMT001A7_p()
{
/* SAMPLE FUNCTION 1 */
  regs.r4 = *(FWORD *) regs.r1;
  memmove((BYTE *) regs.r4, "AAAA", 4);
  result_code = fmt601();
  regs.r15 = result_code;
  return;
}

/* ************************************************************ */
