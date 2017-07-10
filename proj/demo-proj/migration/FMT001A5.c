
#ifdef USE_VPP
/* $::SYSECT = uc("FMT001A5"); ""; */
@@ $::SYSECT = uc("FMT001A5");"";@@
#endif
#include "FMT001A5.h"
/* End of declarations */
#include "FMT001A5_init.h"
  int
fmt001a5(regs_t * p_regs)
{
  regs = *p_regs;
  exit_flag = 0;
/*  <ENTRY POINT>  */
  wds2 = (wds2_t *) regs.r6;
  wds1 = (wds1_t *) regs.r5;
/*  <NAME=FMT001A5>  */
/* SAMPLE PROGRAM TO LOAD A SERIES OF PROGRAMS */
/* ADDRESSES STORED IN A REFERENCE TABLE */
  regs.r11 = *(FWORD *) regs.r1;
  modstore = (modstore_t *) regs.r11;
  result_code = load("FMT113 EP=FMT113", &regs.r0, &regs.r1);
  modstore->amod113 = V_FMT113;
  result_code = load("FMT114 EP=FMT114", &regs.r0, &regs.r1);
  modstore->amod114 = V_FMT114;
  result_code = load("FMT115 EP=FMT115", &regs.r0, &regs.r1);
  regs.r15 = result_code;
  modstore->amod115 = V_FMT115;
/*        USER CALL. DEPENDENCY SATISFIED BY FMT200P9 USING VERSION.TAB */
  regs.r5 = (FWORD) wtab1;
  wds1 = (wds1_t *) regs.r5;
  regs.r6 = (FWORD) wrec2;
  wds2 = (wds2_t *) regs.r6;
  for (;;) {                    /* DO loop 1 */
    if (*wds1->wds1f1 == 0xFF) {
      break;
    } else {
/* <FERMAT ANN><S><1> */
      *(FWORD *) wrec2 = *(FWORD *) wtab1;
      *(FWORD *) wds2->wds2f1 = *(FWORD *) wds1->wds1f1;
      memmove(wds2->wds2f2, wds1->wds1f2, 6);
      memmove(wds2->wds2f3, wds1->wds1f3, 3);
      memmove(wds2->wds2f4, wds1->wds1f4, 3);
/* <FERMAT ANN><E><1> */
                                                   /* DATA MACRO: CNOP 0,4 */
      regs.r0 = 1;
                                                   /* DATA MACRO: CNOP 0,4 */
      result_code = CALL_VIA_PTR(*(FWORD *) (A + 135), &regs);
      regs.r15 = result_code;
    }
  }                             /* OD */
  exit_flag = 0;
  return (regs.r15);
}

/* ************************************************************ */
