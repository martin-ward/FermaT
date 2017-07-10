
                              /* Data initialisation routine: */

void
initialise_FMT001A3()
{
  memmove(wtotal, "\x00\x00\x00\x00\x0C", 5);
  memset(wprt, ' ', 80);
  memset(wspaces, ' ', 80);
  memmove(wrep1, "REPORT HEADING", 14);
  wrep2 = ' ';
  memmove(wrep3, "FIRST LINE OF REPORT", 20);
  memset(((BYTE *)&wrep4), ' ', 80);
  memmove(wktot1, "\x00\x00\x00\x0C", 4);
  memmove(wktot2, "\x00\x00\x00\x0C", 4);
  memmove(wktot3, "\x00\x00\x00\x0C", 4);
  memmove(wktot4, "\x00\x00\x00\x0C", 4);
  memmove(wktot5, "\x00\x00\x00\x0C", 4);
  memmove(wjtot1, "\x00\x00\x00\x0C", 4);
  memmove(wjtot2, "\x00\x00\x00\x0C", 4);
  memmove(wjtot3, "\x00\x00\x00\x0C", 4);
  memmove(wjtot4, "\x00\x00\x00\x0C", 4);
  memmove(wjtot5, "\x00\x00\x00\x0C", 4);
  memmove(wltot1, "\x00\x00\x00\x0C", 4);
  memmove(wltot2, "\x00\x00\x00\x0C", 4);
  memmove(wltot3, "\x00\x00\x00\x0C", 4);
  memmove(wltot4, "\x00\x00\x00\x0C", 4);
  memmove(wltot5, "\x00\x00\x00\x0C", 4);
  memmove(wnew1, "\x00\x00\x00\x0C", 4);
  memmove(wnew9, "\x99\x99\x99\x9C", 4);
  memmove(kktot1, "\x00\x00\x00\x0C", 4);
  memmove(kktot2, "\x00\x00\x00\x0C", 4);
  memmove(kktot3, "\x00\x00\x00\x0C", 4);
  memmove(kktot4, "\x00\x00\x00\x0C", 4);
  memmove(kktot5, "\x00\x00\x00\x0C", 4);
  memmove(wkg, "\x0C", 1);
}
