
/* Data initialisation routine: */

void
initialise_FMT001A4()
{
  memmove(UNNAMED__0001, "FMT001A4", 8);
  memmove(UNNAMED__0002, "06/22/05  ", 10);
  UNNAMED__0003 = V_FMT107;
  memmove(b0024, "YES ", 4);
  memmove(b0025, "NO  ", 4);
  UNNAMED__0006 = V_FMT501;
  UNNAMED__0007 = V_FMT502;
  memmove(b0068, "YES ", 4);
  memmove(b0069, "YES ", 4);
  STAR__0001 = (FWORD)&wktot;
  UNNAMED__0011 = V_FMT302;
  STAR__0002 = (FWORD)&wktot;
  UNNAMED__0013 = (FWORD)wrec.wrtot;
  UNNAMED__0014 = V_FMT303;
  vmod1 = V_FMT101;
  amod3 = V_FMT103;
  wtype = '0';
  memmove(wcalc, "        ", 8);
  memmove(wtotal, "\x00\x00\x00\x0C", 4);
  memset(whead1, ' ', 133);
  memset(whead2, ' ', 133);
  memmove(wprt.UNNAMED__0019, "    ", 4);
  memmove(wprt.wpf1, "            ", 12);
  memmove(wprt.UNNAMED__0020, "4   ", 4);
  memmove(wprt.wpftot1, "          ", 10);
  memmove(wprt.UNNAMED__0021, "     ", 5);
  memmove(wprt.wpftot2, "          ", 10);
  memmove(wprt.UNNAMED__0022, "     ", 5);
  memmove(wprt.wpftot3, "          ", 10);
  memmove(wprt.UNNAMED__0023, "     ", 5);
  memmove(wtot1, "\x00\x00\x00\x0C", 4);
  memmove(wtot2, "\x00\x00\x00\x0C", 4);
  memmove(wtot3, "\x00\x00\x00\x0C", 4);
  memmove(wktot, "\x00\x00\x00\x0C", 4);
  memmove(wztot1, "\x00\x0C", 2);
  memmove(wp1, "1111    ", 8);
  memmove(wp2, "2222    ", 8);
  memmove(wdtot1, "\x00\x00\x00\x0C", 4);
  memmove(wdtot2, "\x00\x00\x00\x0C", 4);
  memmove(wdtot3, "\x00\x00\x00\x0C", 4);
  memmove(wdtot4, "\x00\x00\x00\x0C", 4);
  memmove(wdtot5, "\x00\x00\x00\x0C", 4);
  memmove(wetot1, "\x00\x00\x00\x0C", 4);
  memmove(wetot2, "\x00\x00\x00\x0C", 4);
  memmove(wetot3, "\x00\x00\x00\x0C", 4);
  memmove(wetot4, "\x00\x00\x00\x0C", 4);
  memmove(wetot5, "\x00\x00\x00\x0C", 4);
  memmove(wftot1, "\x00\x00\x00\x0C", 4);
  memmove(wftot2, "\x00\x00\x00\x0C", 4);
  memmove(wftot3, "\x00\x00\x00\x0C", 4);
  memmove(wftot4, "\x00\x00\x00\x0C", 4);
  memmove(wftot5, "\x00\x00\x00\x0C", 4);
}
