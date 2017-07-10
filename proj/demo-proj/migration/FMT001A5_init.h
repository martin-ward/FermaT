
                              /* Data initialisation routine: */

void
initialise_FMT001A5()
{
  memmove(b0007, "YES ", 4);
  STAR__0001 = (FWORD)b0007;
  UNNAMED__0003 = V_FMT200;
  memset(wrec2, ' ', 80);
  memmove(wtab1, "AAAA123456BBBCCC", 16);
  memmove(UNNAMED__0004, "BBBB123456XXXDDD", 16);
  memmove(UNNAMED__0005, "CCCC123456XXXDDD", 16);
  memmove(UNNAMED__0006, "DDDD123456XXXDDD", 16);
  memmove(UNNAMED__0007, "EEEE123456XXXDDD", 16);
  UNNAMED__0008 = 0xFF;
}
