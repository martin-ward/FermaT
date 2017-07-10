
/* Data initialisation routine: */

void
initialise_FMT001A6()
{
  memmove(test1a, "AAAA", 4);
  memmove(test1b, "2222", 4);
  memmove(test2, "BBBB", 4);
  memset(wrec2, ' ', 80);
  memmove(wtab1, "AAAA123456BBBCCC", 16);
  memmove(UNNAMED__0001, "BBBB123456XXXDDD", 16);
  memmove(UNNAMED__0002, "CCCC123456XXXDDD", 16);
  memmove(UNNAMED__0003, "DDDD123456XXXDDD", 16);
  memmove(UNNAMED__0004, "EEEE123456XXXDDD", 16);
  UNNAMED__0005 = 0xFF;
}
