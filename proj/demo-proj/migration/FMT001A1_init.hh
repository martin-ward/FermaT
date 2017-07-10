
/* Data initialisation routine: */

void
initialise_FMT001A1()
{
  memset(wrec, ' ', 80);
  memset(wprt, ' ', 80);
  memmove(wlast, "                    ", 20);
}
