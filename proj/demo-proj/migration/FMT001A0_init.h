
                              /* Data initialisation routine: */

void
initialise_FMT001A0()
{
  memmove(wct, "\x00\x00\x00\x0C", 4);
  memmove(wnum, "\x00\x00\x00\x0C", 4);
  memmove(wtotal, "\x00\x00\x00\x0C", 4);
}
