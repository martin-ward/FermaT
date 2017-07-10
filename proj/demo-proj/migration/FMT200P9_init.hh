
/* Data initialisation routine: */

void
initialise_FMT200P9()
{
  memmove(wreport.wf3, "\x00\x00\x12\x34\x5C\x00\x00\x12\x34\x5C\x00\x00\x12\x34\x5C", 15);
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
