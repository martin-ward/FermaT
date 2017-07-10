#include <assem.h>

/* EQUates table */

#define wds1l    ((FWORD)&wds1->STAR__0001-(FWORD)wds1->wds1f1)


/* Files */


/*           --> DSECT: WDS1 <-- */
typedef struct { /* wds1 */
  BYTE  wds1f1[4];
  BYTE  wds1f2[6];
  BYTE  wds1f3[3];
  BYTE  wds1f4[3];
  BYTE  STAR__0001;
  BYTE  FILLER_wds1_0001;
} __attribute__((packed)) wds1_t;

static wds1_t *wds1 = 0;
static wds1_t *wds1_2 = 0;

/*           --> DSECT: WDS2 <-- */
typedef struct { /* wds2 */
  BYTE  wds2f1[4];
  BYTE  FILLER_wds2_0001[4];
  BYTE  wds2f2[6];
  BYTE  FILLER_wds2_0002[4];
  BYTE  wds2f3[3];
  BYTE  FILLER_wds2_0003[4];
  BYTE  wds2f4[3];
} __attribute__((packed)) wds2_t;

static wds2_t *wds2 = 0;
static wds2_t *wds2_2 = 0;


static struct {
/*           --> CSECT: FMT001A6_ <-- */
  BYTE  __FILLER_fmt001a6__0001[36];
  BYTE  __test1a[4];
  BYTE  __test1b[4];
  BYTE  __FILLER_fmt001a6__0002[88];
  BYTE  __test2[4];
  BYTE  __wrec2[80];
  BYTE  __wtab1[16];
  BYTE  __UNNAMED__0001[16];
  BYTE  __UNNAMED__0002[16];
  BYTE  __UNNAMED__0003[16];
  BYTE  __UNNAMED__0004[16];
  BYTE  __UNNAMED__0005;

/* CSECT #defines */
#define FILLER_fmt001a6__0001 local.__FILLER_fmt001a6__0001
#define test1a local.__test1a
#define test1b local.__test1b
#define FILLER_fmt001a6__0002 local.__FILLER_fmt001a6__0002
#define test2 local.__test2
#define wrec2 local.__wrec2
#define wtab1 local.__wtab1
#define UNNAMED__0001 local.__UNNAMED__0001
#define UNNAMED__0002 local.__UNNAMED__0002
#define UNNAMED__0003 local.__UNNAMED__0003
#define UNNAMED__0004 local.__UNNAMED__0004
#define UNNAMED__0005 local.__UNNAMED__0005
} __attribute__((packed)) local;


/*           --> EXPORTED DATA <-- */

