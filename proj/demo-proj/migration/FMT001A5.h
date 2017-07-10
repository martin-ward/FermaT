#include <assem.h>

                              /* EQUates table */
#define mfi0008  STAR__0001
#define wds1l    ((FWORD)&wds1->STAR__0002-(FWORD)wds1->wds1f1)
#define FILLER_fmt001a5__0001 local.__FILLER_fmt001a5__0001
#define b0007 local.__b0007
#define UNNAMED__0001 local.__UNNAMED__0001
#define FILLER_fmt001a5__0002 local.__FILLER_fmt001a5__0002
#define STAR__0001 local.__STAR__0001
#define FILLER_fmt001a5__0003 local.__FILLER_fmt001a5__0003
#define UNNAMED__0003 local.__UNNAMED__0003
#define FILLER_fmt001a5__0004 local.__FILLER_fmt001a5__0004
#define wrec2 local.__wrec2
#define wtab1 local.__wtab1
#define UNNAMED__0004 local.__UNNAMED__0004
#define UNNAMED__0005 local.__UNNAMED__0005
#define UNNAMED__0006 local.__UNNAMED__0006
#define UNNAMED__0007 local.__UNNAMED__0007
#define UNNAMED__0008 local.__UNNAMED__0008



                              /* Files */


                              /* --> DSECT: WDS1 <-- */
typedef struct { /* wds1 */
  BYTE  wds1f1[4];
  BYTE  wds1f2[6];
  BYTE  wds1f3[3];
  BYTE  wds1f4[3];
  BYTE  STAR__0002;
  BYTE  FILLER__0001;
} __attribute__((packed)) wds1_t;

static wds1_t *wds1 = 0;
static wds1_t *wds1_2 = 0;

                              /* --> DSECT: WDS2 <-- */
typedef struct { /* wds2 */
  BYTE  wds2f1[4];
  BYTE  FILLER__0002[4];
  BYTE  wds2f2[6];
  BYTE  FILLER__0003[4];
  BYTE  wds2f3[3];
  BYTE  FILLER__0004[4];
  BYTE  wds2f4[3];
} __attribute__((packed)) wds2_t;

static wds2_t *wds2 = 0;
static wds2_t *wds2_2 = 0;

                              /* --> DSECT: MODSTORE <-- */
typedef struct { /* modstore */
  FWORD amod113;
  FWORD amod114;
  FWORD amod115;
} __attribute__((packed)) modstore_t;

static modstore_t *modstore = 0;
static modstore_t *modstore_2 = 0;


static struct {
                              /* --> CSECT: FMT001A5_ <-- */
  BYTE  __FILLER__0005[104];
  BYTE  __b0007[4];
  FWORD __UNNAMED__0001;
  BYTE  __FILLER__0006[8];
  FWORD __STAR__0001;
  BYTE  __FILLER__0007[8];
  FWORD __UNNAMED__0003;
  BYTE  __FILLER__0008[12];
  BYTE  __wrec2[80];
  BYTE  __wtab1[16];
  BYTE  __UNNAMED__0004[16];
  BYTE  __UNNAMED__0005[16];
  BYTE  __UNNAMED__0006[16];
  BYTE  __UNNAMED__0007[16];
  BYTE  __UNNAMED__0008;

                              /* CSECT #defines */
} __attribute__((packed)) local;


                              /* --> EXPORTED DATA <-- */


/* External data (V constants) */

extern FWORD V_FMT113;
extern FWORD V_FMT114;
extern FWORD V_FMT115;
extern FWORD V_FMT200;

