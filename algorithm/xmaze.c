/*
ccoptions: -lX11 -Wall

  xmaze.c: (C) 1982,2011 Danny Chouinard.

  xmaze.c is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public
  License as published by the Free Software Foundation;
  either version 2, or (at your option) any later version.

  RDOS 3.2 is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty
  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public
  License along with Bash; see the file COPYING.  If not,
  write to the Free Software Foundation, 59 Temple Place,
  Suite 330, Boston, MA 02111 USA.

  See http://www.gnu.org/licenses/gpl-2.0.html

*/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <X11/Xlib.h>
#include <X11/X.h>
#include <X11/Xutil.h>
#include <sys/types.h>
#include <sys/time.h>
#include <time.h>
#include <string.h>
#include <signal.h>
#include <X11/cursorfont.h>
#include <X11/Xatom.h>

#define NCOLORS 32
#define chldmax 16

#define ULL unsigned long long

int mag;
int fg=15;
int bg=0;
Display *display;
Screen *scr_ptr;
unsigned char *scr;
unsigned short int code=0;
unsigned short int width;
unsigned short int height;
int totalw,totalh,steps;
int screen_num,root_x,root_y,win_x,win_y;
unsigned int mask;
Window main_win,root,win;
Pixmap main_pixmap;
XEvent the_event;
GC the_GC;
unsigned long colors[NCOLORS];
XColor screen_color,exact_color;
int fontwidth, fontheight;
Font font;
XFontStruct *fontinfo;
char fontname[256];
int statusline=1;
char status[256];
Cursor cursor;
Pixmap cursorsource,cursormask;
XColor cursorfg,cursorbg;
GC gcfg,gcbg;
unsigned long gc_mask;
XGCValues xgcv;
int button;
Atom wmDeleteMessage;


time_t start,end;
ULL seednumber=0;
unsigned short int seed=0;
int i,j,c,x,y,d,m,ox,oy,t,h,xx1,yy1,r;
int flatness;
int delay;
int segments;
int targetsegments;
int opt;
int sx[4];
int sy[4];
int n[4];
int chldx[chldmax];
int chldy[chldmax];
int chldd[chldmax];
int maxdepth;
short int *px;
short int *py;

char *colornames[]={
  "#000000", "#cc0000", "#00cc00", "#cccc00",\
  "#0000cc", "#cc00cc", "#00cccc", "#cccccc", \
  "#010101", "#ff0000", "#00ff00", "#ffff00",\
  "#0000ff", "#ff00ff", "#00ffff", "#ffffff",\
  "#666666", "#ff6666", "#66ff66", "#ffff66",\
  "#6666ff", "#ff66ff", "#66ffff", "#fefefe",\
  "#cbcbcb", "#ffcccc", "#ccffcc", "#ffffcc",\
  "#ccccff", "#ffccff", "#ccffff", "#fdfdfd"
};

void nap(int ms) {
  struct timeval tl;
  tl.tv_sec=0;
  tl.tv_usec=ms*1000;
  while(tl.tv_usec) {
    select(1,0,0,0,&tl);
  }
}

void expose(void) {
  XFlush(display);
  while(XPending(display)) {
    XNextEvent(display,&the_event);
    if (the_event.type == ClientMessage && \
        the_event.xclient.data.l[0] == wmDeleteMessage) {
      fprintf(stderr,"\r\n");
      exit(0);
    }
    switch(the_event.type) {
      case ButtonPress:
        button=1;
        break;
      case ButtonRelease:
        button=0;
        break;
      case Expose:
        XCopyArea(display,main_pixmap,main_win,the_GC,0,0,totalw,totalh,0,0);
        break;
    }
  }
}

void waitnobutton(void) {
  while(button) {
    expose();
    nap(50);
  }
}

void waitbutton(void) {
  while(button==0) {
    expose();
    nap(50);
  }
}

void showstatus(int nice) {
  static struct timeval tv,tvo;
  gettimeofday(&tv,0);
  if((tv.tv_usec-tvo.tv_usec > 50000) || (nice==0)) {
    if(statusline) {
    XSetForeground(display,the_GC,colors[fg]);
    XSetBackground(display,the_GC,colors[bg]);
      XDrawImageString(display,main_win,the_GC,0,height*mag+fontheight,status,strlen(status));
      XDrawImageString(display,main_pixmap,the_GC,0,height*mag+fontheight,status,strlen(status));
    }
    fprintf(stderr,"\r%s    ",status);
    expose();
    tvo.tv_usec=tv.tv_usec;
  }
  if(tvo.tv_sec!=tv.tv_sec) {
    tvo.tv_sec=tv.tv_sec;
    tvo.tv_usec=tv.tv_usec;
  }
}

void clearstatus(void) {
  strcpy(status,"                                                          ");
  showstatus(0);
  strcpy(status,"");
  showstatus(0);
}

void pset(int x, int y, int color ) {
  scr[x*height+y]=color;
  XSetForeground(display,the_GC,colors[color]);
  XFillRectangle(display,main_pixmap,the_GC,x*mag,y*mag,mag,mag);
  XFillRectangle(display,main_win,the_GC,x*mag,y*mag,mag,mag);
  expose();
}

int ppoint(int x, int y) {
  /*
  static XImage *ximage;
  int i,r;
  ximage=XGetImage(display,main_pixmap,x*mag,y*mag,1,1,AllPlanes,ZPixmap);
  r=XGetPixel(ximage,0,0);
  for(i=0;i<NCOLORS;i++) {
    if(r==colors[i]) {
      return(i);
    }
  }
  return(r);
  */
  return(scr[x*height+y]);
}

void color(int f, int b) {
  fg=f; bg=b;
  XSetForeground(display,the_GC,colors[fg]);
}

void line(int xx1, int yy1, int x2, int y2, int color) {
  int i;
  XSetForeground(display,the_GC,colors[color]);
  if(yy1==y2) {
    if(x2>xx1) {
      for(i=xx1;i<=x2;i++) pset(i,yy1,color);
    } else {
      for(i=x2;i<=xx1;i++) pset(i,yy1,color);
    }
  } else {
    if(y2>yy1) {
      for(i=yy1;i<=y2;i++) pset(xx1,i,color);
    } else {
      for(i=y2;i<=yy1;i++) pset(xx1,i,color);
    }
  }
  /*
  XDrawLine(display,main_pixmap,the_GC,xx1*mag,yy1*mag,x2*mag,y2*mag);
  XDrawLine(display,main_win,the_GC,xx1*mag,yy1*mag,x2*mag,y2*mag);
  */
  expose();
}

void box(int xx1, int yy1, int x2, int y2, int color) {
  int i,x,y,w,h;
  x=xx1<x2?xx1:x2;
  y=yy1<y2?yy1:y2;
  w=abs(x2-xx1);
  h=abs(y2-yy1);
  for(i=x;i<=x+w;i++) {
    pset(i,y,color);
    pset(i,y+h,color);
  }
  for(i=y;i<=y+h;i++) {
    pset(x,i,color);
    pset(x+w,i,color);
  }
  expose();
}

void filledbox(int xx1, int yy1, int x2, int y2, int color) {
  int i,j,x,y,w,h;
  x=xx1<x2?xx1:x2;
  y=yy1<y2?yy1:y2;
  w=abs(x2-xx1);
  h=abs(y2-yy1);
  XSetForeground(display,the_GC,colors[color]);
  XFillRectangle(display,main_pixmap,the_GC,x*mag,y*mag,w*mag,h*mag);
  XFillRectangle(display,main_win,the_GC,x*mag,y*mag,w*mag,h*mag);
  expose();
  for(i=x;i<=(x+w);i++) {
    for(j=y;j<=(y+h);j++) {
      scr[i*height+j]=color;
    }
  }
}

void pcls(int color) {
  filledbox(0,0,width-1,height-1,color);
}

void screen(int width, int height) {
  int a,r;
  if ((display=XOpenDisplay(NULL)) == NULL) {
    fprintf(stderr,"Could not connect to X server; not using X.\n");
    exit(1);
  }
  scr_ptr = DefaultScreenOfDisplay(display);
  screen_num = DefaultScreen(display);
  for(a=0;a<32;a++) {
    r=XAllocNamedColor(display,DefaultColormap(display,screen_num),\
    colornames[a],&screen_color,&exact_color);
    if(r==0) {
      fprintf(stderr,"Could not allocate color '%s'.\n",colornames[a]);
      exit(1);
    }
    colors[a]=screen_color.pixel;
    if(a==0) cursorbg=screen_color;
    if(a==1) cursorfg=screen_color;
  }
  totalw=width*mag;
  totalh=height*mag;
  fontwidth=0;
  fontheight=0;
  font=XLoadFont(display,fontname);
  if(font==BadName) {
    fprintf(stderr,"Font %s not found, trying \"fixed\".\n",fontname);
    font=XLoadFont(display,"fixed");
    if(font==BadName) {
      fprintf(stderr,"Fixed font not found.  Giving up.\n");
      exit(1);
    }
  }
  fontinfo=XQueryFont(display,font);
  fontwidth=XTextWidth(fontinfo,"Z",1);
  fontheight=fontinfo->ascent+fontinfo->descent;
  if(statusline) {
    totalh+=fontheight+2;
  }
  main_pixmap=XCreatePixmap(display,DefaultRootWindow(display),\
                            totalw,totalh,DefaultDepth(display,screen_num));

  main_win = XCreateSimpleWindow(display,DefaultRootWindow(display), \
                                 0,0,totalw,totalh,1,colors[1],colors[0]);
  the_GC = XCreateGC(display,main_win,0,0);
  XSetForeground(display,the_GC,0);
  XSetFont(display,the_GC,font);
  XStoreName(display,main_win,"xmaze");
  XMapWindow(display,main_win);
  XSelectInput(display, main_win, ButtonReleaseMask|ButtonPressMask|ExposureMask|VisibilityChangeMask);
  XSetGraphicsExposures(display,the_GC,True);
  XSetForeground(display,the_GC,colors[bg]);
  XSetBackground(display,the_GC,colors[bg]);
  XFillRectangle(display,main_pixmap,the_GC,0,0,totalw,totalh);
  XFillRectangle(display,main_win,the_GC,0,0,totalw,totalh);
  XSetForeground(display,the_GC,colors[fg]);
  // cursor = XCreateFontCursor(display,XC_crosshair);
  // XDefineCursor(display,main_win,cursor);
  //
  // This is why X11 turns off a many budding programmers:
  // Make the cursor a simple red box:
  //
  cursorsource=XCreatePixmap(display,DefaultRootWindow(display),\
                            mag,mag,1);
  cursormask=XCreatePixmap(display,DefaultRootWindow(display),\
                            mag,mag,1);
  XAllocNamedColor(display,DefaultColormap(display,screen_num),\
    colornames[1],&screen_color,&exact_color);
  cursorbg=screen_color;
  XAllocNamedColor(display,DefaultColormap(display,screen_num),\
    colornames[0],&screen_color,&exact_color);
  cursorbg=screen_color;
  gc_mask = GCForeground;
  xgcv.foreground = 0;
  gcbg=XCreateGC(display,cursorsource,gc_mask,&xgcv);
  gc_mask = GCForeground;
  xgcv.foreground = 1;
  gcfg=XCreateGC(display,cursorsource,gc_mask,&xgcv);
  XFillRectangle(display,cursormask,gcfg,0,0,mag,mag);
  XFillRectangle(display,cursorsource,gcfg,0,0,mag,mag);
  cursor=XCreatePixmapCursor(display,cursorsource,cursormask,&cursorfg,&cursorbg,mag/2,mag/2);
  XDefineCursor(display,main_win,cursor);
  // See how simple that was?
  wmDeleteMessage = XInternAtom(display, "WM_DELETE_WINDOW", False);
  XSetWMProtocols(display, main_win, &wmDeleteMessage, 1);
  expose();
}

void usage(void) {
  printf("\n\
xmaze produces mazes that have exactly one solution by using the\n\
wandering painter technique.  A pour soul is placed in an empty maze with\n\
a bucket of paint and a spool of thread.  Before he takes any step, he\n\
checks how many directions are free of paint.  If none are free, he goes\n\
back using the thread he laid down previously.  If the direction he was\n\
already going is free, he continues, but switches direction at random.\n\
If the direction he was going isn't free, he chooses one of the other\n\
free directions at random.\n\
\n\
On his way, if he detects more than one free direction, he fabricates\n\
a painter robot and sends it in a random direction.  The robot goes in\n\
random directions, without ever going back on its steps and self-destructs\n\
when it can move no longer.  The wanderer can have up to sixteen robots\n\
helping him at once.\n\
\n\
When the wanderer backs right up to the starting point following his\n\
thread, he stops, relaxes and enjoys a nice pint with the knowledge of\n\
a job well done, for the maze is complete with every corner painted.\n\
\n\
The width and height are given in dots, not pixels, so to figure out\n\
how large a window you'll get, multiply by the magnification factor.\n\
\n\
The delay factor (-d) is used in maze generation and when the CPU is\n\
solving the maze.  In both of those cases, you can hold down the mouse\n\
button to skip the delays.\n\
\n\
The flatness factor (-r) determines how unlikely the wanderer or the\n\
robots are to turn to a new random direction.  Low values near zero make\n\
for a \"spiky\" maze, high values make for a maze with long corridors.\n\
\n\
Every time xmaze produces a maze, it displays the \"maze seed number\"\n\
at the beginning.  This number can be re-used with the -n argument in\n\
order to re-draw the same maze.  This 64 bit hexadecimal number holds\n\
the flatness factor, width, height, random seed of a maze, and a checksum.\n\
\n\
Without any parameters, xmaze produces a 100x80 maze with maginification\n\
of 8 and a delay of 10 milli-seconds.\n\
\n\
The color numbers used by the foreground (-f) and background (-b)\n\
parameters are defined in the source from a set of 32.  0 is black,\n\
15 is white.  Try other values if you're curious.\n\
\n\
An xmaze run consists of three phases, which can be accelerated\n\
by holding down any mouse button:\n\
\n\
1- Maze generation.\n\
2- User solving the maze.\n\
3- Animating the users' solution.\n\
4- Computer solving the maze.\n\
\n\
\n\
Have fun!\n\
\n\
Danny Chouinard, Montreal, Frebruary 2011.\n\
\n\
");
  fflush(stdout);
  fprintf(stderr,"Usage: xmaze -w width -h height  Set size of maze.\n");
  fprintf(stderr,"             -m magnification    Set size of dots.\n");
  fprintf(stderr,"             -d delay            Milliseconds between updates.\n");
  fprintf(stderr,"             -r flatness         Lower values increase chance of turns [0-~].\n");
  fprintf(stderr,"             -f #                Foreground color [0-31].\n");
  fprintf(stderr,"             -b #                Background color [0-31].\n");
  fprintf(stderr,"             -n #                Maze seed number.\n");
  fprintf(stderr,"             -s                  Don't show statusline.\n");
  exit(1);
}

void initmaze(void) {
  strcpy(fontname,"10x20");
  sx[0]=2;  sy[0]=0;
  sx[1]=0;  sy[1]=2;
  sx[2]=-2; sy[2]=0;
  sx[3]=0;  sy[3]=-2;
  if(flatness<0) flatness=0;
  if(fg<0 || fg>31) fg=15;
  if(bg<0 || bg>31) bg=0;
  if(fg==bg) { fg=15;bg=0; }
  if(mag<1) mag=1;
  if(width<14) width=14;
  if(height<14) height=14;
  if(seednumber==0) {
    seednumber=((ULL)(flatness)<<56) | ((ULL)(width)<<40) | ((ULL)(height)<<24) | ((ULL)(seed)<<8) | \
           ((flatness+width+height+seed)&255);
  } else {
    flatness=(seednumber>>56);
    width=(seednumber>>40);
    height=(seednumber>>24);
    seed=(seednumber>>8);
  }
  if(((flatness+width+height+seed)&255) != (seednumber&255)) {
    fprintf(stderr,"Invalid maze seed number.\n");
    exit(1);
  }
  srand(seed);
  maxdepth=height*width/2; // Double what we need to generate. (Solve can be long).
  px=malloc(maxdepth*sizeof(short int));
  py=malloc(maxdepth*sizeof(short int));
  scr=malloc(width*height);
  targetsegments=(width/2-5)*(height/2-5)-1;
  fprintf(stderr,"Maze seed number: %016llX.\n",seednumber);
  fprintf(stderr,"%dx%d M:%d Delay:%d Status:%s FG:%d BG:%d Segments:%d Flatness:%d\n",width,height,mag,delay,statusline?"ON":"OFF",fg,bg,targetsegments,flatness);
}

void initscreen(void) {
  screen(width,height);
  color(fg,bg);
  expose();
  pcls(bg);
  clearstatus();
  box(4,4,width-4,height-4,fg);
}

void makemaze(void) {
  segments=0;
  h=0;
  x=6+((rand()%(width-10))&32766);
  y=6+((rand()%(height-10))&32766);
  ox=x; oy=y;t=flatness; d=1;
  m=0; px[m]=x;py[m]=y;m++;
  start=time(0);
  for(i=0;i<chldmax;i++) {
    chldd[i]=-1;
  }
  while(m>=0) {
    for(i=0;i<chldmax;i++) {
      if(chldd[i]!=-1) {
        xx1=chldx[i]+sx[chldd[i]];
        yy1=chldy[i]+sy[chldd[i]];
        if(ppoint(xx1,yy1)==bg) {
          px[m]=chldx[i];
          py[m]=chldy[i];
          m++;
          line(chldx[i],chldy[i],xx1,yy1,fg);
          segments++;
          chldx[i]=xx1;
          chldy[i]=yy1;
          if((rand()%(flatness+1))==0) chldd[i]=rand()%4;
        } else {
          c=0;
          for(j=0;j<4;j++) {
            if(ppoint(chldx[i]+sx[j],chldy[i]+sy[j])==bg) c++;
          }
          if(c==0) {
            chldd[i]=-1;
          } else {
            chldd[i]=rand()%4;
          }
        }
      }
    }
    if(statusline) {
      sprintf(status," %d %%%d  ",segments,segments*100/targetsegments);
      showstatus(1);
    }
    c=0;
    for(i=0;i<4;i++) {
      r=ppoint(x+sx[i],y+sy[i]);
      if(r==bg) {
        n[c]=i; c++;
      }
    }
    if(c==0) {
      x=px[m-1];
      y=py[m-1];
      m--;
      ox=x;
      oy=y;
    } else {
      if(delay) if (button==0) nap(delay);
      if(ppoint(x+sx[d],y+sy[d])!=bg) {
        d=n[rand()%c];
        while(c) {
          for(i=0;i<chldmax;i++) {
            if(chldd[i]== -1) {
              chldx[i]=x;
              chldy[i]=y;
              chldd[i]=rand()%4;
              break;
            }
          }
          c--;
        }
      } else {
        xx1=x; yy1=y;
        x=x+sx[d]; y=y+sy[d];
        line(x,y,xx1,yy1,fg);
        px[m]=x;
        py[m]=y;
        m++;
        if(m>h) h=m;
        segments++;
        t--;
        if(t<0) {
          t=(rand()%(flatness+1));
          d=rand()%4;
        }
      }
    }
  }
  end=time(0);
  for(i=1;i<4;i++) {
    box(i,i,width-i-1,height-i-1,fg);
  }
  box(4,4,width-4,height-4,bg);
  pset(5,6,fg);
  line(width-2,height-6,width-6,height-6,fg);
  sprintf(status," T:%d D:%d S:%d",(int)(end-start),h,segments);
  showstatus(1);
}

void usersolve() {
  x=5;y=6;ox=x;oy=y;
  steps=0;r=1;
  m=0;
  while((x!=width-3) && (button==0)) {
    expose();
    XQueryPointer(display,main_win,&root,&win,&root_x,&root_y,&win_x,&win_y,&mask);
    while(ox!=x || oy!=y) {
      steps++;
      pset(ox,oy,7);
      px[m]=ox;py[m]=oy; if(m<maxdepth) m++;
      sprintf(status," Steps: %d",steps);
      showstatus(0);
      if(ox!=x) ox=x;
      else oy=y;
    }
    pset(ox,oy,7);
    ox=x;oy=y;
    pset(x,y,(fg+1)&31);
    if((x>win_x/mag) && (ppoint(x-1,y)!=bg)) x--;
    if((x<win_x/mag) && (ppoint(x+1,y)!=bg)) x++;
    if((y>win_y/mag) && (ppoint(x,y-1)!=bg)) y--;
    if((y<win_y/mag) && (ppoint(x,y+1)!=bg)) y++;
    nap(50);
  }
  if(x==width-3) {
    code=rand();
    sprintf(status," %d Steps! Code:%04X ",steps,code);
    showstatus(0);
    i=0; c=1; button=0;
    while(button==0) {
      pset(ox,oy,c);
      ox=px[i]; oy=py[i];
      pset(ox,oy,0);
      i++;
      if(i>=m) {
        i=0;
        c=(c+1)&31;
        if(c==bg) c=(c+1)&31;
      }
      expose();
      nap(20);
    }
  }
  waitnobutton();
}

int solvefrom(int x, int y) {
  int i;
  static int solved=0;
  static int solvedepth=0;
  if(solved) return(1);
  solvedepth++;
  if(solvedepth>h) h=solvedepth;
  pset(x,y,fg);
  sprintf(status," %d,%d Depth:%d  ",x,y,solvedepth);
  showstatus(1);
  if(x>=(width-4)) {
    solved=1;
    sprintf(status,"Solved! Depth:%d ",solvedepth);
    showstatus(0);
    return(1);
  }
  for(i=0;i<4;i++) {
    if(ppoint(x+sx[i]/2,y+sy[i]/2)==(fg^31)) {
      if(solvefrom(x+sx[i]/2,y+sy[i]/2)) {
        if(button==0 && solved==0) nap(delay);
        solvedepth--; return(1);
      }
    }
  }
  pset(x,y,fg^31);
  if(button==0) nap(delay);
  solvedepth--;
  return(0);
}

void cpusolve(void) {
  h=0;
  sprintf(status,"Clearing maze...");
  showstatus(0);
  for(x=4;x<width-3;x++) {
    for(y=4;y<height-3;y++) {
      if(ppoint(x,y)!=bg) pset(x,y,fg^31);
    }
  }
  x=5;y=6;
  r=solvefrom(x,y);
  sprintf(status,"Solution depth: %d",h);
  showstatus(0);
}

int main(argc,argv)
int argc; char *argv[]; {
  delay=10; flatness=0; fg=15;bg=4; mag=8; width=100; height=80; seed=time(0);
  while((opt=getopt(argc,argv,"m:w:h:d:r:sf:b:n:"))!=-1) {
    switch(opt) {
      case 'm': mag=atoi(optarg); break;
      case 'w': width=32766&atoi(optarg); break;
      case 'h': height=32766&atoi(optarg); break;
      case 'd': delay=atoi(optarg); break;
      case 'r': flatness=atoi(optarg); break;
      case 'f': fg=atoi(optarg); break;
      case 'b': bg=atoi(optarg); break;
      case 'n': sscanf(optarg,"%llx",&seednumber); break;
      case 's': statusline=0; break;
      default : usage();
    }
  }
  initmaze();
  initscreen();
  makemaze();
  clearstatus();
  waitnobutton();
  usersolve();
  waitnobutton();
  clearstatus();
  cpusolve();
  waitnobutton();
  waitbutton();
  clearstatus();
  fprintf(stderr,"\r");
  exit(0);
}
