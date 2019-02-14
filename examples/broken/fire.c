#line 2 "fire.sxc"
#include <stdio.h> 
#line 3 "fire.sxc"
#include <string.h> 
#line 4 "fire.sxc"
#include <unistd.h> 
#line 5 "fire.sxc"
#include <stdint.h> 
#line 6 "fire.sxc"
#include <sys/time.h> 
#line 7 "fire.sxc"
#include <GL/gl.h> 
#line 8 "fire.sxc"
#include <GL/glut.h> 
#line 10 "fire.sxc"
float (g_spin ) = (0.0 );
#line 11 "fire.sxc"
int (g_width ) = (250 );
#line 12 "fire.sxc"
int
(g_height ) = (250 );
#line 14 "fire.sxc"
unsigned char
pixels [320 ][240 ][4 ];
#line 16 "fire.sxc"
void  f_keyboard(
#line 16 "fire.sxc"
unsigned char key , #line 16 "fire.sxc"
int x , #line 16 "fire.sxc"
int y ) {
#line 17 "fire.sxc"
exit(0 );

}
#line 19 "fire.sxc"
#line 19 "fire.sxc"
double  gettime(void ) {#line 20 "fire.sxc"
 double #line 20 "fire.sxc"
(now ) = (#line 20 "fire.sxc"
time(0 ));
#line 21 "fire.sxc"
struct timeval now_tv ;
#line 22 "fire.sxc"
now +=#line 22 "fire.sxc"
((#line 22 "fire.sxc"
((double )#line 22 "fire.sxc"
now_tv.tv_usec) ) / (1000000.0 ));
#line 23 "fire.sxc"
return(now );

}
#line 25 "fire.sxc"
#line 25 "fire.sxc"
void  f_display(void ) {#line 26 "fire.sxc"
 double #line 26 "fire.sxc"
(now ) = (#line 26 "fire.sxc"
gettime()), then ;
#line 27 "fire.sxc"
glClear(#line 27 "fire.sxc"
((GL_COLOR_BUFFER_BIT ) | (GL_DEPTH_BUFFER_BIT )));
#line 28 "fire.sxc"
glRasterPos3f(200 , 100 , 0 );
#line 30 "fire.sxc"
glPushMatrix();
#line 31 "fire.sxc"
glTranslatef(#line 31 "fire.sxc"
((g_width ) / (2 )), #line 31 "fire.sxc"
((g_height ) / (2 )), 0 );
#line 32 "fire.sxc"
glRotatef(g_spin , 0 , 0 , 1 );
#line 33 "fire.sxc"
glColor3f(1 , 1 , 1 );
#line 34 "fire.sxc"
glRectf(-25 , -25 , 25 , 25 );
#line 35 "fire.sxc"
glPopMatrix();
#line 36 "fire.sxc"
(then ) = (#line 36 "fire.sxc"
gettime());
#line 37 "fire.sxc"
 int #line 37 "fire.sxc"
(sleepytime ) = (#line 37 "fire.sxc"
((#line 37 "fire.sxc"
((#line 37 "fire.sxc"
((1.0 ) / (60 ))) - (#line 37 "fire.sxc"
((then ) - (now ))))) * (1000000 )));
#line 38 "fire.sxc"
printf("%d\n", sleepytime );
#line 39 "fire.sxc"
fflush(stdout );
#line 40 "fire.sxc"
usleep(sleepytime );
#line 41 "fire.sxc"
glutSwapBuffers();

}
#line 43 "fire.sxc"
#line 43 "fire.sxc"
void  f_idle(void ) {#line 44 "fire.sxc"
g_spin +=2.0 ;
#line 45 "fire.sxc"
if(#line 45 "fire.sxc"
((g_spin ) > (360.0 ))) {#line 46 "fire.sxc"
g_spin -=360.0 ;
};
#line 47 "fire.sxc"
glutPostRedisplay();

}
#line 49 "fire.sxc"
#line 49 "fire.sxc"
void  f_mouse(#line 49 "fire.sxc"
int button , #line 49 "fire.sxc"
int state , #line 49 "fire.sxc"
int x , #line 49 "fire.sxc"
int y ) {
#line 50 "fire.sxc"
switch (button ) {
case GLUT_LEFT_BUTTON : {
#line 52 "fire.sxc"
if(#line 52 "fire.sxc"
state ==GLUT_DOWN ) {#line 53 "fire.sxc"
glutIdleFunc(NULL );
break ;
};
}
case GLUT_RIGHT_BUTTON : {
#line 56 "fire.sxc"
if(#line 56 "fire.sxc"
state ==GLUT_DOWN ) {#line 57 "fire.sxc"
glutIdleFunc(f_idle );
break ;
};
}
}
;

}
#line 60 "fire.sxc"
#line 60 "fire.sxc"
void  f_reshape(#line 60 "fire.sxc"
int width , #line 60 "fire.sxc"
int height ) {
#line 61 "fire.sxc"
(g_width ) = (width );
#line 62 "fire.sxc"
(g_height ) = (height );
#line 63 "fire.sxc"
glViewport(0 , 0 , g_width , g_height );
#line 64 "fire.sxc"
glMatrixMode(GL_PROJECTION );
#line 65 "fire.sxc"
glLoadIdentity();
#line 66 "fire.sxc"
glOrtho(0 , g_width , 0 , g_height , -1 , 1 );
#line 67 "fire.sxc"
glMatrixMode(GL_MODELVIEW );

}
#line 69 "fire.sxc"
#line 69 "fire.sxc"
int  main(#line 69 "fire.sxc"
int argc , #line 69 "fire.sxc"
char #line 69 "fire.sxc"
(**argv )) {
#line 70 "fire.sxc"
glutInit(#line 70 "fire.sxc"
(&(argc )), argv );
#line 72 "fire.sxc"
 int i , j , k , color ;
#line 73 "fire.sxc"
for(#line 73 "fire.sxc"
(i ) = (0 ), #line 73 "fire.sxc"
(color ) = (0 ); #line 73 "fire.sxc"
((i ) < (320 )); #line 73 "fire.sxc"
(i ++), #line 73 "fire.sxc"
(color ++)) {
#line 74 "fire.sxc"
for(#line 74 "fire.sxc"
(j ) = (0 ); #line 74 "fire.sxc"
((j ) < (240 )); #line 74 "fire.sxc"
(j ++)) {
#line 75 "fire.sxc"
for(#line 75 "fire.sxc"
(k ) = (0 ); #line 75 "fire.sxc"
((k ) < (4 )); #line 75 "fire.sxc"
(k ++)) {
#line 76 "fire.sxc"
(#line 76 "fire.sxc"
(pixels [i ][j ][k ])) = (color );
};
};
};
#line 78 "fire.sxc"
glClearColor(0 , 0 , 0 , 0 );
#line 79 "fire.sxc"
glShadeModel(GL_FLAT );
#line 81 "fire.sxc"
glutInitWindowPosition(0 , 0 );
#line 82 "fire.sxc"
glutInitWindowSize(g_width , g_height );
#line 83 "fire.sxc"
glutInitDisplayMode(#line 83 "fire.sxc"
((GLUT_RGBA )|  (GLUT_DOUBLE ) |(GLUT_DEPTH )));
#line 84 "fire.sxc"
 int #line 84 "fire.sxc"
(window ) = (#line 84 "fire.sxc"
glutCreateWindow("sxc-fire"));
#line 85 "fire.sxc"
(window ) = (window );
#line 87 "fire.sxc"
glutDisplayFunc(f_display );
#line 88 "fire.sxc"
glutIdleFunc(f_idle );
#line 89 "fire.sxc"
glutKeyboardFunc(f_keyboard );
#line 90 "fire.sxc"
glutMouseFunc(f_mouse );
#line 91 "fire.sxc"
glutReshapeFunc(f_reshape );
#line 93 "fire.sxc"
glutPostRedisplay();
#line 94 "fire.sxc"
glutMainLoop();
#line 96 "fire.sxc"
return(0 );

}
