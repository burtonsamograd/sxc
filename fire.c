#include <stdio.h> 
#include <string.h> 
#include <unistd.h> 
#include <stdint.h> 
#include <sys/time.h> 
#include <GL/gl.h> 
#include <GL/glut.h> 
 float (g_spin ) = (0.0 );
 int (g_width ) = (250 );
 int (g_height ) = (250 );
 unsigned char pixels [320 ][240 ][4 ];
void  f_keyboard(unsigned char key , int x , int y ) {
exit(0 );

}
double  gettime(void ) { double (now ) = (time(0 ));
struct timeval now_tv ;
now +=((((double )now_tv.tv_usec) ) / (1000000.0 ));
return(now );

}
void  f_display(void ) { double (now ) = (gettime()), then ;
glClear(((GL_COLOR_BUFFER_BIT ) | (GL_DEPTH_BUFFER_BIT )));
glRasterPos3f(200 , 100 , 0 );
glPushMatrix();
glTranslatef(((g_width ) / (2 )), ((g_height ) / (2 )), 0 );
glRotatef(g_spin , 0 , 0 , 1 );
glColor3f(1 , 1 , 1 );
glRectf(-25 , -25 , 25 , 25 );
glPopMatrix();
(then ) = (gettime());
 int (sleepytime ) = (((((((1.0 ) / (60 ))) - (((then ) - (now ))))) * (1000000 )));
printf("%d\n", sleepytime );
fflush(stdout );
usleep(sleepytime );
glutSwapBuffers();

}
void  f_idle(void ) {g_spin +=2.0 ;
if(((g_spin ) > (360.0 ))) {g_spin -=360.0 ;
};
glutPostRedisplay();

}
void  f_mouse(int button , int state , int x , int y ) {
switch (button ) {
case GLUT_LEFT_BUTTON : {
if(state ==GLUT_DOWN ) {glutIdleFunc(NULL );
break ;
};
}
case GLUT_RIGHT_BUTTON : {
if(state ==GLUT_DOWN ) {glutIdleFunc(f_idle );
break ;
};
}
}
;

}
void  f_reshape(int width , int height ) {
(g_width ) = (width );
(g_height ) = (height );
glViewport(0 , 0 , g_width , g_height );
glMatrixMode(GL_PROJECTION );
glLoadIdentity();
glOrtho(0 , g_width , 0 , g_height , -1 , 1 );
glMatrixMode(GL_MODELVIEW );

}
int  main(int argc , char (**argv )) {
glutInit((&(argc )), argv );
 int i , j , k , color ;
for((i ) = (0 ), (color ) = (0 ); ((i ) < (320 )); (i ++), (color ++)) {
for((j ) = (0 ); ((j ) < (240 )); (j ++)) {
for((k ) = (0 ); ((k ) < (4 )); (k ++)) {
((pixels [i ][j ][k ])) = (color );
};
};
};
glClearColor(0 , 0 , 0 , 0 );
glShadeModel(GL_FLAT );
glutInitWindowPosition(0 , 0 );
glutInitWindowSize(g_width , g_height );
glutInitDisplayMode(((GLUT_RGBA )|  (GLUT_DOUBLE ) |(GLUT_DEPTH )));
 int (window ) = (glutCreateWindow("sxc-fire"));
(window ) = (window );
glutDisplayFunc(f_display );
glutIdleFunc(f_idle );
glutKeyboardFunc(f_keyboard );
glutMouseFunc(f_mouse );
glutReshapeFunc(f_reshape );
glutPostRedisplay();
glutMainLoop();
return(0 );

}