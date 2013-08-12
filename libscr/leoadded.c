/* Stuff added by people other than Paul Parker */

void lscrsize_(display_width,display_height)

unsigned int *display_width,*display_height;
{
   void connect_to_display();
   connect_to_display();
   *display_width = DisplayWidth(display,screen_num);
   *display_height = DisplayHeight(display,screen_num);
   return;
}
