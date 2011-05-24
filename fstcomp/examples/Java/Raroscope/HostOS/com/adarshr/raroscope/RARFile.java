////

package com.adarshr.raroscope;
class RARFile {
  /** 
 * Translate the OS byte to a human readable string.
 * @param o the number to be translated (1 byte).
 * @return the OS string.
 */
   protected String toOS(  int o){
    String os=null;
switch (o) {
case 0:
      os="MS DOS";
    break;
case 1:
  os="OS/2";
break;
case 2:
os="Win32";
break;
case 3:
os="Unix";
break;
case 4:
os="Mac OS";
break;
case 5:
os="BeOS";
break;
default :
os="Unknown";
break;
}
return os;
}
}
