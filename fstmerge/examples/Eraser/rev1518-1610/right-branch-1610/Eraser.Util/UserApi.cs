

using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using System.Drawing;

namespace Eraser.Util
{
 public static class UserApi
 {



  public static Point CaretPos
  {
   get
   {
    Point result = new Point();
    if (NativeMethods.GetCaretPos(out result))
     return result;
    return Point.Empty;
   }
  }




  public static uint MessagePos
  {
   get
   {
    return NativeMethods.GetMessagePos();
   }
  }




  public static int MessageTime
  {
   get
   {
    return NativeMethods.GetMessageTime();
   }
  }
 }
}
