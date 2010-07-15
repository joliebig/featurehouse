using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using System.Diagnostics.CodeAnalysis;

namespace Aga.Controls
{



 public static class TimeCounter
 {
  private static Int64 _start;




  public static void Start()
  {
   _start = 0;
   QueryPerformanceCounter(ref _start);
  }

  public static Int64 GetStartValue()
  {
   Int64 t = 0;
   QueryPerformanceCounter(ref t);
   return t;
  }





  public static double Finish()
  {
   return Finish(_start);
  }

  public static double Finish(Int64 start)
  {
   Int64 finish = 0;
   QueryPerformanceCounter(ref finish);

   Int64 freq = 0;
   QueryPerformanceFrequency(ref freq);
   return (finish - start) / (double)freq;
  }

  [DllImport("Kernel32.dll")]
  [return: MarshalAs(UnmanagedType.Bool)]
  static extern bool QueryPerformanceCounter(ref Int64 performanceCount);

  [DllImport("Kernel32.dll")]
  [return: MarshalAs(UnmanagedType.Bool)]
  static extern bool QueryPerformanceFrequency(ref Int64 frequency);
 }
}
