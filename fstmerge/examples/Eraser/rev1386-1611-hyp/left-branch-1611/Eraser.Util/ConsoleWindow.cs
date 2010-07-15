

using System;
using System.Collections.Generic;
using System.Text;

namespace Eraser.Util
{




 public class ConsoleWindow : IDisposable
 {
  public ConsoleWindow()
  {
   NativeMethods.AllocConsole();
  }



  ~ConsoleWindow()
  {
   Dispose(false);
  }

  public void Dispose()
  {
   Dispose(true);
   GC.SuppressFinalize(this);
  }

  [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage", "CA1801:ReviewUnusedParameters", MessageId = "disposing")]
  private void Dispose(bool disposing)
  {
   NativeMethods.FreeConsole();
  }


 }
}
