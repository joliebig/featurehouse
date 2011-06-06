using System;
using System.Collections.Generic;
using System.Text;
namespace Eraser.Util
{
 public static class RecycleBin
 {
  public static void Empty(EmptyRecycleBinOptions options)
  {
   NativeMethods.SHEmptyRecycleBin(IntPtr.Zero, null,
    (NativeMethods.SHEmptyRecycleBinFlags)options);
  }
 }
 [Flags]
 public enum EmptyRecycleBinOptions
 {
  None = 0,
  NoConfirmation = (int)NativeMethods.SHEmptyRecycleBinFlags.SHERB_NOCONFIRMATION,
  NoProgressUI = (int)NativeMethods.SHEmptyRecycleBinFlags.SHERB_NOPROGRESSUI,
  NoSound = (int)NativeMethods.SHEmptyRecycleBinFlags.SHERB_NOSOUND
 }
}
