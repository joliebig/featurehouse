

using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using System.Windows.Forms;

namespace Eraser.Util
{
 public static class ShellApi
 {





  public static void EmptyRecycleBin(EmptyRecycleBinOptions options)
  {
   NativeMethods.SHEmptyRecycleBin(IntPtr.Zero, null,
    (NativeMethods.SHEmptyRecycleBinFlags)options);
  }




  internal static class NativeMethods
  {
   [DllImport("Shlwapi.dll", CharSet = CharSet.Unicode)]
   [return: MarshalAs(UnmanagedType.Bool)]
   public static extern bool PathCompactPathEx(StringBuilder pszOut,
    string pszSrc, uint cchMax, uint dwFlags);
   [DllImport("Shell32.dll", CharSet = CharSet.Unicode)]
   public static extern uint SHEmptyRecycleBin(IntPtr hwnd, string pszRootPath,
    SHEmptyRecycleBinFlags dwFlags);
   public enum SHEmptyRecycleBinFlags : uint
   {
    SHERB_NOCONFIRMATION = 0x00000001,
    SHERB_NOPROGRESSUI = 0x00000002,
    SHERB_NOSOUND = 0x00000004
   }
   [DllImport("Shell32.dll", CharSet = CharSet.Unicode)]
   public static extern IntPtr SHGetFileInfo(string path, uint fileAttributes,
    ref SHFILEINFO psfi, int cbFileInfo, SHGetFileInfoFlags uFlags);
   public enum SHGetFileInfoFlags
   {
    SHGFI_ICON = 0x000000100,
    SHGFI_DISPLAYNAME = 0x000000200,
    SHGFI_TYPENAME = 0x000000400,
    SHGFI_ATTRIBUTES = 0x000000800,
    SHGFI_ICONLOCATION = 0x000001000,
    SHGFI_EXETYPE = 0x000002000,
    SHGFI_SYSICONINDEX = 0x000004000,
    SHGFI_LINKOVERLAY = 0x000008000,
    SHGFI_SELECTED = 0x000010000,
    SHGFI_ATTR_SPECIFIED = 0x000020000,
    SHGFI_LARGEICON = 0x000000000,
    SHGFI_SMALLICON = 0x000000001,
    SHGFI_OPENICON = 0x000000002,
    SHGFI_SHELLICONSIZE = 0x000000004,
    SHGFI_PIDL = 0x000000008,
    SHGFI_USEFILEATTRIBUTES = 0x000000010,
    SHGFI_ADDOVERLAYS = 0x000000020,
    SHGFI_OVERLAYINDEX = 0x000000040
   }
   [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)]
   public struct SHFILEINFO
   {
    public IntPtr hIcon;
    public int iIcon;
    public uint dwAttributes;
    [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 260)]
    public string szDisplayName;
    [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 80)]
    public string szTypeName;
   }
  }
 }
 [Flags]
 public enum EmptyRecycleBinOptions
 {
  None = 0,
  NoConfirmation = (int)ShellApi.NativeMethods.SHEmptyRecycleBinFlags.SHERB_NOCONFIRMATION,
  NoProgressUI = (int)ShellApi.NativeMethods.SHEmptyRecycleBinFlags.SHERB_NOPROGRESSUI,
  NoSound = (int)ShellApi.NativeMethods.SHEmptyRecycleBinFlags.SHERB_NOSOUND
 }
}
