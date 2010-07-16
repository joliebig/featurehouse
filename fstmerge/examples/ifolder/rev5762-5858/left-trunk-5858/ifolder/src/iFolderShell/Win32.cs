

using System;
using System.Runtime.InteropServices;

namespace Novell.Win32Util
{



 [ComVisible(false)]
 public class Win32
 {



  public const int LVS_REPORT = 0x0001;



  public const int LVS_SHOWSELALWAYS = 0x0008;



  public const int LVS_OWNERDATA = 0x1000;




  public const int LVIF_TEXT = 0x0001;



  public const int LVIF_IMAGE = 0x0002;



  public const int LVIF_STATE = 0x0008;



  public const int LVIF_INDENT = 0x0010;




  public const int WM_DESTROY = 0x0002;



  public const int WM_NOTIFY = 0x004E;




  public const int LVN_FIRST = -100;



  public const int LVN_ITEMCHANGED = LVN_FIRST-1;



  public const int LVN_ODCACHEHINT = LVN_FIRST-13;



  public const int LVN_GETDISPINFOW = LVN_FIRST - 77;



  public const int LVN_ODFINDITEMW = LVN_FIRST-79;




  public const int LVM_FIRST = 0x1000;



  public const int LVM_GETITEMCOUNT = LVM_FIRST + 4;



  public const int LVM_DELETEALLITEMS = LVM_FIRST + 9;



  public const int LVM_GETNEXTITEM = LVM_FIRST + 12;



  public const int LVM_GETITEMRECT = LVM_FIRST + 14;



  public const int LVM_GETTOPINDEX = LVM_FIRST + 39;



  public const int LVM_SETITEMCOUNT = LVM_FIRST + 47;



  public const int LVM_GETSELECTEDCOUNT = LVM_FIRST + 50;



  public const int LVM_GETITEM = LVM_FIRST + 75;




  public const int LVNI_SELECTED = 0x0002;




  public const int LVSICF_NOINVALIDATEALL = 0x00000001;



  public const int LVSICF_NOSCROLL = 0x00000002;
  [DllImport("User32.dll",CharSet = CharSet.Auto,SetLastError=true)]
  public static extern IntPtr SendMessage(IntPtr hWnd, int msg, IntPtr wParam, IntPtr lParam);
  [DllImport("User32.dll", SetLastError=true)]
  public static extern int SendMessage(IntPtr hWnd, int msg, int wParam, IntPtr lParam);
  [DllImport("User32.dll",CharSet = CharSet.Auto, SetLastError=true)]
  public static extern int SendMessage(IntPtr hWnd, int msg, int wParam, int lParam);
  [DllImport("User32.dll", CharSet=CharSet.Auto, SetLastError=true)]
  public static extern int SendMessage(IntPtr hWnd, int msg, int wParam, ref LVITEM lvi);
  [DllImport("User32.dll", CharSet=CharSet.Auto, SetLastError=true)]
  public static extern int SendMessage(IntPtr hWnd, int msg, int wParam, ref RECT prc);
  [StructLayout(LayoutKind.Sequential)]
  [ComVisible(false)]
  public struct LVDISPINFOW
  {
   public NMHDR hdr;
   public LVITEM item;
  }
  [StructLayout(LayoutKind.Sequential)]
  [ComVisible(false)]
  public struct LVFINDINFO
  {
   public uint flags;
   [MarshalAs(UnmanagedType.LPWStr)]
   public string psz;
   public IntPtr lParam;
   public POINT pt;
   public uint vkDirection;
  }
  [StructLayout(LayoutKind.Sequential)]
  [ComVisible(false)]
  public struct POINT
  {
   public int x;
   public int y;
  }
  [StructLayout(LayoutKind.Sequential,CharSet=CharSet.Unicode)]
  [ComVisible(false)]
  public struct LVITEM
  {
   public uint mask;
   public int iItem;
   public int iSubItem;
   public uint state;
   public uint stateMask;
   public IntPtr pszText;
   public int cchTextMax;
   public int iImage;
   public IntPtr lParam;
   public int iIndent;
   public int iGroupId;
   public uint cColumns;
   public IntPtr puColumns;
  }
  [StructLayout(LayoutKind.Sequential)]
  [ComVisible(false)]
  public struct NMHDR
  {
   public IntPtr hwndFrom;
   public int idFrom;
   public int code;
  }
  [StructLayout(LayoutKind.Sequential)]
  [ComVisible(false)]
  public struct NMLVCACHEHINT
  {
   public NMHDR hdr;
   public int iFrom;
   public int iTo;
  }
  [StructLayout(LayoutKind.Sequential)]
  [ComVisible(false)]
  public struct NMLVFINDITEM
  {
   public NMHDR hdr;
   public int iStart;
   public LVFINDINFO lvfi;
  }
  [StructLayout(LayoutKind.Sequential)]
  [ComVisible(false)]
  public struct NMLISTVIEW
  {
   public NMHDR hdr;
   public int iItem;
   public int iSubItem;
   public uint uNewState;
   public uint uOldState;
   public uint uChanged;
   public POINT ptAction;
   public IntPtr lParam;
  }
  [StructLayout(LayoutKind.Sequential)]
  [ComVisible(false)]
  public struct RECT
  {
   public int left;
   public int top;
   public int right;
   public int bottom;
  }
 }
}
