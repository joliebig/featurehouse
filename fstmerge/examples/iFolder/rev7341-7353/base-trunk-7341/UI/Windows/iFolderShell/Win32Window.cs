using System;
using System.Runtime.InteropServices;
using System.Drawing;
using System.Drawing.Imaging;
namespace Novell.Win32Util
{
 [ComVisible(false)]
 public class Win32Window
 {
  private IntPtr handle;
  public const int LR_LOADFROMFILE = 0x10;
  public const int IMAGE_ICON = 1;
  public const int FILE_ATTRIBUTE_DIRECTORY = 0x10;
  public const int SHGFI_ICON = 0x100;
  public const int SHGFI_USEFILEATTRIBUTES = 0x10;
  public const int SHCNE_UPDATEITEM = 0x00002000;
  public const int SHCNF_PATHW = 0x0005;
  const int WM_USER = 0x0400;
  public static readonly int TB_SETDISABLEDIMAGELIST = WM_USER + 54;
  public static readonly int TB_SETHOTIMAGELIST = WM_USER + 52;
  const int GWL_EXSTYLE = -20;
  const int WS_EX_TOOLWINDOW = 0x00000080;
  const int WS_EX_APPWINDOW = 0x00040000;
  const int SW_HIDE = 0;
  const int SW_SHOWNORMAL = 1;
  const int SW_RESTORE = 9;
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
  public const int LVM_DELETEALLITEMS = LVM_FIRST + 9;
  public const int LVM_GETNEXTITEM = LVM_FIRST + 12;
  public const int LVM_SETITEMCOUNT = LVM_FIRST + 47;
  public const int LVM_GETSELECTEDCOUNT = LVM_FIRST + 50;
  public const int LVM_GETITEM = LVM_FIRST + 75;
  public const int LVNI_SELECTED = 0x0002;
  public const int LVSICF_NOINVALIDATEALL = 0x00000001;
  public const int LVSICF_NOSCROLL = 0x00000002;
  [ComVisible(false)]
  private struct ICONINFO
  {
   public bool fIcon;
   public int xHotspot;
   public int yHotspot;
   public IntPtr hbmMask;
   public IntPtr hbmColor;
  }
  [DllImport("user32.dll")]
  static extern bool BringWindowToTop(IntPtr hwnd);
  [DllImport("user32.dll", SetLastError=true)]
  static extern int DestroyIcon(IntPtr hIcon);
  [DllImport("user32.dll", EntryPoint="FindWindow")]
  static extern IntPtr FindWindowWin32(string className, string windowName);
  [DllImport("user32.dll")]
  static extern bool GetIconInfo(IntPtr hIcon, out ICONINFO piconinfo);
  [DllImport("user32.dll")]
  static extern int GetWindowLong(IntPtr hwnd, int index);
  [DllImportAttribute("user32.dll")]
  static extern IntPtr LoadImage(int hInst, string name, int type, int cx, int cy, int load);
  [DllImport("user32.dll")]
  static extern bool SetForegroundWindow(IntPtr hWnd);
  [DllImport("user32.dll")]
  static extern int SetWindowLong(IntPtr hwnd, int index, int dwNewLong);
  [DllImport("shell32.dll")]
  static extern void SHChangeNotify(int wEventId, int uFlags, [MarshalAs(UnmanagedType.LPWStr)] string dwItem1, IntPtr dwItem2);
  [DllImport("shell32.dll")]
  static extern IntPtr SHGetFileInfo([MarshalAs(UnmanagedType.LPWStr)] string path, int attr, out IFSHFILEINFO fi, int cbfi, int flags);
  [DllImport("shell32.dll")]
  static extern bool SHObjectProperties(IntPtr hwnd, int type, [MarshalAs(UnmanagedType.LPWStr)] string lpObject, [MarshalAs(UnmanagedType.LPWStr)] string lpPage);
  [DllImport("user32.dll")]
  static extern bool ShowWindow(IntPtr hwnd, int nCmdShow);
        [DllImport("user32.dll")]
        static extern IntPtr SendMessage(int hwnd, uint msg, IntPtr wParam, IntPtr lParam);
        [DllImport("user32.dll")]
        static extern uint RegisterWindowMessage(string hwnd);
  public Win32Window()
  {
  }
  public IntPtr Handle
  {
            get { return handle; }
            set
   {
    handle = value;
   }
  }
  public bool Visible
  {
   set
   {
    if (value)
    {
     ShowWindow(handle, SW_SHOWNORMAL | SW_RESTORE);
     SetForegroundWindow(handle);
    }
    else
    {
     ShowWindow(handle, SW_HIDE);
    }
   }
  }
  public int GetWindowLong(int index)
  {
   return GetWindowLong(handle, index);
  }
  public int SetWindowLong(int index, int dwNewLong)
  {
   return SetWindowLong(handle, index, dwNewLong);
  }
  public void MakeToolWindow()
  {
   int windowStyle = GetWindowLong(GWL_EXSTYLE);
   SetWindowLong(GWL_EXSTYLE, windowStyle | WS_EX_TOOLWINDOW);
  }
  public void MakeNormalWindow()
  {
   int windowStyle = GetWindowLong(GWL_EXSTYLE);
   SetWindowLong(GWL_EXSTYLE, windowStyle & ~WS_EX_TOOLWINDOW);
  }
  public bool BringWindowToTop()
  {
   return BringWindowToTop(handle);
  }
        public static uint RegisterWindowMsg(string msg)
        {
            return RegisterWindowMessage(msg);
        }
        public IntPtr SendMsg(int hWnd, uint msg, IntPtr wParam, IntPtr lParam)
        {
            return SendMessage(hWnd, msg, wParam, lParam);
        }
  public static Win32Window FindWindow(string className, string windowName)
  {
   IntPtr handle = FindWindowWin32(className, windowName);
   Win32Window win32Window = null;
   if (handle != IntPtr.Zero)
   {
    win32Window = new Win32Window();
    win32Window.Handle = handle;
   }
   return win32Window;
  }
  public static bool ShObjectProperties(IntPtr handle, int type, string objectName, string pageName)
  {
   return SHObjectProperties(handle, type, objectName, pageName);
  }
  public static IntPtr ShGetFileInfo(string path, int attr, out IFSHFILEINFO fi, int cbfi, int flags)
  {
   return SHGetFileInfo(path, attr, out fi, cbfi, flags);
  }
  public static void ShChangeNotify(int wEventId, int flags, string dwItem1, IntPtr dwItem2)
  {
   SHChangeNotify(wEventId, flags, dwItem1, dwItem2);
  }
  public static IntPtr LoadImageFromFile(int hInst, string name, int type, int cx, int cy, int load)
  {
   return LoadImage(hInst, name, type, cx, cy, load);
  }
  public static Bitmap IconToAlphaBitmap(Icon ico)
  {
   ICONINFO ii = new ICONINFO();
   GetIconInfo(ico.Handle, out ii);
   Bitmap bmp = Bitmap.FromHbitmap(ii.hbmColor);
   DestroyIcon(ii.hbmColor);
   DestroyIcon(ii.hbmMask);
   if (Bitmap.GetPixelFormatSize(bmp.PixelFormat) < 32)
    return ico.ToBitmap();
   BitmapData bmData;
   Rectangle bmBounds = new Rectangle(0,0,bmp.Width,bmp.Height);
   bmData = bmp.LockBits(bmBounds,ImageLockMode.ReadOnly, bmp.PixelFormat);
   Bitmap dstBitmap=new Bitmap(bmData.Width, bmData.Height, bmData.Stride, PixelFormat.Format32bppArgb, bmData.Scan0);
   bool IsAlphaBitmap = false;
   for (int y=0; y <= bmData.Height-1; y++)
   {
    for (int x=0; x <= bmData.Width-1; x++)
    {
     Color PixelColor = Color.FromArgb(Marshal.ReadInt32(bmData.Scan0, (bmData.Stride * y) + (4 * x)));
     if (PixelColor.A > 0 & PixelColor.A < 255)
     {
      IsAlphaBitmap = true;
      break;
     }
    }
    if (IsAlphaBitmap) break;
   }
   bmp.UnlockBits(bmData);
   if (IsAlphaBitmap==true)
    return new Bitmap(dstBitmap);
   else
    return new Bitmap(ico.ToBitmap());
  }
 }
}
[StructLayout(LayoutKind.Sequential)]
[ComVisible(false)]
public struct IFSHFILEINFO
{
 public IntPtr hIcon;
 public int iIcon;
 public int attributes;
 [MarshalAs(UnmanagedType.LPWStr, SizeConst=256)]
 public String displayName;
 [MarshalAs(UnmanagedType.LPWStr, SizeConst=80)]
 public String typeName;
}
