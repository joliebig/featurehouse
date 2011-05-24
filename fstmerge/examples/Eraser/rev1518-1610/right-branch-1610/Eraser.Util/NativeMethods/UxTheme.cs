

using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using System.Drawing;
using System.Windows.Forms;

namespace Eraser.Util
{
 internal static partial class NativeMethods
 {
  [DllImport("UxTheme.dll", CharSet = CharSet.Unicode)]
  [return: MarshalAs(UnmanagedType.Bool)]
  public static extern bool IsThemeActive();
  [DllImport("UxTheme.dll", CharSet = CharSet.Unicode)]
  public static extern void SetWindowTheme(IntPtr hwnd, string pszSubAppName,
   string pszSubIdList);
  [DllImport("UxTheme.dll", CharSet = CharSet.Unicode)]
  public static extern SafeThemeHandle OpenThemeData(IntPtr hwnd, string pszClassList);
  [DllImport("UxTheme.dll", CharSet = CharSet.Unicode)]
  public static extern IntPtr CloseThemeData(IntPtr hwndTeme);
  [DllImport("UxTheme.dll", CharSet = CharSet.Unicode)]
  public static extern IntPtr DrawThemeParentBackground(IntPtr hwnd,
   IntPtr hdc, ref Rectangle prc);
  [DllImport("UxTheme.dll", CharSet = CharSet.Unicode)]
  [return: MarshalAs(UnmanagedType.Bool)]
  public static extern bool IsThemeBackgroundPartiallyTransparent(
   SafeThemeHandle hTheme, int iPartId, int iStateId);
  [DllImport("UxTheme.dll", CharSet = CharSet.Unicode)]
  public static extern IntPtr DrawThemeBackground(
   SafeThemeHandle hTheme, IntPtr hdc, int iPartId, int iStateId,
   ref Rectangle pRect, ref Rectangle pClipRect);
  [DllImport("UxTheme.dll", CharSet = CharSet.Unicode)]
  public extern static int DrawThemeText(SafeThemeHandle hTheme,
   IntPtr hDC, int iPartId, int iStateId,
   [MarshalAs(UnmanagedType.LPWStr)] string pszText, int iCharCount,
   TextFormatFlags dwTextFlag, int dwTextFlags2, ref Rectangle pRect);
  [DllImport("UxTheme.dll", CharSet = CharSet.Unicode)]
  public static extern IntPtr GetThemeMargins(SafeThemeHandle hTheme,
   IntPtr hdc, int iPartId, int iStateId, int iPropId, ref Rectangle prc,
   ref Rectangle pMargins);
  [DllImport("UxTheme.dll", CharSet = CharSet.Unicode)]
  public static extern IntPtr GetThemeMargins(SafeThemeHandle hTheme,
   IntPtr hdc, int iPartId, int iStateId, int iPropId, IntPtr prc,
   ref Rectangle pMargins);
  [DllImport("UxTheme.dll", CharSet = CharSet.Unicode)]
  public static extern IntPtr GetThemePartSize(SafeThemeHandle hTheme,
   IntPtr hdc, int iPartId, int iStateId, ref Rectangle prc,
   THEMESIZE eSize, ref Size psz);
  [DllImport("UxTheme.dll", CharSet = CharSet.Unicode)]
  public static extern IntPtr GetThemePartSize(SafeThemeHandle hTheme,
   IntPtr hdc, int iPartId, int iStateId, IntPtr prc,
   THEMESIZE eSize, ref Size psz);
  public enum MENUPARTS
  {
   MENU_MENUITEM_TMSCHEMA = 1,
   MENU_MENUDROPDOWN_TMSCHEMA = 2,
   MENU_MENUBARITEM_TMSCHEMA = 3,
   MENU_MENUBARDROPDOWN_TMSCHEMA = 4,
   MENU_CHEVRON_TMSCHEMA = 5,
   MENU_SEPARATOR_TMSCHEMA = 6,
   MENU_BARBACKGROUND = 7,
   MENU_BARITEM = 8,
   MENU_POPUPBACKGROUND = 9,
   MENU_POPUPBORDERS = 10,
   MENU_POPUPCHECK = 11,
   MENU_POPUPCHECKBACKGROUND = 12,
   MENU_POPUPGUTTER = 13,
   MENU_POPUPITEM = 14,
   MENU_POPUPSEPARATOR = 15,
   MENU_POPUPSUBMENU = 16,
   MENU_SYSTEMCLOSE = 17,
   MENU_SYSTEMMAXIMIZE = 18,
   MENU_SYSTEMMINIMIZE = 19,
   MENU_SYSTEMRESTORE = 20,
  }
  public enum POPUPCHECKSTATES
  {
   MC_CHECKMARKNORMAL = 1,
   MC_CHECKMARKDISABLED = 2,
   MC_BULLETNORMAL = 3,
   MC_BULLETDISABLED = 4,
  }
  public enum POPUPCHECKBACKGROUNDSTATES
  {
   MCB_DISABLED = 1,
   MCB_NORMAL = 2,
   MCB_BITMAP = 3,
  }
  public enum POPUPITEMSTATES
  {
   MPI_NORMAL = 1,
   MPI_HOT = 2,
   MPI_DISABLED = 3,
   MPI_DISABLEDHOT = 4,
  }
  public enum POPUPSUBMENUSTATES
  {
   MSM_NORMAL = 1,
   MSM_DISABLED = 2,
  }
  public enum TMT_MARGINS
  {
   TMT_SIZINGMARGINS = 3601,
   TMT_CONTENTMARGINS,
   TMT_CAPTIONMARGINS
  }
  public enum THEMESIZE
  {
   TS_MIN,
   TS_TRUE,
   TS_DRAW
  }
  public const int WM_THEMECHANGED = 0x031A;
  public const int WM_DWMCOMPOSITIONCHANGED = 0x031E;
 }
}
