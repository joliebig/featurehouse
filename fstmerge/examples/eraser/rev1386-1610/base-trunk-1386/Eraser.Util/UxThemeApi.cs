

using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using System.Windows.Forms;
using System.Drawing;
using System.IO;

namespace Eraser.Util
{
 public static class UXThemeApi
 {




  public static void UpdateControlTheme(Control control)
  {
   if (control is ContainerControl)
    ((ContainerControl)control).Font = SystemFonts.MessageBoxFont;
   else if (control.Font != SystemFonts.MessageBoxFont)
    control.Font = new Font(SystemFonts.MessageBoxFont.FontFamily,
     control.Font.Size, control.Font.Style);

   if (control is ListView)
    UpdateControlTheme((ListView)control);
   else if (control is ToolStrip)
    UpdateControlTheme((ToolStrip)control);

   if (control.ContextMenuStrip != null)
    UpdateControlTheme(control.ContextMenuStrip);

   foreach (Control child in control.Controls)
    UpdateControlTheme(child);
  }





  public static void UpdateControlTheme(ListView lv)
  {
   try
   {
    NativeMethods.SetWindowTheme(lv.Handle, "EXPLORER", null);
    UserApi.NativeMethods.SendMessage(lv.Handle,
     UserApi.NativeMethods.LVM_SETEXTENDEDLISTVIEWSTYLE,
     (UIntPtr)UserApi.NativeMethods.LVS_EX_DOUBLEBUFFER,
     (IntPtr)UserApi.NativeMethods.LVS_EX_DOUBLEBUFFER);
   }
   catch (DllNotFoundException)
   {
   }
  }





  public static void UpdateControlTheme(ToolStrip menu)
  {

   if (ThemeMessageFilter.Instance == null)
   {
    ThemeMessageFilter filter = new ThemeMessageFilter();
    ThemeMessageFilter.Instance.ThemeChanged += OnThemeChanged;
   }

   if (Environment.OSVersion.Version.Major >= 6)
   {

    UXThemeMenuRenderer renderer = new UXThemeMenuRenderer();
    if (menu.Renderer is ToolStripProfessionalRenderer)
    {
     menu.Disposed += OnThemedMenuDisposed;
     ThemedMenus.Add(menu, renderer);
     if (NativeMethods.ThemesActive)
      menu.Renderer = renderer;
    }
   }

   foreach (ToolStripItem item in menu.Items)
   {
    ToolStripMenuItem toolStripItem = item as ToolStripMenuItem;
    if (toolStripItem != null)
     UpdateControlTheme(toolStripItem);
   }
  }





  public static void UpdateControlTheme(ToolStripDropDownItem menuItem)
  {
   if (menuItem.Font != SystemFonts.MessageBoxFont)
    menuItem.Font = new Font(SystemFonts.MessageBoxFont, menuItem.Font.Style);

   UpdateControlTheme(menuItem.DropDown);
  }





  private static void OnThemeChanged(object sender, EventArgs e)
  {
   bool themesActive = NativeMethods.ThemesActive;
   foreach (KeyValuePair<ToolStrip, UXThemeMenuRenderer> value in ThemedMenus)
   {
    if (themesActive)
     value.Key.Renderer = value.Value;
    else
     value.Key.RenderMode = ToolStripRenderMode.ManagerRenderMode;
   }
  }





  private static void OnThemedMenuDisposed(object sender, EventArgs e)
  {
   ThemedMenus.Remove(sender as ToolStrip);
  }






  private static Dictionary<ToolStrip, UXThemeMenuRenderer> ThemedMenus =
   new Dictionary<ToolStrip,UXThemeMenuRenderer>();





  private class ThemeMessageFilter : IMessageFilter
  {
   public ThemeMessageFilter()
   {
    if (Instance != null)
     throw new InvalidOperationException("Only one instance of the " +
      "ThemeMessageFilter can exist at any one time,");
    Instance = this;
    ThemesActive = NativeMethods.ThemesActive;
    Application.AddMessageFilter(this);
   }


   public bool PreFilterMessage(ref Message m)
   {
    if (m.Msg == WM_THEMECHANGED)
    {
     ThemesActive = NativeMethods.ThemesActive;
     ThemeChanged(null, EventArgs.Empty);
    }
    else if (m.Msg == WM_DWMCOMPOSITIONCHANGED)
    {
     if (ThemesActive != NativeMethods.ThemesActive)
     {
      ThemesActive = NativeMethods.ThemesActive;
      ThemeChanged(null, EventArgs.Empty);
     }
    }

    return false;
   }





   public static ThemeMessageFilter Instance
   {
    get;
    private set;
   }




   public EventHandler<EventArgs> ThemeChanged
   {
    get;
    set;
   }

   private const int WM_THEMECHANGED = 0x031A;
   private const int WM_DWMCOMPOSITIONCHANGED = 0x031E;
   private bool ThemesActive;
  }




  internal static class NativeMethods
  {
   [DllImport("UxTheme.dll", CharSet = CharSet.Unicode)]
   [return: MarshalAs(UnmanagedType.Bool)]
   private static extern bool IsThemeActive();

   public static bool ThemesActive
   {
    get
    {
     try
     {
      return IsThemeActive();
     }
     catch (FileLoadException)
     {
      return false;
     }
    }
   }
   [DllImport("UxTheme.dll", CharSet = CharSet.Unicode)]
   public static extern void SetWindowTheme(IntPtr hwnd, string pszSubAppName,
    string pszSubIdList);
  }
 }
 public class UXThemeMenuRenderer : ToolStripRenderer
 {
  ~UXThemeMenuRenderer()
  {
   if (hTheme != null)
    hTheme.Close();
  }
  protected override void Initialize(ToolStrip toolStrip)
  {
   base.Initialize(toolStrip);
   control = toolStrip;
   hTheme = NativeMethods.OpenThemeData(toolStrip.Handle, "MENU");
  }
  protected override void OnRenderToolStripBackground(ToolStripRenderEventArgs e)
  {
   IntPtr hDC = e.Graphics.GetHdc();
   Rectangle rect = e.AffectedBounds;
   if (NativeMethods.IsThemeBackgroundPartiallyTransparent(hTheme,
    (int)NativeMethods.MENUPARTS.MENU_POPUPITEM, 0))
   {
    NativeMethods.DrawThemeBackground(hTheme, hDC,
     (int)NativeMethods.MENUPARTS.MENU_POPUPBACKGROUND, 0, ref rect, ref rect);
   }
   NativeMethods.DrawThemeBackground(hTheme, hDC, (int)
    NativeMethods.MENUPARTS.MENU_POPUPBORDERS, 0, ref rect, ref rect);
   e.Graphics.ReleaseHdc();
   rect.Inflate(-Margin, -Margin);
   e.Graphics.FillRectangle(new SolidBrush(e.BackColor), rect);
  }
  protected override void OnRenderImageMargin(ToolStripRenderEventArgs e)
  {
   IntPtr hDC = e.Graphics.GetHdc();
   Rectangle rect = e.AffectedBounds;
   rect.Inflate(-2, -2);
   rect.Offset(1, 1);
   rect.Size = new Size(GutterWidth, rect.Height + 1);
   NativeMethods.DrawThemeBackground(hTheme, hDC,
    (int)NativeMethods.MENUPARTS.MENU_POPUPGUTTER, 0, ref rect, ref rect);
   e.Graphics.ReleaseHdc();
  }
  protected override void OnRenderMenuItemBackground(ToolStripItemRenderEventArgs e)
  {
   Rectangle rect = Rectangle.Truncate(e.Graphics.VisibleClipBounds);
   rect.Inflate(-1, 0);
   rect.Offset(2, 0);
   IntPtr hDC = e.Graphics.GetHdc();
   int itemState = (int)(e.Item.Selected ?
    (e.Item.Enabled ? NativeMethods.POPUPITEMSTATES.MPI_HOT :
     NativeMethods.POPUPITEMSTATES.MPI_DISABLEDHOT) :
    (e.Item.Enabled ? NativeMethods.POPUPITEMSTATES.MPI_NORMAL :
     NativeMethods.POPUPITEMSTATES.MPI_DISABLED));
   NativeMethods.DrawThemeBackground(hTheme, hDC,
    (int)NativeMethods.MENUPARTS.MENU_POPUPITEM, itemState, ref rect, ref rect);
   e.Graphics.ReleaseHdc();
  }
  protected override void OnRenderSeparator(ToolStripSeparatorRenderEventArgs e)
  {
   IntPtr hDC = e.Graphics.GetHdc();
   Rectangle rect = new Rectangle(GutterWidth, 0, e.Item.Width, e.Item.Height);
   rect.Inflate(4, 0);
   NativeMethods.DrawThemeBackground(hTheme, hDC,
    (int)NativeMethods.MENUPARTS.MENU_POPUPSEPARATOR, 0, ref rect, ref rect);
   e.Graphics.ReleaseHdc();
  }
  protected override void OnRenderItemCheck(ToolStripItemImageRenderEventArgs e)
  {
   if (!(e.Item is ToolStripMenuItem))
   {
    base.OnRenderItemCheck(e);
    return;
   }
   Rectangle imgRect = new Rectangle(e.ImageRectangle.Left + 2 - 4,
    e.ImageRectangle.Top - 3,
    e.ImageRectangle.Width + 4 * 2 + 1, e.ImageRectangle.Height + 3 * 2);
   IntPtr hDC = e.Graphics.GetHdc();
   ToolStripMenuItem item = (ToolStripMenuItem)e.Item;
   int bgState = (int)(e.Item.Enabled ? NativeMethods.POPUPCHECKBACKGROUNDSTATES.MCB_NORMAL :
    NativeMethods.POPUPCHECKBACKGROUNDSTATES.MCB_DISABLED);
   NativeMethods.DrawThemeBackground(hTheme, hDC,
    (int)NativeMethods.MENUPARTS.MENU_POPUPCHECKBACKGROUND, bgState,
    ref imgRect, ref imgRect);
   int checkState = (int)(item.Checked ?
    (item.Enabled ? NativeMethods.POPUPCHECKSTATES.MC_CHECKMARKNORMAL :
     NativeMethods.POPUPCHECKSTATES.MC_CHECKMARKDISABLED) : 0);
   NativeMethods.DrawThemeBackground(hTheme, hDC,
    (int)NativeMethods.MENUPARTS.MENU_POPUPCHECK, checkState,
    ref imgRect, ref imgRect);
   e.Graphics.ReleaseHdc();
  }
  protected override void OnRenderItemText(ToolStripItemTextRenderEventArgs e)
  {
   int itemState = (int)(e.Item.Selected ?
    (e.Item.Enabled ? NativeMethods.POPUPITEMSTATES.MPI_HOT :
     NativeMethods.POPUPITEMSTATES.MPI_DISABLEDHOT) :
    (e.Item.Enabled ? NativeMethods.POPUPITEMSTATES.MPI_NORMAL :
     NativeMethods.POPUPITEMSTATES.MPI_DISABLED));
   Rectangle newRect = e.TextRectangle;
   newRect.Offset(2, 0);
   e.TextRectangle = newRect;
   Rectangle rect = new Rectangle(e.TextRectangle.Left, 0,
    e.Item.Width - e.TextRectangle.Left, e.Item.Height);
   IntPtr hFont = e.TextFont.ToHfont();
   IntPtr hDC = e.Graphics.GetHdc();
   NativeMethods.SelectObject(hDC, hFont);
   NativeMethods.DrawThemeText(hTheme, hDC,
    (int)NativeMethods.MENUPARTS.MENU_POPUPITEM, itemState, e.Text,
    -1, e.TextFormat | TextFormatFlags.WordEllipsis | TextFormatFlags.SingleLine,
    0, ref rect);
   e.Graphics.ReleaseHdc();
  }
  protected override void OnRenderArrow(ToolStripArrowRenderEventArgs e)
  {
   int itemState = (int)(e.Item.Enabled ? NativeMethods.POPUPSUBMENUSTATES.MSM_NORMAL :
    NativeMethods.POPUPSUBMENUSTATES.MSM_DISABLED);
   using (Bitmap backBmp = new Bitmap(e.ArrowRectangle.Width, e.ArrowRectangle.Height))
   {
    using (Graphics backGfx = Graphics.FromImage(backBmp))
    {
     IntPtr hDC = backGfx.GetHdc();
     Rectangle backRect = new Rectangle(new Point(0, 0), backBmp.Size);
     NativeMethods.DrawThemeBackground(hTheme, hDC,
      (int)NativeMethods.MENUPARTS.MENU_POPUPSUBMENU, itemState,
      ref backRect, ref backRect);
     backGfx.ReleaseHdc();
    }
    e.Graphics.DrawImageUnscaled(backBmp, e.ArrowRectangle);
   }
  }
  private int GutterWidth
  {
   get
   {
    Rectangle margins = Rectangle.Empty;
    Size checkSize = Size.Empty;
    NativeMethods.GetThemeMargins(hTheme, IntPtr.Zero,
     (int)NativeMethods.MENUPARTS.MENU_POPUPCHECK, 0,
     (int)NativeMethods.TMT_MARGINS.TMT_SIZINGMARGINS,
     IntPtr.Zero, ref margins);
    NativeMethods.GetThemePartSize(hTheme, IntPtr.Zero,
     (int)NativeMethods.MENUPARTS.MENU_POPUPCHECK, 0,
     IntPtr.Zero, NativeMethods.THEMESIZE.TS_TRUE, ref checkSize);
    return 2 * checkSize.Width + margins.Left + margins.Width - 1;
   }
  }
  private int Margin
  {
   get
   {
    Size borderSize = Size.Empty;
    NativeMethods.GetThemePartSize(hTheme, IntPtr.Zero,
     (int)NativeMethods.MENUPARTS.MENU_POPUPBORDERS, 0,
     IntPtr.Zero, NativeMethods.THEMESIZE.TS_TRUE, ref borderSize);
    return borderSize.Width;
   }
  }
  private ToolStrip control;
  private SafeThemeHandle hTheme;
  internal static class NativeMethods
  {
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
   [DllImport("Gdi32.dll")]
   public extern static IntPtr SelectObject(IntPtr hdc, IntPtr hgdiobj);
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
  }
 }
 internal class SafeThemeHandle : SafeHandle
 {
  public SafeThemeHandle()
   : base(IntPtr.Zero, true)
  {
  }
  public override bool IsInvalid
  {
   get { return handle == IntPtr.Zero; }
  }
  protected override bool ReleaseHandle()
  {
   UXThemeMenuRenderer.NativeMethods.CloseThemeData(handle);
   handle = IntPtr.Zero;
   return true;
  }
 }
}
