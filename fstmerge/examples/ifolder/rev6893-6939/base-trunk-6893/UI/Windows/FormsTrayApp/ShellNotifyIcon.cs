

using System;
using System.Drawing;
using System.Windows.Forms;
using System.Runtime.InteropServices;

namespace Novell.CustomUIControls
{



 public class ShellNotifyIcon
 {

  internal uint shellRestart;
  private string text;
  private Icon icon;
  private ContextMenu contextMenu;
  private bool visible = false;
  private readonly NotifyMessageLoop messageLoop = null;
  private readonly IntPtr messageLoopHandle = IntPtr.Zero;
    private IntPtr handle = IntPtr.Zero;






  public ShellNotifyIcon(IntPtr handle)
  {
   this.handle = handle;
   WM_NOTIFY_TRAY += 1;
   uID += 1;
   messageLoop = new NotifyMessageLoop(this);
   messageLoopHandle = messageLoop.Handle;

   messageLoop.Click += new Novell.CustomUIControls.NotifyMessageLoop.ClickDelegate(messageLoop_Click);
   messageLoop.DoubleClick += new Novell.CustomUIControls.NotifyMessageLoop.DoubleClickDelegate(messageLoop_DoubleClick);
   messageLoop.BalloonClick += new Novell.CustomUIControls.NotifyMessageLoop.BalloonClickDelegate(messageLoop_BalloonClick);
   messageLoop.ContextMenuPopup += new Novell.CustomUIControls.NotifyMessageLoop.ContextMenuPopupDelegate(messageLoop_ContextMenuPopup);
  }




  ~ ShellNotifyIcon()
  {
   deleteNotifyIcon();
  }


  internal readonly int WM_NOTIFY_TRAY = 0x0400 + 1000;
  internal readonly int uID = 1000;


  private static readonly int NIF_MESSAGE = 0x01;
  private static readonly int NIF_ICON = 0x02;
  private static readonly int NIF_TIP = 0x04;
  private static readonly int NIF_INFO = 0x10;

  private static readonly int NIM_ADD = 0x00;
  private static readonly int NIM_MODIFY = 0x01;
  private static readonly int NIM_DELETE = 0x02;
  private static readonly int NIM_SETVERSION = 0x04;

  private static readonly int NOTIFYICON_VERSION = 3;

  [DllImport("shell32.dll")]
  private static extern bool Shell_NotifyIcon(int dwMessage, ref NOTIFYICONDATA lpData);





  [DllImport("user32.dll")]
  public static extern bool SetForegroundWindow(IntPtr hwnd);

  [DllImport("user32.dll")]
  private static extern uint RegisterWindowMessage(string lpString);

  [StructLayout(LayoutKind.Sequential)]
  private struct NOTIFYICONDATA
  {
   internal int cbSize;
   internal IntPtr hwnd;
   internal int uID;
   internal int uFlags;
   internal int uCallbackMessage;
   internal IntPtr hIcon;
   [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 128)]
   internal string szTip;
   internal int dwState;
   internal int dwStateMask;
   [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 256)]
   internal string szInfo;
   internal int uTimeoutAndVersion;
   [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 64)]
   internal string szInfoTitle;
   internal BalloonType dwInfoFlags;
  }






  public delegate void ClickDelegate(object sender, EventArgs e);





  public event ClickDelegate Click;




  public delegate void DoubleClickDelegate(object sender, EventArgs e);



  public event DoubleClickDelegate DoubleClick;




  public delegate void BalloonClickDelegate(object sender, EventArgs e);





  public event BalloonClickDelegate BalloonClick;




  public delegate void ContextMenuPopupDelegate(object sender, EventArgs e);



  public event ContextMenuPopupDelegate ContextMenuPopup;






  public ContextMenu ContextMenu
  {
   get { return contextMenu; }
   set { contextMenu = value; }
  }




  public IntPtr Handle
  {
   get { return handle; }
  }




  public Icon Icon
  {
   get { return icon; }
   set
   {
    if (icon == null)
    {

     icon = value;
     visible = addNotifyIcon(text);
    }
    else
    {

     icon = value;
     modifyNotifyIcon(NIF_ICON, null, null, null, BalloonType.None);
    }
   }
  }




  public string Text
  {
   get { return text; }
   set
   {
    if (icon != null)
    {

     if (modifyNotifyIcon(NIF_ICON | NIF_TIP, value, null, null, BalloonType.None))
     {
      text = value;
     }
    }
    else
    {

     text = value;
    }
   }
  }




  public bool Visible
  {
   get { return visible; }
   set
   {
    if (!value.Equals(visible) && (icon != null))
    {
     if (value)
     {
      visible = addNotifyIcon(text);
     }
     else
     {
      deleteNotifyIcon();
      visible = false;
     }
    }
   }
  }






  public void Dispose()
  {
   deleteNotifyIcon();
  }







  public void DisplayBalloonTooltip(string InfoTitle, string Info, BalloonType BalloonType)
  {
   modifyNotifyIcon(NIF_ICON | NIF_INFO, text, InfoTitle, Info, BalloonType);
  }






        private NOTIFYICONDATA getNOTIFYICONDATA(IntPtr iconHwnd, int flags, string tip, string infoTitle, string info, BalloonType balloonType)
  {
   NOTIFYICONDATA data = new NOTIFYICONDATA();

   data.cbSize = Marshal.SizeOf(data);
   data.hwnd = messageLoopHandle;
   data.uID = uID;
   data.uFlags = flags;
   data.uCallbackMessage = WM_NOTIFY_TRAY;
   data.hIcon = iconHwnd;
   data.uTimeoutAndVersion = 10 * 1000;
   data.dwInfoFlags = balloonType;

   data.szTip = tip;
   data.szInfoTitle = infoTitle;
   data.szInfo = info;

   return data;
  }






  private bool addNotifyIcon(string tip)
  {
   shellRestart = RegisterWindowMessage("TaskbarCreated");
   NOTIFYICONDATA data = getNOTIFYICONDATA(icon.Handle, NIF_MESSAGE | NIF_ICON | NIF_TIP | NIF_INFO, tip, null, null, BalloonType.None);
   return Shell_NotifyIcon(NIM_ADD, ref data);
  }





        private bool deleteNotifyIcon()
  {
   NOTIFYICONDATA data = getNOTIFYICONDATA(IntPtr.Zero, NIF_MESSAGE | NIF_ICON | NIF_TIP | NIF_INFO, null, null, null, BalloonType.None);
   return Shell_NotifyIcon(NIM_DELETE, ref data);
  }





  private bool modifyNotifyIcon(int flags, string tip, string infoTitle, string info, BalloonType balloonType)
  {
   NOTIFYICONDATA data = getNOTIFYICONDATA(icon.Handle, flags, tip, infoTitle, info, balloonType);
   return Shell_NotifyIcon(NIM_MODIFY, ref data);
  }






        private void messageLoop_Click(object sender, EventArgs e)
  {
   if (Click != null)
   {

    Click(this, e);
   }
  }




        private void messageLoop_DoubleClick(object sender, EventArgs e)
  {
   if (DoubleClick != null)
   {

    DoubleClick(this, e);
   }
  }




        private void messageLoop_BalloonClick(object sender, EventArgs e)
  {
   if (BalloonClick != null)
   {

    BalloonClick(this, e);
   }
  }




        private void messageLoop_ContextMenuPopup(object sender, EventArgs e)
  {
   if (ContextMenuPopup != null)
   {

    ContextMenuPopup(this, e);
   }
  }

 }





 internal class NotifyMessageLoop : System.Windows.Forms.Form
 {
  private bool balloonClicked = false;
  private bool iconClicked = false;
  private ShellNotifyIcon shellNotifyIcon = null;




  internal NotifyMessageLoop(ShellNotifyIcon shellNotifyIcon)
  {
   this.shellNotifyIcon = shellNotifyIcon;
  }



  private const int NIN_BALLOONUSERCLICK = 0x0405;


  private const int WM_MOUSEMOVE = 0x0200;
  private const int WM_LBUTTONDOWN = 0x0201;
  private const int WM_LBUTTONUP = 0x0202;
  private const int WM_LBUTTONDBLCLK = 0x0203;
  private const int WM_RBUTTONDOWN = 0x0204;
  private const int WM_RBUTTONUP = 0x0205;
  private const int WM_MBUTTONDOWN = 0x0207;

  private const int TPM_RIGHTBUTTON = 0x0002;

  [DllImport("user32.dll")]
  private static extern int TrackPopupMenu(
   IntPtr hMenu,
   int wFlags,
   int x,
   int y,
   int nReserved,
   IntPtr hwnd,
   IntPtr prcRect);






  public delegate void ClickDelegate(object sender, EventArgs e);



  new public event ClickDelegate Click;




  public delegate void DoubleClickDelegate(object sender, EventArgs e);



  new public event DoubleClickDelegate DoubleClick;




  public delegate void BalloonClickDelegate(object sender, EventArgs e);





  public event BalloonClickDelegate BalloonClick;




  public delegate void ContextMenuPopupDelegate(object sender, EventArgs e);



  public event ContextMenuPopupDelegate ContextMenuPopup;
  protected override void WndProc(ref Message msg)
  {
   if (msg.Msg == shellNotifyIcon.WM_NOTIFY_TRAY)
   {
    if ((int)msg.WParam == shellNotifyIcon.uID)
    {
     switch ((int)msg.LParam)
     {
      case NIN_BALLOONUSERCLICK:
       balloonClicked = true;
       if (BalloonClick != null)
       {
        BalloonClick(this, new EventArgs());
        return;
       }
       break;
      case WM_MOUSEMOVE:
       if (!iconClicked)
        balloonClicked = false;
       break;
      case WM_LBUTTONDOWN:
       iconClicked = true;
       break;
      case WM_LBUTTONUP:
       if (!balloonClicked)
       {
        if (Click != null)
        {
         Click(this, new EventArgs());
         iconClicked = balloonClicked = false;
         return;
        }
       }
       iconClicked = balloonClicked = false;
       break;
      case WM_LBUTTONDBLCLK:
       if (DoubleClick != null)
       {
        DoubleClick(this, new EventArgs());
        return;
       }
       break;
      case WM_RBUTTONDOWN:
       break;
      case WM_RBUTTONUP:
       if ((shellNotifyIcon.ContextMenu != null) &&
        (shellNotifyIcon.ContextMenu.Handle != IntPtr.Zero))
       {
        if (ContextMenuPopup != null)
        {
         ContextMenuPopup(this, new EventArgs());
        }
        ShellNotifyIcon.SetForegroundWindow(shellNotifyIcon.Handle);
        TrackPopupMenu(
         shellNotifyIcon.ContextMenu.Handle,
         TPM_RIGHTBUTTON,
         System.Windows.Forms.Cursor.Position.X,
         System.Windows.Forms.Cursor.Position.Y,
         0,
         shellNotifyIcon.Handle,
         IntPtr.Zero);
        ShellNotifyIcon.SetForegroundWindow(shellNotifyIcon.Handle);
        return;
       }
       break;
     }
    }
   }
   else if (msg.Msg == shellNotifyIcon.shellRestart)
   {
    if (shellNotifyIcon.Visible)
    {
     shellNotifyIcon.Visible = false;
     shellNotifyIcon.Visible = true;
    }
   }
   base.WndProc(ref msg);
  }
 }
}
