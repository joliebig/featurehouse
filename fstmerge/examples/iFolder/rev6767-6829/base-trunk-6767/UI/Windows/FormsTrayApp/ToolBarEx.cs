using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Windows.Forms;
using Novell.Win32Util;
namespace Novell.FormsTrayApp
{
 public class ToolBarEx : System.Windows.Forms.ToolBar
 {
  private System.ComponentModel.Container components = null;
  private ImageList disabledImageList;
  private ImageList hotImageList;
  public ToolBarEx()
  {
   InitializeComponent();
  }
  protected override void Dispose( bool disposing )
  {
   if( disposing )
   {
    if( components != null )
     components.Dispose();
   }
   base.Dispose( disposing );
  }
  private void InitializeComponent()
  {
   components = new System.ComponentModel.Container();
  }
  public ImageList DisabledImageList
  {
   get { return disabledImageList; }
   set
   {
    if (value != null)
    {
     disabledImageList = value;
     disabledImageList.RecreateHandle += new EventHandler(disabledImageList_RecreateHandle);
     Win32.SendMessage(Handle, Win32Window.TB_SETDISABLEDIMAGELIST, IntPtr.Zero, disabledImageList.Handle);
    }
   }
  }
  public ImageList HotImageList
  {
   get { return hotImageList; }
   set
   {
    if (value != null)
    {
     hotImageList = value;
     hotImageList.RecreateHandle += new EventHandler(hotImageList_RecreateHandle);
     Win32.SendMessage(Handle, Win32Window.TB_SETHOTIMAGELIST, IntPtr.Zero, hotImageList.Handle);
    }
   }
  }
  private void disabledImageList_RecreateHandle(object sender, EventArgs e)
  {
   Win32.SendMessage(Handle, Win32Window.TB_SETDISABLEDIMAGELIST, IntPtr.Zero, disabledImageList.Handle);
  }
  private void hotImageList_RecreateHandle(object sender, EventArgs e)
  {
   Win32.SendMessage(Handle, Win32Window.TB_SETHOTIMAGELIST, IntPtr.Zero, hotImageList.Handle);
  }
 }
}
