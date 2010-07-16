

using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Windows.Forms;
using System.Runtime.InteropServices;
using System.Diagnostics;

using Novell.Win32Util;

namespace Novell.iFolderCom
{



 [ComVisible(false)]
 public class VirtualListView : System.Windows.Forms.ListView
 {



  private System.ComponentModel.Container components = null;




  public VirtualListView()
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





  public int Count
  {
   get
   {
    return Win32.SendMessage(Handle, Win32.LVM_GETITEMCOUNT, 0, 0);
   }
   set
   {
    Win32.SendMessage(this.Handle, Win32.LVM_SETITEMCOUNT, value, Win32.LVSICF_NOINVALIDATEALL | Win32.LVSICF_NOSCROLL);
   }
  }




  public int SelectedCount
  {
   get
   {
    return Win32.SendMessage(Handle, Win32.LVM_GETSELECTEDCOUNT, 0, 0);
   }
  }




  new public ArrayList SelectedIndices
  {
   get
   {
    ArrayList list = new ArrayList();

    int index = -1;
    while (true)
    {
     index = Win32.SendMessage(Handle, Win32.LVM_GETNEXTITEM, index, Win32.LVNI_SELECTED);
     if (index == -1)
      break;

     list.Add(index);
    }

    return list;
   }
  }




  new public ArrayList SelectedItems
  {
   get
   {
    ArrayList list = new ArrayList();

    Win32.LVITEM lvi = new Win32.LVITEM();

    lvi.iItem = -1;
    while (true)
    {
     lvi.iItem = Win32.SendMessage(Handle, Win32.LVM_GETNEXTITEM, lvi.iItem, Win32.LVNI_SELECTED);
     if (lvi.iItem == -1)
      break;

     lvi.mask = Win32.LVIF_TEXT | Win32.LVIF_IMAGE;
     lvi.cchTextMax = 1024;
     lvi.pszText = Marshal.AllocHGlobal(lvi.cchTextMax);
     try
     {

      int result = Win32.SendMessage(
       Handle,
       Win32.LVM_GETITEM,
       0,
       ref lvi);
      if (result > 0)
      {
       string str = Marshal.PtrToStringUni(lvi.pszText);
       list.Add(new ListViewItem(str, lvi.iImage));
      }
     }
     finally
     {
      Marshal.FreeHGlobal(lvi.pszText);
     }
    }

    return list;
   }
  }




  public int TopItemIndex
  {
   get
   {
    return Win32.SendMessage(Handle, Win32.LVM_GETTOPINDEX, 0, 0);
   }
  }







  private void InitializeComponent()
  {
   components = new System.ComponentModel.Container();
  }





        protected override CreateParams CreateParams
  {
   get
   {
    CreateParams cp = base.CreateParams;
    cp.Style |= Win32.LVS_OWNERDATA | Win32.LVS_REPORT | Win32.LVS_SHOWSELALWAYS;

    return cp;
   }
  }

  protected override void WndProc(ref Message m)
  {
   switch(m.Msg)
   {
    case Win32.WM_DESTROY:


     Win32.SendMessage(Handle, Win32.LVM_DELETEALLITEMS, 0, 0);
     base.WndProc(ref m);
     break;

    default:
     try
     {
      base.WndProc(ref m);
     }
     catch {}
     break;
   }
  }
 }
}
