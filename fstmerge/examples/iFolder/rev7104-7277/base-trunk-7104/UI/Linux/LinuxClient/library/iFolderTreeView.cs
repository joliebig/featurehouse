using Gtk;
using System;
namespace Novell.iFolder
{
 public class iFolderButtonPressEventArgs : EventArgs
 {
  private Gdk.EventButton eb;
  public Gdk.EventButton Event
  {
   get
   {
    return this.eb;
   }
  }
        public iFolderButtonPressEventArgs(Gdk.EventButton eb)
  {
   this.eb = eb;
  }
 }
 public class iFolderTreeView : Gtk.TreeView
 {
  public iFolderTreeView()
   : base()
  {
  }
  protected override bool OnButtonPressEvent(Gdk.EventButton evnt)
  {
   base.OnButtonPressEvent(evnt);
   return false;
  }
 }
}
