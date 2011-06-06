using System;
using System.Drawing;
using Gtk;
using Gdk;
using Glade;
using GtkSharp;
using GLib;
namespace Novell.AddressBook.UI.gtk
{
 public class BookEditor
 {
  [Glade.Widget] internal Gtk.Entry beName;
  [Glade.Widget] internal Gtk.Button okButton;
  Gtk.Dialog beDlg = null;
  private string name;
  public string Name
  {
   get
   {
    return name;
   }
   set
   {
    name = value;
   }
  }
  public Gtk.Window TransientFor
  {
   set
   {
    if(beDlg != null)
     beDlg.TransientFor = value;
   }
  }
  public BookEditor()
  {
   Glade.XML gxml = new Glade.XML (Util.GladePath("contact-browser.glade"),
     "BookEditor", null);
   gxml.Autoconnect (this);
   beDlg = (Gtk.Dialog) gxml.GetWidget("BookEditor");
  }
  public int Run()
  {
   int rc = 0;
   if(beDlg != null)
   {
    rc = beDlg.Run();
    name = beName.Text;
    beDlg.Hide();
    beDlg.Destroy();
    beDlg = null;
   }
   return rc;
  }
  public void on_beName_changed(object o, EventArgs args)
  {
   if(beName.Text.Length > 0)
    okButton.Sensitive = true;
   else
    okButton.Sensitive = false;
  }
 }
}
