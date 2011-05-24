

using System;
using System.Drawing;

using Gtk;
using Gdk;
using Glade;
using GtkSharp;
using GLib;

namespace SloggerApplication
{
 public class SlogEditor
 {
  [Glade.Widget] internal Gtk.Dialog NewSlogDialog;
  [Glade.Widget] internal Gtk.Entry NameEntry;
  [Glade.Widget] internal Gtk.Button OKButton;

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
    if(NewSlogDialog != null)
     NewSlogDialog.TransientFor = value;
   }
  }

  public SlogEditor()
  {
   Glade.XML gxml = new Glade.XML (Util.GladePath("slogger.glade"),
     "NewSlogDialog", null);
   gxml.Autoconnect (this);
  }

  public int Run()
  {
   int rc = 0;
   if(NewSlogDialog != null)
   {
    rc = NewSlogDialog.Run();
    name = NameEntry.Text;
    NewSlogDialog.Hide();
    NewSlogDialog.Destroy();
    NewSlogDialog = null;
   }
   return rc;
  }

  public void on_name_changed(object o, EventArgs args)
  {
   if(NameEntry.Text.Length > 0)
    OKButton.Sensitive = true;
   else
    OKButton.Sensitive = false;
  }
 }
}
