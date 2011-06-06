using System;
using System.Drawing;
using Gtk;
using Gdk;
using Glade;
using GtkSharp;
using GLib;
namespace SloggerApplication
{
 public class EntryEditor
 {
  [Glade.Widget] internal Gtk.Dialog NewEntryDialog;
  [Glade.Widget] internal Gtk.Calendar EntryCalendar;
  [Glade.Widget] internal Gtk.Entry TitleEntry;
  [Glade.Widget] internal Gtk.TextView MessageTextView;
  [Glade.Widget] internal Gtk.Button OKButton;
  private string title;
  private string message;
  private DateTime date;
  public string Title
  {
   get
   {
    return title;
   }
   set
   {
    title = value;
   }
  }
  public string Message
  {
   get
   {
    return message;
   }
   set
   {
    message = value;
   }
  }
  public DateTime Date
  {
   get
   {
    return date;
   }
   set
   {
    date = value;
   }
  }
  public Gtk.Window TransientFor
  {
   set
   {
    if(NewEntryDialog != null)
     NewEntryDialog.TransientFor = value;
   }
  }
  public EntryEditor()
  {
   Glade.XML gxml = new Glade.XML (Util.GladePath("slogger.glade"),
     "NewEntryDialog", null);
   gxml.Autoconnect (this);
   OKButton.Sensitive = false;
  }
  public int Run()
  {
   int rc = 0;
   if(NewEntryDialog != null)
   {
    rc = NewEntryDialog.Run();
    title = TitleEntry.Text;
    date = EntryCalendar.Date;
    message = MessageTextView.Buffer.Text;
    NewEntryDialog.Hide();
    NewEntryDialog.Destroy();
    NewEntryDialog = null;
   }
   return rc;
  }
  public void on_title_changed(object o, EventArgs args)
  {
   if(TitleEntry.Text.Length > 0)
    OKButton.Sensitive = true;
   else
    OKButton.Sensitive = false;
  }
 }
}
