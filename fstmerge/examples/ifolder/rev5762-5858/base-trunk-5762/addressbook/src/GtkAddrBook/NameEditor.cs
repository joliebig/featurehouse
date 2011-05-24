

using System;
using System.Drawing;
using Novell.AddressBook;

using Gtk;
using Gdk;
using Glade;
using GtkSharp;
using GLib;

namespace Novell.AddressBook.UI.gtk
{
 public class NameEditor
 {
  public Novell.AddressBook.Name Name
  {
   get
   {
    return this.name;
   }

   set
   {
    this.name = value;
   }
  }

  public Gtk.Window TransientFor
  {
   set
   {
    nameEditorDialog.TransientFor = value;
   }
  }

  [Glade.Widget] private Gtk.Entry titleEntry = null;
  [Glade.Widget] private Gtk.Entry firstEntry = null;
  [Glade.Widget] private Gtk.Entry middleEntry = null;
  [Glade.Widget] private Gtk.Entry lastEntry = null;
  [Glade.Widget] private Gtk.Entry suffixEntry = null;

  private Gtk.Dialog nameEditorDialog;
  private Novell.AddressBook.Name name;

  public NameEditor()
  {
   InitGlade();
  }

  private void InitGlade()
  {
   Glade.XML gxml = new Glade.XML (Util.GladePath("name-editor.glade"),
     "NameEditorDialog", null);
   gxml.Autoconnect (this);
   nameEditorDialog = (Gtk.Dialog) gxml.GetWidget("NameEditorDialog");
  }

  private void PopulateWidgets()
  {
   if(name.Prefix != null)
    titleEntry.Text = name.Prefix;

   if(name.Given != null)
    firstEntry.Text = name.Given;

   if(name.Other != null)
    middleEntry.Text = name.Other;

   if(name.Family != null)
    lastEntry.Text = name.Family;

   if(name.Suffix != null)
    suffixEntry.Text = name.Suffix;
  }

  public int Run()
  {
   if(nameEditorDialog != null)
   {
    int rc;
    if(name == null)
     name = new Name();

    PopulateWidgets();

    nameEditorDialog.ShowAll();
    rc = nameEditorDialog.Run();
    if(rc == -5)
    {
     SaveData();
    }
    CloseDialog();
    return rc;
   }
   return 0;
  }

  private void SaveData()
  {
   if(titleEntry.Text.Length > 0)
    name.Prefix = titleEntry.Text;
   else
    name.Prefix = null;

   if(firstEntry.Text.Length > 0)
    name.Given = firstEntry.Text;
   else
    name.Given = null;

   if(middleEntry.Text.Length > 0)
    name.Other = middleEntry.Text;
   else
    name.Other = null;

   if(lastEntry.Text.Length > 0)
    name.Family = lastEntry.Text;
   else
     name.Family = null;

   if(suffixEntry.Text.Length > 0)
    name.Suffix = suffixEntry.Text;
   else
    name.Suffix = null;
  }

  private void CloseDialog()
  {
   nameEditorDialog.Hide();
   nameEditorDialog.Destroy();
   nameEditorDialog = null;
  }
 }
}
