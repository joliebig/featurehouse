using System;
using System.IO;
using System.Drawing;
using System.Collections;
using Simias.Storage;
using Simias.POBox;
using Simias;
using Gtk;
using Gdk;
using Glade;
using GtkSharp;
using GLib;
namespace Novell.AddressBook.UI.gtk
{
 public class CollectionProperties
 {
  [Glade.Widget] private Gtk.Dialog PropDialog = null;
  [Glade.Widget] private Gtk.VBox DialogVBox = null;
  private Gtk.Notebook propNoteBook = null;
  private ContactCollectionSharingPage sharingPage = null;
  private CollectionGeneralPage generalPage = null;
  private CollectionPropertiesPage propPage = null;
  private int activeTag = 0;
  private Collection collection;
  private bool runCalled = false;
  public event EventHandler Closed;
  public Gtk.Window TransientFor
  {
   set
   {
    if(PropDialog != null)
     PropDialog.TransientFor = value;
   }
  }
  public int ActiveTag
  {
   set
   {
    activeTag = value;
   }
  }
  public Collection Collection
  {
   get
   {
    return collection;
   }
   set
   {
    collection = value;
   }
  }
  public CollectionProperties()
  {
   InitGlade();
  }
  public void InitGlade()
  {
   Glade.XML gxml =
    new Glade.XML (Util.GladePath("collection-properties.glade"),
    "PropDialog",
    null);
   gxml.Autoconnect (this);
  }
  private void InitDialog()
  {
   if(collection != null)
   {
    propNoteBook = new Gtk.Notebook();
    sharingPage = new ContactCollectionSharingPage();
    generalPage = new CollectionGeneralPage();
    propPage = new CollectionPropertiesPage();
    sharingPage.Collection = collection;
    generalPage.Collection = collection;
    propPage.Collection = collection;
    propNoteBook.AppendPage(generalPage.MainWidget,
      new Label("General"));
    propNoteBook.AppendPage(sharingPage.MainWidget,
      new Label("Sharing"));
    propNoteBook.AppendPage(propPage.MainWidget,
      new Label("All Properties"));
    DialogVBox.PackStart(propNoteBook);
    DialogVBox.ShowAll();
   }
  }
  public int Run()
  {
   int rc = 0;
   InitDialog();
   if(PropDialog != null)
   {
    if(propNoteBook.NPages >= activeTag)
     propNoteBook.CurrentPage = activeTag;
    while(rc == 0)
    {
     runCalled = true;
     rc = PropDialog.Run();
     if(rc == -11)
     {
      rc = 0;
      switch(propNoteBook.CurrentPage)
      {
       case 1:
        Util.ShowHelp("bq6lwlu.html", null);
        break;
       case 2:
        Util.ShowHelp("bq6lwlj.html", null);
        break;
       default:
        Util.ShowHelp("front.html", null);
        break;
      }
     }
    }
    on_close(null, null);
   }
   return rc;
  }
  public void Show()
  {
   int rc = 0;
   InitDialog();
   if(PropDialog != null)
   {
    if(propNoteBook.NPages >= activeTag)
     propNoteBook.CurrentPage = activeTag;
    PropDialog.Show();
   }
  }
  public void on_close(object o, EventArgs args)
  {
   PropDialog.Hide();
   PropDialog.Destroy();
   PropDialog = null;
   if(Closed != null)
   {
    EventArgs e = new EventArgs();
    Closed(this, e);
   }
  }
  public void on_delete_event(object o, DeleteEventArgs args)
  {
   args.RetVal = true;
   on_close(o, args);
  }
  public void on_close_clicked(object o, EventArgs args)
  {
   if(!runCalled)
   {
    on_close(o, args);
   }
  }
 }
}
