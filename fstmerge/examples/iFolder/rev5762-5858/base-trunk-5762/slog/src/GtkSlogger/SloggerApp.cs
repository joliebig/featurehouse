using System;
using Novell.Collaboration;
using Simias.Storage;
using Simias.POBox;
using Simias;
using System.Collections;
using System.IO;
using Novell.AddressBook.UI.gtk;
using Novell.AddressBook;
using Gtk;
using Gdk;
using Glade;
using GtkSharp;
using GLib;
using Gnome;
namespace SloggerApplication
{
 public class GtkSlogger
 {
  [Glade.Widget] private Gnome.App SloggerApp = null;
  [Glade.Widget] private TreeView SlogTreeView = null;
  [Glade.Widget] private TreeView EntryTreeView = null;
  [Glade.Widget] private Gtk.Calendar SlogCal = null;
  [Glade.Widget] private Gtk.Button DeleteButton = null;
  [Glade.Widget] private Gtk.Button PropButton = null;
  [Glade.Widget] private Gtk.Entry TitleEntry = null;
  [Glade.Widget] private Gtk.Entry DateEntry = null;
  [Glade.Widget] private Gtk.TextView SlogTextView = null;
  private ListStore SlogTreeStore;
  private ListStore EntryTreeStore;
  private SlogManager sMan;
  private Novell.AddressBook.Manager abMan;
  private Slog curSlog = null;
  private SlogEntry curSlogEntry = null;
  private Pixbuf defaultPixbuf = null;
  public GtkSlogger()
  {
   Init();
   if(sMan == null)
    sMan = SlogManager.Connect();
   if(abMan == null)
    abMan = Novell.AddressBook.Manager.Connect();
   defaultPixbuf = new Pixbuf(Util.ImagesPath("sloghead.png"));
   Refresh();
  }
  public void Refresh()
  {
   SlogTreeStore.Clear();
   EntryTreeStore.Clear();
   DeleteButton.Sensitive = false;
   PropButton.Sensitive = false;
   TitleEntry.Text = "";
   DateEntry.Text = "";
   SlogTextView.Buffer.Text = "";
   DateEntry.Text = SlogCal.Date.ToLongDateString();
   foreach(Slog slog in sMan)
   {
    SlogTreeStore.AppendValues(slog);
   }
  }
  public void Init ()
  {
   Glade.XML gxml =
    new Glade.XML (Util.GladePath("slogger.glade"),
      "SloggerApp", null);
   gxml.Autoconnect (this);
   SlogTreeStore = new ListStore(typeof(Slog));
   SlogTreeView.Model = SlogTreeStore;
   SlogTreeView.AppendColumn("Slogs",
     new CellRendererText(),
     new TreeCellDataFunc(SlogCellTextDataFunc));
   SlogTreeView.Selection.Changed +=
    new EventHandler(on_slog_selection_changed);
   EntryTreeStore = new ListStore(typeof(SlogEntry));
   EntryTreeView.Model = EntryTreeStore;
   TreeViewColumn entryColumn = new TreeViewColumn();
   CellRendererPixbuf entryPBCR = new CellRendererPixbuf();
   entryColumn.PackStart(entryPBCR, false);
   entryColumn.SetCellDataFunc(entryPBCR, new TreeCellDataFunc(
      EntryCellPixbufDataFunc));
   CellRendererText entryTextCR = new CellRendererText();
   entryColumn.PackStart(entryTextCR, false);
   entryColumn.SetCellDataFunc(entryTextCR, new TreeCellDataFunc(
      EntryCellTextDataFunc));
   entryColumn.Title = "Entries";
   EntryTreeView.AppendColumn(entryColumn);
   EntryTreeView.Selection.Changed +=
    new EventHandler(on_entry_selection_changed);
  }
  private void EntryCellTextDataFunc (Gtk.TreeViewColumn tree_column,
    Gtk.CellRenderer cell, Gtk.TreeModel tree_model,
    Gtk.TreeIter iter)
  {
   SlogEntry se = (SlogEntry) EntryTreeStore.GetValue(iter,0);
   ((CellRendererText) cell).Text = se.Title;
  }
  private void EntryCellPixbufDataFunc (Gtk.TreeViewColumn tree_column,
    Gtk.CellRenderer cell, Gtk.TreeModel tree_model,
    Gtk.TreeIter iter)
  {
   SlogEntry se = (SlogEntry) EntryTreeStore.GetValue(iter,0);
   Contact con = abMan.GetContact(curSlog, se.UserID);
   ((CellRendererPixbuf) cell).Pixbuf = GetScaledPhoto(con, 24);
  }
  private void SlogCellTextDataFunc (Gtk.TreeViewColumn tree_column,
    Gtk.CellRenderer cell, Gtk.TreeModel tree_model,
    Gtk.TreeIter iter)
  {
   Slog slog = (Slog) SlogTreeStore.GetValue(iter,0);
   ((CellRendererText) cell).Text = slog.Name;
  }
  public void ShowAll()
  {
   SloggerApp.ShowAll();
  }
  public void on_app_delete_event(object o, DeleteEventArgs args)
  {
   args.RetVal = true;
   on_quit(o, args);
  }
  public void on_slog_selection_changed(object o, EventArgs args)
  {
   TreeSelection tSelect = SlogTreeView.Selection;
   if(tSelect.CountSelectedRows() == 1)
   {
    TreeModel tModel;
    TreeIter iter;
    tSelect.GetSelected(out tModel, out iter);
    if(tModel != null)
     tModel = null;
    curSlog = (Slog)
     SlogTreeStore.GetValue(iter,0);
    EntryTreeStore.Clear();
    PropButton.Sensitive = true;
    foreach(SlogEntry se in curSlog)
    {
     EntryTreeStore.AppendValues(se);
    }
    DeleteButton.Sensitive = true;
   }
   else
   {
    curSlog = null;
    PropButton.Sensitive = false;
    DeleteButton.Sensitive = false;
   }
  }
  public void on_entry_selection_changed(object o, EventArgs args)
  {
   TreeSelection tSelect = EntryTreeView.Selection;
   if(tSelect.CountSelectedRows() == 1)
   {
    TreeModel tModel;
    TreeIter iter;
    tSelect.GetSelected(out tModel, out iter);
    if(tModel != null)
     tModel = null;
    curSlogEntry = (SlogEntry)
     EntryTreeStore.GetValue(iter,0);
    TitleEntry.Text = curSlogEntry.Title;
    DateEntry.Text = curSlogEntry.PublishDate;
    SlogTextView.Buffer.Text = curSlogEntry.Description;
    DeleteButton.Sensitive = true;
   }
   else
   {
    curSlogEntry = null;
    DeleteButton.Sensitive = false;
   }
  }
  public void on_refresh(object o, EventArgs eventArgs)
  {
   Refresh();
  }
  public void on_delete(object o, EventArgs eventArgs)
  {
   if(EntryTreeView.HasFocus)
   {
    MessageDialog dialog = new MessageDialog(SloggerApp,
      DialogFlags.Modal |
      DialogFlags.DestroyWithParent,
      MessageType.Question,
      ButtonsType.YesNo,
      "Do you want to delete the selected Slog Entries?");
    dialog.Title = "Delete Slog Entries";
    dialog.TransientFor = SloggerApp;
    int rc = dialog.Run();
    dialog.Hide();
    if(rc == (int)ResponseType.Yes)
    {
     DeleteSelectedEntries();
    }
   }
   else if(SlogTreeView.HasFocus)
   {
    MessageDialog dialog = new MessageDialog(SloggerApp,
      DialogFlags.Modal |
      DialogFlags.DestroyWithParent,
      MessageType.Question,
      ButtonsType.YesNo,
      "Do you want to delete the selected Slogs?");
    dialog.Title = "Delete Slogs";
    dialog.TransientFor = SloggerApp;
    int rc = dialog.Run();
    dialog.Hide();
    if(rc == (int)ResponseType.Yes)
    {
     DeleteSelectedSlogs();
    }
   }
   TitleEntry.Text = "";
   SlogTextView.Buffer.Text = "";
   DeleteButton.Sensitive = false;
  }
  public void DeleteSelectedEntries()
  {
   TreeSelection tSelect = EntryTreeView.Selection;
   if(tSelect.CountSelectedRows() == 1)
   {
    TreeModel tModel;
    TreeIter iter;
    tSelect.GetSelected(out tModel, out iter);
    if(tModel != null)
     tModel = null;
    curSlogEntry = (SlogEntry)
     EntryTreeStore.GetValue(iter,0);
    curSlog.Commit(curSlog.Delete(curSlogEntry));
    EntryTreeStore.Remove(ref iter);
   }
  }
  public void DeleteSelectedSlogs()
  {
   TreeSelection tSelect = SlogTreeView.Selection;
   if(tSelect.CountSelectedRows() == 1)
   {
    TreeModel tModel;
    TreeIter iter;
    tSelect.GetSelected(out tModel, out iter);
    if(tModel != null)
     tModel = null;
    curSlog = (Slog) SlogTreeStore.GetValue(iter,0);
    curSlog.Commit(curSlog.Delete());
    SlogTreeStore.Remove(ref iter);
    curSlog = null;
    Refresh();
   }
  }
  public void on_new_slog(object o, EventArgs eventArgs)
  {
   SlogEditor se = new SlogEditor();
   if(se.Run() == -5)
   {
    curSlog = sMan.CreateSlog(se.Name);
    curSlog.Commit();
    SlogTreeStore.AppendValues(curSlog);
   }
  }
  public void on_add_slog(object o, EventArgs eventArgs)
  {
   CollectionPicker cp = new CollectionPicker();
   cp.IgnoreType = typeof(Slog).Name;
   int rc = cp.Run();
   if(rc == -5)
   {
    Collection col = cp.Collection;
    Console.WriteLine("Convert collection {0}", col.Name);
    col.SetType(col, typeof(Slog).Name);
    col.Commit();
    Refresh();
   }
  }
  public void on_new_entry(object o, EventArgs eventArgs)
  {
   EntryEditor ee = new EntryEditor();
   ee.TransientFor = SloggerApp;
   if(ee.Run() == -5)
   {
    SlogEntry se = new SlogEntry(curSlog,
      ee.Date.ToLongDateString());
    se.Title = ee.Title;
    se.Description = ee.Message;
    se.UserID = curSlog.GetCurrentMember().UserID;
    curSlog.Commit(se);
    EntryTreeStore.AppendValues(se);
   }
  }
  public void on_about(object o, EventArgs eventArgs)
  {
   Util.ShowAbout();
  }
  public void on_day_selected(object o, EventArgs eventArgs)
  {
   TreeSelection tSelect = EntryTreeView.Selection;
   tSelect.UnselectAll();
   DateEntry.Text = SlogCal.Date.ToLongDateString();
   TitleEntry.Text = "";
   SlogTextView.Buffer.Text = "";
  }
  public void on_properties(object o, EventArgs eventArgs)
  {
   CollectionProperties colProp = new CollectionProperties();
   colProp.TransientFor = SloggerApp;
   colProp.Collection = curSlog;
   colProp.ActiveTag = 1;
   colProp.Run();
  }
  public void on_title_changed(object o, EventArgs eventArgs)
  {
  }
  public void on_quit(object o, EventArgs args)
  {
   SloggerApp.Hide();
   SloggerApp.Destroy();
   SloggerApp = null;
   Application.Quit();
  }
  public static void Main (string[] args)
  {
   Gnome.Program program =
    new Program("slogger", "0.10.0", Modules.UI, args);
   GtkSlogger slogger = new GtkSlogger();
   slogger.ShowAll();
   program.Run();
  }
  private Pixbuf GetScaledPhoto(Contact c, int height)
  {
   Pixbuf pb = null;
   if(c == null)
    return defaultPixbuf;
   try
   {
    int newWidth, newHeight;
    pb = new Pixbuf(c.ExportPhoto());
    newHeight = height;
    newWidth = height;
    if(pb.Height != pb.Width)
    {
     int perc = (height * 1000) / pb.Height;
     newWidth = pb.Width * perc / 1000;
    }
    pb = pb.ScaleSimple(newWidth, newHeight,
      InterpType.Bilinear);
   }
   catch(Exception e)
   {
    pb = defaultPixbuf;
   }
   return pb;
  }
 }
}
