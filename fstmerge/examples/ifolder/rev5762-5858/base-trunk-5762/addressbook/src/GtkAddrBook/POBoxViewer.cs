
using System;
using Novell.AddressBook;
using Simias.Storage;
using Simias.POBox;
using Simias;
using System.Collections;
using System.IO;

using Gtk;
using Gdk;
using Glade;
using GtkSharp;
using GLib;
using Gnome;


namespace Novell.AddressBook.UI.gtk
{
 public class POBoxViewer
 {
  [Glade.Widget] private Gnome.App POViewerApp = null;
  [Glade.Widget] private TreeView SubTreeView = null;

  [Glade.Widget] private Gtk.MenuItem AcceptItem = null;
  [Glade.Widget] private Gtk.MenuItem DeclineItem = null;
  [Glade.Widget] private Gtk.MenuItem DeleteItem = null;

  [Glade.Widget] private Gtk.Button AcceptButton = null;
  [Glade.Widget] private Gtk.Button DeclineButton = null;
  [Glade.Widget] private Gtk.Button DeleteButton = null;

  private ListStore SubTreeStore;
  private Pixbuf InvitationPixbuf;
  private Subscription curSub;

  private Simias.POBox.POBox pobox;
  private Store store;

  public event EventHandler ViewerClosed;

  public POBoxViewer()
  {
   Init();
   Refresh();
  }

  public void Refresh()
  {
   SubTreeStore.Clear();

   if(store == null)
    store = Store.GetStore();
   if(store != null)
   {
    pobox = Simias.POBox.POBox.GetPOBox(store,
             store.DefaultDomain);
   }

   if(pobox != null)
   {

    ICSList poList = pobox.Search(
      PropertyTags.Types,
      typeof(Subscription).Name,
      SearchOp.Equal);

    foreach(ShallowNode sNode in poList)
    {
     Subscription sub = new Subscription(pobox, sNode);

     if (sub.SubscriptionState == SubscriptionStates.Ready)
     {
      if (store.GetCollectionByID(sub.SubscriptionCollectionID) != null)
      {
       continue;
      }
     }
     SubTreeStore.AppendValues(sub);
    }
   }
   curSub = null;
  }


  public void Init ()
  {
   Glade.XML gxml =
    new Glade.XML (Util.GladePath("pobox-viewer.glade"),
    "POViewerApp", null);

   gxml.Autoconnect (this);




   SubTreeStore = new ListStore(typeof(Subscription));
   SubTreeView.Model = SubTreeStore;

   CellRendererPixbuf bcrp = new CellRendererPixbuf();
   TreeViewColumn btvc = new TreeViewColumn();
   btvc.PackStart(bcrp, false);
   btvc.SetCellDataFunc(bcrp, new TreeCellDataFunc(
      SubCellPixbufDataFunc));

   CellRendererText bcrt = new CellRendererText();
   btvc.PackStart(bcrt, false);
   btvc.SetCellDataFunc(bcrt, new TreeCellDataFunc(
      SubCellTextDataFunc));
   btvc.Title = "Subscription";
   SubTreeView.AppendColumn(btvc);

   SubTreeView.AppendColumn("State",
     new CellRendererText(),
     new TreeCellDataFunc(StateCellTextDataFunc));

   SubTreeView.Selection.Changed +=
    new EventHandler(on_selection_changed);

   InvitationPixbuf =
     new Pixbuf(Util.ImagesPath("invitation_16.png"));

   AcceptItem.Sensitive = false;
   DeclineItem.Sensitive = false;
   DeleteItem.Sensitive = false;

   AcceptButton.Sensitive = false;
   DeclineButton.Sensitive = false;
   DeleteButton.Sensitive = false;
  }

  private void SubCellTextDataFunc (Gtk.TreeViewColumn tree_column,
    Gtk.CellRenderer cell, Gtk.TreeModel tree_model,
    Gtk.TreeIter iter)
  {
   Subscription sub = (Subscription) SubTreeStore.GetValue(iter,0);
   ((CellRendererText) cell).Text = sub.Name;
  }

  private void StateCellTextDataFunc (Gtk.TreeViewColumn tree_column,
    Gtk.CellRenderer cell, Gtk.TreeModel tree_model,
    Gtk.TreeIter iter)
  {
   Subscription sub = (Subscription) SubTreeStore.GetValue(iter,0);
   ((CellRendererText) cell).Text = sub.SubscriptionState.ToString();
  }

  private void SubCellPixbufDataFunc (Gtk.TreeViewColumn tree_column,
    Gtk.CellRenderer cell, Gtk.TreeModel tree_model,
    Gtk.TreeIter iter)
  {
   ((CellRendererPixbuf) cell).Pixbuf = InvitationPixbuf;
  }

  public void ShowAll()
  {
   POViewerApp.ShowAll();
  }

  public void on_POViewerApp_delete_event(object o, DeleteEventArgs args)
  {
   args.RetVal = true;
   on_quit(o, args);
  }

  public void on_selection_changed(object o, EventArgs args)
  {
   TreeSelection tSelect = SubTreeView.Selection;
   if(tSelect.CountSelectedRows() == 1)
   {
    TreeModel tModel;
    TreeIter iter;

    tSelect.GetSelected(out tModel, out iter);
    if(tModel != null)
     tModel = null;

    Subscription sub = (Subscription)
     SubTreeStore.GetValue(iter,0);

    curSub = sub;

    if( (sub.SubscriptionState == SubscriptionStates.Pending) ||
     (sub.SubscriptionState == SubscriptionStates.Received) ||
     (sub.SubscriptionState == SubscriptionStates.Ready) )
    {
     AcceptItem.Sensitive = true;
     DeclineItem.Sensitive = true;
     AcceptButton.Sensitive = true;
     DeclineButton.Sensitive = true;
    }

    DeleteItem.Sensitive = true;
    DeleteButton.Sensitive = true;
   }
   else
   {
    AcceptItem.Sensitive = false;
    DeclineItem.Sensitive = false;
    DeleteItem.Sensitive = false;
    AcceptButton.Sensitive = false;
    DeclineButton.Sensitive = false;
    DeleteButton.Sensitive = false;
    curSub = null;
   }
  }

  public void on_refresh(object o, EventArgs eventArgs)
  {
   Refresh();
  }

  public void on_accept(object o, EventArgs eventArgs)
  {
   if(curSub != null)
   {
    if(curSub.SubscriptionState == SubscriptionStates.Pending)
    {
     curSub.Accept(store, curSub.SubscriptionRights);
     pobox.Commit(curSub);
    }
    else if(curSub.SubscriptionState == SubscriptionStates.Received ||
      (curSub.SubscriptionState == SubscriptionStates.Ready) )
    {
     InvitationAssistant ia = new InvitationAssistant(curSub);
     ia.ShowAll();
    }
   }
  }

  public void on_decline(object o, EventArgs eventArgs)
  {
   if(curSub != null)
   {
    curSub.Decline();
    pobox.Commit(curSub);
   }
  }

  public void on_delete(object o, EventArgs eventArgs)
  {
   if(curSub != null)
   {
    TreeSelection tSelect = SubTreeView.Selection;
    if(tSelect.CountSelectedRows() == 1)
    {
     TreeModel tModel;
     TreeIter iter;

     tSelect.GetSelected(out tModel, out iter);

     pobox.Commit(pobox.Delete(curSub));

     SubTreeStore.Remove(ref iter);
     curSub = null;
    }
   }
  }

  public void on_about(object o, EventArgs eventArgs)
  {
   try
   {


    string[] authors = new string[]
    {
     "Jared Allen <jpallen@novell.com>",
     "Brady Anderson <banderso@novell.com>",
     "James Bell",
     "Doug Eddy",
     "Calvin Gaisford <cgaisford@novell.com>",
     "Bruce Getter",
     "Mike Lasky",
     "Rob Lyon",
     "Dale Olds",
     "Sharon Smith",
     "Joe Stark",
     "Paul Thomas",
     "Russ Young",
    };
    string[] documentors = new string[]
    {
     "Catherine Craft",
     "Sean Beall"
    };

    About about = new About("Post Office", "0.10",
     "Copyright (C) 2004 Novell, Inc.",
     "Post Office is the happy place where all invitations to Simias Collections go!",
     authors,
     documentors,
     "Translated to English from the language of \"The Incan Monkey God\" by the Mono compiler",
     new Pixbuf( Util.ImagesPath("invitation_128.png")));
    about.Show();
   }
   catch(Exception e)
   {
   }
  }

  public void on_quit(object o, EventArgs args)
  {
   POViewerApp.Hide();
   POViewerApp.Destroy();
   POViewerApp = null;

   if(ViewerClosed != null)
   {
    EventArgs e = new EventArgs();
    ViewerClosed(this, e);
   }
  }
 }
}
