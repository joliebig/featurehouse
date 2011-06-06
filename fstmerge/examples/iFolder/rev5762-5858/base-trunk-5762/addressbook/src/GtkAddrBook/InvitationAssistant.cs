using System;
using System.IO;
using System.Drawing;
using Simias.Sync;
using Simias.POBox;
using Simias.Storage;
using Simias;
using Gtk;
using Gdk;
using Glade;
using GtkSharp;
using GLib;
namespace Novell.AddressBook.UI.gtk
{
 public class InvitationAssistant
 {
  [Glade.Widget] private Gnome.Druid InvitationDruid = null;
  [Glade.Widget] private Gnome.DruidPageStandard SelectLocationPage;
  [Glade.Widget] private Gtk.Entry InvitationEntry = null;
  [Glade.Widget] private Gtk.Entry CollectionPathEntry = null;
  [Glade.Widget] private Gtk.Button CollectionBrowseButton = null;
  [Glade.Widget] private Gtk.Label CollectionNameLabel = null;
  [Glade.Widget] private Gtk.Label CollectionTypeLabel = null;
  [Glade.Widget] private Gtk.Label CollectionFromLabel = null;
  [Glade.Widget] private Gtk.Label SelectLabel = null;
  [Glade.Widget] private Gtk.Label SelectPathLabel = null;
  [Glade.Widget] private Gtk.Window InviteWindow = null;
  private string inviteFile = null;
  private SubscriptionInfo subInfo = null;
  private Subscription subscription = null;
  private Store store = null;
  private POBox pobox = null;
  private bool SelectorIsFirst = false;
  public event EventHandler AssistantClosed;
  public InvitationAssistant()
  {
   Init_Glade();
  }
  public InvitationAssistant(string inviteFile)
  {
   this.inviteFile = inviteFile;
   Init_Glade();
  }
  public InvitationAssistant(SubscriptionInfo subInfo)
  {
   if(store == null)
    store = Store.GetStore();
   subscription = Subscription.GetSubscriptionFromSubscriptionInfo(store, subInfo);
   Init_Glade();
  }
  public InvitationAssistant(Subscription sub)
  {
   subscription = sub;
   Init_Glade();
  }
  private void Init_Glade()
  {
   Glade.XML mainXml =
    new Glade.XML (Util.GladePath("invitation-assistant.glade"),
    "InviteWindow",
    null);
   mainXml.Autoconnect (this);
   if( (subscription != null) || (inviteFile != null) )
   {
    SelectorIsFirst = true;
    InvitationDruid.Page = SelectLocationPage;
   }
  }
  public void ShowAll()
  {
   if(InviteWindow != null)
   {
    InviteWindow.ShowAll();
   }
  }
  private void on_close(object o, EventArgs args)
  {
   InviteWindow.Hide();
   InviteWindow.Destroy();
   InviteWindow = null;
   if(AssistantClosed != null)
   {
    EventArgs e = new EventArgs();
    AssistantClosed(this, e);
   }
  }
  private void on_delete_event(object o, DeleteEventArgs args)
  {
   args.RetVal = true;
   on_close(o, args);
  }
  private void on_select_file_prepare(object o, EventArgs args)
  {
   if(InvitationEntry.Text.Length > 0)
    InvitationDruid.SetButtonsSensitive(true, true, true, true);
   else
    InvitationDruid.SetButtonsSensitive(true, false, true, true);
  }
  private void on_InvitationEntry_changed(object o, EventArgs args)
  {
   if(InvitationEntry.Text.Length > 0)
    InvitationDruid.SetButtonsSensitive(true, true, true, true);
   else
    InvitationDruid.SetButtonsSensitive(true, false, true, true);
  }
  private void on_OpenInvitationButton_clicked(object o, EventArgs args)
  {
   FileSelection fs = new FileSelection ("Choose an invitation file");
   fs.ShowFileops = false;
   if(InvitationEntry.Text.Length > 0)
   {
    fs.Filename = InvitationEntry.Text;
   }
   int rc = fs.Run ();
   fs.Hide ();
   if(rc == -5)
   {
    try
    {
     InitSubscriptionInfo(fs.Filename);
     InvitationEntry.Text = fs.Filename;
    }
    catch(Exception e)
    {
     MessageDialog md = new MessageDialog(InviteWindow,
      DialogFlags.DestroyWithParent | DialogFlags.Modal,
      MessageType.Error,
      ButtonsType.Close,
      "The selected file was not a valid invitation file.");
     md.Run();
     md.Hide();
     InvitationEntry.Text = "";
     InvitationDruid.SetButtonsSensitive(true, false,
               true, true);
    }
   }
  }
  private void InitSubscriptionInfo(string filename)
  {
   if(store == null)
    store = Store.GetStore();
   subscription = Subscription.GetSubscriptionFromSubscriptionInfo(store, filename);
  }
  private void on_select_location_prepare(object o, EventArgs args)
  {
   if(SelectorIsFirst)
   {
    if(store == null)
     store = Store.GetStore();
    if(inviteFile != null)
    {
     InitSubscriptionInfo(inviteFile);
    }
   }
   if(subscription != null)
   {
    if(pobox == null)
    {
     pobox = POBox.GetPOBox(store, subscription.DomainID);
    }
    CollectionNameLabel.Text =
      subscription.SubscriptionCollectionName;
    if( (subscription.SubscriptionCollectionType != null) &&
      (subscription.SubscriptionCollectionType.Length > 0) )
     CollectionTypeLabel.Text =
       subscription.SubscriptionCollectionType;
    else
     CollectionTypeLabel.Text = "iFolder";
    CollectionFromLabel.Text = subscription.FromName;
    if(!subscription.HasDirNode)
    {
     SelectLabel.Hide();
     SelectPathLabel.Hide();
     CollectionPathEntry.Hide();
     CollectionBrowseButton.Hide();
    }
    if( (subscription.HasDirNode) &&
      (CollectionPathEntry.Text.Length == 0))
     InvitationDruid.SetButtonsSensitive((!SelectorIsFirst),
            false, true, true);
    else
     InvitationDruid.SetButtonsSensitive((!SelectorIsFirst),
            true, true, true);
   }
   else
   {
    Console.WriteLine("Ney, Ialksdjflkasjdfkl'm getting called");
    InvitationDruid.SetButtonsSensitive((!SelectorIsFirst),
             false, true, true);
   }
  }
  private void on_collection_path_changed(object o, EventArgs args)
  {
   if(CollectionPathEntry.Text.Length > 0)
    InvitationDruid.SetButtonsSensitive(true, true, true, true);
   else
    InvitationDruid.SetButtonsSensitive(true, false, true, true);
  }
  private void on_collection_path_browse_clicked(object o, EventArgs args)
  {
   FileSelection fs = new FileSelection ("Choose a directory");
   fs.FileList.Parent.Hide();
   fs.SelectionEntry.Hide();
   fs.FileopDelFile.Hide();
   fs.FileopRenFile.Hide();
   if(CollectionPathEntry.Text.Length > 0)
   {
    fs.Filename = CollectionPathEntry.Text;
    fs.Filename += "/";
   }
   int rc = fs.Run ();
   fs.Hide ();
   if(rc == -5)
   {
    CollectionPathEntry.Text = fs.Filename;
   }
  }
  private void on_select_file_next(object o, EventArgs args)
  {
  }
  private void on_druid_finish(object o, EventArgs args)
  {
   try
   {
    if(store == null)
     store = Store.GetStore();
    subscription.CollectionRoot = Path.GetFullPath(CollectionPathEntry.Text);
    if(subscription.SubscriptionState == SubscriptionStates.Ready)
    {
     pobox.Commit(subscription);
     subscription.CreateSlave(store);
    }
    else
    {
     subscription.Accept(store, SubscriptionDispositions.Accepted);
     pobox.Commit(subscription);
    }
   }
   catch(SimiasException ex)
   {
    Console.WriteLine(ex);
   }
   on_close(o, args);
  }
  private void on_druid_cancel(object o, EventArgs args)
  {
   on_close(o, args);
  }
 }
}
