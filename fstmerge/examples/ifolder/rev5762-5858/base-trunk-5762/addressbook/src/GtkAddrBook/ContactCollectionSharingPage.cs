
using System;
using System.IO;
using System.Drawing;
using System.Collections;
using Simias.Storage;
using Simias.POBox;
using Simias;
using Novell.AddressBook;
using Novell.AddressBook.UI.gtk;

using Gtk;
using Gdk;
using Glade;
using GtkSharp;
using GLib;


namespace Novell.AddressBook.UI.gtk
{
 public class ContactCollectionSharingPage
 {
  [Glade.Widget] private Gtk.VBox SharingVBox = null;
  [Glade.Widget] private TreeView ContactTreeView = null;
  [Glade.Widget] private Button addSharingButton = null;
  [Glade.Widget] private Button removeSharingButton = null;
  [Glade.Widget] private Button AcceptButton = null;
  [Glade.Widget] private Button DeclineButton = null;
  [Glade.Widget] private RadioButton FullControlRB = null;
  [Glade.Widget] private RadioButton ReadWriteRB = null;
  [Glade.Widget] private RadioButton ReadOnlyRB = null;

  private ListStore ContactTreeStore;
  private Pixbuf ContactPixBuf;
  private Pixbuf CurContactPixBuf;
  private Pixbuf InvContactPixBuf;
  private Novell.AddressBook.Manager abMan;
  private ArrayList guidList;

  private Collection collection;
  private Simias.POBox.POBox pobox;

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

  public Widget MainWidget
  {
   get
   {
    if(collection == null)
     return null;

    if(SharingVBox == null)
     InitGlade();

    return SharingVBox;
   }
  }

  public ContactCollectionSharingPage()
  {
  }

  private void InitGlade()
  {

   Glade.XML gxml =
    new Glade.XML (Util.GladePath("collection-properties.glade"),
    "SharingVBox",
    null);
   gxml.Autoconnect (this);

   if(SharingVBox == null)
    Console.WriteLine("SharingVBox didn't load correctly");

   if( pobox == null)
   {
    pobox = Simias.POBox.POBox.GetPOBox(collection.StoreReference,
       collection.StoreReference.DefaultDomain);
   }

   Init_Page();
  }




  private void Init_Page()
  {
   ContactTreeStore = new ListStore(typeof(SharingListHolder));
   ContactTreeView.Model = ContactTreeStore;
   CellRendererPixbuf ccrp = new CellRendererPixbuf();
   TreeViewColumn ctvc = new TreeViewColumn();
   ctvc.PackStart(ccrp, false);
   ctvc.SetCellDataFunc(ccrp,
     new TreeCellDataFunc(ContactCellPixbufDataFunc));

   CellRendererText ccrt = new CellRendererText();
   ctvc.PackStart(ccrt, false);
   ctvc.SetCellDataFunc(ccrt,
     new TreeCellDataFunc(ContactCellTextDataFunc));
   ctvc.Title = "Contacts";
   ContactTreeView.AppendColumn(ctvc);

   ContactTreeView.AppendColumn("State",
     new CellRendererText(),
     new TreeCellDataFunc(StateCellTextDataFunc));

   ContactTreeView.AppendColumn("Access",
     new CellRendererText(),
     new TreeCellDataFunc(AccessCellTextDataFunc));

   ContactTreeView.Selection.Changed +=
     new EventHandler(on_selection_changed);

   ContactPixBuf = new Pixbuf(Util.ImagesPath("contact.png"));
   CurContactPixBuf = new Pixbuf(Util.ImagesPath("contact_me.png"));
   InvContactPixBuf = new Pixbuf(Util.ImagesPath("invited-contact.png"));

   guidList = new ArrayList();

   abMan = Novell.AddressBook.Manager.Connect( );

   if(abMan == null)
    Console.WriteLine("Warning: Unable to connect to AddressBook");

   ICSList memList = collection.GetMemberList();
   foreach(ShallowNode sNode in memList)
   {
    Member m = new Member(collection, sNode);
    SharingListHolder slh = new SharingListHolder(
     m, null);

    ContactTreeStore.AppendValues(slh);
    guidList.Add(m.UserID);
   }

   ICSList poList = pobox.Search(
     Subscription.SubscriptionCollectionIDProperty,
     collection.ID,
     SearchOp.Equal);

   foreach(ShallowNode sNode in poList)
   {
    Subscription sub = new Subscription(pobox, sNode);

    if (sub.SubscriptionState == SubscriptionStates.Ready)
    {
     if (pobox.StoreReference.GetCollectionByID(sub.SubscriptionCollectionID) != null)
     {
      continue;
     }
    }
    SharingListHolder slh = new SharingListHolder(
     null, sub);

    ContactTreeStore.AppendValues(slh);
   }

   if(collection.IsShareable(collection.GetCurrentMember()))
    addSharingButton.Sensitive = true;
   else
   {
    addSharingButton.Sensitive = false;

    FullControlRB.Sensitive = false;
    ReadWriteRB.Sensitive = false;
    ReadOnlyRB.Sensitive = false;
   }

   removeSharingButton.Sensitive = false;
   AcceptButton.Sensitive = false;
   DeclineButton.Sensitive = false;
  }

  private void ContactCellTextDataFunc (Gtk.TreeViewColumn tree_column,
    Gtk.CellRenderer cell, Gtk.TreeModel tree_model,
    Gtk.TreeIter iter)
  {
   SharingListHolder slh = (SharingListHolder)
    ContactTreeStore.GetValue(iter,0);
   if(slh.Member != null)
   {
    Contact con = abMan.GetContact(slh.Member);
    if(con != null)
     ((CellRendererText) cell).Text = con.FN;
    else
     ((CellRendererText) cell).Text = slh.Member.Name;
   }
   else if(slh.Subscription != null)
   {
    ((CellRendererText) cell).Text = slh.Subscription.ToName;
   }
   else
    ((CellRendererText) cell).Text = "null user";
  }

  private void ContactCellPixbufDataFunc(Gtk.TreeViewColumn tree_column,
    Gtk.CellRenderer cell, Gtk.TreeModel tree_model,
    Gtk.TreeIter iter)
  {
   SharingListHolder slh =
     (SharingListHolder) ContactTreeStore.GetValue(iter,0);
   if( (slh != null) && (slh.Member != null) &&
    (collection.GetCurrentMember().UserID == slh.Member.UserID) )
    ((CellRendererPixbuf) cell).Pixbuf = CurContactPixBuf;
   else if( (slh != null) && (slh.Subscription != null) )
    ((CellRendererPixbuf) cell).Pixbuf = InvContactPixBuf;
   else
    ((CellRendererPixbuf) cell).Pixbuf = ContactPixBuf;
  }

  private void StateCellTextDataFunc (Gtk.TreeViewColumn tree_column,
    Gtk.CellRenderer cell, Gtk.TreeModel tree_model,
    Gtk.TreeIter iter)
  {
   SharingListHolder slh =
     (SharingListHolder) ContactTreeStore.GetValue(iter,0);
   if( (slh != null) && (slh.Member != null) )
   {
    if(collection.Owner.UserID == slh.Member.UserID)
     ((CellRendererText) cell).Text = "Owner";
    else
     ((CellRendererText) cell).Text = "Member";
   }
   else if( (slh != null) && (slh.Subscription != null) )
    ((CellRendererText) cell).Text = slh.Subscription.SubscriptionState.ToString();
   else
    ((CellRendererText) cell).Text = "";
  }


  private void AccessCellTextDataFunc (Gtk.TreeViewColumn tree_column,
    Gtk.CellRenderer cell, Gtk.TreeModel tree_model,
    Gtk.TreeIter iter)
  {
   Access.Rights rights = 0;

   SharingListHolder slh = (SharingListHolder)
    ContactTreeStore.GetValue(iter,0);
   if(slh.Member != null)
   {
    rights = slh.Member.Rights;
   }
   else if(slh.Subscription != null)
   {
    rights = slh.Subscription.SubscriptionRights;
   }

   switch(rights)
   {
    case Access.Rights.Deny:
     ((CellRendererText) cell).Text = "No Access";
     break;
    case Access.Rights.ReadOnly:
     ((CellRendererText) cell).Text = "Read Only";
     break;
    case Access.Rights.ReadWrite:
     ((CellRendererText) cell).Text = "Read / Write";
     break;
    case Access.Rights.Admin:
     ((CellRendererText) cell).Text = "Full Control";
     break;
   }
  }

  private void on_selection_changed(object o, EventArgs args)
  {
   AcceptButton.Sensitive = false;
   DeclineButton.Sensitive = false;

   TreeSelection tSelect = ContactTreeView.Selection;
   if(tSelect.CountSelectedRows() == 1)
   {
    TreeModel tModel;
    TreeIter iter;

    tSelect.GetSelected(out tModel, out iter);
    if(tModel != null)
     tModel = null;
    SharingListHolder slh = (SharingListHolder)
     ContactTreeStore.GetValue(iter,0);



    Member curMember = collection.GetCurrentMember();
    Access.Rights curRights = 0;
    if(slh.Member != null)
    {
     curRights = slh.Member.Rights;
    }
    else if(slh.Subscription != null)
    {
     curRights = slh.Subscription.SubscriptionRights;
     if(slh.Subscription.SubscriptionState ==
       SubscriptionStates.Pending)
     {
      AcceptButton.Sensitive = true;
      DeclineButton.Sensitive = true;
     }
    }

    if( (collection.IsShareable(curMember) != true) ||
     ( (slh.Member != null) &&
     (slh.Member.UserID == curMember.UserID) ) )
    {
     removeSharingButton.Sensitive = false;
     FullControlRB.Sensitive = false;
     ReadWriteRB.Sensitive = false;
     ReadOnlyRB.Sensitive = false;
    }
    else
    {
     removeSharingButton.Sensitive = true;
     FullControlRB.Sensitive = true;
     ReadWriteRB.Sensitive = true;
     ReadOnlyRB.Sensitive = true;
    }

    switch(curRights)
    {
     case Access.Rights.Deny:
      break;
     case Access.Rights.ReadOnly:
      ReadOnlyRB.Active = true;
      break;
     case Access.Rights.ReadWrite:
      ReadWriteRB.Active = true;
      break;
     case Access.Rights.Admin:
      FullControlRB.Active = true;
      break;
    }
   }
  }

  private void on_add_sharing(object o, EventArgs args)
  {
   ContactPicker cp = new ContactPicker();
   cp.TransientFor = (Gtk.Window) SharingVBox.Toplevel;
   cp.AddrBookManager = abMan;
   if(cp.Run() == -5)
   {
    foreach(Contact c in cp.Contacts)
    {
     ShareWithContact(c);
    }
   }
  }

  public void ShareWithContact(Contact c)
  {

   if( (!collection.Domain.Equals(Domain.WorkGroupDomainID)) &&
    ( (c.UserID == null) || (c.UserID.Length == 0) ) )
   {
    MessageDialog md = new MessageDialog(
     (Gtk.Window) SharingVBox.Toplevel,
     DialogFlags.DestroyWithParent | DialogFlags.Modal,
     MessageType.Error,
     ButtonsType.Close,
     "Unable to share with the following incomplete user:\n\n" + c.FN);
    md.Run();
    md.Hide();
    return;
   }

   try
   {
    Subscription sub = pobox.CreateSubscription(collection,
          collection.GetCurrentMember(),
          "iFolder");

    sub.SubscriptionRights = Access.Rights.ReadWrite;
    sub.ToName = c.FN;

    if(collection.Domain.Equals(Domain.WorkGroupDomainID))
    {


     sub.FromAddress = String.Format("{0}@{1}",
          System.Environment.UserName,
          Simias.MyDns.GetHostName());
     sub.ToAddress = c.EMail;
     sub.ToIdentity = c.UserID;
    }
    else
    {
     sub.ToIdentity = c.UserID;
    }

    pobox.AddMessage(sub);

    SharingListHolder slh = new SharingListHolder(null, sub);

    ContactTreeStore.AppendValues(slh);
   }
   catch(Exception e)
   {
    Console.WriteLine(e);
   }
  }

  private void SetCurrentAccessRights(Access.Rights rights)
  {
   TreeSelection tSelect = ContactTreeView.Selection;
   if(tSelect.CountSelectedRows() == 1)
   {
    TreeModel tModel;
    TreeIter iter;

    tSelect.GetSelected(out tModel, out iter);

    SharingListHolder slh = (SharingListHolder)
     tModel.GetValue(iter,0);

    if(slh.Member != null)
    {
     slh.Member.Rights = rights;
     collection.Commit(slh.Member);
    }
    else if(slh.Subscription != null)
    {
     slh.Subscription.SubscriptionRights = rights;
     pobox.Commit(slh.Subscription);
    }
   }
  }

  private void on_remove_sharing(object o, EventArgs args)
  {
   TreeSelection tSelect = ContactTreeView.Selection;
   if(tSelect.CountSelectedRows() == 1)
   {
    TreeModel tModel;
    TreeIter iter;

    tSelect.GetSelected(out tModel, out iter);

    SharingListHolder slh = (SharingListHolder)
     tModel.GetValue(iter,0);

    if(slh.Member != null)
    {
     guidList.Remove(slh.Member.UserID);
     collection.Commit(collection.Delete(slh.Member));

    }
    else if(slh.Subscription != null)
    {
     pobox.Commit(pobox.Delete(slh.Subscription));
    }

    ContactTreeStore.Remove(ref iter);

    removeSharingButton.Sensitive = false;
    FullControlRB.Sensitive = false;
    ReadWriteRB.Sensitive = false;
    ReadOnlyRB.Sensitive = false;
   }
  }

  private void on_readwrite_clicked(object o, EventArgs args)
  {
   SetCurrentAccessRights(Access.Rights.ReadWrite);
  }

  private void on_readonly_clicked(object o, EventArgs args)
  {
   SetCurrentAccessRights(Access.Rights.ReadOnly);
  }

  private void on_fullcontrol_clicked(object o, EventArgs args)
  {
   SetCurrentAccessRights(Access.Rights.Admin);
  }

  private void on_decline_clicked(object o, EventArgs args)
  {
   TreeSelection tSelect = ContactTreeView.Selection;
   if(tSelect.CountSelectedRows() == 1)
   {
    TreeModel tModel;
    TreeIter iter;

    tSelect.GetSelected(out tModel, out iter);
    if(tModel != null)
     tModel = null;
    SharingListHolder slh = (SharingListHolder)
     ContactTreeStore.GetValue(iter,0);

    if(slh.Subscription != null)
    {
     slh.Subscription.Decline();
     pobox.Commit(slh.Subscription);
    }
   }
   AcceptButton.Sensitive = false;
   DeclineButton.Sensitive = false;
  }

  private void on_accept_clicked(object o, EventArgs args)
  {
   TreeSelection tSelect = ContactTreeView.Selection;
   if(tSelect.CountSelectedRows() == 1)
   {
    TreeModel tModel;
    TreeIter iter;

    tSelect.GetSelected(out tModel, out iter);
    if(tModel != null)
     tModel = null;
    SharingListHolder slh = (SharingListHolder)
     ContactTreeStore.GetValue(iter,0);

    if(slh.Subscription != null)
    {
     slh.Subscription.Accept(collection.StoreReference,
       slh.Subscription.SubscriptionRights);
     pobox.Commit(slh.Subscription);
    }
   }
   AcceptButton.Sensitive = false;
   DeclineButton.Sensitive = false;
  }
 }


 public class SharingListHolder
 {
  private Member member;
  private Subscription sub;

  public SharingListHolder( Member member, Subscription sub)
  {
   this.member = member;
   this.sub = sub;
  }

  public Member Member
  {
   get
   {
    return(member);
   }

   set
   {
    this.member = value;
   }
  }

  public Subscription Subscription
  {
   get
   {
    return sub;
   }
  }
 }
}
