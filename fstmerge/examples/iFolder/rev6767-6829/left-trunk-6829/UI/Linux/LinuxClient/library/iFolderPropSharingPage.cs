using System;
using Gtk;
using System.Collections;
using Simias.Client;
namespace Novell.iFolder
{
 public class iFolderPropSharingPage : VBox
 {
  private iFolderWebService ifws;
  private SimiasWebService simws;
  private iFolderWeb ifolder;
  private iFolderTreeView UserTreeView;
  private ListStore UserTreeStore;
  private Gdk.Pixbuf OwnerUserPixbuf;
  private Gdk.Pixbuf NormalUserPixbuf;
  private Gdk.Pixbuf CurrentUserPixbuf;
  private Gtk.Window topLevelWindow;
  private Button AddButton;
  private Button RemoveButton;
  private Button AccessButton;
  private iFolderUserSelector UserSelector;
  private Hashtable curUsers;
  private Hashtable memberFullNames;
  private Hashtable duplicateMembers;
  public iFolderPropSharingPage( Gtk.Window topWindow,
          iFolderWebService iFolderWS,
          SimiasWebService SimiasWS)
   : base()
  {
   this.ifws = iFolderWS;
   this.simws = SimiasWS;
   this.topLevelWindow = topWindow;
   curUsers = new Hashtable();
   memberFullNames = new Hashtable();
   duplicateMembers = new Hashtable();
   InitializeWidgets();
  }
  public void UpdateiFolder(iFolderWeb ifolder)
  {
   this.ifolder = ifolder;
   if( this.ifolder.encryptionAlgorithm != null && this.ifolder.encryptionAlgorithm != "")
    this.AddButton.Sensitive = false;
   RefreshUserList();
   UpdateWidgets();
  }
  private void InitializeWidgets()
  {
   this.Spacing = 10;
   this.BorderWidth = 10;
   UserTreeView = new iFolderTreeView();
   ScrolledWindow sw = new ScrolledWindow();
   sw.ShadowType = Gtk.ShadowType.EtchedIn;
   sw.Add(UserTreeView);
   this.PackStart(sw, true, true, 0);
   UserTreeStore = new ListStore(typeof(iFolderUser));
   UserTreeView.Model = UserTreeStore;
   CellRendererPixbuf mcrp = new CellRendererPixbuf();
   TreeViewColumn UserColumn = new TreeViewColumn();
   UserColumn.PackStart(mcrp, false);
   UserColumn.Spacing = 2;
   UserColumn.SetCellDataFunc(mcrp,
     new TreeCellDataFunc(UserCellPixbufDataFunc));
   CellRendererText mcrt = new CellRendererText();
   UserColumn.PackStart(mcrt, false);
   UserColumn.SetCellDataFunc(mcrt,
     new TreeCellDataFunc(UserCellTextDataFunc));
   UserColumn.Title = Util.GS("User");
   UserTreeView.AppendColumn(UserColumn);
   UserColumn.Resizable = true;
   CellRendererText statecr = new CellRendererText();
   statecr.Xpad = 5;
   TreeViewColumn stateColumn =
   UserTreeView.AppendColumn(Util.GS("Role"),
     statecr,
     new TreeCellDataFunc(StateCellTextDataFunc));
   stateColumn.Resizable = true;
   stateColumn.MinWidth = 150;
   CellRendererText accesscr = new CellRendererText();
   accesscr.Xpad = 5;
   TreeViewColumn accessColumn =
   UserTreeView.AppendColumn(Util.GS("Rights"),
     accesscr,
     new TreeCellDataFunc(AccessCellTextDataFunc));
   accessColumn.Resizable = true;
   UserTreeView.Selection.Mode = SelectionMode.Multiple;
   UserTreeView.Selection.Changed +=
    new EventHandler(OnUserSelectionChanged);
   UserTreeView.ButtonPressEvent += new ButtonPressEventHandler(
      OnUserTreeViewButtonPressed);
   UserTreeView.RowActivated += new RowActivatedHandler(
      OnUserTreeViewRowActivated);
   OwnerUserPixbuf =
    new Gdk.Pixbuf(Util.ImagesPath("ifolder-user-owner16.png"));
   CurrentUserPixbuf =
    new Gdk.Pixbuf(Util.ImagesPath("ifolder-user-current16.png"));
   NormalUserPixbuf =
    new Gdk.Pixbuf(OwnerUserPixbuf.Colorspace, true,
        OwnerUserPixbuf.BitsPerSample,
        OwnerUserPixbuf.Width,
        OwnerUserPixbuf.Height);
   NormalUserPixbuf.Fill(0x00000000);
   HBox buttonBox = new HBox();
   buttonBox.Spacing = 10;
   this.PackStart(buttonBox, false, false, 0);
   HBox leftBox = new HBox();
   leftBox.Spacing = 10;
   buttonBox.PackStart(leftBox, false, false, 0);
   HBox midBox = new HBox();
   midBox.Spacing = 10;
   buttonBox.PackStart(midBox, true, true, 0);
   HBox rightBox = new HBox();
   rightBox.Spacing = 10;
   buttonBox.PackStart(rightBox, false, false, 0);
   AddButton = new Button(Gtk.Stock.Add);
   rightBox.PackStart(AddButton);
   AddButton.Clicked += new EventHandler(OnAddUser);
   RemoveButton = new Button(Gtk.Stock.Remove);
   rightBox.PackStart(RemoveButton);
   RemoveButton.Clicked += new EventHandler(OnRemoveUser);
   AccessButton = new Button(Util.GS("R_ights..."));
   leftBox.PackStart(AccessButton);
   AccessButton.Clicked += new EventHandler(OnAccessClicked);
  }
  private void RefreshUserList()
  {
   curUsers.Clear();
   memberFullNames.Clear();
   duplicateMembers.Clear();
   UserTreeStore.Clear();
      iFolderUser[] userlist = ifws.GetiFolderUsers(ifolder.ID);
   foreach(iFolderUser user in userlist)
   {
    string name = user.FN;
    if (name == null || name.Length <= 0)
     name = user.Name;
    if (name == null || name.Length <= 0)
     continue;
    if (memberFullNames.Contains(name))
    {
     duplicateMembers[name] = 0;
    }
    else
     memberFullNames[name] = 0;
    if(!curUsers.ContainsKey(user.UserID))
    {
     TreeIter iter = UserTreeStore.AppendValues(user);
     curUsers.Add(user.UserID, iter);
    }
   }
  }
  private void UpdateWidgets()
  {
   OnUserSelectionChanged(null, null);
  }
  private void UserCellTextDataFunc (Gtk.TreeViewColumn tree_column,
    Gtk.CellRenderer cell, Gtk.TreeModel tree_model,
    Gtk.TreeIter iter)
  {
   iFolderUser user = (iFolderUser) tree_model.GetValue(iter,0);
   if( (user.FN != null) && (user.FN.Length > 0) )
    if (duplicateMembers.Contains(user.FN))
     ((CellRendererText) cell).Text = string.Format("{0} ({1})", user.FN, user.Name);
    else
     ((CellRendererText) cell).Text = user.FN;
   else
    ((CellRendererText) cell).Text = user.Name;
  }
  private void UserCellPixbufDataFunc(Gtk.TreeViewColumn tree_column,
    Gtk.CellRenderer cell, Gtk.TreeModel tree_model,
    Gtk.TreeIter iter)
  {
   iFolderUser user = (iFolderUser) tree_model.GetValue(iter,0);
   if (user.UserID == ifolder.OwnerID)
    ((CellRendererPixbuf) cell).Pixbuf = OwnerUserPixbuf;
   else if (user.UserID == ifolder.CurrentUserID)
    ((CellRendererPixbuf) cell).Pixbuf = CurrentUserPixbuf;
   else
    ((CellRendererPixbuf) cell).Pixbuf = NormalUserPixbuf;
  }
  private void StateCellTextDataFunc (Gtk.TreeViewColumn tree_column,
    Gtk.CellRenderer cell, Gtk.TreeModel tree_model,
    Gtk.TreeIter iter)
  {
   iFolderUser user = (iFolderUser) tree_model.GetValue(iter,0);
   if(ifolder.OwnerID == user.UserID)
    ((CellRendererText) cell).Text = Util.GS("Owner");
   else if(user.State != "Member")
    ((CellRendererText) cell).Text = Util.GS("Invited User");
   else
    ((CellRendererText) cell).Text = Util.GS("iFolder User");
  }
  private void AccessCellTextDataFunc (Gtk.TreeViewColumn tree_column,
    Gtk.CellRenderer cell, Gtk.TreeModel tree_model,
    Gtk.TreeIter iter)
  {
   iFolderUser user = (iFolderUser) tree_model.GetValue(iter,0);
   ((CellRendererText) cell).Text = GetDisplayRights(user.Rights);
  }
        private void OnAddUser(object o, EventArgs args)
  {
   bool SharingIsDisabled = ifws.GetDisableSharingPolicy(ifolder.CurrentUserID, ifolder.ID, ifolder.OwnerID, ifolder.DomainID);
   if( SharingIsDisabled == true)
   {
    UserSelector = new iFolderUserSelector( topLevelWindow,
              simws,
              ifolder.DomainID);
    if (!Util.RegisterModalWindow(UserSelector))
    {
     try
     {
      Util.CurrentModalWindow.Present();
     }
     catch{}
     UserSelector.Destroy();
     return;
    }
    UserSelector.Response +=
       new ResponseHandler(OnUserSelectorResponse);
    UserSelector.ShowAll();
   }
   else
   {
    iFolderMsgDialog dialog = new iFolderMsgDialog(
     null,
     iFolderMsgDialog.DialogType.Info,
      iFolderMsgDialog.ButtonSet.Ok,
      Util.GS("Policy Violation"),
     Util.GS("Sharing is disabled so this iFolder can not be shared."),Util.GS(" "));
    dialog.Run();
    dialog.Hide();
    dialog.Destroy();
    return;
   }
  }
        private void OnRemoveUser(object o, EventArgs args)
  {
   TreeModel tModel;
   TreeSelection tSelect = UserTreeView.Selection;
   if(tSelect.CountSelectedRows() > 0)
   {
    iFolderMsgDialog dialog = new iFolderMsgDialog(
     topLevelWindow,
     iFolderMsgDialog.DialogType.Question,
     iFolderMsgDialog.ButtonSet.YesNo,
     "",
     Util.GS("Remove the selected users?"),
     Util.GS("This will remove the selected users from this iFolder.  They will no longer be able to synchronize files with this iFolder."));
    int rc = dialog.Run();
    dialog.Hide();
    dialog.Destroy();
    if(rc == -8)
    {
     Queue iterQueue;
     Array treePaths = tSelect.GetSelectedRows(out tModel);
     iterQueue = new Queue();
     foreach(TreePath tPath in treePaths)
     {
      TreeIter iter;
      if(UserTreeStore.GetIter(out iter, tPath))
      {
       iterQueue.Enqueue(iter);
      }
     }
     while(iterQueue.Count > 0)
     {
      TreeIter iter = (TreeIter) iterQueue.Dequeue();
      iFolderUser user =
        (iFolderUser) tModel.GetValue(iter, 0);
      try
      {
           ifws.RemoveiFolderUser(ifolder.ID,
             user.UserID);
       UserTreeStore.Remove(ref iter);
       curUsers.Remove(user.UserID);
      }
      catch(Exception e)
      {
       iFolderExceptionDialog ied =
         new iFolderExceptionDialog(
           topLevelWindow, e);
       ied.Run();
       ied.Hide();
       ied.Destroy();
       ied = null;
      }
     }
    }
   }
  }
        private void OnAccessClicked(object o, EventArgs args)
  {
   TreeModel tModel;
   iFolderAccessDialog accDialog = null;
   string defaultRights = "ReadWrite";
   string userName = null;
   bool allowOwner = false;
   TreeSelection tSelect = UserTreeView.Selection;
   if(tSelect.CountSelectedRows() == 1)
   {
    Array treePaths = tSelect.GetSelectedRows(out tModel);
    foreach(TreePath tPath in treePaths)
    {
     TreeIter iter;
     if(UserTreeStore.GetIter(out iter, tPath))
     {
      iFolderUser user =
        (iFolderUser) tModel.GetValue(iter, 0);
      if (user.FN != null)
       userName = user.FN;
      else
       userName = user.Name;
      defaultRights = user.Rights;
      if( (ifolder.CurrentUserID == ifolder.OwnerID) &&
       (user.State == "Member") )
       allowOwner = true;
     }
     break;
    }
   }
   accDialog = new iFolderAccessDialog(
    topLevelWindow, userName, defaultRights, allowOwner);
   int rc = accDialog.Run();
   accDialog.Hide();
   if(rc == -5)
   {
    string newrights = accDialog.Rights;
    string oldOwnerID;
    Array treePaths = tSelect.GetSelectedRows(out tModel);
    foreach(TreePath tPath in treePaths)
    {
     TreeIter iter;
     if(UserTreeStore.GetIter(out iter, tPath))
     {
      iFolderUser user =
        (iFolderUser) tModel.GetValue(iter,0);
      try
      {
          ifws.SetUserRights( ifolder.ID,
            user.UserID,
            newrights);
       user.Rights = newrights;
       if(accDialog.IsOwner)
       {
        if( ifws.CanOwnerBeChanged(user.UserID,ifolder.DomainID))
        {
         ifws.ChangeOwner( ifolder.ID,
             user.UserID,
             "Admin");
         oldOwnerID = ifolder.OwnerID;
         user.Rights = "Admin";
         ifolder.Owner = user.Name;
         ifolder.OwnerID = user.UserID;
         TreeIter ownIter;
         if(UserTreeStore.GetIterFirst(out ownIter))
         {
          do
          {
           iFolderUser ownUser = (iFolderUser)
           UserTreeStore.GetValue(ownIter,0);
           if(oldOwnerID == ownUser.UserID)
           {
            ownUser.Rights = "Admin";
            tModel.SetValue(ownIter,
                 0, ownUser);
            break;
           }
          }
          while(UserTreeStore.IterNext(ref ownIter));
         }
        }
        else
        {
         iFolderMsgDialog messdialog = new iFolderMsgDialog(
                                             null,
                                             iFolderMsgDialog.DialogType.Error,
                                             iFolderMsgDialog.ButtonSet.Ok,
                                             Util.GS("Policy Violation"),
                                             String.Format(Util.GS("Ownership of the iFolder {0} could not be transferred to {1} as it is violating the limit of iFolders set by the Administrator."),ifolder.Name,user.Name),Util.GS(" "));
                                     messdialog.Run();
                                     messdialog.Hide();
                                     messdialog.Destroy();
        }
       }
       tModel.SetValue(iter, 0, user);
      }
      catch(Exception e)
      {
       iFolderExceptionDialog ied =
         new iFolderExceptionDialog(
           topLevelWindow, e);
       ied.Run();
       ied.Hide();
       ied.Destroy();
       ied = null;
      }
     }
    }
   }
   accDialog.Destroy();
   accDialog = null;
  }
        private void OnUserSelectionChanged(object o, EventArgs args)
  {
   if(ifolder.CurrentUserRights != "Admin")
   {
    AddButton.Sensitive = false;
    RemoveButton.Sensitive = false;
    AccessButton.Sensitive = false;
   }
   else
   {
    if(!ifolder.IsWorkgroup)
    {
     AddButton.Sensitive = true;
    }
    else
    {
     AddButton.Sensitive = false;
    }
    TreeSelection tSelect = UserTreeView.Selection;
    if((tSelect.CountSelectedRows() < 1) ||
     SelectionHasOwnerOrCurrent() )
    {
     RemoveButton.Sensitive = false;
     AccessButton.Sensitive = false;
    }
    else
    {
     RemoveButton.Sensitive = true;
     AccessButton.Sensitive = true;
    }
   }
   if( this.ifolder.encryptionAlgorithm != null && this.ifolder.encryptionAlgorithm != "")
    this.AddButton.Sensitive = false;
  }
  private void OnUserSelectorResponse(object o, ResponseArgs args)
  {
   if(UserSelector != null)
   {
    switch(args.ResponseId)
    {
     case Gtk.ResponseType.Ok:
     {
      foreach(MemberInfo member in UserSelector.SelectedUsers)
      {
       if(!curUsers.ContainsKey(member.UserID))
       {
        try
        {
            iFolderUser newUser =
             ifws.AddAndInviteUser(
              ifolder.ID,
              member.Name,
              member.GivenName,
              member.FamilyName,
              member.UserID,
              null,
              "ReadWrite" );
         TreeIter iter =
          UserTreeStore.AppendValues(newUser);
         if (memberFullNames.Contains(newUser.FN))
         {
          duplicateMembers[newUser.FN] = 0;
         }
         else
          memberFullNames[newUser.FN] = 0;
         curUsers.Add(newUser.UserID, iter);
        }
        catch(Exception e)
        {
         iFolderExceptionDialog ied =
           new iFolderExceptionDialog(
             topLevelWindow, e);
         ied.Run();
         ied.Hide();
         ied.Destroy();
         ied = null;
         break;
        }
       }
      }
      UserSelector.Hide();
      UserSelector.Destroy();
      UserSelector = null;
      break;
     }
     case Gtk.ResponseType.Help:
     {
      Util.ShowHelp("sharewith.html", topLevelWindow);
      break;
     }
     case Gtk.ResponseType.Cancel:
     {
      UserSelector.Hide();
      UserSelector.Destroy();
      UserSelector = null;
      break;
     }
    }
   }
  }
  private string GetDisplayRights(string rights)
  {
   if(rights == "ReadWrite")
    return Util.GS("Read/Write");
   else if(rights == "Admin")
    return Util.GS("Full Control");
   else if(rights == "ReadOnly")
    return Util.GS("Read Only");
   else
    return Util.GS("Unknown");
  }
  public bool SelectionHasOwnerOrCurrent()
  {
   TreeModel tModel;
   TreeSelection tSelect = UserTreeView.Selection;
   Array treePaths = tSelect.GetSelectedRows(out tModel);
   if(tModel != null)
    tModel = null;
   foreach(TreePath tPath in treePaths)
   {
    TreeIter iter;
    if(UserTreeStore.GetIter(out iter, tPath))
    {
     iFolderUser user =
       (iFolderUser) UserTreeStore.GetValue(iter,0);
     if(user.UserID == ifolder.OwnerID)
      return true;
     if(user.UserID == ifolder.CurrentUserID)
      return true;
    }
   }
   return false;
  }
  public void OnUserTreeViewButtonPressed(object obj,
           ButtonPressEventArgs args)
  {
   switch(args.Event.Button)
   {
    case 1:
     break;
    case 2:
     break;
    case 3:
    {
     TreePath tPath = null;
     TreeViewColumn tColumn = null;
     if(UserTreeView.GetPathAtPos( (int)args.Event.X,
             (int)args.Event.Y,
             out tPath,
             out tColumn) == true)
     {
      TreeSelection tSelect = UserTreeView.Selection;
      if (tSelect.CountSelectedRows() > 0)
      {
       Menu rightsMenu = new Menu();
       RadioMenuItem adminItem =
        new RadioMenuItem (Util.GS("Full Control"));
       rightsMenu.Append(adminItem);
       RadioMenuItem rwItem =
        new RadioMenuItem (adminItem.Group,
             Util.GS("Read/Write"));
       rightsMenu.Append(rwItem);
       RadioMenuItem roItem =
        new RadioMenuItem (adminItem.Group,
             Util.GS("Read Only"));
       rightsMenu.Append(roItem);
       if (ifolder.CurrentUserRights != "Admin"
        || SelectionHasOwnerOrCurrent())
       {
        adminItem.Sensitive = false;
        rwItem.Sensitive = false;
        roItem.Sensitive = false;
       }
       TreeIter iter;
       if(UserTreeStore.GetIter(out iter, tPath))
       {
        iFolderUser user = (iFolderUser)
          UserTreeStore.GetValue(iter, 0);
        if(user.Rights == "ReadWrite")
         rwItem.Active = true;
        else if(user.Rights == "Admin")
         adminItem.Active = true;
        else
         roItem.Active = true;
       }
       adminItem.Activated += new EventHandler(
         OnAdminRightsMenu);
       rwItem.Activated += new EventHandler(
         OnRWRightsMenu);
       roItem.Activated += new EventHandler(
         OnRORightsMenu);
       rightsMenu.ShowAll();
       rightsMenu.Popup(null, null, null,
        IntPtr.Zero, 3,
        Gtk.Global.CurrentEventTime);
      }
     }
     break;
    }
   }
  }
  private void OnUserTreeViewRowActivated(object o, RowActivatedArgs args)
  {
   TreeSelection tSelect = UserTreeView.Selection;
   if (tSelect.CountSelectedRows() == 1)
   {
    if (ifolder.CurrentUserRights == "Admin"
     && !SelectionHasOwnerOrCurrent())
    {
     OnAccessClicked(null, null);
    }
   }
  }
        private void OnAdminRightsMenu(object o, EventArgs args)
  {
   SetSelectedUserRights("Admin");
  }
  private void OnRWRightsMenu(object o, EventArgs args)
  {
   SetSelectedUserRights("ReadWrite");
  }
  private void OnRORightsMenu(object o, EventArgs args)
  {
   SetSelectedUserRights("ReadOnly");
  }
  private void SetSelectedUserRights(string rights)
  {
   TreeModel tModel;
   TreeSelection tSelect = UserTreeView.Selection;
   Array treePaths =
     tSelect.GetSelectedRows(out tModel);
   foreach(TreePath tPath in treePaths)
   {
    TreeIter iter;
    if(UserTreeStore.GetIter(out iter, tPath))
    {
     iFolderUser user =
      (iFolderUser) tModel.GetValue(iter, 0);
     try
     {
         ifws.SetUserRights( ifolder.ID,
           user.UserID,
           rights);
      user.Rights = rights;
      tModel.SetValue(iter, 0, user);
     }
     catch(Exception e)
     {
      iFolderExceptionDialog ied =
        new iFolderExceptionDialog(
          topLevelWindow, e);
      ied.Run();
      ied.Hide();
      ied.Destroy();
      ied = null;
     }
    }
   }
  }
 }
}
