

using System;
using System.IO;
using System.Collections;
using Gtk;
using Simias.Client.Event;
using Simias.Client;
using Simias.Client.Authentication;

using Novell.iFolder.Events;
using Novell.iFolder.Controller;

namespace Novell.iFolder
{




 public class PrefsAccountsPage : VBox
 {
  private Gtk.Window topLevelWindow;
  private SimiasWebService simws;

  private iFolderTreeView AccTreeView;
  private ListStore AccTreeStore;

  private CellRendererToggle onlineToggleButton;

  private Button AddButton;
  private Button RemoveButton;
  private Button DetailsButton;

  private Hashtable curDomains;

  private DomainController domainController;




  private Hashtable detailsDialogs;

  private Manager simiasManager;

  private iFolderWaitDialog WaitDialog;




  public PrefsAccountsPage( Gtk.Window topWindow )
   : base()
  {
   this.topLevelWindow = topWindow;
   this.simiasManager = Util.GetSimiasManager();
   this.simws = new SimiasWebService();
   simws.Url = simiasManager.WebServiceUri.ToString() +
     "/Simias.asmx";
   LocalService.Start(simws, simiasManager.WebServiceUri, simiasManager.DataPath);

   curDomains = new Hashtable();

   InitializeWidgets();

   domainController = DomainController.GetDomainController();
   if (domainController != null)
   {
    domainController.DomainAdded +=
     new DomainAddedEventHandler(OnDomainAddedEvent);
    domainController.DomainDeleted +=
     new DomainDeletedEventHandler(OnDomainDeletedEvent);
    domainController.DomainLoggedIn +=
     new DomainLoggedInEventHandler(OnDomainLoggedInEvent);
    domainController.DomainLoggedOut +=
     new DomainLoggedOutEventHandler(OnDomainLoggedOutEvent);
    domainController.DomainActivated +=
     new DomainActivatedEventHandler(OnDomainActivatedEvent);
    domainController.DomainInactivated +=
     new DomainInactivatedEventHandler(OnDomainInactivatedEvent);
    domainController.NewDefaultDomain +=
     new DomainNewDefaultEventHandler(OnNewDefaultDomainEvent);
    domainController.DomainInGraceLoginPeriod +=
     new DomainInGraceLoginPeriodEventHandler(OnDomainInGraceLoginPeriodEvent);
   }

   detailsDialogs = new Hashtable();

   this.Realized += new EventHandler(OnRealizeWidget);
  }

  ~PrefsAccountsPage()
  {
   if (domainController != null)
   {

    domainController.DomainAdded -=
     new DomainAddedEventHandler(OnDomainAddedEvent);
    domainController.DomainDeleted -=
     new DomainDeletedEventHandler(OnDomainDeletedEvent);
    domainController.DomainLoggedIn -=
     new DomainLoggedInEventHandler(OnDomainLoggedInEvent);
    domainController.DomainLoggedOut -=
     new DomainLoggedOutEventHandler(OnDomainLoggedOutEvent);
    domainController.DomainActivated -=
     new DomainActivatedEventHandler(OnDomainActivatedEvent);
    domainController.DomainInactivated -=
     new DomainInactivatedEventHandler(OnDomainInactivatedEvent);
    domainController.NewDefaultDomain -=
     new DomainNewDefaultEventHandler(OnNewDefaultDomainEvent);
    domainController.DomainInGraceLoginPeriod -=
     new DomainInGraceLoginPeriodEventHandler(OnDomainInGraceLoginPeriodEvent);
   }
  }
  private void InitializeWidgets()
  {
   this.Spacing = 10;
   this.BorderWidth = 10;
   AccTreeView = new iFolderTreeView();
   ScrolledWindow sw = new ScrolledWindow();
   sw.ShadowType = Gtk.ShadowType.EtchedIn;
   sw.Add(AccTreeView);
   this.PackStart(sw, true, true, 0);
   AccTreeStore = new ListStore(typeof(string));
   AccTreeView.Model = AccTreeStore;
   TreeViewColumn onlineColumn = new TreeViewColumn();
   onlineColumn.Title = Util.GS("Online");
   onlineToggleButton = new CellRendererToggle();
   onlineToggleButton.Xpad = 5;
   onlineToggleButton.Xalign = 0.5F;
   onlineColumn.PackStart(onlineToggleButton, true);
   onlineColumn.SetCellDataFunc(onlineToggleButton,
    new TreeCellDataFunc(OnlineCellToggleDataFunc));
   onlineToggleButton.Toggled += new ToggledHandler(OnlineToggled);
   AccTreeView.AppendColumn(onlineColumn);
   TreeViewColumn serverColumn = new TreeViewColumn();
   serverColumn.Title = Util.GS("Server Name");
   CellRendererText servercr = new CellRendererText();
   servercr.Xpad = 5;
   serverColumn.PackStart(servercr, false);
   serverColumn.SetCellDataFunc(servercr,
           new TreeCellDataFunc(ServerCellTextDataFunc));
   serverColumn.Resizable = true;
   serverColumn.MinWidth = 150;
   AccTreeView.AppendColumn(serverColumn);
   TreeViewColumn nameColumn = new TreeViewColumn();
   nameColumn.Title = Util.GS("User Name");
   CellRendererText ncrt = new CellRendererText();
   nameColumn.PackStart(ncrt, false);
   nameColumn.SetCellDataFunc(ncrt,
            new TreeCellDataFunc(NameCellTextDataFunc));
   nameColumn.Resizable = true;
   nameColumn.MinWidth = 150;
   AccTreeView.AppendColumn(nameColumn);
   AccTreeView.Selection.Mode = SelectionMode.Single;
   AccTreeView.Selection.Changed +=
    new EventHandler(AccSelectionChangedHandler);
   HButtonBox buttonBox = new HButtonBox();
   buttonBox.Spacing = 10;
   buttonBox.Layout = ButtonBoxStyle.End;
   this.PackStart(buttonBox, false, false, 0);
   AddButton = new Button(Gtk.Stock.Add);
   buttonBox.PackStart(AddButton);
   AddButton.Clicked += new EventHandler(OnAddAccount);
   RemoveButton = new Button(Gtk.Stock.Remove);
   buttonBox.PackStart(RemoveButton);
   RemoveButton.Clicked += new EventHandler(OnRemoveAccount);
   DetailsButton = new Button(Gtk.Stock.Properties);
   buttonBox.PackStart(DetailsButton);
   DetailsButton.Clicked += new EventHandler(OnDetailsClicked);
   AccTreeView.RowActivated += new RowActivatedHandler(
      OnAccTreeRowActivated);
  }
  private void PopulateWidgets()
  {
   PopulateDomains();
   UpdateWidgetSensitivity();
  }
  private void PopulateDomains()
  {
   DomainInformation[] domains = domainController.GetDomains();
   foreach(DomainInformation dom in domains)
   {
    if(dom.IsSlave)
    {
     TreeIter iter = AccTreeStore.AppendValues(dom.ID);
     curDomains[dom.ID] = iter;
    }
   }
  }
  private void OnRealizeWidget(object o, EventArgs args)
  {
   PopulateWidgets();
  }
  private void OnlineCellToggleDataFunc(Gtk.TreeViewColumn tree_column,
    Gtk.CellRenderer cell, Gtk.TreeModel tree_model,
    Gtk.TreeIter iter)
  {
   string domainID = (string) tree_model.GetValue(iter, 0);
   DomainInformation dom = domainController.GetDomain(domainID);
   if (dom != null && dom.Authenticated)
    ((CellRendererToggle) cell).Active = true;
   else
    ((CellRendererToggle) cell).Active = false;
  }
  private void ServerCellTextDataFunc (Gtk.TreeViewColumn tree_column,
    Gtk.CellRenderer cell, Gtk.TreeModel tree_model,
    Gtk.TreeIter iter)
  {
   string domainID = (string) tree_model.GetValue(iter, 0);
   DomainInformation dom = domainController.GetDomain(domainID);
   if (dom != null)
    ((CellRendererText) cell).Text = dom.Name;
   else
    ((CellRendererText) cell).Text = "";
  }
  private void NameCellTextDataFunc (Gtk.TreeViewColumn tree_column,
    Gtk.CellRenderer cell, Gtk.TreeModel tree_model,
    Gtk.TreeIter iter)
  {
   string domainID = (string) tree_model.GetValue(iter, 0);
   DomainInformation dom = domainController.GetDomain(domainID);
   if (dom != null)
    ((CellRendererText) cell).Text = dom.MemberName;
   else
    ((CellRendererText) cell).Text = "";
  }
  private void OnAddAccount(object o, EventArgs args)
  {
   AddAccountWizard aaw = new AddAccountWizard(simws);
   aaw.TransientFor = topLevelWindow;
   if (!Util.RegisterModalWindow(aaw))
   {
    try
    {
     Util.CurrentModalWindow.Present();
    }
    catch{}
    aaw.Destroy();
    return;
   }
   aaw.ShowAll();
  }
  private void OnRemoveAccount(object o, EventArgs args)
  {
   TreeSelection tSelect = AccTreeView.Selection;
   if(tSelect.CountSelectedRows() == 1)
   {
    TreeModel tModel;
    TreeIter iter;
    tSelect.GetSelected(out tModel, out iter);
    string domainID = (string) tModel.GetValue(iter, 0);
    DomainInformation dom = domainController.GetDomain(domainID);
    RemoveAccountDialog rad = new RemoveAccountDialog(dom);
    rad.TransientFor = topLevelWindow;
    int rc = rad.Run();
    rad.Hide();
    if((ResponseType)rc == ResponseType.Yes)
    {
     try
     {
      domainController.RemoveDomain(dom.ID, rad.RemoveiFoldersFromServer);
     }
     catch(Exception e)
     {
      iFolderExceptionDialog ied =
       new iFolderExceptionDialog( topLevelWindow, e);
      ied.Run();
      ied.Hide();
      ied.Destroy();
      rad.Destroy();
      return;
     }
     AddButton.Sensitive = false;
     RemoveButton.Sensitive = false;
     DetailsButton.Sensitive = false;
    }
    rad.Destroy();
   }
  }
  private void OnAccTreeRowActivated(object o, RowActivatedArgs args)
  {
   OnDetailsClicked(o, args);
  }
  private void OnDetailsClicked(object o, EventArgs args)
  {
   TreeSelection tSelect = AccTreeView.Selection;
   if(tSelect.CountSelectedRows() == 1)
   {
    TreeModel tModel;
    TreeIter iter;
    tSelect.GetSelected(out tModel, out iter);
    string domainID = (string) tModel.GetValue(iter, 0);
    DomainInformation dom = domainController.GetDomain(domainID);
    AccountDialog accDialog = null;
    if (detailsDialogs.ContainsKey(domainID))
    {
     accDialog = (AccountDialog) detailsDialogs[domainID];
     accDialog.Present();
    }
    else
    {
     accDialog = new AccountDialog(dom);
     detailsDialogs[domainID] = accDialog;
     accDialog.SetPosition(WindowPosition.Center);
     accDialog.Destroyed +=
       new EventHandler(OnAccountDialogDestroyedEvent);
     accDialog.ShowAll();
    }
   }
  }
  private void OnAccountDialogDestroyedEvent(object o, EventArgs args)
  {
   AccountDialog accDialog = (AccountDialog) o;
   if (accDialog != null)
   {
    string domainID = accDialog.DomainID;
    if (domainID != null && detailsDialogs.ContainsKey(domainID))
    {
     detailsDialogs.Remove(domainID);
    }
   }
  }
  public void AccSelectionChangedHandler(object o, EventArgs args)
  {
   UpdateWidgetSensitivity();
  }
  private void OnlineToggled(object o, ToggledArgs args)
  {
   onlineToggleButton.Activatable = false;
   TreeIter iter;
   TreePath path = new TreePath(args.Path);
   if (AccTreeStore.GetIter(out iter, path))
   {
    string domainID = (string)AccTreeStore.GetValue(iter, 0);
    DomainInformation dom = domainController.GetDomain(domainID);
    if (dom != null)
    {
     if (!dom.Authenticated)
      LoginDomain(dom, iter);
     else
      LogoutDomain(dom, iter);
    }
   }
   onlineToggleButton.Activatable = true;
  }
  private void LoginDomain(DomainInformation dom, TreeIter iter)
  {
   try
   {
    bool bSavePassword = false;
    string password = domainController.GetDomainPassword(dom.ID);
    if (password != null)
     bSavePassword = true;
    else
    {
     iFolderLoginDialog dialog =
      new iFolderLoginDialog(
       dom.ID, dom.Name, dom.MemberName);
     int rc = dialog.Run();
     dialog.Hide();
     if (rc == (int)ResponseType.Ok)
     {
      password = dialog.Password;
      bSavePassword = dialog.ShouldSavePassword;
     }
     else
     {
      dialog.Destroy();
      return;
     }
     dialog.Destroy();
    }
    if (WaitDialog != null)
    {
     WaitDialog.Hide();
     WaitDialog.Destroy();
     WaitDialog = null;
    }
    VBox vbox = new VBox(false, 0);
    Image connectingImage = new Image(Util.ImagesPath("ifolder48.png"));
    vbox.PackStart(connectingImage, false, false, 0);
    Label l = new Label("<span size=\"xx-small\">FIXME: This will be\nreplaced with an\nanimated image</span>");
    vbox.PackStart(l);
    l.UseMarkup = true;
    l.LineWrap = true;
    WaitDialog =
     new iFolderWaitDialog(
      topLevelWindow,
      vbox,
      iFolderWaitDialog.ButtonSet.None,
      Util.GS("Connecting..."),
      Util.GS("Connecting..."),
      Util.GS("Please wait while your iFolder account is connecting."));
    WaitDialog.Show();
    DomainLoginThread domainLoginThread =
     new DomainLoginThread(
      domainController, dom.ID, password, bSavePassword);
    domainLoginThread.Completed +=
     new EventHandler(OnDomainLoginCompleted);
    domainLoginThread.Login();
   }
   catch
   {
    Util.ShowLoginError(topLevelWindow, StatusCodes.Unknown);
    UpdateDomainStatus(dom.ID);
   }
  }
  private void OnDomainLoginCompleted(object o, EventArgs args)
  {
Console.WriteLine("PrefsAccountPage.OnDomainLoginCompleted");
   if (WaitDialog != null)
   {
    WaitDialog.Hide();
    WaitDialog.Destroy();
    WaitDialog = null;
   }
   DomainLoginThread domainLoginThread = (DomainLoginThread)o;
   Status authStatus = domainLoginThread.AuthenticationStatus;
   if (authStatus != null)
   {
    if (authStatus.statusCode == StatusCodes.Success ||
     authStatus.statusCode == StatusCodes.SuccessInGrace)
    {
     UpdateWidgetSensitivity();
    }
    else
    {
     Util.ShowLoginError(topLevelWindow, authStatus.statusCode);
     UpdateDomainStatus(domainLoginThread.DomainID);
    }
   }
   else
   {
    Util.ShowLoginError(topLevelWindow, StatusCodes.Unknown);
    UpdateDomainStatus(domainLoginThread.DomainID);
   }
  }
  private void LogoutDomain(DomainInformation dom, TreeIter iter)
  {
   try
   {
    domainController.LogoutDomain(dom.ID);
    dom.Authenticated = false;
     UpdateDomainStatus(dom.ID);
   }
   catch (Exception ex)
   {
    iFolderMsgDialog dg = new iFolderMsgDialog(
     topLevelWindow,
     iFolderMsgDialog.DialogType.Error,
     iFolderMsgDialog.ButtonSet.Ok,
     "",
     Util.GS("Unable to log out of the iFolder Server"),
     Util.GS("An error was encountered while logging out of the iFolder Server.  If the problem persists, please contact your network administrator."));
    dg.Run();
    dg.Hide();
    dg.Destroy();
   }
  }
  private void UpdateWidgetSensitivity()
  {
   TreeSelection tSelect = AccTreeView.Selection;
   if(tSelect.CountSelectedRows() == 1)
   {
    TreeModel tModel;
    TreeIter iter;
    tSelect.GetSelected(out tModel, out iter);
    string domainID = (string) tModel.GetValue(iter, 0);
    DomainInformation dom = domainController.GetDomain(domainID);
    if (dom == null) return;
    AddButton.Sensitive = true;
    RemoveButton.Sensitive = true;
    DetailsButton.Sensitive = true;
   }
   else
   {
    AddButton.Sensitive = true;
    RemoveButton.Sensitive = false;
    DetailsButton.Sensitive = false;
   }
  }
  public void OnDomainAddedEvent(object sender, DomainEventArgs args)
  {
   if (curDomains.ContainsKey(args.DomainID))
   {
    TreeIter iter = (TreeIter)curDomains[args.DomainID];
    AccTreeStore.SetValue(iter, 0, args.DomainID);
   }
   else
   {
    TreeIter iter = AccTreeStore.AppendValues(args.DomainID);
    curDomains[args.DomainID] = iter;
    TreeSelection tSelect = AccTreeView.Selection;
    if(tSelect != null)
     tSelect.SelectIter(iter);
   }
  }
  public void OnDomainDeletedEvent(object sender, DomainEventArgs args)
  {
   if (curDomains.ContainsKey(args.DomainID))
   {
    TreeIter iter = (TreeIter)curDomains[args.DomainID];
    AccTreeStore.Remove(ref iter);
    curDomains.Remove(args.DomainID);
   }
  }
  public void OnDomainLoggedInEvent(object sender, DomainEventArgs args)
  {
   UpdateDomainStatus(args.DomainID);
  }
  public void OnDomainLoggedOutEvent(object sender, DomainEventArgs args)
  {
   UpdateDomainStatus(args.DomainID);
  }
  public void OnDomainActivatedEvent(object sender, DomainEventArgs args)
  {
   UpdateDomainStatus(args.DomainID);
  }
  public void OnDomainInactivatedEvent(object sender, DomainEventArgs args)
  {
   UpdateDomainStatus(args.DomainID);
  }
  public void OnNewDefaultDomainEvent(object sender, NewDefaultDomainEventArgs args)
  {
   TreeIter iter;
   DomainInformation dom;
   iter = (TreeIter)curDomains[args.NewDomainID];
   dom = domainController.GetDomain(args.NewDomainID);
   if (dom != null)
   {
    AccTreeStore.SetValue(iter, 0, dom.ID);
   }
   UpdateWidgetSensitivity();
  }
  public void OnDomainInGraceLoginPeriodEvent(object sender, DomainInGraceLoginPeriodEventArgs args)
  {
   DomainInformation dom = domainController.GetDomain(args.DomainID);
   iFolderMsgDialog dg =
    new iFolderMsgDialog(
     topLevelWindow,
     iFolderMsgDialog.DialogType.Error,
     iFolderMsgDialog.ButtonSet.Ok,
     dom != null ? dom.Name : "",
     Util.GS("Your password has expired"),
     string.Format(Util.GS("You have {0} grace logins remaining."), args.RemainingGraceLogins));
   dg.Run();
   dg.Hide();
   dg.Destroy();
  }
  public void UpdateDomainStatus(string domainID)
  {
   if (curDomains.ContainsKey(domainID))
   {
    TreeIter iter = (TreeIter)curDomains[domainID];
    DomainInformation dom = domainController.GetDomain(domainID);
    if (dom != null)
    {
     AccTreeStore.SetValue(iter, 0, dom.ID);
    }
    else
    {
     AccTreeStore.Remove(ref iter);
     curDomains.Remove(domainID);
    }
   }
   UpdateWidgetSensitivity();
  }
 }
}
