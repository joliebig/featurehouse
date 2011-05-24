

using System;
using System.IO;
using System.Collections;

using Gtk;
using Simias.Client.Event;
using Simias.Client;
using Simias.Client.Authentication;

using Novell.iFolder.Events;
using Novell.iFolder.Controller;
using Novell.iFolder.DomainProvider;

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




  private Hashtable removedDomains;

  private DomainController domainController;




  private Hashtable detailsDialogs;

  private Manager simiasManager;

  private iFolderWaitDialog WaitDialog;

  private DomainProviderUI domainProviderUI;

  private iFolderLoginDialog LoginDialog;





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

   removedDomains = new Hashtable();

   InitializeWidgets();

   domainProviderUI = DomainProviderUI.GetDomainProviderUI();

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
    string domainID = dom.ID;
    if(dom.IsSlave ||
     domainProviderUI.GetProviderForID(domainID) != null)
    {
     TreeIter iter = AccTreeStore.AppendValues(domainID);
     curDomains[domainID] = iter;
    }
   }
  }
  private void nothing()
  {
   return;
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
   IDomainProviderUI provider = domainProviderUI.GetProviderForID(domainID);
   if (provider != null)
   {
    if (dom.Active)
     ((CellRendererToggle) cell).Active = true;
    else
     ((CellRendererToggle) cell).Active = false;
   }
   else
   {
    if (dom != null && dom.Authenticated)
     ((CellRendererToggle) cell).Active = true;
    else
     ((CellRendererToggle) cell).Active = false;
   }
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
      removedDomains[dom.ID] = dom.ID;
      domainController.RemoveDomain(dom.ID, rad.RemoveiFoldersFromServer);
      RemoveDomain(dom.ID);
     }
     catch(Exception e)
     {
      if (removedDomains.ContainsKey(dom.ID))
       removedDomains.Remove(dom.ID);
      iFolderExceptionDialog ied =
       new iFolderExceptionDialog( topLevelWindow, e);
      ied.Run();
      ied.Hide();
      ied.Destroy();
      rad.Destroy();
      return;
     }
     AddButton.Sensitive = true;
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
     IDomainProviderUI provider = domainProviderUI.GetProviderForID(domainID);
     if (provider != null)
      accDialog = provider.CreateAccountDialog(topLevelWindow, dom);
     else
      accDialog = new EnterpriseAccountDialog(topLevelWindow, dom);
     if (accDialog != null)
     {
      detailsDialogs[domainID] = accDialog;
      accDialog.SetPosition(WindowPosition.Center);
      accDialog.Destroyed +=
        new EventHandler(OnAccountDialogDestroyedEvent);
      accDialog.ShowAll();
     }
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
    IDomainProviderUI provider = domainProviderUI.GetProviderForID(domainID);
    if (provider != null)
    {
     if (dom.Active)
      domainController.InactivateDomain(dom.ID);
     else
      domainController.ActivateDomain(dom.ID);
    }
    else
    {
     if (dom != null)
     {
      if (!dom.Authenticated)
       LoginDomain(dom);
      else
       LogoutDomain(dom);
     }
    }
    UpdateDomainStatus(dom.ID);
   }
   onlineToggleButton.Activatable = true;
  }
  public void LoginDomain(DomainInformation dom)
  {
   try
   {
    LoginDialog =
     new iFolderLoginDialog(dom.ID, dom.Name, dom.MemberName);
    if (!Util.RegisterModalWindow(LoginDialog))
    {
     LoginDialog.Destroy();
     LoginDialog = null;
     return;
    }
    LoginDialog.Response +=
     new ResponseHandler(OnLoginDialogResponse);
    LoginDialog.ShowAll();
    string password = domainController.GetDomainPassword(dom.ID);
    if (password != null)
    {
     LoginDialog.Hide();
     LoginDialog.Password = password;
     LoginDialog.Respond(Gtk.ResponseType.Ok);
    }
   }
   catch
   {
    Util.ShowLoginError(topLevelWindow, StatusCodes.Unknown);
    UpdateDomainStatus(dom.ID);
   }
  }
        private void OnLoginDialogResponse(object o, ResponseArgs args)
  {
   switch (args.ResponseId)
   {
    case Gtk.ResponseType.Ok:
     DomainInformation dom = domainController.GetDomain(LoginDialog.Domain);
     if (WaitDialog != null)
     {
      WaitDialog.Hide();
      WaitDialog.Destroy();
      WaitDialog = null;
     }
     VBox vbox = new VBox(false, 0);
     Image connectingImage = new Image(Util.ImagesPath("ifolder-add-account48.png"));
     vbox.PackStart(connectingImage, false, false, 0);
     WaitDialog =
      new iFolderWaitDialog(
       topLevelWindow,
       vbox,
       iFolderWaitDialog.ButtonSet.None,
       Util.GS("Connecting..."),
       Util.GS("Connecting..."),
       Util.GS("Please wait while your iFolder account is connecting."));
     if (!Util.RegisterModalWindow(WaitDialog))
     {
      try
      {
       Util.CurrentModalWindow.Present();
      }
      catch{}
      WaitDialog.Destroy();
      return;
     }
     WaitDialog.Show();
     DomainLoginThread domainLoginThread =
      new DomainLoginThread(domainController);
     domainLoginThread.Completed +=
      new DomainLoginCompletedHandler(OnDomainLoginCompleted);
     domainLoginThread.Login(dom.ID, LoginDialog.Password, LoginDialog.ShouldSavePassword);
     break;
    case Gtk.ResponseType.Cancel:
    case Gtk.ResponseType.DeleteEvent:
     LoginDialog.Hide();
     LoginDialog.Destroy();
     LoginDialog = null;
     break;
   }
  }
        private void OnDomainLoginCompleted(object o, DomainLoginCompletedArgs args)
  {
   if (WaitDialog != null)
   {
    WaitDialog.Hide();
    WaitDialog.Destroy();
    WaitDialog = null;
   }
   Status authStatus = args.AuthenticationStatus;
   if (authStatus != null)
   {
    switch (authStatus.statusCode)
    {
     case StatusCodes.Success:
     case StatusCodes.SuccessInGrace:
      if (LoginDialog != null)
      {
       LoginDialog.Hide();
       LoginDialog.Destroy();
       LoginDialog = null;
      }
      int result;
      iFolderWebService ifws = DomainController.GetiFolderService();
      int policy = ifws.GetSecurityPolicy(args.DomainID);
      if( policy % 2 == 0)
       break;
      bool passphraseStatus = simws.IsPassPhraseSet(args.DomainID);
      if(passphraseStatus == true)
      {
       bool rememberOption = simws.GetRememberOption(args.DomainID);
       if( rememberOption == false)
       {
        ShowVerifyDialog( args.DomainID, simws);
       }
       else
       {
        Debug.PrintLine(" remember Option true. Checking for passphrase existence");
        string passphrasecheck = simws.GetPassPhrase(args.DomainID);
        if(passphrasecheck == null || passphrasecheck == "")
        {
         Debug.PrintLine("BugBug: Passphrase doesn't exist");
         ShowVerifyDialog( args.DomainID, simws);
        }
       }
      }
      else
      {
       iFolderWindow.ShowEnterPassPhraseDialog(args.DomainID, simws);
      }
      iFolderData ifdata = iFolderData.GetData();
                        ifdata.Refresh();
      UpdateWidgetSensitivity();
      break;
     case StatusCodes.InvalidCertificate:
      DomainInformation dom = domainController.GetDomain(args.DomainID);
      if( authStatus.UserName != null)
      {
       dom.Host = authStatus.UserName;
      }
      byte[] byteArray = simws.GetCertificate(dom.Host);
      System.Security.Cryptography.X509Certificates.X509Certificate cert = new System.Security.Cryptography.X509Certificates.X509Certificate(byteArray);
      iFolderMsgDialog dialog = new iFolderMsgDialog(
       null,
       iFolderMsgDialog.DialogType.Question,
       iFolderMsgDialog.ButtonSet.YesNo,
       "",
       Util.GS("Accept the certificate of this server?"),
       string.Format(Util.GS("iFolder is unable to verify \"{0}\" as a trusted server.  You should examine this server's identity certificate carefully."), dom.Host),
       cert.ToString(true));
      Gdk.Pixbuf certPixbuf = Util.LoadIcon("gnome-mime-application-x-x509-ca-cert", 48);
      if (certPixbuf != null && dialog.Image != null)
       dialog.Image.Pixbuf = certPixbuf;
      int rc = dialog.Run();
      dialog.Hide();
      dialog.Destroy();
      if(rc == -8)
      {
       simws.StoreCertificate(byteArray, dom.Host);
       LoginDialog.Respond(Gtk.ResponseType.Ok);
      }
      else
      {
       LoginDialog.Respond(Gtk.ResponseType.Cancel);
      }
      break;
      case StatusCodes.UserAlreadyMoved:
       LoginDialog.Respond(Gtk.ResponseType.Ok);
       break;
     default:
      Util.ShowLoginError(topLevelWindow, authStatus.statusCode);
      if (LoginDialog != null)
       LoginDialog.Present();
      UpdateDomainStatus(args.DomainID);
      break;
    }
   }
   else
   {
    Util.ShowLoginError(topLevelWindow, StatusCodes.Unknown);
    if (LoginDialog != null)
     LoginDialog.Present();
    UpdateDomainStatus(args.DomainID);
   }
   iFolderWindow ifwin = Util.GetiFolderWindow();
   ifwin.UpdateCurrentServer();
  }
  private bool ShowVerifyDialog(string DomainID, SimiasWebService simws)
  {
   bool status = false;
   int result;
   Status passPhraseStatus= null;
   VerifyPassPhraseDialog vpd = new VerifyPassPhraseDialog();
   if (!Util.RegisterModalWindow(vpd))
   {
    vpd.Destroy();
    vpd = null;
    return false;
   }
   try
   {
   do
   {
    result = vpd.Run();
    vpd.Hide();
    if( result == (int)ResponseType.Cancel || result == (int)ResponseType.DeleteEvent)
     break;
    if( result == (int)ResponseType.Ok)
    {
     passPhraseStatus = simws.ValidatePassPhrase(DomainID, vpd.PassPhrase);
    }
    if( passPhraseStatus != null)
    {
     if( passPhraseStatus.statusCode == StatusCodes.PassPhraseInvalid)
     {
      iFolderMsgDialog dialog = new iFolderMsgDialog(
       null,
       iFolderMsgDialog.DialogType.Error,
       iFolderMsgDialog.ButtonSet.None,
       Util.GS("Invalid passPhrase"),
       Util.GS("The PassPhrase entered is invalid"),
       Util.GS("Please re-enter the passphrase"));
       dialog.Run();
       dialog.Hide();
       dialog.Destroy();
       dialog = null;
      passPhraseStatus = null;
     }
     else if(passPhraseStatus.statusCode == StatusCodes.Success)
      break;
    }
   }while( result != (int)ResponseType.Cancel && result !=(int)ResponseType.DeleteEvent);
   if(result == (int)ResponseType.Cancel || result ==(int)ResponseType.DeleteEvent)
   {
    status = false;
    simws.StorePassPhrase(DomainID, "", CredentialType.None, false);
   }
   else if( passPhraseStatus != null && passPhraseStatus.statusCode == StatusCodes.Success)
   {
    try
    {
     simws.StorePassPhrase( DomainID, vpd.PassPhrase, CredentialType.Basic, vpd.ShouldSavePassPhrase);
     status = true;
    }
    catch(Exception ex)
    {
     return false;
    }
   }
   }
   catch(Exception e)
   {
    return false;
   }
   return status;
  }
  public void LogoutDomain(DomainInformation dom)
  {
   try
   {
    domainController.LogoutDomain(dom.ID);
    dom.Authenticated = false;
     iFolderData ifdata = iFolderData.GetData();
     ifdata.Refresh();
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
   AddButton.Sensitive= true;
   RemoveButton.Sensitive= false;
   DetailsButton.Sensitive= false;
   if(tSelect != null)
   {
    if(tSelect.CountSelectedRows() == 1)
    {
     TreeModel tModel;
     TreeIter iter;
     tSelect.GetSelected(out tModel, out iter);
     string domainID = (string) tModel.GetValue(iter, 0);
     DomainInformation dom = domainController.GetDomain(domainID);
     if (dom == null) return;
     IDomainProviderUI provider = domainProviderUI.GetProviderForID(domainID);
     if (provider != null)
     {
      if (provider.CanDelete)
       RemoveButton.Sensitive = true;
      else
       RemoveButton.Sensitive = false;
      if (provider.HasDetails)
       DetailsButton.Sensitive = true;
      else
       DetailsButton.Sensitive = false;
     }
     else
     {
      RemoveButton.Sensitive = true;
      DetailsButton.Sensitive = true;
     }
     AddButton.Sensitive= true;
    }
   }
  }
  public void OnDomainAddedEvent(object sender, DomainEventArgs args)
  {
   if (removedDomains.ContainsKey(args.DomainID))
    removedDomains.Remove(args.DomainID);
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
  private void RemoveDomain(string domainID)
  {
   if (curDomains.ContainsKey(domainID))
   {
    TreeIter iter = (TreeIter)curDomains[domainID];
    AccTreeStore.Remove(ref iter);
    curDomains.Remove(domainID);
   }
   if (curDomains.Count == 0)
   {
    iFolderWindow ifwin = Util.GetiFolderWindow();
    if (ifwin.Visible)
     ifwin.CloseWindow();
   }
  }
  public void OnDomainDeletedEvent(object sender, DomainEventArgs args)
  {
   if (removedDomains.ContainsKey(args.DomainID))
   {
    removedDomains.Remove(args.DomainID);
    return;
   }
   else
   {
    RemoveDomain(args.DomainID);
   }
  }
  public void OnDomainLoggedInEvent(object sender, DomainEventArgs args)
  {
  }
  public void OnDomainLoggedOutEvent(object sender, DomainEventArgs args)
  {
  }
  public void OnDomainActivatedEvent(object sender, DomainEventArgs args)
  {
  }
  public void OnDomainInactivatedEvent(object sender, DomainEventArgs args)
  {
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
  public void ToggelDomainState(DomainInformation domainInfo, bool login)
  {
   onlineToggleButton.Activatable = false;
   IDomainProviderUI provider = domainProviderUI.GetProviderForID(domainInfo.ID);
   if(provider != null)
   {
    if (domainInfo.Active)
     domainController.InactivateDomain(domainInfo.ID);
    else
     domainController.ActivateDomain(domainInfo.ID);
   }
   else
   {
    if(true == login)
    {
      LoginDomain(domainInfo);
    }
    else
    {
      LogoutDomain(domainInfo);
    }
   }
      UpdateDomainStatus(domainInfo.ID);
   onlineToggleButton.Activatable = true;
  }
 }
}
