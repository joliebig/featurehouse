using System;
using System.IO;
using System.Collections;
using Gtk;
using Novell.iFolder.Controller;
namespace Novell.iFolder
{
 public class MigrationWizard : Window
 {
                enum SecurityState
                {
                        encrypt = 1,
                        enforceEncrypt = 2,
                        SSL = 4,
                        enforceSSL = 8
                }
  private Gnome.Druid AccountDruid;
  private Gnome.DruidPageEdge IntroductoryPage;
  private Gnome.DruidPageStandard MigrationOptionsPage;
  private Gnome.DruidPageStandard UserInformationPage;
  private Gnome.DruidPageStandard RAPage;
  private DruidConnectPage MigratePage;
  private Gnome.DruidPageEdge SummaryPage;
  private DomainController domainController;
  private DomainInformation[] domains;
  private iFolderWebService ifws;
  private SimiasWebService simws;
  private bool ControlKeyPressed;
  private Button ForwardButton;
  private Button FinishButton;
  private string location;
  private string prevLocation;
  private string Uname;
  private bool Prepared;
  private iFolderData ifdata;
  private bool migrationStatus;
  private bool alreadyEncrypted;
  public event MigrationValidateClickedHandler ValidateClicked;
  private Gdk.Pixbuf AddAccountPixbuf;
  private MigrationWindow page;
  private RadioButton deleteFromServer;
  private RadioButton copyToServer;
  private CheckButton copyDir;
  private Button BrowseButton;
  private Entry LocationEntry;
  private ComboBox domainList;
  private RadioButton encryptionCheckButton;
  private RadioButton sslCheckButton;
  private Label ServerAddressLabel;
  private Label Location;
  private Label MigrationOptionLabel;
  private Label ShowSecurityLabel;
  private Label SecurityLabel;
         private iFolderTreeView RATreeView;
         private Entry PassPhraseEntry;
         private Entry PassPhraseVerifyEntry;
         private CheckButton RememberPassPhraseCheckButton;
         private ListStore RATreeStore;
         private bool PassPhraseSet;
  private Label RetypePassPhraseLabel;
  private Label SelectRALabel;
  iFolderWaitDialog WaitDialog;
  public MigrationWizard(string User, string path, iFolderWebService ifws, SimiasWebService simws, MigrationWindow page, bool encrypted) : base(WindowType.Toplevel)
  {
   this.Title = Util.GS("iFolder Migration Assistant");
   this.Resizable = false;
   this.WindowPosition = Gtk.WindowPosition.Center;
   this.location = path;
   prevLocation = "";
   this.ifdata = iFolderData.GetData();
   this.ifws = ifws;
   this.simws = simws;
   this.Uname = User;
   this.page = page;
   this.Icon = new Gdk.Pixbuf(Util.ImagesPath("ifolder16.png"));
   this.alreadyEncrypted = encrypted;
   domainController = DomainController.GetDomainController();
   WaitDialog = null;
   this.Add(CreateWidgets());
   ControlKeyPressed = false;
   KeyPressEvent += new KeyPressEventHandler(KeyPressHandler);
   KeyReleaseEvent += new KeyReleaseEventHandler(KeyReleaseHandler);
   SetUpButtons();
  }
  private Widget CreateWidgets()
  {
   VBox vbox = new VBox(false, 0);
   AddAccountPixbuf = new Gdk.Pixbuf(Util.ImagesPath("ifolder-add-account48.png"));
   AddAccountPixbuf = AddAccountPixbuf.ScaleSimple(48, 48, Gdk.InterpType.Bilinear);
   AccountDruid = new Gnome.Druid();
   vbox.PackStart(AccountDruid, true, true, 0);
   AccountDruid.ShowHelp = true;
   AccountDruid.Help += new EventHandler(OnAccountWizardHelp);
   AccountDruid.AppendPage(CreateIntroductoryPage());
   AccountDruid.AppendPage(CreateMigrationOptionsPage());
   AccountDruid.AppendPage(CreateUserInformationPage());
   AccountDruid.AppendPage(CreateRAPage());
   AccountDruid.AppendPage(CreateMigratePage());
   AccountDruid.AppendPage(CreateSummaryPage());
   return vbox;
  }
  private Gnome.DruidPage CreateIntroductoryPage()
  {
   IntroductoryPage = new Gnome.DruidPageEdge(Gnome.EdgePosition.Start,
    true,
    Util.GS("Migrate the iFolder "),
    Util.GS("Welcome to iFolder Migration Assistant.\nThe next few screens will let you,\nmigrate your data to iFolder 3.x.\n\nClick \"Forward\" to continue."),
    AddAccountPixbuf, null, null);
   IntroductoryPage.CancelClicked +=
    new Gnome.CancelClickedHandler(OnCancelClicked);
   IntroductoryPage.Prepared +=
    new Gnome.PreparedHandler(OnIntroductoryPagePrepared);
   return IntroductoryPage;
  }
  private Gnome.DruidPage CreateMigrationOptionsPage()
  {
   MigrationOptionsPage =
    new Gnome.DruidPageStandard(
     Util.GS("Migration Options"),
     AddAccountPixbuf,
     null);
   MigrationOptionsPage.CancelClicked +=
    new Gnome.CancelClickedHandler(OnCancelClicked);
   MigrationOptionsPage.Prepared +=
    new Gnome.PreparedHandler(OnMigrationOptionsPagePrepared);
   Table table = new Table(5, 6, false);
   MigrationOptionsPage.VBox.PackStart(table, true, true, 0);
   table.ColumnSpacing = 6;
   table.RowSpacing = 6;
   table.BorderWidth = 12;
   Label l = new Label(Util.GS("Select one of the following options")+":");
   table.Attach(l, 0,6, 0,1,
    AttachOptions.Fill | AttachOptions.Expand, 0,0,0);
   l.LineWrap = true;
   l.Xalign = 0.0F;
   table.Attach(new Label(""), 0,1, 1,2,
    AttachOptions.Fill, 0,12,0);
   deleteFromServer = new RadioButton(Util.GS("Migrate the iFolder and disconnect it from 2.x domain"));
   deleteFromServer.Active = true;
   deleteFromServer.Sensitive = true;
   deleteFromServer.Toggled += new EventHandler(OndeleteCheckButtonChanged);
   table.Attach(deleteFromServer, 1,6, 1,2,
    AttachOptions.Shrink | AttachOptions.Fill, 0,0,0);
   table.Attach(new Label(""), 0,1, 2,3,
    AttachOptions.Fill, 0,12,0);
   copyToServer = new RadioButton(deleteFromServer, Util.GS("Create a copy of the iFolder and migrate"));
   copyToServer.Active = false;
   copyToServer.Toggled += new EventHandler(OncopyCheckButtonChanged);
   table.Attach(copyToServer, 1,6, 2,3,
    AttachOptions.Fill | AttachOptions.Expand, 0,0,0);
   table.Attach(new Label(""), 0,1, 3,4,
    AttachOptions.Shrink, 0,12,0);
   table.Attach(new Label(""), 1,2, 3,4, AttachOptions.Fill, 0,3,0);
   copyDir = new CheckButton(Util.GS("Copy the parent folder"));
   copyDir.Sensitive = false;
   table.Attach(copyDir, 2,6, 3,4, AttachOptions.Fill| AttachOptions.Expand, 0,0,0);
   table.Attach(new Label("Location:"), 0,1, 4,5, AttachOptions.Fill, 0,0,0);
   LocationEntry = new Entry();
   LocationEntry.Text = location;
   LocationEntry.Sensitive = false;
   LocationEntry.Changed += new EventHandler( OnTextChanged);
   table.Attach(LocationEntry, 1,5, 4,5, AttachOptions.Fill, 0,0,0);
   BrowseButton = new Button(Util.GS("_Browse"));
   table.Attach(BrowseButton, 5,6, 4,5, AttachOptions.Fill, 0,0,0);
   BrowseButton.Sensitive = false;
   BrowseButton.Clicked += new EventHandler(OnBrowseButtonClicked);
   return MigrationOptionsPage;
  }
        private void OnTextChanged(object o, EventArgs e)
  {
   if(copyToServer.Active == false )
   {
    AccountDruid.SetButtonsSensitive(true, true, true, true);
    return;
   }
   if( LocationEntry.Text != null && LocationEntry.Text != String.Empty)
   {
    AccountDruid.SetButtonsSensitive(true, true, true, true);
   }
   else
   {
    AccountDruid.SetButtonsSensitive(true, false, true, true);
   }
  }
        private void OnBrowseButtonClicked(object o, EventArgs e)
  {
   System.IO.DirectoryInfo source = new System.IO.DirectoryInfo(location);
   string fileName = null;
   if( copyDir.Active )
   {
    fileName = source.Name;
   }
   MigrateLocation NewLoc = new MigrateLocation(this, fileName, ifws );
   NewLoc.TransientFor = this;
   int rc = 0;
   do
   {
    rc = NewLoc.Run();
    NewLoc.Hide();
    if(rc ==(int)ResponseType.Ok)
    {
     string selectedFolder = NewLoc.iFolderPath.Trim();
     LocationEntry.Text = selectedFolder;
     NewLoc.Destroy();
     NewLoc = null;
     break;
    }
   }while( rc == (int)ResponseType.Ok);
  }
        private void OnForwardClicked(object o, EventArgs e)
  {
  }
  private Gnome.DruidPage CreateUserInformationPage()
  {
   UserInformationPage =
    new Gnome.DruidPageStandard(
     Util.GS("Server Information"),
     AddAccountPixbuf,
     null);
   UserInformationPage.CancelClicked +=
    new Gnome.CancelClickedHandler(OnCancelClicked);
   UserInformationPage.Prepared +=
    new Gnome.PreparedHandler(OnUserInformationPagePrepared);
   UserInformationPage.NextClicked += new Gnome.NextClickedHandler(OnUserInfoForwardClicked);
   Table table = new Table(5, 3, false);
   UserInformationPage.VBox.PackStart(table, false, false, 0);
   table.ColumnSpacing = 6;
   table.RowSpacing = 6;
   table.BorderWidth = 12;
   Label l = new Label(Util.GS("Select the server "));
   table.Attach(l, 0,3, 0,1,
    AttachOptions.Fill | AttachOptions.Expand, 0,0,0);
   l.LineWrap = true;
   l.Xalign = 0.0F;
   table.Attach(new Label(""), 0,1, 1,2,
    AttachOptions.Fill, 0,12,0);
   l = new Label(Util.GS("Server Address:"));
   table.Attach(l, 1,2, 1,2,
    AttachOptions.Shrink | AttachOptions.Fill, 0,0,0);
   l.Xalign = 0.0F;
   domainList= ComboBox.NewText();
   table.Attach(domainList, 2,3, 1,2,
    AttachOptions.Fill | AttachOptions.Expand, 0,0,0);
   l = new Label(Util.GS("Select Security options"));
   table.Attach(l, 0,3, 2,3,
    AttachOptions.Shrink | AttachOptions.Fill, 0,0,0);
   l.Xalign = 0.0F;
   encryptionCheckButton = new RadioButton(Util.GS("Encrypted"));
   table.Attach(encryptionCheckButton, 1,3, 3,4,
    AttachOptions.Fill | AttachOptions.Expand, 0,0,0);
   sslCheckButton = new RadioButton(encryptionCheckButton, Util.GS("Regular"));
   table.Attach(sslCheckButton, 1,3, 4,5, AttachOptions.Fill | AttachOptions.Expand, 0,0,0);
   return UserInformationPage;
  }
  private Gnome.DruidPage CreateRAPage()
  {
   RAPage = new Gnome.DruidPageStandard(
    Util.GS("Passphrase"),
    AddAccountPixbuf,
    null);
   RAPage.CancelClicked += new Gnome.CancelClickedHandler(OnCancelClicked);
   RAPage.Prepared +=
    new Gnome.PreparedHandler(OnRAPagePrepared);
   RAPage.NextClicked +=
    new Gnome.NextClickedHandler(OnValidateClicked);
   Table table = new Table(6, 3, false);
   RAPage.VBox.PackStart(table, false, false, 0);
   table.ColumnSpacing = 6;
   table.RowSpacing = 6;
   table.BorderWidth = 12;
   Label l = new Label(Util.GS("Enter the passphrase"));
   table.Attach(l, 0,3, 0,1,
    AttachOptions.Fill | AttachOptions.Expand, 0,0,0);
   l.LineWrap = true;
   l.Xalign = 0.0F;
   table.Attach(new Label(""), 0,1, 1,2,
    AttachOptions.Fill, 0,12,0);
   l = new Label(Util.GS("_Passphrase:"));
   table.Attach(l, 1,2, 1,2,
    AttachOptions.Shrink | AttachOptions.Fill, 0,0,0);
   l.Xalign = 0.0F;
   PassPhraseEntry = new Entry();
   PassPhraseEntry.Visibility = false;
   table.Attach(PassPhraseEntry, 2,3, 1,2,
    AttachOptions.Fill | AttachOptions.Expand, 0,0,0);
   PassPhraseEntry.Changed += new EventHandler(ChangeSensitivity);
   l.MnemonicWidget = PassPhraseEntry;
   RetypePassPhraseLabel = new Label(Util.GS("R_etype the passphrase:"));
   table.Attach(RetypePassPhraseLabel, 1,2, 2,3,
    AttachOptions.Shrink | AttachOptions.Fill, 0,0,0);
   l.Xalign = 0.0F;
   PassPhraseVerifyEntry = new Entry();
   PassPhraseVerifyEntry.Visibility = false;
   table.Attach(PassPhraseVerifyEntry, 2,3, 2,3,
    AttachOptions.Fill | AttachOptions.Expand, 0,0,0);
   PassPhraseVerifyEntry.Changed += new EventHandler(ChangeSensitivity);
   l.MnemonicWidget = PassPhraseVerifyEntry;
   RememberPassPhraseCheckButton = new CheckButton(Util.GS("_Remember the passphrase"));
   table.Attach(RememberPassPhraseCheckButton, 2,3, 3,4,
    AttachOptions.Fill | AttachOptions.Expand, 0,0,0);
   SelectRALabel = new Label(Util.GS("Select the Passphrase Recovery Agent"));
   table.Attach(SelectRALabel, 0,3, 4,5,
    AttachOptions.Fill | AttachOptions.Expand, 0,0,0);
   l.LineWrap = true;
   l.Xalign = 0.0F;
   RATreeView = new iFolderTreeView ();
   ScrolledWindow RAScrolledWindow = new ScrolledWindow();
   RAScrolledWindow.ShadowType = Gtk.ShadowType.None;
   RAScrolledWindow.HscrollbarPolicy = Gtk.PolicyType.Automatic;
   RAScrolledWindow.VscrollbarPolicy = Gtk.PolicyType.Automatic;
   RAScrolledWindow.Add(RATreeView);
   RATreeStore = new ListStore(typeof(string));
   RATreeView.Model = RATreeStore;
   TreeViewColumn raNameColumn = new TreeViewColumn();
   raNameColumn.Title = Util.GS("Passphrase Recovery Agents");
   CellRendererText cr = new CellRendererText();
   cr.Xpad = 5;
   raNameColumn.PackStart(cr, false);
   raNameColumn.SetCellDataFunc(cr,
           new TreeCellDataFunc(RANameCellTextDataFunc));
   raNameColumn.Resizable = true;
   raNameColumn.MinWidth = 250;
   RATreeView.AppendColumn(raNameColumn);
   RATreeView.Selection.Mode = SelectionMode.Single;
    table.Attach(RAScrolledWindow, 0,3, 5,7,
     AttachOptions.Expand | AttachOptions.Fill, 0,0,0);
   return RAPage;
  }
  private void RANameCellTextDataFunc (Gtk.TreeViewColumn tree_column,
    Gtk.CellRenderer cell, Gtk.TreeModel tree_model,
    Gtk.TreeIter iter)
  {
   string value = (string) tree_model.GetValue(iter, 0);
   ((CellRendererText) cell).Text = value;
  }
        private void OnValidateClicked(object o, EventArgs args)
  {
   bool NextPage = true;
   string publicKey = null;
      try {
          if ( !PassPhraseSet )
   {
           if (PassPhraseEntry.Text == PassPhraseVerifyEntry.Text)
    {
     string recoveryAgentName = "";
     TreeSelection tSelect = RATreeView.Selection;
     if(tSelect != null && tSelect.CountSelectedRows() == 1)
     {
      TreeModel tModel;
      TreeIter iter;
      tSelect.GetSelected(out tModel, out iter);
      recoveryAgentName = (string) tModel.GetValue(iter, 0);
      if(recoveryAgentName == Util.GS("None"))
       recoveryAgentName = null;
     }
     if( recoveryAgentName != null && recoveryAgentName != Util.GS("None"))
     {
      byte [] RACertificateObj = domainController.GetRACertificate((domains[domainList.Active]).ID, recoveryAgentName);
      if( RACertificateObj != null && RACertificateObj.Length != 0)
      {
       System.Security.Cryptography.X509Certificates.X509Certificate Cert = new System.Security.Cryptography.X509Certificates.X509Certificate(RACertificateObj);
       CertificateDialog dlg = new CertificateDialog(Cert.ToString(true));
       int res = dlg.Run();
       dlg.Hide();
       dlg.Destroy();
       dlg = null;
       if( res == (int)ResponseType.Ok)
       {
        publicKey = Convert.ToBase64String(Cert.GetPublicKey());
        Debug.PrintLine(String.Format(" The public key is: {0}", publicKey));
       }
       else
       {
        Debug.PrintLine("Response type is not ok");
                                    simws.StorePassPhrase((domains[domainList.Active]).ID, "", CredentialType.None, false);
        NextPage = false;
       }
      }
     }
     if( NextPage)
     {
             Status passPhraseStatus = simws.SetPassPhrase ((domains[domainList.Active]).ID, PassPhraseEntry.Text, recoveryAgentName, publicKey);
      if(passPhraseStatus.statusCode == StatusCodes.Success)
      {
       simws.StorePassPhrase( (domains[domainList.Active]).ID, PassPhraseEntry.Text,
        CredentialType.Basic, RememberPassPhraseCheckButton.Active);
      }
      else
      {
             iFolderMsgDialog dialog = new iFolderMsgDialog(
                                                 null,
                                                 iFolderMsgDialog.DialogType.Error,
                                                 iFolderMsgDialog.ButtonSet.None,
                                                 Util.GS("Error setting the passphrase"),
                                                 Util.GS("Unable to change the Passphrase"),
                                                  Util.GS("Please try again"));
                                         dialog.Run();
                                         dialog.Hide();
                                         dialog.Destroy();
                                         dialog = null;
       NextPage = false;
      }
     }
    } else {
           iFolderMsgDialog dialog = new iFolderMsgDialog(
                                               null,
                                               iFolderMsgDialog.DialogType.Error,
                                               iFolderMsgDialog.ButtonSet.None,
                                               Util.GS("Passphrase mismatch"),
                                               Util.GS("The passphrase and retyped passphrase are not same"),
                                               Util.GS("Please enter the passphrase again"));
                                       dialog.Run();
                                       dialog.Hide();
                                       dialog.Destroy();
                                       dialog = null;
     NextPage = false;
    }
   } else {
           Status validationStatus = domainController.ValidatePassPhrase ((domains[domainList.Active]).ID, PassPhraseEntry.Text );
    if (validationStatus.statusCode == StatusCodes.PassPhraseInvalid )
    {
     NextPage = false;
           iFolderMsgDialog dialog = new iFolderMsgDialog(
                                               null,
                                               iFolderMsgDialog.DialogType.Error,
                                               iFolderMsgDialog.ButtonSet.None,
                                               Util.GS("PassPhrase Invalid"),
                                               Util.GS("The PassPhrase entered is not valid"),
                                               Util.GS("Please enter the passphrase again"));
                                       dialog.Run();
                                       dialog.Hide();
                                       dialog.Destroy();
                                       dialog = null;
    }
    else if(validationStatus.statusCode == StatusCodes.Success )
                                 domainController.StorePassPhrase( (domains[domainList.Active]).ID, PassPhraseEntry.Text,
          CredentialType.Basic, RememberPassPhraseCheckButton.Active);
   }
      }
   catch (Exception ex)
       {
    iFolderMsgDialog dialog = new iFolderMsgDialog(
                                                  null,
                                                  iFolderMsgDialog.DialogType.Error,
                                                  iFolderMsgDialog.ButtonSet.None,
                                                  Util.GS("Unable to set the passphrase"),
                                                  Util.GS(ex.Message),
                                                  Util.GS("Please enter the passphrase again"));
    dialog.Run();
    dialog.Hide();
    dialog.Destroy();
    dialog = null;
    NextPage = false;
      }
   if( NextPage == false)
   {
    Debug.PrintLine("In the same page");
    AccountDruid.Page = UserInformationPage;
   }
  }
  private Gnome.DruidPage CreateMigratePage()
  {
   MigratePage = new DruidConnectPage(Util.GS("Verify and Migrate"),AddAccountPixbuf,null);
   MigratePage.CancelClicked +=
    new Gnome.CancelClickedHandler(OnCancelClicked);
   MigratePage.ConnectClicked +=
    new ConnectClickedHandler(OnMigrateClicked);
   MigratePage.Prepared +=
    new Gnome.PreparedHandler(OnMigratePagePrepared);
   MigratePage.BackClicked += new Gnome.BackClickedHandler(OnBackButtonClicked);
   Table table = new Table(6, 3, false);
   MigratePage.VBox.PackStart(table, false, false, 0);
   table.ColumnSpacing = 6;
   table.RowSpacing = 6;
   table.BorderWidth = 12;
   Label l = new Label(Util.GS("Please verify that the information you've entered is correct")+".");
   table.Attach(l, 0,3, 0,1,
    AttachOptions.Fill | AttachOptions.Expand, 0,0,0);
   l.LineWrap = true;
   l.Xalign = 0.0F;
   table.Attach(new Label(""), 0,1, 1,2,
    AttachOptions.Fill, 0,12,0);
   l = new Label(Util.GS("Server Address:"));
   table.Attach(l, 1,2, 1,2,
    AttachOptions.Shrink | AttachOptions.Fill, 0,0,0);
   l.Xalign = 0.0F;
   ServerAddressLabel = new Label("");
   table.Attach(ServerAddressLabel, 2,3, 1,2,
    AttachOptions.Fill | AttachOptions.Expand, 0,0,0);
   ServerAddressLabel.Xalign = 0.0F;
   l = new Label(Util.GS("Location:"));
   table.Attach(l, 1,2, 2,3,
    AttachOptions.Shrink | AttachOptions.Fill, 0,0,0);
   l.Xalign = 0.0F;
   Location = new Label("");
   table.Attach(Location, 2,3, 2,3,
    AttachOptions.Fill | AttachOptions.Expand, 0,0,0);
   Location.Xalign = 0.0F;
   l = new Label(Util.GS("Migration Option")+":");
   table.Attach(l, 1,2, 3,4,
    AttachOptions.Shrink | AttachOptions.Fill, 0,0,0);
   l.Xalign = 0.0F;
   MigrationOptionLabel = new Label("");
   table.Attach(MigrationOptionLabel, 2,3, 3,4,
    AttachOptions.Fill | AttachOptions.Expand, 0,0,0);
   MigrationOptionLabel.Xalign = 0.0F;
   ShowSecurityLabel = new Label(Util.GS("Security")+":");
   table.Attach(ShowSecurityLabel, 1,2, 4,5,
    AttachOptions.Shrink | AttachOptions.Fill, 0,0,0);
   ShowSecurityLabel.Xalign = 0.0F;
   SecurityLabel = new Label("");
   table.Attach(SecurityLabel, 2,3, 4,5,
    AttachOptions.Fill | AttachOptions.Expand, 0,0,0);
   SecurityLabel.Xalign = 0.0F;
   l = new Label(
    string.Format(
     "\n\n{0}",
     Util.GS("Click \"Migrate\" to migrate your folder to the server specified")+"."));
   table.Attach(l, 0,3, 5,6,
    AttachOptions.Fill | AttachOptions.Expand, 0,0,0);
   l.LineWrap = true;
   l.Xalign = 0.0F;
   return MigratePage;
  }
  private Gnome.DruidPage CreateSummaryPage()
  {
   SummaryPage = new Gnome.DruidPageEdge(Gnome.EdgePosition.Finish,
    true,
    Util.GS("Congratulations!"),
    "",
    AddAccountPixbuf,
    null, null);
   SummaryPage.FinishClicked +=
    new Gnome.FinishClickedHandler(OnFinishClicked);
   SummaryPage.Prepared +=
    new Gnome.PreparedHandler(OnSummaryPagePrepared);
   return SummaryPage;
  }
  private void UpdateServerInformationPageSensitivity(object o, EventArgs args)
  {
  }
  private void OnAccountWizardHelp(object o, EventArgs args)
  {
   Util.ShowHelp("migration.html", this);
  }
  private void OnIntroductoryPagePrepared(object o, Gnome.PreparedArgs args)
  {
   this.Title = Util.GS("iFolder Migration Assistant");
   AccountDruid.SetButtonsSensitive(false, true, true, true);
  }
  private void OnMigrationOptionsPagePrepared(object o, Gnome.PreparedArgs args)
  {
   this.Title = Util.GS("iFolder Migration Assistant - (1 of 5)");
  }
                private void OnDomainChangedEvent(System.Object o, EventArgs args)
                {
                        int status = ifws.GetSecurityPolicy((domains[domainList.Active]).ID);
                        ChangeStatus(status);
                }
  private void ChangeStatus(int SecurityPolicy)
  {
   encryptionCheckButton.Active = sslCheckButton.Active = false;
   encryptionCheckButton.Sensitive = sslCheckButton.Sensitive = false;
   if(SecurityPolicy !=0)
   {
    if( (SecurityPolicy & (int)SecurityState.encrypt) == (int) SecurityState.encrypt)
    {
     if( (SecurityPolicy & (int)SecurityState.enforceEncrypt) == (int) SecurityState.enforceEncrypt)
      encryptionCheckButton.Active = true;
     else
     {
      encryptionCheckButton.Sensitive = true;
      sslCheckButton.Sensitive = true;
     }
    }
    else
    {
     sslCheckButton.Active = true;
    }
   }
   else
   {
    sslCheckButton.Active = true;
   }
  }
  private void OnUserInformationPagePrepared(object o, Gnome.PreparedArgs args)
  {
   this.Title = Util.GS("iFolder Migration Assistant - (2 of 5)");
   ForwardButton.Label = "gtk-go-forward";
   if(Prepared)
    return;
   Prepared = true;
   Debug.PrintLine("Preparing UserInformation Page");
   domains = domainController.GetDomains();
   DomainInformation defaultDomain = domainController.GetDefaultDomain();
   string domainID = "";
   if( defaultDomain != null)
    domainID = defaultDomain.ID;
   int defaultDomainID = 0;
   for( int i=0;i<domains.Length;i++)
   {
    domainList.AppendText(domains[i].Name);
    if(domainID == domains[i].ID)
     defaultDomainID = i;
   }
   domainList.Active = defaultDomainID;
   domainList.Changed += new EventHandler(OnDomainChangedEvent);
   ChangeStatus(ifws.GetSecurityPolicy(domains[domainList.Active].ID));
   ForwardButton.Label = "gtk-go-forward";
  }
  private void OnRAPagePrepared(object o, Gnome.PreparedArgs args)
  {
   this.Title = Util.GS("iFolder Migration Assistant - (4 of 5)");
   PassPhraseSet = false;
   if( encryptionCheckButton.Active == false)
   {
    Debug.PrintLine("On rapage prepared. Encryption");
    return;
   }
   Debug.PrintLine("OnRAPagePrepared");
   try
   {
    if ( domainController.IsPassPhraseSet ((domains[domainList.Active]).ID) == false)
    {
           string[] list = domainController.GetRAList ((domains[domainList.Active]).ID);
     PassPhraseVerifyEntry.Show();
           RATreeView.Show();
            SelectRALabel.Show();
            RetypePassPhraseLabel.Show();
           RATreeStore.Clear();
           foreach (string raagent in list )
            RATreeStore.AppendValues (raagent);
     RATreeStore.AppendValues(Util.GS("None"));
    } else {
           PassPhraseSet = true;
            PassPhraseVerifyEntry.Hide ();
           RATreeView.Hide();
            SelectRALabel.Hide();
            RetypePassPhraseLabel.Hide();
    }
    AccountDruid.SetButtonsSensitive(true , true, true, true);
    ForwardButton.Sensitive = false;
   }
   catch(Exception)
   {
    iFolderMsgDialog errorMsg = new iFolderMsgDialog(null,
            iFolderMsgDialog.DialogType.Info,
            iFolderMsgDialog.ButtonSet.Ok,
            Util.GS("Migration"),
            Util.GS("User not Logged-in to the domain"),
            string.Format(Util.GS("For creating an encrypted iFolder you should be connected to the domain.")));
    errorMsg.Run();
    errorMsg.Destroy();
    AccountDruid.Page = MigrationOptionsPage;
   }
  }
  private void OnMigratePagePrepared(object o, Gnome.PreparedArgs args)
  {
   this.Title = Util.GS("iFolder Migration Assistant - (3 of 5)");
   ServerAddressLabel.Text = (domains[domainList.Active]).Name;
   Location.Text = location;
   if(deleteFromServer.Active)
    MigrationOptionLabel.Text = Util.GS("Delete from iFolder2.X server");
   else
    MigrationOptionLabel.Text = Util.GS("Create a copy and migrate to iFolder3.x");
   if(encryptionCheckButton.Active)
   {
    SecurityLabel.Text = Util.GS("Encrypt the iFolder ");
    if(sslCheckButton.Active)
    {
     SecurityLabel.Text += Util.GS("and Use secure channel for data transfer");
    }
   }
   else if(sslCheckButton.Active)
   {
    SecurityLabel.Text = Util.GS("Regular iFolder");
   }
   else
    SecurityLabel.Text = Util.GS("None");
   ForwardButton.Label = Util.GS("Migrate");
  }
  private void OnSummaryPagePrepared(object o, Gnome.PreparedArgs args)
  {
   this.Title = Util.GS("iFolder Migration Assistant");
   if(migrationStatus == true)
   {
    SummaryPage.Text =
     string.Format(
      Util.GS("Congratulations!  Your folder has been\nsuccessfully migrated to \nthe latest version of iFolder.\nIt is recommended to \ndisconnect from 2.x server to \n avoid multiple synchronizations.\n\nClick \"Finish\" to close this window."));
    FinishButton.Label = Util.GS("_Finish");
    AccountDruid.SetButtonsSensitive(false, true, false, true);
   }
   else
   {
    FinishButton.Label = Util.GS("_Finish");
    SummaryPage.Title = Util.GS("Error in Migration");
    SummaryPage.Text = string.Format(Util.GS("Sorry! The iFolder cannot be migrated to the specified account.\n\nPlease try again."));
    AccountDruid.SetButtonsSensitive(false, true, false, false);
   }
  }
 void OnBackButtonClicked(object o, EventArgs args)
 {
   if( encryptionCheckButton.Active == false)
    AccountDruid.Page = RAPage;
   else
   {
    Debug.PrintLine("Checking for passphrase entered at login");
    string passphrasecheck = simws.GetPassPhrase((domains[domainList.Active]).ID);
    if( passphrasecheck != null && passphrasecheck != "")
    {
     Debug.PrintLine(String.Format("some passphrase exists: {0}", passphrasecheck));
     Status passPhraseStatus = simws.ValidatePassPhrase((domains[domainList.Active]).ID, passphrasecheck);
     if( passPhraseStatus != null && passPhraseStatus.statusCode == StatusCodes.Success )
     {
      Debug.PrintLine("Passphrase is validated");
      AccountDruid.Page = RAPage;
     }
     else if( passPhraseStatus == null)
      Debug.PrintLine("Status is null:");
     else if( passPhraseStatus.statusCode == StatusCodes.PassPhraseInvalid)
      Debug.PrintLine("Passphrase is invalid");
     else
      Debug.PrintLine("Some other error");
    }
    else
     Debug.PrintLine("No passphrase exists");
   }
 }
 public static bool CopyDirectory(DirectoryInfo source, DirectoryInfo destination)
 {
  bool success = false;
         if (!destination.Exists)
  {
   try
   {
               destination.Create();
   }
   catch(Exception)
   {
    return success;
   }
         }
  if(!source.Exists)
  {
   return success;
  }
  try
  {
          FileInfo[] files = source.GetFiles();
          foreach (FileInfo file in files)
   {
              file.CopyTo(System.IO.Path.Combine(destination.FullName, file.Name));
          }
  }
  catch(Exception)
  {
   return success;
  }
  try
  {
          DirectoryInfo[] dirs = source.GetDirectories();
          foreach (DirectoryInfo dir in dirs)
   {
              string destinationDir = System.IO.Path.Combine(destination.FullName, dir.Name);
              return CopyDirectory(dir, new DirectoryInfo(destinationDir));
          }
  }
  catch(Exception)
  {
   return success;
  }
  return true;
     }
  private bool OnMigrateClicked(object o, EventArgs args)
  {
   if (WaitDialog == null)
   {
    Console.WriteLine("Inside calling wait dialog");
    VBox vbox = new VBox(false, 0);
    Image connectingImage = new Image(Util.ImagesPath("ifolder-add-account48.png"));
    vbox.PackStart(connectingImage, false, false, 0);
    Debug.PrintLine("Before creating WaitDialog instance");
    WaitDialog =
     new iFolderWaitDialog(
      this,
      vbox,
      iFolderWaitDialog.ButtonSet.None,
      Util.GS("Migrating..."),
      Util.GS("Migrating..."),
      Util.GS("Please wait while your iFolder is being migrated"));
    WaitDialog.Show();
    Console.WriteLine(" Showing wait dialog:");
   }
   bool status = DoMigrate();
   if(WaitDialog != null)
   {
    Debug.PrintLine("Killing the wait dialog");
    WaitDialog.Hide();
    WaitDialog.Destroy();
    WaitDialog =null;
   }
   return status;
  }
  private bool DoMigrate()
  {
   System.IO.DirectoryInfo source = new System.IO.DirectoryInfo(location);
   System.IO.DirectoryInfo destination = new System.IO.DirectoryInfo(LocationEntry.Text);
   string destDir = LocationEntry.Text;
   if( copyDir.Active )
   {
    string fileName;
    fileName = source.Name;
    destDir += "/" + fileName;
    System.IO.DirectoryInfo di = new System.IO.DirectoryInfo(destDir);
    if( !di.Exists)
    {
     try
     {
      di.Create();
     }
     catch(Exception)
     {
      Debug.PrintLine("Unable to create directory for copy option..");
      migrationStatus = false;
     }
    }
    destination = new System.IO.DirectoryInfo(destDir);
   }
   if( ifws.CanBeiFolder(destDir))
   {
    if(copyToServer.Active == true)
    {
     if( !CopyDirectory(source, destination) )
     {
      Debug.PrintLine("Cannot copy the directory from source to destination. Check whether the source is present..");
      migrationStatus = false;
      return true;
     }
    }
    Debug.PrintLine("Migrating:");
    DirectoryInfo d = new DirectoryInfo(location);
    string Algorithm = null;
    Algorithm = (encryptionCheckButton.Active) ? "BlowFish" : null;
    if((d.Exists) && (ifdata.CreateiFolder(destDir, (domains[domainList.Active]).ID, sslCheckButton.Active, Algorithm ) == null))
    {
     Debug.PrintLine("Unable to create iFolder...");
     migrationStatus = false;
    }
    else
    {
     migrationStatus = true;
     Debug.PrintLine("Created successfully");
    }
   }
   else
   {
    Debug.PrintLine("The location selected cannot be ifolder..");
    migrationStatus = false;
   }
   if(migrationStatus)
   {
    if(deleteFromServer.Active == true)
    {
     string str = Mono.Unix.UnixEnvironment.EffectiveUser.HomeDirectory;
     if(System.IO.Directory.Exists(str+"/.novell/ifolder/"+Uname))
      System.IO.Directory.Delete(str+"/.novell/ifolder/"+Uname, true);
     if(System.IO.Directory.Exists(str+"/.novell/ifolder/reg/"+Uname))
      System.IO.Directory.Delete(str+"/.novell/ifolder/reg/"+Uname, true);
     page.RemoveItem();
    }
   }
   return true;
  }
        private void OndeleteCheckButtonChanged(object o, EventArgs e)
  {
   if(deleteFromServer.Active == true)
   {
    prevLocation = LocationEntry.Text;
    LocationEntry.Text = location;
    LocationEntry.Sensitive = false;
    BrowseButton.Sensitive = false;
    copyDir.Sensitive = false;
   }
  }
        private void OncopyCheckButtonChanged(object o, EventArgs e)
  {
   if(copyToServer.Active == true)
   {
    LocationEntry.Sensitive = true;
    BrowseButton.Sensitive = true;
    LocationEntry.Text = prevLocation;
    copyDir.Sensitive = true;
   }
  }
  private void OnUserInfoForwardClicked(object o, EventArgs args)
  {
   Debug.PrintLine("Forward clicked");
   if( encryptionCheckButton.Active == false && alreadyEncrypted == true)
   {
    iFolderMsgDialog dlg = new iFolderMsgDialog( null, iFolderMsgDialog.DialogType.Info, iFolderMsgDialog.ButtonSet.OkCancel, "Caution", "The original 2.x iFolder is encrypted and the migrated folder is chosen not to encrypt", "Do you want to continue?" );
    int result = dlg.Run();
    dlg.Hide();
    dlg.Destroy();
    if( result != (int) ResponseType.Ok)
    {
     AccountDruid.Page = MigrationOptionsPage;
     return;
    }
   }
   if( encryptionCheckButton.Active == false)
    AccountDruid.Page = RAPage;
   else
   {
    Debug.PrintLine("Checking for passphrase entered at login");
    string passphrasecheck = simws.GetPassPhrase((domains[domainList.Active]).ID);
    if( passphrasecheck != null && passphrasecheck != "")
    {
     Debug.PrintLine(String.Format("some passphrase exists: {0}", passphrasecheck));
     Status passPhraseStatus = simws.ValidatePassPhrase((domains[domainList.Active]).ID, passphrasecheck);
     if( passPhraseStatus != null && passPhraseStatus.statusCode == StatusCodes.Success )
     {
      Debug.PrintLine("Passphrase is validated");
      AccountDruid.Page = RAPage;
     }
     else if( passPhraseStatus == null)
      Debug.PrintLine("Status is null:");
     else if( passPhraseStatus.statusCode == StatusCodes.PassPhraseInvalid)
      Debug.PrintLine("Passphrase is invalid");
     else
      Debug.PrintLine("Some other error");
    }
    else
     Debug.PrintLine("No passphrase exists");
   }
  }
  private void OnCancelClicked(object o, Gnome.CancelClickedArgs args)
  {
   CloseDialog();
  }
  private void OnFinishClicked(object o, Gnome.FinishClickedArgs args)
  {
   CloseDialog();
   Util.ShowiFolderWindow();
  }
  void KeyPressHandler(object o, KeyPressEventArgs args)
  {
   args.RetVal = true;
   switch(args.Event.Key)
   {
    case Gdk.Key.Escape:
     CloseDialog();
     break;
    case Gdk.Key.Control_L:
    case Gdk.Key.Control_R:
     ControlKeyPressed = true;
     args.RetVal = false;
     break;
    case Gdk.Key.W:
    case Gdk.Key.w:
     if (ControlKeyPressed)
      CloseDialog();
     else
      args.RetVal = false;
     break;
    default:
     args.RetVal = false;
     break;
   }
  }
  void KeyReleaseHandler(object o, KeyReleaseEventArgs args)
  {
   args.RetVal = false;
   switch(args.Event.Key)
   {
    case Gdk.Key.Control_L:
    case Gdk.Key.Control_R:
     ControlKeyPressed = false;
     break;
    default:
     break;
   }
  }
  private void SetUpButtons()
  {
   AccountDruid.Forall(SetUpButtonsCallback);
  }
  private void SetUpButtonsCallback(Widget w)
  {
   if (w is HButtonBox)
   {
    HButtonBox hButtonBox = w as HButtonBox;
    foreach(Widget buttonWidget in hButtonBox)
    {
     if (buttonWidget is Button)
     {
      Button button = buttonWidget as Button;
      if (button.Label == "gtk-go-forward")
       ForwardButton = button;
      else if (button.Label == "gtk-apply")
       FinishButton = button;
     }
    }
   }
  }
  private void ChangeSensitivity( object o, EventArgs args)
  {
   if( PassPhraseEntry.Text == PassPhraseVerifyEntry.Text)
    ForwardButton.Sensitive = true;
   else if(PassPhraseVerifyEntry.Text == "" && PassPhraseEntry.Text.Length > 0)
   {
    ForwardButton.Sensitive = true;
   }
   else
    ForwardButton.Sensitive = false;
  }
  public void CloseDialog()
  {
   this.Hide();
   this.Destroy();
  }
 }
 public delegate bool MigrationValidateClickedHandler(object o, EventArgs args);
}
