

using System;
using System.Collections;
using Gtk;

using Novell.iFolder.Controller;

namespace Novell.iFolder
{
 public class AddAccountWizard : Window
 {
  private Gnome.Druid AccountDruid;
  private Gnome.DruidPageEdge IntroductoryPage;
  private Gnome.DruidPageStandard ServerInformationPage;
  private Gnome.DruidPageStandard UserInformationPage;
  private DruidConnectPage ConnectPage;
  private Gnome.DruidPageEdge SummaryPage;
  private DomainController domainController;
  private SimiasWebService simws;
  private bool ControlKeyPressed;
  private Button ForwardButton;
  private Button FinishButton;

  private Gdk.Pixbuf AddAccountPixbuf;




  private Entry ServerNameEntry;
  private Label MakeDefaultLabel;
  private CheckButton DefaultServerCheckButton;




  private Entry UserNameEntry;
  private Entry PasswordEntry;
  private CheckButton RememberPasswordCheckButton;




  private Label ServerNameVerifyLabel;
  private Label UserNameVerifyLabel;
  private Label RememberPasswordVerifyLabel;
  private Label MakeDefaultPromptLabel;
  private Label MakeDefaultVerifyLabel;




  DomainInformation ConnectedDomain;




  iFolderWaitDialog WaitDialog;

  public AddAccountWizard(SimiasWebService simws) : base(WindowType.Toplevel)
  {
   this.Title = Util.GS("iFolder Account Assistant");
   this.Resizable = false;
   this.Modal = true;
   this.WindowPosition = Gtk.WindowPosition.Center;

   this.Icon = new Gdk.Pixbuf(Util.ImagesPath("ifolder24.png"));

   this.simws = simws;

   domainController = DomainController.GetDomainController();

   ConnectedDomain = null;

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

   AddAccountPixbuf = new Gdk.Pixbuf(Util.ImagesPath("add-account.png"));
   AddAccountPixbuf = AddAccountPixbuf.ScaleSimple(48, 48, Gdk.InterpType.Bilinear);

   AccountDruid = new Gnome.Druid();
   vbox.PackStart(AccountDruid, true, true, 0);

   AccountDruid.ShowHelp = true;
   AccountDruid.Help += new EventHandler(OnAccountWizardHelp);

   AccountDruid.AppendPage(CreateIntroductoryPage());
   AccountDruid.AppendPage(CreateServerInformationPage());
   AccountDruid.AppendPage(CreateUserInformationPage());
   AccountDruid.AppendPage(CreateConnectPage());
   AccountDruid.AppendPage(CreateSummaryPage());

   return vbox;
  }




  private Gnome.DruidPage CreateIntroductoryPage()
  {
   IntroductoryPage = new Gnome.DruidPageEdge(Gnome.EdgePosition.Start,
    true,
    Util.GS("Configure an iFolder Account"),
    Util.GS("Welcome to the iFolder Account Assistant.\n\nClick \"Forward\" to begin."),
    AddAccountPixbuf, null, null);

   IntroductoryPage.CancelClicked +=
    new Gnome.CancelClickedHandler(OnCancelClicked);

   IntroductoryPage.Prepared +=
    new Gnome.PreparedHandler(OnIntroductoryPagePrepared);

   return IntroductoryPage;
  }




  private Gnome.DruidPage CreateServerInformationPage()
  {
   ServerInformationPage =
    new Gnome.DruidPageStandard(
     Util.GS("iFolder Server"),
     AddAccountPixbuf,
     null);

   ServerInformationPage.CancelClicked +=
    new Gnome.CancelClickedHandler(OnCancelClicked);

   ServerInformationPage.Prepared +=
    new Gnome.PreparedHandler(OnServerInformationPagePrepared);




   Table table = new Table(4, 3, false);
   ServerInformationPage.VBox.PackStart(table, true, true, 0);
   table.ColumnSpacing = 6;
   table.RowSpacing = 6;
   table.BorderWidth = 12;


   Label l = new Label(Util.GS("Enter the name of your iFolder Server (for example, \"ifolder.example.net\")."));
   table.Attach(l, 0,3, 0,1,
    AttachOptions.Fill | AttachOptions.Expand, 0,0,0);
   l.LineWrap = true;
   l.Xalign = 0.0F;


   table.Attach(new Label(""), 0,1, 1,2,
    AttachOptions.Fill, 0,12,0);
   l = new Label(Util.GS("Server _Address:"));
   table.Attach(l, 1,2, 1,2,
    AttachOptions.Shrink | AttachOptions.Fill, 0,0,0);
   l.Xalign = 0.0F;
   ServerNameEntry = new Entry();
   table.Attach(ServerNameEntry, 2,3, 1,2,
    AttachOptions.Fill | AttachOptions.Expand, 0,0,0);
   l.MnemonicWidget = ServerNameEntry;
   ServerNameEntry.Changed += new EventHandler(UpdateServerInformationPageSensitivity);
   ServerNameEntry.KeyPressEvent
    += new KeyPressEventHandler(OnServerNameEntryKeyPress);


   MakeDefaultLabel = new Label(Util.GS("Setting this iFolder Server as your default server will allow iFolder to automatically select this server when adding new folders."));
   table.Attach(MakeDefaultLabel, 0,3, 2,3,
    AttachOptions.Fill | AttachOptions.Expand, 0,0,0);
   MakeDefaultLabel.LineWrap = true;
   MakeDefaultLabel.Xalign = 0.0F;


   DefaultServerCheckButton = new CheckButton(Util.GS("Make this my _default server"));
   table.Attach(DefaultServerCheckButton, 1,3, 3,4,
    AttachOptions.Fill | AttachOptions.Expand, 0,0,0);

   return ServerInformationPage;
  }




  private Gnome.DruidPage CreateUserInformationPage()
  {
   UserInformationPage =
    new Gnome.DruidPageStandard(
     Util.GS("Identity"),
     AddAccountPixbuf,
     null);

   UserInformationPage.CancelClicked +=
    new Gnome.CancelClickedHandler(OnCancelClicked);

   UserInformationPage.Prepared +=
    new Gnome.PreparedHandler(OnUserInformationPagePrepared);




   Table table = new Table(4, 3, false);
   UserInformationPage.VBox.PackStart(table, false, false, 0);
   table.ColumnSpacing = 6;
   table.RowSpacing = 6;
   table.BorderWidth = 12;


   Label l = new Label(Util.GS("Enter your iFolder user name and password (for example, \"jsmith\")."));
   table.Attach(l, 0,3, 0,1,
    AttachOptions.Fill | AttachOptions.Expand, 0,0,0);
   l.LineWrap = true;
   l.Xalign = 0.0F;


   table.Attach(new Label(""), 0,1, 1,2,
    AttachOptions.Fill, 0,12,0);
   l = new Label(Util.GS("_User Name:"));
   table.Attach(l, 1,2, 1,2,
    AttachOptions.Shrink | AttachOptions.Fill, 0,0,0);
   l.Xalign = 0.0F;
   UserNameEntry = new Entry();
   table.Attach(UserNameEntry, 2,3, 1,2,
    AttachOptions.Fill | AttachOptions.Expand, 0,0,0);
   l.MnemonicWidget = UserNameEntry;
   UserNameEntry.Changed += new EventHandler(UpdateUserInformationPageSensitivity);


   l = new Label(Util.GS("_Password:"));
   table.Attach(l, 1,2, 2,3,
    AttachOptions.Shrink | AttachOptions.Fill, 0,0,0);
   l.Xalign = 0.0F;
   PasswordEntry = new Entry();
   table.Attach(PasswordEntry, 2,3, 2,3,
    AttachOptions.Fill | AttachOptions.Expand, 0,0,0);
   l.MnemonicWidget = PasswordEntry;
   PasswordEntry.Visibility = false;
   PasswordEntry.Changed += new EventHandler(UpdateUserInformationPageSensitivity);


   RememberPasswordCheckButton = new CheckButton(Util.GS("_Remember my password"));
   table.Attach(RememberPasswordCheckButton, 2,3, 3,4,
    AttachOptions.Fill | AttachOptions.Expand, 0,0,0);

   return UserInformationPage;
  }




  private Gnome.DruidPage CreateConnectPage()
  {
   ConnectPage =
    new DruidConnectPage(
     Util.GS("Verify and Connect"),
     AddAccountPixbuf,
     null);

   ConnectPage.CancelClicked +=
    new Gnome.CancelClickedHandler(OnCancelClicked);

   ConnectPage.ConnectClicked +=
    new ConnectClickedHandler(OnConnectClicked);

   ConnectPage.Prepared +=
    new Gnome.PreparedHandler(OnConnectPagePrepared);




   Table table = new Table(6, 3, false);
   ConnectPage.VBox.PackStart(table, false, false, 0);
   table.ColumnSpacing = 6;
   table.RowSpacing = 6;
   table.BorderWidth = 12;


   Label l = new Label(Util.GS("Please verify that the information you've entered is correct."));
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
   ServerNameVerifyLabel = new Label("");
   table.Attach(ServerNameVerifyLabel, 2,3, 1,2,
    AttachOptions.Fill | AttachOptions.Expand, 0,0,0);
   ServerNameVerifyLabel.Xalign = 0.0F;


   l = new Label(Util.GS("User Name:"));
   table.Attach(l, 1,2, 2,3,
    AttachOptions.Shrink | AttachOptions.Fill, 0,0,0);
   l.Xalign = 0.0F;
   UserNameVerifyLabel = new Label("");
   table.Attach(UserNameVerifyLabel, 2,3, 2,3,
    AttachOptions.Fill | AttachOptions.Expand, 0,0,0);
   UserNameVerifyLabel.Xalign = 0.0F;


   l = new Label(Util.GS("Remember password:"));
   table.Attach(l, 1,2, 3,4,
    AttachOptions.Shrink | AttachOptions.Fill, 0,0,0);
   l.Xalign = 0.0F;
   RememberPasswordVerifyLabel = new Label("");
   table.Attach(RememberPasswordVerifyLabel, 2,3, 3,4,
    AttachOptions.Fill | AttachOptions.Expand, 0,0,0);
   RememberPasswordVerifyLabel.Xalign = 0.0F;


   MakeDefaultPromptLabel = new Label(Util.GS("Make default account:"));
   table.Attach(MakeDefaultPromptLabel, 1,2, 4,5,
    AttachOptions.Shrink | AttachOptions.Fill, 0,0,0);
   MakeDefaultPromptLabel.Xalign = 0.0F;
   MakeDefaultVerifyLabel = new Label("");
   table.Attach(MakeDefaultVerifyLabel, 2,3, 4,5,
    AttachOptions.Fill | AttachOptions.Expand, 0,0,0);
   MakeDefaultVerifyLabel.Xalign = 0.0F;


   l = new Label(
    string.Format(
     "\n\n{0}",
     Util.GS("Click \"Connect\" to validate your connection with the server.")));
   table.Attach(l, 0,3, 5,6,
    AttachOptions.Fill | AttachOptions.Expand, 0,0,0);
   l.LineWrap = true;
   l.Xalign = 0.0F;

   return ConnectPage;
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
   string currentServerName = ServerNameEntry.Text;
   if (currentServerName != null)
   {
    currentServerName = currentServerName.Trim();
    if (currentServerName.Length > 0)
     AccountDruid.SetButtonsSensitive(true, true, true, true);
    else
     AccountDruid.SetButtonsSensitive(true, false, true, true);
   }
   else
    AccountDruid.SetButtonsSensitive(true, false, true, true);
  }

  private void UpdateUserInformationPageSensitivity(object o, EventArgs args)
  {
   string currentUserName = UserNameEntry.Text;
   string currentPassword = PasswordEntry.Text;
   if (currentUserName != null && currentPassword != null)
   {
    currentUserName = currentUserName.Trim();
    currentPassword = currentPassword.Trim();
    if (currentUserName.Length > 0 && currentPassword.Length > 0)
     AccountDruid.SetButtonsSensitive(true, true, true, true);
    else
     AccountDruid.SetButtonsSensitive(true, false, true, true);
   }
   else
    AccountDruid.SetButtonsSensitive(true, false, true, true);
  }




  private void OnAccountWizardHelp(object o, EventArgs args)
  {
   Util.ShowHelp("accounts.html", this);
  }

  private void OnIntroductoryPagePrepared(object o, Gnome.PreparedArgs args)
  {
   this.Title = Util.GS("iFolder Account Assistant");
   AccountDruid.SetButtonsSensitive(false, true, true, true);
  }

  private void OnServerInformationPagePrepared(object o, Gnome.PreparedArgs args)
  {
   this.Title = Util.GS("iFolder Account Assistant - (1 of 3)");
   UpdateServerInformationPageSensitivity(null, null);

   DomainInformation[] domains = domainController.GetDomains();
   if (domains != null && domains.Length > 0)
   {
    MakeDefaultLabel.Visible = true;
    DefaultServerCheckButton.Visible = true;
   }
   else
   {
    DefaultServerCheckButton.Active = true;
    MakeDefaultLabel.Visible = false;
    DefaultServerCheckButton.Visible = false;
   }

   ServerNameEntry.GrabFocus();
  }

  private void OnUserInformationPagePrepared(object o, Gnome.PreparedArgs args)
  {
   this.Title = Util.GS("iFolder Account Assistant - (2 of 3)");
   UpdateUserInformationPageSensitivity(null, null);
   UserNameEntry.GrabFocus();


   ForwardButton.Label = "gtk-go-forward";
  }

  private void OnConnectPagePrepared(object o, Gnome.PreparedArgs args)
  {
   this.Title = Util.GS("iFolder Account Assistant - (3 of 3)");

   ServerNameVerifyLabel.Text = ServerNameEntry.Text;
   UserNameVerifyLabel.Text = UserNameEntry.Text;

   RememberPasswordVerifyLabel.Text =
    RememberPasswordCheckButton.Active ?
     Util.GS("Yes") :
     Util.GS("No");

   MakeDefaultVerifyLabel.Text =
    DefaultServerCheckButton.Active ?
     Util.GS("Yes") :
     Util.GS("No");

   DomainInformation[] domains = domainController.GetDomains();
   if (domains != null && domains.Length > 0)
   {
    MakeDefaultPromptLabel.Visible = true;
    MakeDefaultVerifyLabel.Visible = true;
   }
   else
   {
    MakeDefaultPromptLabel.Visible = false;
    MakeDefaultVerifyLabel.Visible = false;
   }


   ForwardButton.Label = Util.GS("Co_nnect");

  }

  private void OnSummaryPagePrepared(object o, Gnome.PreparedArgs args)
  {
   this.Title = Util.GS("iFolder Account Assistant");

   if (ConnectedDomain != null && ConnectedDomain.Name != null && ConnectedDomain.Host != null)
   {
    SummaryPage.Text =
     string.Format(
      "Congratulations!  You are now connected to:\n\n{0}\n({1})\n\nYou can now add folders to be synchronized to the server.  You may also download folders from the server and have them be synchronized to your computer.\n\nClick \"Finish\" to close this window.",
      ConnectedDomain.Name,
      ConnectedDomain.Host);
   }



   FinishButton.Label = Util.GS("_Finish");

   AccountDruid.SetButtonsSensitive(false, true, false, true);
  }





  private bool OnConnectClicked(object o, EventArgs args)
  {
   string serverName = ServerNameEntry.Text.Trim();
   string userName = UserNameEntry.Text.Trim();
   string password = PasswordEntry.Text;

   if (WaitDialog == null)
   {

    VBox vbox = new VBox(false, 0);
    Image connectingImage = new Image(Util.ImagesPath("ifolder48.png"));
    vbox.PackStart(connectingImage, false, false, 0);
    Label l = new Label("<span size=\"xx-small\">FIXME: This will be\nreplaced with an\nanimated image</span>");
    vbox.PackStart(l);
    l.UseMarkup = true;
    l.LineWrap = true;

    WaitDialog =
     new iFolderWaitDialog(
      this,
      vbox,
      iFolderWaitDialog.ButtonSet.None,
      Util.GS("Connecting..."),
      Util.GS("Connecting..."),
      Util.GS("Please wait while your iFolder account is connecting."));


    WaitDialog.Show();
   }

   AddDomainThread addDomainThread =
    new AddDomainThread(
     domainController,
     serverName,
     userName,
     password,
     RememberPasswordCheckButton.Active,
     DefaultServerCheckButton.Active);

   addDomainThread.Completed +=
    new EventHandler(OnAddDomainCompleted);

   addDomainThread.AddDomain();

   return false;
  }

  private void OnAddDomainCompleted(object o, EventArgs args)
  {
   AddDomainThread addDomainThread = (AddDomainThread)o;
   DomainInformation dom = addDomainThread.Domain;
   Exception e = addDomainThread.Exception;
   if (dom == null && e != null)
   {
    if (e is DomainAccountAlreadyExistsException)
    {
     iFolderMsgDialog dg = new iFolderMsgDialog(
      this,
      iFolderMsgDialog.DialogType.Error,
      iFolderMsgDialog.ButtonSet.Ok,
      "",
      Util.GS("An account already exists"),
      Util.GS("An account for this server already exists on the local machine.  Only one account per server is allowed."));
     dg.Run();
     dg.Hide();
     dg.Destroy();
    }
    else
    {
     iFolderMsgDialog dg2 = new iFolderMsgDialog(
      this,
      iFolderMsgDialog.DialogType.Error,
      iFolderMsgDialog.ButtonSet.Ok,
      "",
      Util.GS("Unable to connect to the iFolder Server"),
      Util.GS("An error was encountered while connecting to the iFolder server.  Please verify the information entered and try again.  If the problem persists, please contact your network administrator."),
      Util.GS(e.Message));
     dg2.Run();
     dg2.Hide();
     dg2.Destroy();
    }

    if (WaitDialog != null)
    {
     WaitDialog.Hide();
     WaitDialog.Destroy();
     WaitDialog = null;
    }
   }

   if (dom == null)
   {
    if (WaitDialog != null)
    {
     WaitDialog.Hide();
     WaitDialog.Destroy();
     WaitDialog = null;
    }

    return;
   }

   switch(dom.StatusCode)
   {
    case StatusCodes.InvalidCertificate:
     if (WaitDialog != null)
     {
      WaitDialog.Hide();
      WaitDialog.Destroy();
      WaitDialog = null;
     }

     string serverName = addDomainThread.ServerName;

     byte[] byteArray = simws.GetCertificate(serverName);
     System.Security.Cryptography.X509Certificates.X509Certificate cert = new System.Security.Cryptography.X509Certificates.X509Certificate(byteArray);

     iFolderMsgDialog dialog = new iFolderMsgDialog(
      this,
      iFolderMsgDialog.DialogType.Question,
      iFolderMsgDialog.ButtonSet.YesNo,
      "",
      Util.GS("Accept the certificate of this server?"),
      string.Format(Util.GS("iFolder is unable to verify \"{0}\" as a trusted server.  You should examine this server's identity certificate carefully."), serverName),
      cert.ToString(true));

     Gdk.Pixbuf certPixbuf = Util.LoadIcon("gnome-mime-application-x-x509-ca-cert", 48);
     if (certPixbuf != null && dialog.Image != null)
      dialog.Image.Pixbuf = certPixbuf;

     int rc = dialog.Run();
     dialog.Hide();
     dialog.Destroy();
     if(rc == -8)
     {
      simws.StoreCertificate(byteArray, serverName);
      OnConnectClicked(o, args);
     }
     break;
    case StatusCodes.Success:
    case StatusCodes.SuccessInGrace:
     if (WaitDialog != null)
     {
      WaitDialog.Hide();
      WaitDialog.Destroy();
      WaitDialog = null;
     }

     string password = addDomainThread.Password;
     bool bRememberPassword = addDomainThread.RememberPassword;

     Status authStatus =
      domainController.AuthenticateDomain(
       dom.ID, password, bRememberPassword);

     if (authStatus != null)
     {
      if (authStatus.statusCode == StatusCodes.Success ||
       authStatus.statusCode == StatusCodes.SuccessInGrace)
      {

       ConnectedDomain = dom;

       AccountDruid.Page = SummaryPage;
       break;
      }
      else
      {
       Util.ShowLoginError(this, authStatus.statusCode);
      }
     }
     else
     {
      Util.ShowLoginError(this, StatusCodes.Unknown);
     }
     break;
    default:
     if (WaitDialog != null)
     {
      WaitDialog.Hide();
      WaitDialog.Destroy();
      WaitDialog = null;
     }


     Util.ShowLoginError(this, dom.StatusCode);
     break;
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

  private void OnServerNameEntryKeyPress(object o, KeyPressEventArgs args)
  {
   args.RetVal = true;

   Console.WriteLine(args.Event.Key);


   switch(args.Event.Key)
   {
    case Gdk.Key.KP_Enter:
    case Gdk.Key.Return:
     if (ForwardButton.Sensitive)
      AccountDruid.Page = UserInformationPage;
     break;
   }
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

  public void CloseDialog()
  {
   this.Hide();
   this.Destroy();
  }
 }





 public class DruidConnectPage : Gnome.DruidPageStandard
 {
  public event ConnectClickedHandler ConnectClicked;

  public DruidConnectPage(string title, Gdk.Pixbuf logo, Gdk.Pixbuf top_watermark)
   : base (title, logo, top_watermark)
  {
  }

  protected override bool OnNextClicked(Widget druid)
  {
   if (ConnectClicked != null)
   {
    if (!ConnectClicked(this, EventArgs.Empty))
     return true;
   }

   return false;
  }
 }




 public delegate bool ConnectClickedHandler(object o, EventArgs args);
}
