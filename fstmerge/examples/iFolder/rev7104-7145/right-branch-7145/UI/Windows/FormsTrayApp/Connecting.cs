using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;
using System.Net;
using System.Threading;
using System.IO;
using Novell.iFolderCom;
using Simias.Client;
using Simias.Client.Authentication;
using Novell.iFolder.Web;
using Novell.Wizard;
namespace Novell.FormsTrayApp
{
 public class Connecting : System.Windows.Forms.Form
 {
  System.Resources.ResourceManager resourceManager = new System.Resources.ResourceManager(typeof(Connecting));
  private System.Windows.Forms.PictureBox pictureBox1;
  private System.Windows.Forms.Label label1;
  private bool first = true;
  private string server;
  private string user;
  private string password;
  private bool defaultServer;
  private bool rememberPassword;
  private bool updatePasswordPreference = false;
  private SimiasWebService simiasWebService;
  private Manager simiasManager;
  private DomainInformation domainInfo = null;
  private Thread connectThread;
  private delegate void DisplayMessageDelegate(string message, string title, string details, MyMessageBoxButtons buttons, MyMessageBoxIcon icon, MyMessageBoxDefaultButton defaultButton);
  private DisplayMessageDelegate displayMessageDelegate;
  private delegate void ConnectDoneDelegate();
  private ConnectDoneDelegate connectDoneDelegate;
  private bool connectResult;
  private DialogResult messageDialogResult;
  protected AutoResetEvent messageEvent = new AutoResetEvent( false );
  private iFolderWebService ifWebService;
        private bool autoAccountEnabled;
        private bool promptForInvalidCert;
        private bool httpsConnect = false;
  private bool CertAcceptedCond1 = false;
  private bool CertAcceptedCond2 = false;
  private ArrayList ServersForCertStore = new ArrayList();
  private System.ComponentModel.Container components = null;
  public Connecting()
  {
   InitializeMigrationComponent();
            this.autoAccountEnabled = false;
  }
  public Connecting( iFolderWebService ifws, SimiasWebService simiasWebService, Manager simiasManager, string server, string user, string password, bool defaultServer, bool rememberPassword ) :
   this( ifws, simiasWebService, simiasManager, password )
  {
            UriBuilder UriSchemeForServer = new UriBuilder(server);
            this.server = UriSchemeForServer.Uri.Authority;
            this.user = user;
   this.defaultServer = defaultServer;
   this.rememberPassword = rememberPassword;
            this.autoAccountEnabled = false;
  }
        public Connecting( iFolderWebService ifws, SimiasWebService simiasWebService, Manager simiasManager, DomainInformation domainInfo, string password, bool rememberPassword ) :
   this( ifws, simiasWebService, simiasManager, domainInfo, password )
  {
   this.updatePasswordPreference = true;
   this.rememberPassword = rememberPassword;
            this.autoAccountEnabled = false;
  }
  public Connecting( iFolderWebService ifws, SimiasWebService simiasWebService, Manager simiasManager, DomainInformation domainInfo, string password ) :
   this( ifws, simiasWebService, simiasManager, password )
  {
   this.domainInfo = domainInfo;
            this.autoAccountEnabled = false;
  }
        public Connecting(iFolderWebService ifws, SimiasWebService simiasWebService, Manager simiasManager, DomainInformation domainInfo) :
   this( ifws, simiasWebService, simiasManager, domainInfo, null )
  {
            this.autoAccountEnabled = false;
  }
        public Connecting(iFolderWebService ifws, SimiasWebService simiasWebService, Manager simiasManager, string serv, string username, string pass, bool defAcct, bool remPass, bool certprompt)
        {
            this.simiasWebService = simiasWebService;
            this.simiasManager = simiasManager;
            this.ifWebService = ifws;
            UriBuilder UriSchemeForServer = new UriBuilder(serv);
            this.server = UriSchemeForServer.Host;
            this.user = username;
            this.defaultServer = defAcct;
            this.rememberPassword = remPass;
            this.password = pass;
            this.autoAccountEnabled = true;
            promptForInvalidCert = certprompt;
            displayMessageDelegate = new DisplayMessageDelegate(displayMessage);
        }
  private Connecting( iFolderWebService ifws, SimiasWebService simiasWebService, Manager simiasManager, string password )
  {
   this.simiasWebService = simiasWebService;
   this.simiasManager = simiasManager;
   this.password = password;
   this.ifWebService = ifws;
   displayMessageDelegate = new DisplayMessageDelegate( displayMessage );
   connectDoneDelegate = new ConnectDoneDelegate( connectDone );
            InitializeComponent();
  }
  protected override void Dispose( bool disposing )
  {
   if( disposing )
   {
    if(components != null)
    {
     components.Dispose();
    }
   }
   base.Dispose( disposing );
  }
  private void InitializeComponent()
  {
   System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(Connecting));
   this.pictureBox1 = new System.Windows.Forms.PictureBox();
   this.label1 = new System.Windows.Forms.Label();
   this.SuspendLayout();
   this.pictureBox1.AccessibleDescription = resources.GetString("pictureBox1.AccessibleDescription");
   this.pictureBox1.AccessibleName = resources.GetString("pictureBox1.AccessibleName");
   this.pictureBox1.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("pictureBox1.Anchor")));
   this.pictureBox1.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("pictureBox1.BackgroundImage")));
   this.pictureBox1.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("pictureBox1.Dock")));
   this.pictureBox1.Enabled = ((bool)(resources.GetObject("pictureBox1.Enabled")));
   this.pictureBox1.Font = ((System.Drawing.Font)(resources.GetObject("pictureBox1.Font")));
   this.pictureBox1.Image = ((System.Drawing.Image)(resources.GetObject("pictureBox1.Image")));
   this.pictureBox1.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("pictureBox1.ImeMode")));
   this.pictureBox1.Location = ((System.Drawing.Point)(resources.GetObject("pictureBox1.Location")));
   this.pictureBox1.Name = "pictureBox1";
   this.pictureBox1.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("pictureBox1.RightToLeft")));
   this.pictureBox1.Size = ((System.Drawing.Size)(resources.GetObject("pictureBox1.Size")));
   this.pictureBox1.SizeMode = ((System.Windows.Forms.PictureBoxSizeMode)(resources.GetObject("pictureBox1.SizeMode")));
   this.pictureBox1.TabIndex = ((int)(resources.GetObject("pictureBox1.TabIndex")));
   this.pictureBox1.TabStop = false;
   this.pictureBox1.Text = resources.GetString("pictureBox1.Text");
   this.pictureBox1.Visible = ((bool)(resources.GetObject("pictureBox1.Visible")));
   this.label1.AccessibleDescription = resources.GetString("label1.AccessibleDescription");
   this.label1.AccessibleName = resources.GetString("label1.AccessibleName");
   this.label1.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("label1.Anchor")));
   this.label1.AutoSize = ((bool)(resources.GetObject("label1.AutoSize")));
   this.label1.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("label1.Dock")));
   this.label1.Enabled = ((bool)(resources.GetObject("label1.Enabled")));
   this.label1.Font = ((System.Drawing.Font)(resources.GetObject("label1.Font")));
   this.label1.Image = ((System.Drawing.Image)(resources.GetObject("label1.Image")));
   this.label1.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label1.ImageAlign")));
   this.label1.ImageIndex = ((int)(resources.GetObject("label1.ImageIndex")));
   this.label1.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("label1.ImeMode")));
   this.label1.Location = ((System.Drawing.Point)(resources.GetObject("label1.Location")));
   this.label1.Name = "label1";
   this.label1.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("label1.RightToLeft")));
   this.label1.Size = ((System.Drawing.Size)(resources.GetObject("label1.Size")));
   this.label1.TabIndex = ((int)(resources.GetObject("label1.TabIndex")));
   this.label1.Text = resources.GetString("label1.Text");
   this.label1.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label1.TextAlign")));
   this.label1.Visible = ((bool)(resources.GetObject("label1.Visible")));
   this.AccessibleDescription = resources.GetString("$this.AccessibleDescription");
   this.AccessibleName = resources.GetString("$this.AccessibleName");
   this.AutoScaleBaseSize = ((System.Drawing.Size)(resources.GetObject("$this.AutoScaleBaseSize")));
   this.AutoScroll = ((bool)(resources.GetObject("$this.AutoScroll")));
   this.AutoScrollMargin = ((System.Drawing.Size)(resources.GetObject("$this.AutoScrollMargin")));
   this.AutoScrollMinSize = ((System.Drawing.Size)(resources.GetObject("$this.AutoScrollMinSize")));
   this.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("$this.BackgroundImage")));
   this.ClientSize = ((System.Drawing.Size)(resources.GetObject("$this.ClientSize")));
   this.ControlBox = false;
   this.Controls.Add(this.label1);
   this.Controls.Add(this.pictureBox1);
   this.Enabled = ((bool)(resources.GetObject("$this.Enabled")));
   this.Font = ((System.Drawing.Font)(resources.GetObject("$this.Font")));
   this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
   this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
   this.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("$this.ImeMode")));
   this.Location = ((System.Drawing.Point)(resources.GetObject("$this.Location")));
   this.MaximizeBox = false;
   this.MaximumSize = ((System.Drawing.Size)(resources.GetObject("$this.MaximumSize")));
   this.MinimizeBox = false;
   this.MinimumSize = ((System.Drawing.Size)(resources.GetObject("$this.MinimumSize")));
   this.Name = "Connecting";
   this.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("$this.RightToLeft")));
   this.ShowInTaskbar = false;
   this.StartPosition = ((System.Windows.Forms.FormStartPosition)(resources.GetObject("$this.StartPosition")));
   this.Text = resources.GetString("$this.Text");
   this.Activated += new System.EventHandler(this.Connecting_Activated);
   this.ResumeLayout(false);
  }
  private void InitializeMigrationComponent()
  {
   System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(Connecting));
   this.AutoScaleBaseSize = ((System.Drawing.Size)(resources.GetObject("$this.AutoScaleBaseSize")));
   this.Enabled = ((bool)(resources.GetObject("$this.Enabled")));
   this.Name = "Migrating";
   this.Activated += new System.EventHandler(this.Connecting_Activated);
   this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
   this.Location = ((System.Drawing.Point)(resources.GetObject("$this.Location")));
   this.MaximizeBox = false;
   this.MaximumSize = ((System.Drawing.Size)(resources.GetObject("$this.MaximumSize")));
   this.MinimizeBox = false;
   this.MinimumSize = ((System.Drawing.Size)(resources.GetObject("$this.MinimumSize")));
   this.Font = ((System.Drawing.Font)(resources.GetObject("$this.Font")));
   this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
   this.Text = resources.GetString("$this.Text");
   this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
   this.ClientSize = ((System.Drawing.Size)(resources.GetObject("$this.ClientSize")));
   this.Text = resources.GetString("Migration.Title");
   this.Cursor = Cursors.WaitCursor;
   this.SuspendLayout();
   this.label1 = new System.Windows.Forms.Label();
   this.label1.AutoSize = ((bool)(resources.GetObject("label1.AutoSize")));
   this.label1.Enabled = ((bool)(resources.GetObject("label1.Enabled")));
   this.label1.Location = ((System.Drawing.Point)(resources.GetObject("label1.Location")));
   this.label1.Visible = ((bool)(resources.GetObject("label1.Visible")));
   this.label1.Text = resources.GetString("Migration.Text");
   this.label1.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("label1.Dock")));
   this.label1.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("label1.RightToLeft")));
   this.label1.Size = ((System.Drawing.Size)(resources.GetObject("label1.Size")));
   this.label1.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label1.TextAlign")));
   this.label1.Visible = ((bool)(resources.GetObject("label1.Visible")));
   this.ControlBox = false;
   this.Controls.Add(this.label1);
   this.ResumeLayout(false);
  }
        private void Connecting_Activated(object sender, System.EventArgs e)
  {
   if ( first )
   {
    first = false;
    connectThread = new Thread( new ThreadStart( connectToServer ) );
                connectThread.Name = "Connection";
                connectThread.CurrentUICulture = Thread.CurrentThread.CurrentUICulture;
    connectThread.IsBackground = true;
    connectThread.Priority = ThreadPriority.BelowNormal;
    connectThread.Start();
   }
  }
  public delegate void EnterpriseConnectDelegate(object sender, DomainConnectEventArgs e);
  public event EnterpriseConnectDelegate EnterpriseConnect;
  private bool authenticate()
  {
   bool result = false;
   try
   {
    DomainAuthentication domainAuth =
     new DomainAuthentication(
     "iFolder",
     domainInfo.ID,
     null);
    Status status = domainAuth.Authenticate(simiasManager.WebServiceUri, simiasManager.DataPath);
    switch (status.statusCode)
    {
     case StatusCodes.InvalidCertificate:
      byte[] byteArray = simiasWebService.GetCertificate(domainInfo.Host);
      System.Security.Cryptography.X509Certificates.X509Certificate cert = new System.Security.Cryptography.X509Certificates.X509Certificate(byteArray);
      BeginInvoke( displayMessageDelegate,
       new object[] { string.Format(resourceManager.GetString("verifyCert"), domainInfo.Host), resourceManager.GetString("verifyCertTitle"), cert.ToString(true), MyMessageBoxButtons.YesNo, MyMessageBoxIcon.Question, MyMessageBoxDefaultButton.Button2 } );
      messageEvent.WaitOne();
      if ( messageDialogResult == DialogResult.Yes )
      {
       simiasWebService.StoreCertificate(byteArray, domainInfo.Host);
       result = authenticate();
      }
      break;
     case StatusCodes.Success:
      result = true;
      break;
     case StatusCodes.SuccessInGrace:
      BeginInvoke( displayMessageDelegate,
       new object[] { string.Format(resourceManager.GetString("graceLogin"), status.RemainingGraceLogins),
            resourceManager.GetString("graceLoginTitle"),
            string.Empty,
            MyMessageBoxButtons.OK,
            MyMessageBoxIcon.Information, MyMessageBoxDefaultButton.Button1 } );
      messageEvent.WaitOne();
      result = true;
      break;
     default:
     {
      string userID;
      CredentialType credType = simiasWebService.GetDomainCredentials(domainInfo.ID, out userID, out password);
      if ((credType == CredentialType.Basic) && (password != null))
      {
       domainAuth = new DomainAuthentication("iFolder", domainInfo.ID, password);
       Status authStatus = domainAuth.Authenticate(simiasManager.WebServiceUri, simiasManager.DataPath);
       switch (authStatus.statusCode)
       {
        case StatusCodes.Success:
         result = true;
         break;
        case StatusCodes.SuccessInGrace:
         BeginInvoke( displayMessageDelegate,
          new object[] { string.Format(resourceManager.GetString("graceLogin"), status.RemainingGraceLogins),
               resourceManager.GetString("graceLoginTitle"),
               string.Empty,
               MyMessageBoxButtons.OK,
               MyMessageBoxIcon.Information, MyMessageBoxDefaultButton.Button1 } );
         messageEvent.WaitOne();
         result = true;
         break;
        case StatusCodes.AccountDisabled:
         BeginInvoke( displayMessageDelegate,
          new object[] { resourceManager.GetString("accountDisabled"), resourceManager.GetString("serverConnectErrorTitle"), string.Empty, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error, MyMessageBoxDefaultButton.Button1 } );
         messageEvent.WaitOne();
         break;
        case StatusCodes.AccountLockout:
         BeginInvoke( displayMessageDelegate,
          new object[] { resourceManager.GetString("accountLockout"), resourceManager.GetString("serverConnectErrorTitle"), string.Empty, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error, MyMessageBoxDefaultButton.Button1 } );
         messageEvent.WaitOne();
         break;
        case StatusCodes.SimiasLoginDisabled:
         BeginInvoke( displayMessageDelegate,
          new object[] { resourceManager.GetString("iFolderAccountDisabled"), resourceManager.GetString("serverConnectErrorTitle"), string.Empty, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error, MyMessageBoxDefaultButton.Button1 } );
         messageEvent.WaitOne();
         break;
        case StatusCodes.UnknownUser:
        case StatusCodes.InvalidPassword:
        case StatusCodes.InvalidCredentials:
         simiasWebService.SetDomainCredentials(domainInfo.ID, null, CredentialType.None);
         BeginInvoke( displayMessageDelegate,
          new object[] { resourceManager.GetString("failedAuth"), resourceManager.GetString("serverConnectErrorTitle"), string.Empty, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error, MyMessageBoxDefaultButton.Button1 } );
         messageEvent.WaitOne();
         break;
       }
      }
      break;
     }
    }
   }
   catch {}
   return result;
  }
  private void connectDone()
  {
   if ( connectResult )
   {
    DialogResult = DialogResult.OK;
                (FormsTrayApp.globalProp()).refreshAll();
   }
   else
   {
    DialogResult = DialogResult.No;
   }
   Close();
  }
  private void connectToServer()
  {
   if ( this.domainInfo != null )
   {
    if ( password == null )
    {
     connectResult = authenticate();
    }
    else
    {
     connectResult = login();
    }
   }
   else
   {
    connectResult = initialConnect();
   }
            if (connectResult)
            {
                try
                {
                    bool serverOld = false;
                    bool res = FormsTrayApp.ClientUpdates(domainInfo.ID, out serverOld);
                    if (res == false)
                    {
                        try
                        {
                            simiasWebService.LeaveDomain(domainInfo.ID, false);
                        }
                        catch { }
                        if (serverOld == false)
                        {
                            if (!autoAccountEnabled)
                            {
                                MessageBox.Show(resourceManager.GetString("UpgradeNeededMsg"), resourceManager.GetString("DisableLoginMsg"), MessageBoxButtons.OK);
                            }
                            else
                            {
                                FormsTrayApp.log.Info(resourceManager.GetString("UpgradeNeededMsg"));
                            }
                            connectResult = false;
                        }
                    }
                }
                catch (Exception ex)
                {
                    FormsTrayApp.log.Info("Error in webservice {0}", ex.ToString());
                }
            }
   if( connectResult)
   {
    bool passPhraseStatus = false;
    bool passphraseStatus = false;
    int policy =0;
    if( this.ifWebService != null )
    {
     policy = this.ifWebService.GetSecurityPolicy(this.domainInfo.ID);
    }
    if( policy %2==0 || this.ifWebService == null)
    {
     BeginInvoke( this.connectDoneDelegate );
     return;
    }
    try
    {
     passphraseStatus = this.simiasWebService.IsPassPhraseSet(this.domainInfo.ID);
    }
    catch(Exception ex)
    {
     MessageBox.Show( resourceManager.GetString("IsPassphraseSetException") + ex.Message);
     BeginInvoke( this.connectDoneDelegate );
     return;
    }
    if(passphraseStatus == true)
    {
     string passphrasecheck = null;
     if( this.simiasWebService.GetRememberOption(this.domainInfo.ID) == true )
     {
      passphrasecheck = this.simiasWebService.GetPassPhrase(this.domainInfo.ID);
     }
     if( passphrasecheck == null || passphrasecheck =="")
     {
      VerifyPassphraseDialog vpd = new VerifyPassphraseDialog(this.domainInfo.ID, this.simiasWebService);
      vpd.ShowDialog();
      passPhraseStatus = vpd.PassphraseStatus;
     }
     else
     {
      passPhraseStatus = true;
     }
    }
    else
    {
     EnterPassphraseDialog enterPassPhrase= new EnterPassphraseDialog(this.domainInfo.ID, this.simiasWebService, this.ifWebService);
     enterPassPhrase.ShowDialog();
     passPhraseStatus = enterPassPhrase.PassphraseStatus;
    }
    if( passPhraseStatus == false)
    {
    }
   }
   BeginInvoke( this.connectDoneDelegate );
  }
  private void displayMessage( string message, string title, string details, MyMessageBoxButtons buttons, MyMessageBoxIcon icon, MyMessageBoxDefaultButton defaultButton )
  {
   MyMessageBox mmb = new MyMessageBox( message, title, details, buttons, icon, defaultButton );
   messageDialogResult = mmb.ShowDialog();
   messageEvent.Set();
  }
        public bool initialConnect()
        {
            bool result = false;
            bool certPrompt = false;
            try
            {
                SetProxyForDomain(server, true);
                domainInfo = simiasWebService.ConnectToDomain(user, password, server);
                switch (domainInfo.StatusCode)
                {
                    case StatusCodes.InvalidCertificate:
                        string serverName = domainInfo.HostUrl != null ? domainInfo.HostUrl : server;
                        byte[] byteArray = simiasWebService.GetCertificate(serverName);
                        System.Security.Cryptography.X509Certificates.X509Certificate cert = new System.Security.Cryptography.X509Certificates.X509Certificate(byteArray);
                        if (!autoAccountEnabled || (autoAccountEnabled && promptForInvalidCert))
                            certPrompt = true;
                        if (certPrompt)
                        {
                            if (!autoAccountEnabled)
                            {
                                BeginInvoke(displayMessageDelegate,
                                    new object[] { string.Format(resourceManager.GetString("verifyCert"), serverName), resourceManager.GetString("verifyCertTitle"), cert.ToString(true), MyMessageBoxButtons.YesNo, MyMessageBoxIcon.Question, MyMessageBoxDefaultButton.Button2 });
                                messageEvent.WaitOne();
                            }
                            else
                            {
                                MyMessageBox mmb = new MyMessageBox(string.Format(resourceManager.GetString("verifyCert"), serverName), resourceManager.GetString("verifyCertTitle"), cert.ToString(true), MyMessageBoxButtons.YesNo, MyMessageBoxIcon.Question, MyMessageBoxDefaultButton.Button2);
                                mmb.StartPosition = FormStartPosition.CenterScreen;
                                messageDialogResult = mmb.ShowDialog();
                            }
                            if (messageDialogResult == DialogResult.Yes)
                            {
                                if (!(serverName.ToLower()).StartsWith(Uri.UriSchemeHttp))
                                {
                                    server = (new Uri(Uri.UriSchemeHttps + Uri.SchemeDelimiter + serverName.TrimEnd(new char[] { '/' }))).ToString();
                                }
                                else
                                {
                                    UriBuilder ub = new UriBuilder(serverName);
                                    ub.Scheme = Uri.UriSchemeHttps;
                                    server = ub.ToString();
                                }
                                simiasWebService.StoreCertificate(byteArray, server);
                                ServersForCertStore.Add(server);
                                result = initialConnect();
                            }
                            else
                            {
                                simiasWebService.RemoveCertFromTable(serverName);
                            }
                        }
                        else
                        {
                            if (!(serverName.ToLower()).StartsWith(Uri.UriSchemeHttp))
                            {
                                server = (new Uri(Uri.UriSchemeHttps + Uri.SchemeDelimiter + serverName.TrimEnd(new char[] { '/' }))).ToString();
                            }
                            else
                            {
                                UriBuilder ub = new UriBuilder(serverName);
                                ub.Scheme = Uri.UriSchemeHttps;
                                server = ub.ToString();
                            }
                            simiasWebService.StoreCertificate(byteArray, server);
                            ServersForCertStore.Add(server);
                            result = initialConnect();
                        }
                        break;
                    case StatusCodes.Success:
                    case StatusCodes.SuccessInGrace:
                        DomainAuthentication domainAuth = new DomainAuthentication("iFolder", domainInfo.ID, password);
                        domainAuth.Authenticate(simiasManager.WebServiceUri, simiasManager.DataPath);
                        domainInfo.Authenticated = true;
                        if (rememberPassword)
                        {
                            try
                            {
                                simiasWebService.SetDomainCredentials(domainInfo.ID, password, CredentialType.Basic);
                            }
                            catch (Exception ex)
                            {
                                if (!autoAccountEnabled)
                                {
                                    BeginInvoke(displayMessageDelegate,
                                        new object[] { resourceManager.GetString("savePasswordError"), string.Empty, ex.Message, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error, MyMessageBoxDefaultButton.Button1 });
                                    messageEvent.WaitOne();
                                }
                                else
                                {
                                    FormsTrayApp.log.Info("Exception is {0}", ex.Message);
                                }
                            }
                        }
                        ServersForCertStore.Clear();
                        if (defaultServer)
                        {
                            try
                            {
                                simiasWebService.SetDefaultDomain(domainInfo.ID);
                                domainInfo.IsDefault = true;
                            }
                            catch (Exception ex)
                            {
                                if (!autoAccountEnabled)
                                {
                                    BeginInvoke(displayMessageDelegate,
                                        new object[] { resourceManager.GetString("setDefaultError"), resourceManager.GetString("accountErrorTitle"), ex.Message, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error, MyMessageBoxDefaultButton.Button1 });
                                    messageEvent.WaitOne();
                                }
                                else
                                {
                                    FormsTrayApp.log.Info("{0} : {1}", resourceManager.GetString("accountErrorTitle"), resourceManager.GetString("setDefaultError"));
                                }
                            }
                        }
                        if (domainInfo.StatusCode.Equals(StatusCodes.SuccessInGrace) && !autoAccountEnabled)
                        {
                            BeginInvoke(displayMessageDelegate,
                                new object[] { string.Format(resourceManager.GetString("graceLogin"), domainInfo.RemainingGraceLogins),
             resourceManager.GetString("graceLoginTitle"),
             string.Empty,
             MyMessageBoxButtons.OK,
             MyMessageBoxIcon.Information, MyMessageBoxDefaultButton.Button1 });
                            messageEvent.WaitOne();
                        }
                        if (EnterpriseConnect != null)
                        {
                            EnterpriseConnect(this, new DomainConnectEventArgs(domainInfo));
                        }
                        result = true;
                        break;
                    case StatusCodes.InvalidCredentials:
                    case StatusCodes.InvalidPassword:
                    case StatusCodes.UnknownUser:
                        string servername = domainInfo.HostUrl != null ? domainInfo.HostUrl : server;
                        if (!autoAccountEnabled)
                        {
                            BeginInvoke(displayMessageDelegate,
                                new object[] { resourceManager.GetString("failedAuth"), resourceManager.GetString("serverConnectErrorTitle"), string.Empty, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error, MyMessageBoxDefaultButton.Button1 });
                            messageEvent.WaitOne();
                        }
                        else
                        {
                            if (password != "")
                            {
                                FormsTrayApp.log.Info("{0}:{1} RetCode: {2}", resourceManager.GetString("serverConnectErrorTitle"), resourceManager.GetString("failedAuth"), domainInfo.StatusCode);
                            }
                        }
                        simiasWebService.RemoveCertFromTable(servername);
                        break;
                    case StatusCodes.AccountDisabled:
                        if (!autoAccountEnabled)
                        {
                            BeginInvoke(displayMessageDelegate,
                                new object[] { resourceManager.GetString("accountDisabled"), resourceManager.GetString("serverConnectErrorTitle"), string.Empty, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error, MyMessageBoxDefaultButton.Button1 });
                            messageEvent.WaitOne();
                        }
                        else
                        {
                            FormsTrayApp.log.Info("{0}:{1}", resourceManager.GetString("serverConnectErrorTitle"), resourceManager.GetString("accountDisabled"));
                        }
                        break;
                    case StatusCodes.AccountLockout:
                        if (!autoAccountEnabled)
                        {
                            BeginInvoke(displayMessageDelegate,
                                new object[] { resourceManager.GetString("accountLockout"), resourceManager.GetString("serverConnectErrorTitle"), string.Empty, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error, MyMessageBoxDefaultButton.Button1 });
                            messageEvent.WaitOne();
                        }
                        else
                        {
                            FormsTrayApp.log.Info("{0}:{1}", resourceManager.GetString("serverConnectErrorTitle"), resourceManager.GetString("accountLockout"));
                        }
                        break;
                    case StatusCodes.SimiasLoginDisabled:
                        if (!autoAccountEnabled)
                        {
                            BeginInvoke(displayMessageDelegate,
                                new object[] { resourceManager.GetString("iFolderAccountDisabled"), resourceManager.GetString("serverConnectErrorTitle"), string.Empty, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error, MyMessageBoxDefaultButton.Button1 });
                            messageEvent.WaitOne();
                        }
                        else
                        {
                            FormsTrayApp.log.Info("{0}:{1}", resourceManager.GetString("serverConnectErrorTitle"), resourceManager.GetString("iFolderAccountDisabled"));
                        }
                        break;
                    case StatusCodes.UnknownDomain:
                        if (!autoAccountEnabled)
                        {
                            BeginInvoke(displayMessageDelegate,
                                new object[] { resourceManager.GetString("unknownDomain"), resourceManager.GetString("serverConnectErrorTitle"), string.Empty, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error, MyMessageBoxDefaultButton.Button1 });
                            messageEvent.WaitOne();
                        }
                        else
                        {
                            FormsTrayApp.log.Info("{0}:{1}", resourceManager.GetString("serverConnectErrorTitle"), resourceManager.GetString("unknownDomain"));
                        }
                        break;
                    default:
                        if (!autoAccountEnabled)
                        {
                            BeginInvoke(displayMessageDelegate,
                                new object[] { resourceManager.GetString("serverConnectError"), resourceManager.GetString("serverConnectErrorTitle"), string.Empty, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error, MyMessageBoxDefaultButton.Button1 });
                            messageEvent.WaitOne();
                        }
                        else
                        {
                            FormsTrayApp.log.Info("{0}:{1}", resourceManager.GetString("serverConnectErrorTitle"), resourceManager.GetString("serverConnectError"));
                        }
                        break;
                }
            }
            catch (Exception ex)
            {
                if (ex.Message.Contains("logging to old server"))
                {
                    MyMessageBox box = new MyMessageBox(resourceManager.GetString("Upgrade.Text"), resourceManager.GetString("Upgrade.Title"), "", MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
                    box.StartPosition = FormStartPosition.CenterScreen;
                    switch (box.ShowDialog())
                    {
                        case DialogResult.OK:
                        case DialogResult.Cancel:
                            return false;
                    }
                }
                if ((ex.Message.IndexOf("Simias.ExistsException") != -1) ||
                    (ex.Message.IndexOf("already exists") != -1))
                {
                    if (!autoAccountEnabled)
                    {
                        BeginInvoke(displayMessageDelegate,
                            new object[] { resourceManager.GetString("alreadyJoined"), resourceManager.GetString("alreadyJoinedTitle"), string.Empty, MyMessageBoxButtons.OK, MyMessageBoxIcon.Information, MyMessageBoxDefaultButton.Button1 });
                        messageEvent.WaitOne();
                    }
                    else
                    {
                        FormsTrayApp.log.Info("{0}:{1}", resourceManager.GetString("alreadyJoinedTitle"), resourceManager.GetString("alreadyJoined"));
                    }
                }
                else if (!httpsConnect && (ex.Message.IndexOf("InvalidOperationException") != -1))
                {
                    if (!(server.ToLower()).StartsWith(Uri.UriSchemeHttp))
                    {
                        server = (new Uri(Uri.UriSchemeHttps + Uri.SchemeDelimiter + server.TrimEnd(new char[] { '/' }))).ToString();
                    }
                    else
                    {
                        UriBuilder ub = new UriBuilder(server);
                        ub.Scheme = Uri.UriSchemeHttps;
                        server = ub.ToString();
                    }
                    httpsConnect = true;
                    result = initialConnect();
                }
                else
                {
                    if (!autoAccountEnabled)
                    {
                        BeginInvoke(displayMessageDelegate,
                            new object[] { resourceManager.GetString("serverConnectError"),
                                resourceManager.GetString("serverConnectErrorTitle"),
                                "",
                                MyMessageBoxButtons.OK,
                                MyMessageBoxIcon.Error,
                                MyMessageBoxDefaultButton.Button1
                        });
                        messageEvent.WaitOne();
                        FormsTrayApp.log.Info("{0}:{1}", resourceManager.GetString("serverConnectError"), ex.Message);
                    }
                    else
                    {
                        FormsTrayApp.log.Info("{0}:{1}", resourceManager.GetString("serverConnectErrorTitle"),
                            resourceManager.GetString("serverConnectError"));
                    }
                }
            }
            return result;
        }
  private bool login()
  {
   bool result = false;
   DomainAuthentication domainAuth = new DomainAuthentication("iFolder", domainInfo.ID, password);
            string HostUrl = domainInfo.Host;
   Status authStatus = domainAuth.Authenticate(simiasManager.WebServiceUri, simiasManager.DataPath);
   switch (authStatus.statusCode)
   {
    case StatusCodes.InvalidCertificate:
                    if (authStatus.UserName != null)
                        domainInfo.Host = authStatus.UserName;
                    byte[] byteArray = simiasWebService.GetCertificate(domainInfo.Host);
     System.Security.Cryptography.X509Certificates.X509Certificate cert = new System.Security.Cryptography.X509Certificates.X509Certificate(byteArray);
     BeginInvoke( displayMessageDelegate,
                        new object[] { string.Format(resourceManager.GetString("verifyCert"), domainInfo.Host), resourceManager.GetString("verifyCertTitle"), cert.ToString(true), MyMessageBoxButtons.YesNo, MyMessageBoxIcon.Question, MyMessageBoxDefaultButton.Button2 });
     messageEvent.WaitOne();
     if ( messageDialogResult == DialogResult.Yes )
     {
                        simiasWebService.StoreCertificate(byteArray, domainInfo.Host);
      result = login();
     }
     break;
    case StatusCodes.Success:
    case StatusCodes.SuccessInGrace:
     result = true;
     if (authStatus.statusCode.Equals(StatusCodes.SuccessInGrace))
     {
      BeginInvoke( displayMessageDelegate,
       new object[] { string.Format(resourceManager.GetString("graceLogin"), authStatus.RemainingGraceLogins),
            resourceManager.GetString("graceLoginTitle"),
            string.Empty,
            MyMessageBoxButtons.OK,
            MyMessageBoxIcon.Information, MyMessageBoxDefaultButton.Button1 } );
      messageEvent.WaitOne();
     }
     if ( updatePasswordPreference )
     {
      try
      {
       if ( rememberPassword )
       {
        simiasWebService.SetDomainCredentials( domainInfo.ID, password, CredentialType.Basic );
       }
       else
       {
        simiasWebService.SetDomainCredentials( domainInfo.ID, null, CredentialType.None );
       }
      }
      catch (Exception ex)
      {
       BeginInvoke( displayMessageDelegate,
        new object[] { resourceManager.GetString("savePasswordError"), string.Empty, ex.Message, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error, MyMessageBoxDefaultButton.Button1 } );
       messageEvent.WaitOne();
      }
     }
     break;
    case StatusCodes.InvalidCredentials:
    case StatusCodes.InvalidPassword:
    case StatusCodes.UnknownUser:
     BeginInvoke( displayMessageDelegate,
      new object[] { resourceManager.GetString("failedAuth"), resourceManager.GetString("serverConnectErrorTitle"), string.Empty, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error, MyMessageBoxDefaultButton.Button1 } );
     messageEvent.WaitOne();
     break;
    case StatusCodes.AccountDisabled:
     BeginInvoke( displayMessageDelegate,
      new object[] { resourceManager.GetString("accountDisabled"), resourceManager.GetString("serverConnectErrorTitle"), string.Empty, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error, MyMessageBoxDefaultButton.Button1 } );
     messageEvent.WaitOne();
     break;
    case StatusCodes.AccountLockout:
     BeginInvoke( displayMessageDelegate,
      new object[] { resourceManager.GetString("accountLockout"), resourceManager.GetString("serverConnectErrorTitle"), string.Empty, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error, MyMessageBoxDefaultButton.Button1 } );
     messageEvent.WaitOne();
     break;
    case StatusCodes.SimiasLoginDisabled:
     BeginInvoke( displayMessageDelegate,
      new object[] { resourceManager.GetString("iFolderAccountDisabled"), resourceManager.GetString("serverConnectErrorTitle"), string.Empty, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error, MyMessageBoxDefaultButton.Button1 } );
     messageEvent.WaitOne();
     break;
                case StatusCodes.UserAlreadyMoved:
                        result = login();
                    break;
    default:
     BeginInvoke( displayMessageDelegate,
      new object[] { resourceManager.GetString("serverConnectError"), resourceManager.GetString("serverConnectErrorTitle"), string.Empty, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error, MyMessageBoxDefaultButton.Button1 } );
     messageEvent.WaitOne();
     break;
   }
   return result;
  }
  public void SetProxyForDomain( string hostUrl, bool unknownScheme )
  {
   UriBuilder ubHost = new UriBuilder( hostUrl );
   if ( unknownScheme )
   {
    ubHost.Scheme = Uri.UriSchemeHttp;
    ubHost.Port = 80;
    SetProxyForDomain( ubHost.Uri.ToString(), false );
    ubHost.Scheme = Uri.UriSchemeHttps;
    ubHost.Port = 443;
    SetProxyForDomain( ubHost.Uri.ToString(), false );
   }
   else
   {
    IWebProxy iwp = GlobalProxySelection.Select;
    if ( !iwp.IsBypassed( ubHost.Uri ) )
    {
     string proxyUser = null;
     string proxyPassword = null;
     Uri proxyUri = iwp.GetProxy( ubHost.Uri );
     if ( iwp.Credentials != null )
     {
      NetworkCredential netCred = iwp.Credentials.GetCredential( proxyUri, "Basic" );
      if ( netCred != null )
      {
       proxyUser = netCred.UserName;
       proxyPassword = netCred.Password;
      }
     }
     simiasWebService.SetProxyAddress( ubHost.Uri.ToString(), proxyUri.ToString(), proxyUser, proxyPassword );
    }
   }
  }
  public DomainInformation DomainInformation
  {
   get { return this.domainInfo; }
  }
        public string ServerName
        {
            get
            {
                return server;
            }
        }
        public string UserName
        {
            get
            {
                return user;
            }
        }
        public string Password
        {
            get
            {
                return password;
            }
            set
            {
                password = value;
            }
        }
        public bool RememberPassword
        {
            get
            {
                return rememberPassword;
            }
            set
            {
                rememberPassword = value;
            }
        }
 }
}
