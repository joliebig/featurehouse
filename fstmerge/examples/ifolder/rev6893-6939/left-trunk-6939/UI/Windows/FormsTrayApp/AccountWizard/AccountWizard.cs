

using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;
using System.Data;
using System.IO;
using System.Reflection;
using System.Text;
using System.Net;

using Novell.iFolder.Web;
using Novell.FormsTrayApp;
using Novell.iFolderCom;
using Simias.Client;
using Simias.Client.Authentication;
using Simias.Client.Event;

namespace Novell.Wizard
{



 [Flags]
 public enum WizardButtons
 {



  None = 0,




  Back = 1,




  Next = 2,




  Cancel = 4,
 }




 public class AccountWizard : System.Windows.Forms.Form
 {

  DomainInformation domainInfo;
  private Preferences prefs;
        private GlobalProperties globalProperties;
  private System.Windows.Forms.Button cancel;
  private System.Windows.Forms.Button next;
  private System.Windows.Forms.Button back;
  private System.Windows.Forms.Button btnHelp;
  internal System.Windows.Forms.GroupBox groupBox1;
  private WelcomePage welcomePage;
  private ServerPage serverPage;
  private IdentityPage identityPage;
  private VerifyPage verifyPage;
  private DefaultiFolderPage defaultiFolderPage;
  private CompletionPage completionPage;
  private BaseWizardPage[] pages;
  internal const int maxPages = 6;
  private int currentIndex = 0;
  private Manager simiasManager;
  private SimiasWebService simiasWebService;
  private iFolderWebService ifWebService;
  private static System.Resources.ResourceManager Resource = new System.Resources.ResourceManager(typeof(Novell.FormsTrayApp.FormsTrayApp));




  private System.ComponentModel.Container components = null;





  public AccountWizard( iFolderWebService ifWebService, SimiasWebService simiasWebService, Manager simiasManager, bool firstAccount, Preferences prefs )
  {
   this.simiasManager = simiasManager;
   this.simiasWebService = simiasWebService;
   this.ifWebService = ifWebService;
   this.prefs = prefs;




   InitializeComponent();



   this.welcomePage = new WelcomePage();
   this.serverPage = new ServerPage( firstAccount );
   this.identityPage = new IdentityPage();
   this.verifyPage = new VerifyPage( firstAccount );
   this.completionPage = new CompletionPage();
   this.defaultiFolderPage = new DefaultiFolderPage(this.ifWebService, this.simiasWebService, this.domainInfo);




   this.welcomePage.DescriptionText = Resource.GetString("WelcomePageDescription");
   this.welcomePage.ActionText = Resource.GetString("WelcomePageAction");
   this.welcomePage.Location = new System.Drawing.Point(0, 0);
   this.welcomePage.Name = Resource.GetString("WelcomePageName");
   this.welcomePage.Size = new System.Drawing.Size(496, 304);
   this.welcomePage.TabIndex = 1;
   this.welcomePage.WelcomeTitle = Resource.GetString("WelcomePageTitle");




   this.serverPage.HeaderTitle = Resource.GetString("ServerPageHeaderTitle");
   this.serverPage.Location = new System.Drawing.Point(0, 0);
   this.serverPage.Name = Resource.GetString("ServerPageName");
   this.serverPage.Size = new System.Drawing.Size(496, 304);
   this.serverPage.TabIndex = 1;




   this.identityPage.HeaderTitle = Resource.GetString("IdentityPageHeaderTitle");
   this.identityPage.Location = new System.Drawing.Point(0, 0);
   this.identityPage.Name = Resource.GetString("IdentityPageName");
   this.identityPage.Size = new System.Drawing.Size(496, 304);
   this.identityPage.TabIndex = 1;




   this.verifyPage.HeaderTitle = Resource.GetString("VerifyPageHeaderTitle");
   this.verifyPage.Location = new System.Drawing.Point(0, 0);
   this.verifyPage.Name = Resource.GetString("VerifyPageName");
   this.verifyPage.Size = new System.Drawing.Size(496, 304);
   this.verifyPage.TabIndex = 1;



   this.defaultiFolderPage.Name = "defaultiFolderPage";
   this.defaultiFolderPage.Location = new System.Drawing.Point(0, 0);
   this.defaultiFolderPage.Size = new System.Drawing.Size(496, 304);
   this.defaultiFolderPage.HeaderTitle = Resource.GetString("DefaultiFolder");




   this.completionPage.DescriptionText = Resource.GetString("CompletionPageDT");
   this.completionPage.Location = new System.Drawing.Point(0, 0);
   this.completionPage.Name = "completionPage";
   this.completionPage.Size = new System.Drawing.Size(496, 304);
   this.completionPage.TabIndex = 1;
   this.completionPage.WelcomeTitle = Resource.GetString("CompletionPageWT");

   this.Controls.Add(this.welcomePage);
   this.Controls.Add(this.serverPage);
   this.Controls.Add(this.identityPage);
   this.Controls.Add(this.verifyPage);
   this.Controls.Add(this.defaultiFolderPage);
   this.Controls.Add(this.completionPage);


   try
   {
    this.Icon = new Icon(Path.Combine(Application.StartupPath, @"res\ifolder_16.ico"));
   }
   catch {}


   pages = new BaseWizardPage[maxPages];
   pages[0] = this.welcomePage;
   pages[1] = this.serverPage;
   pages[2] = this.identityPage;
   pages[3] = this.verifyPage;
   pages[4] = this.defaultiFolderPage;
   pages[5] = this.completionPage;

   try
   {


    Image image = Image.FromFile(Path.Combine(Application.StartupPath, @"res\ifolder_account_wiz.png"));
    this.welcomePage.Watermark = image;
    this.completionPage.Watermark = image;


    image = Image.FromFile(Path.Combine(Application.StartupPath, @"res\ifolder-add-account48.png"));
    this.serverPage.Thumbnail = image;
    this.verifyPage.Thumbnail = image;
    this.identityPage.Thumbnail = image;
    this.defaultiFolderPage.Thumbnail = image;
   }
   catch {}

   foreach (BaseWizardPage page in pages)
   {
    page.Hide();
   }


   pages[0].ActivatePage(0);
  }
        public AccountWizard(iFolderWebService ifWebService, SimiasWebService simiasWebService, Manager simiasManager, bool firstAccount, Preferences prefs, string ip)
            : this(ifWebService, simiasWebService, simiasManager, firstAccount, prefs)
        {
            this.serverPage.ServerAddress = ip;
            this.serverPage.DisableServerEntry();
        }
        public AccountWizard(iFolderWebService ifWebService, SimiasWebService simiasWebService, Manager simiasManager, bool firstAccount, Preferences prefs, GlobalProperties gProps)
            : this(ifWebService, simiasWebService, simiasManager, firstAccount, prefs)
        {
            this.globalProperties = gProps;
        }
        public GlobalProperties GlobalProps
        {
            get
            {
                return this.globalProperties;
            }
            set
            {
                this.globalProperties = value;
            }
        }
  protected override void Dispose( bool disposing )
  {
   if( disposing )
   {
    if (components != null)
    {
     components.Dispose();
    }
   }
   base.Dispose( disposing );
  }
  private void InitializeComponent()
  {
   this.cancel = new System.Windows.Forms.Button();
   this.next = new System.Windows.Forms.Button();
   this.back = new System.Windows.Forms.Button();
   this.btnHelp = new System.Windows.Forms.Button();
   this.groupBox1 = new System.Windows.Forms.GroupBox();
   this.SuspendLayout();
   this.cancel.FlatStyle = System.Windows.Forms.FlatStyle.System;
   this.cancel.ImeMode = System.Windows.Forms.ImeMode.NoControl;
   this.cancel.Location = new System.Drawing.Point(416, 318);
   this.cancel.Name = Resource.GetString("CancelText");
   this.cancel.Size = new System.Drawing.Size(72, 23);
   this.cancel.TabIndex = 3;
   this.cancel.Text = Resource.GetString("CancelText");
   this.cancel.Click += new EventHandler(cancel_Click);
   this.next.FlatStyle = System.Windows.Forms.FlatStyle.System;
   this.next.ImeMode = System.Windows.Forms.ImeMode.NoControl;
   this.next.Location = new System.Drawing.Point(328, 318);
   this.next.Name = Resource.GetString("NextText");
   this.next.Size = new System.Drawing.Size(72, 23);
   this.next.TabIndex = 2;
   this.next.Text = Resource.GetString("NextText")+" >";
   this.next.Click += new System.EventHandler(this.next_Click);
   this.back.FlatStyle = System.Windows.Forms.FlatStyle.System;
   this.back.ImeMode = System.Windows.Forms.ImeMode.NoControl;
   this.back.Location = new System.Drawing.Point(253, 318);
   this.back.Name = Resource.GetString("BackText");
   this.back.Size = new System.Drawing.Size(72, 23);
   this.back.TabIndex = 1;
   this.back.Text = "< "+Resource.GetString("BackText");
   this.back.Click += new System.EventHandler(this.back_Click);
   this.btnHelp.FlatStyle = System.Windows.Forms.FlatStyle.System;
   this.btnHelp.ImeMode = System.Windows.Forms.ImeMode.NoControl;
   this.btnHelp.Location = new System.Drawing.Point(16, 318);
   this.btnHelp.Name = Resource.GetString("BackText");
   this.btnHelp.Size = new System.Drawing.Size(72, 23);
   this.btnHelp.TabIndex = 1;
   this.btnHelp.Text = Resource.GetString("menuHelp.Text");
   this.btnHelp.Click += new System.EventHandler(this.help_Click);
   this.groupBox1.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)
    | System.Windows.Forms.AnchorStyles.Right)));
   this.groupBox1.Location = new System.Drawing.Point(0, 302);
   this.groupBox1.Name = "groupBox1";
   this.groupBox1.Size = new System.Drawing.Size(496, 4);
   this.groupBox1.TabIndex = 4;
   this.groupBox1.TabStop = false;
   this.groupBox1.Text = "";
   this.AcceptButton = this.next;
   this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
   this.ClientSize = new System.Drawing.Size(496, 348);
   this.ControlBox = false;
   this.Controls.Add(this.groupBox1);
   this.Controls.Add(this.btnHelp);
   this.Controls.Add(this.back);
   this.Controls.Add(this.next);
   this.Controls.Add(this.cancel);
   this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle;
            this.BackColor = System.Drawing.Color.Gainsboro;
   this.ImeMode = System.Windows.Forms.ImeMode.NoControl;
   this.MaximizeBox = false;
   this.MinimizeBox = false;
   this.Name = "AccountWizard";
   this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
   this.Text = Resource.GetString("AccWizardText");
   this.next.Select();
   this.Activated += new System.EventHandler(this.AccountWizard_Activated);
   this.ResumeLayout(false);
  }
        private void AccountWizard_Activated(object sender, System.EventArgs e)
  {
   Novell.CustomUIControls.ShellNotifyIcon.SetForegroundWindow(Handle);
  }
        private void back_Click(object sender, System.EventArgs e)
  {
            if (currentIndex != (maxPages - 4))
   {
    this.next.Text = Resource.GetString("NextText")+" >";
   }
   if( currentIndex == (maxPages -2) )
    return;
   int previousIndex = this.pages[currentIndex].DeactivatePage();
   this.pages[previousIndex].ActivatePage(0);
   currentIndex = previousIndex;
  }
        private void connecting_EnterpriseConnect(object sender, DomainConnectEventArgs e)
  {
   if (EnterpriseConnect != null)
   {
    EnterpriseConnect(this, new DomainConnectEventArgs( e.DomainInfo ));
   }
  }
        private void next_Click(object sender, System.EventArgs e)
  {
   if (currentIndex == (maxPages - 1))
   {
    return;
   }
   int nextIndex = this.pages[currentIndex].ValidatePage(currentIndex);
   if (nextIndex != currentIndex)
   {
    this.pages[currentIndex].DeactivatePage();
    this.pages[nextIndex].ActivatePage(currentIndex);
    currentIndex = nextIndex;
    if( currentIndex == (maxPages - 3))
    {
     next.Text = Resource.GetString("ConnectText");
    }
    if (currentIndex == (maxPages - 2))
    {
     next.Text = Resource.GetString("NextText")+" >";
     this.back.Enabled = false;
     this.cancel.Enabled = false;
    }
    else if (currentIndex == (maxPages - 1))
    {
     next.DialogResult = DialogResult.OK;
     next.Text = Resource.GetString("FinishText");
    }
   }
  }
        private string GetDefaultPath(string userName, string domainName)
  {
   string appdata = System.Environment.GetEnvironmentVariable("APPDATA");
   int i = appdata.LastIndexOf("\\");
   appdata = appdata.Substring(0, i);
   appdata = appdata + Resource.GetString("ifolderDirText") + "\\" + domainName + "\\" + userName ;
   return appdata;
  }
  public delegate void EnterpriseConnectDelegate(object sender, DomainConnectEventArgs e);
  public event EnterpriseConnectDelegate EnterpriseConnect;
  public IdentityPage IdentityPage
  {
   get { return identityPage; }
  }
  public int MaxPages
  {
   get { return maxPages; }
  }
  public ServerPage ServerPage
  {
   get { return serverPage; }
  }
  public string SummaryText
  {
   get
   {
    StringBuilder sb = new StringBuilder(Resource.GetString("CongratsMsg"));
    sb.AppendFormat( "\n\n{0}:\t{1}\n", Resource.GetString("AccountName"), domainInfo.Name );
    sb.AppendFormat( "{0}:\t{1}\n", Resource.GetString("ServerNameText"), serverPage.ServerAddress );
    sb.AppendFormat( "{0}:\t{1}\n\n", Resource.GetString("UName"), identityPage.Username );
    sb.Append( Resource.GetString("CongratsDescription") );
    return sb.ToString();
   }
  }
  public WizardButtons WizardButtons
  {
   get
   {
    WizardButtons wizardButtons = WizardButtons.None;
    wizardButtons |= cancel.Enabled ? WizardButtons.Cancel : WizardButtons.None;
    wizardButtons |= next.Enabled ? WizardButtons.Next : WizardButtons.None;
    wizardButtons |= back.Enabled ? WizardButtons.Back : WizardButtons.None;
    return wizardButtons;
   }
   set
   {
    WizardButtons wizardButtons = value;
    cancel.Enabled = ((wizardButtons & WizardButtons.Cancel) == WizardButtons.Cancel);
    next.Enabled = ((wizardButtons & WizardButtons.Next) == WizardButtons.Next);
    back.Enabled = ((wizardButtons & WizardButtons.Back) == WizardButtons.Back);
   }
  }
  public bool ConnectToServer()
  {
   bool result = false;
   Connecting connecting = new Connecting( this.ifWebService, simiasWebService, simiasManager, serverPage.ServerAddress, identityPage.Username, identityPage.Password, serverPage.DefaultServer, identityPage.RememberPassword );
   connecting.EnterpriseConnect += new Novell.FormsTrayApp.Connecting.EnterpriseConnectDelegate(connecting_EnterpriseConnect);
   if ( connecting.ShowDialog() == DialogResult.OK )
   {
    result = true;
    domainInfo = connecting.DomainInformation;
    this.defaultiFolderPage.DomainInfo = this.domainInfo;
    this.defaultiFolderPage.defaultPath = this.GetDefaultPath(this.identityPage.Username, this.defaultiFolderPage.DomainInfo.Name);
                this.defaultiFolderPage.defaultPath += "\\" + Resource.GetString("DefaultDirName") + "_" + this.identityPage.Username;
                Novell.FormsTrayApp.FormsTrayApp.globalProp().updateifListViewDomainStatus(domainInfo.ID, domainInfo.Authenticated);
                if (true == domainInfo.Authenticated)
                {
                    Novell.FormsTrayApp.FormsTrayApp.globalProp().AddDomainToUIList(domainInfo);
                }
   }
   return result;
  }
        public void UpdateDisplay( iFolderWeb ifolder, string path)
  {
   (Novell.FormsTrayApp.FormsTrayApp.globalProp()).UpdateDisplay(ifolder, path);
  }
        private void cancel_Click(object sender, EventArgs e)
  {
   if( this.currentIndex == this.MaxPages-2)
   {
    this.pages[currentIndex].DeactivatePage();
    this.pages[currentIndex+1].ActivatePage(currentIndex);
    currentIndex++;
    next.DialogResult = DialogResult.OK;
    next.Text = Resource.GetString("FinishText");
    return;
   }
   else
   {
    this.Close();
    this.Dispose();
   }
  }
        private void help_Click( object o, EventArgs args)
  {
   string helpFile = Path.Combine(Path.Combine(Path.Combine(Application.StartupPath, "help"), iFolderAdvanced.GetLanguageDirectory()), @"accounts.html");
   new iFolderComponent().ShowHelp(Application.StartupPath, helpFile);
  }
 }
}
