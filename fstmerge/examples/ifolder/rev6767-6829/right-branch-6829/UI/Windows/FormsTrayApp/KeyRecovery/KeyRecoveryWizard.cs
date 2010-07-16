


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
using TrayApp.Properties;

using Novell.iFolder.Web;
using Novell.FormsTrayApp;
using Novell.iFolderCom;
using Simias.Client;
using Simias.Client.Authentication;
using Simias.Client.Event;


namespace Novell.Wizard
{



    [Flags]
    public enum KeyRecoveryWizardButtons
    {



        None = 0,




        Back = 1,




        Next = 2,




        Cancel = 4
    }




    public partial class KeyRecoveryWizard : System.Windows.Forms.Form
    {


        public System.Windows.Forms.Button cancel;
        public System.Windows.Forms.Button next;
        public System.Windows.Forms.Button back;
        private System.Windows.Forms.Button btnHelp;
        internal System.Windows.Forms.GroupBox groupBox1;

        private DomainSelectionPage domainSelectionPage;
        private EnterPassphrasePage enterPassphrasePage;
        private InfoPage infoPage;
        private SelectionPage selectionPage;
        private SinglePageWizard singleWizPage;
        private ImportKeyPage importKeyPage;
        private FinalPage finalPage;
        private ExportKeyPage exportKeyPage;
        private EmailPage emailPage;

        private BaseWizardPage[] pages;

        internal const int maxPages = 9;
        protected int currentIndex = 0;

        private SimiasWebService simiasWebService;
        private iFolderWebService ifWebService;
        private Manager simiasManager;







        private System.ComponentModel.Container components = null;
        public KeyRecoveryWizard(iFolderWebService ifWebService, SimiasWebService simiasWebService,Manager simiasManager)
        {
            this.simiasWebService = simiasWebService;
            this.ifWebService = ifWebService;
            this.simiasManager = simiasManager;
            InitializeComponent();
            this.domainSelectionPage = new DomainSelectionPage(this.ifWebService,this.simiasWebService);
            this.enterPassphrasePage = new EnterPassphrasePage(this.ifWebService,this.simiasWebService,this.simiasManager);
            this.infoPage = new InfoPage();
            this.selectionPage = new SelectionPage();
            this.singleWizPage = new SinglePageWizard(this.ifWebService, this.simiasWebService);
            this.importKeyPage = new ImportKeyPage(this.ifWebService, this.simiasWebService);
            this.finalPage = new FinalPage();
            this.exportKeyPage = new ExportKeyPage(this.ifWebService, this.simiasWebService);
            this.emailPage = new EmailPage();
            this.domainSelectionPage.HeaderTitle = TrayApp.Properties.Resources.domainSelectionPageTitle;
            this.domainSelectionPage.Location = new System.Drawing.Point(0, 0);
            this.domainSelectionPage.Name = TrayApp.Properties.Resources.domainSelectionPageName;
            this.domainSelectionPage.Size = new System.Drawing.Size(496, 304);
            this.domainSelectionPage.TabIndex = 1;
            this.enterPassphrasePage.HeaderTitle = TrayApp.Properties.Resources.setNewPassphraseTitle;
            this.enterPassphrasePage.Location = new System.Drawing.Point(0, 0);
            this.enterPassphrasePage.Name = TrayApp.Properties.Resources.enterPassphrasePageName;
            this.enterPassphrasePage.Size = new System.Drawing.Size(496, 304);
            this.enterPassphrasePage.TabIndex = 1;
            this.infoPage.HeaderTitle = TrayApp.Properties.Resources.infoPageTitle;
            this.infoPage.Location = new System.Drawing.Point(0, 0);
            this.infoPage.Name = TrayApp.Properties.Resources.infoPageName;
            this.infoPage.Size = new System.Drawing.Size(496, 304);
            this.infoPage.TabIndex = 1;
            this.selectionPage.HeaderTitle = TrayApp.Properties.Resources.selectionPageTitle;
            this.selectionPage.Location = new System.Drawing.Point(0, 0);
            this.selectionPage.Name = TrayApp.Properties.Resources.selectionPageName;
            this.selectionPage.Size = new System.Drawing.Size(496, 304);
            this.selectionPage.TabIndex = 1;
            this.singleWizPage.HeaderTitle = TrayApp.Properties.Resources.setNewPassphraseTitle;
            this.singleWizPage.Location = new System.Drawing.Point(0, 0);
            this.singleWizPage.Name = TrayApp.Properties.Resources.singleWizPageName;
            this.singleWizPage.Size = new System.Drawing.Size(496, 304);
            this.singleWizPage.TabIndex = 1;
            this.importKeyPage.HeaderTitle = TrayApp.Properties.Resources.setNewPassphraseTitle;
            this.importKeyPage.Location = new System.Drawing.Point(0, 0);
            this.importKeyPage.Name = TrayApp.Properties.Resources.importPageName;
            this.importKeyPage.Size = new System.Drawing.Size(496, 304);
            this.importKeyPage.TabIndex = 1;
            this.finalPage.HeaderTitle = TrayApp.Properties.Resources.finalPageTitle;
            this.finalPage.Location = new System.Drawing.Point(0, 0);
            this.finalPage.Name = TrayApp.Properties.Resources.finalPageName;
            this.finalPage.Size = new System.Drawing.Size(496, 304);
            this.finalPage.TabIndex = 1;
            this.exportKeyPage.HeaderTitle = TrayApp.Properties.Resources.exportPageTitle;
            this.exportKeyPage.Location = new System.Drawing.Point(0, 0);
            this.exportKeyPage.Name = TrayApp.Properties.Resources.exportPageName;
            this.exportKeyPage.Size = new System.Drawing.Size(496, 304);
            this.exportKeyPage.TabIndex = 1;
            this.emailPage.HeaderTitle = TrayApp.Properties.Resources.emailPageTitle;
            this.emailPage.Location = new System.Drawing.Point(0, 0);
            this.emailPage.Name = TrayApp.Properties.Resources.emailPageName;
            this.emailPage.Size = new System.Drawing.Size(496, 304);
            this.emailPage.TabIndex = 1;
            this.Controls.Add(this.domainSelectionPage);
            this.Controls.Add(this.enterPassphrasePage);
            this.Controls.Add(this.infoPage);
            this.Controls.Add(this.selectionPage);
            this.Controls.Add(this.singleWizPage);
            this.Controls.Add(this.importKeyPage);
            this.Controls.Add(this.finalPage);
            this.Controls.Add(this.exportKeyPage);
            this.Controls.Add(this.emailPage);
            try
            {
                this.Icon = new Icon(Path.Combine(Application.StartupPath, @"res\ifolder_16.ico"));
            }
            catch { }
            pages = new BaseWizardPage[maxPages];
            pages[0] = this.domainSelectionPage;
            pages[1] = this.enterPassphrasePage;
            pages[2] = this.infoPage;
            pages[3] = this.selectionPage;
            pages[4] = this.singleWizPage;
            pages[5] = this.importKeyPage;
            pages[6] = this.finalPage;
            pages[7] = this.exportKeyPage;
            pages[8] = this.emailPage;
            try
            {
                Image image = Image.FromFile(System.IO.Path.Combine(Application.StartupPath, @"res\ifolder48.png"));
                this.domainSelectionPage.Thumbnail = image;
                this.enterPassphrasePage.Thumbnail = image;
                this.infoPage.Thumbnail = image;
                this.selectionPage.Thumbnail = image;
                this.singleWizPage.Thumbnail = image;
                this.importKeyPage.Thumbnail = image;
                this.finalPage.Thumbnail = image;
                this.exportKeyPage.Thumbnail = image;
                this.emailPage.Thumbnail = image;
            }
            catch { }
            foreach (BaseWizardPage page in pages)
            {
                page.Hide();
            }
            bool result = GetLoggedInDomains();
            if(result == true)
                domainSelectionPage.ActivatePage(0);
        }
        public bool GetLoggedInDomains()
        {
            bool result = false;
            try
            {
                DomainInformation[] domains;
                domains = this.simiasWebService.GetDomains(true);
                int domainCount = 0;
                foreach (DomainInformation di in domains)
                {
                    if (di.Authenticated)
                    {
                        domainCount++;
                    }
                }
                if (domainCount >= 1)
                    result = true;
                return result;
            }
            catch (Exception ex)
            {
                return false;
            }
        }
        private void back_Click(object sender, System.EventArgs e)
        {
            this.WizardButtons = KeyRecoveryWizardButtons.Next | KeyRecoveryWizardButtons.Back | KeyRecoveryWizardButtons.Cancel;
            int previousIndex = this.pages[currentIndex].DeactivatePage();
            this.pages[previousIndex].ActivatePage(0);
            currentIndex = previousIndex;
        }
        private void next_Click(object sender, System.EventArgs e)
        {
            int nextIndex = this.pages[currentIndex].ValidatePage(currentIndex);
            if (nextIndex == -999)
            {
                this.Close();
                this.Dispose();
                return;
            }
            if (nextIndex != currentIndex)
            {
                this.pages[currentIndex].DeactivatePage();
                this.pages[nextIndex].ActivatePage(currentIndex);
                currentIndex = nextIndex;
              }
            if (this.currentIndex == this.MaxPages - 1 || this.currentIndex == this.MaxPages - 3)
            {
                this.cancel.Text = TrayApp.Properties.Resources.finishText;
                next.DialogResult = DialogResult.OK;
                WizardButtons = KeyRecoveryWizardButtons.Cancel;
            }
        }
        public int MaxPages
        {
            get { return maxPages; }
        }
        public ExportKeyPage ExportKeyPage
        {
            get { return exportKeyPage; }
        }
        public DomainSelectionPage DomainSelectionPage
        {
            get { return domainSelectionPage; }
        }
        public KeyRecoveryWizardButtons WizardButtons
        {
            get
            {
                KeyRecoveryWizardButtons wizardButtons = KeyRecoveryWizardButtons.None;
                wizardButtons |= cancel.Enabled ? KeyRecoveryWizardButtons.Cancel : KeyRecoveryWizardButtons.None;
                wizardButtons |= next.Enabled ? KeyRecoveryWizardButtons.Next : KeyRecoveryWizardButtons.None;
                wizardButtons |= back.Enabled ? KeyRecoveryWizardButtons.Back : KeyRecoveryWizardButtons.None;
                return wizardButtons;
            }
            set
            {
                KeyRecoveryWizardButtons wizardButtons = value;
                cancel.Enabled = ((wizardButtons & KeyRecoveryWizardButtons.Cancel) == KeyRecoveryWizardButtons.Cancel);
                next.Enabled = ((wizardButtons & KeyRecoveryWizardButtons.Next) == KeyRecoveryWizardButtons.Next);
                back.Enabled = ((wizardButtons & KeyRecoveryWizardButtons.Back) == KeyRecoveryWizardButtons.Back);
            }
        }
        private void cancel_Click(object sender, EventArgs e)
        {
            if (this.currentIndex == this.MaxPages - 1)
            {
                this.cancel.Text = TrayApp.Properties.Resources.finishText;
                WizardButtons = KeyRecoveryWizardButtons.Cancel;
            }
            this.Close();
            this.Dispose();
        }
            private void help_Click(object o, EventArgs args)
         {
             string helpFile = Path.Combine(Path.Combine(Path.Combine(Application.StartupPath, "help"), iFolderAdvanced.GetLanguageDirectory()), @"managingpassphrse.html");
             new iFolderComponent().ShowHelp(Application.StartupPath, helpFile);
         }
    }
}
