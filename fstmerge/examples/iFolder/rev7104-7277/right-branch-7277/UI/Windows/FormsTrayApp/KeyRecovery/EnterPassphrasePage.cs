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
    public partial class EnterPassphrasePage : Novell.Wizard.InteriorPageTemplate
    {
        private KeyRecoveryWizard wizard;
        private SimiasWebService simiasWebService;
        private iFolderWebService ifolderWebService;
        private Manager simiasManager;
        public EnterPassphrasePage(iFolderWebService ifws, SimiasWebService simws, Manager simiasManager)
        {
            InitializeComponent();
            this.simiasWebService = simws;
            this.ifolderWebService = ifws;
            this.simiasManager = simiasManager;
        }
        internal override int DeactivatePage()
        {
            return base.DeactivatePage();
        }
        internal override void ActivatePage(int previousIndex)
        {
            base.ActivatePage(previousIndex);
            wizard = (KeyRecoveryWizard)this.Parent;
            ((KeyRecoveryWizard)this.Parent).WizardButtons = KeyRecoveryWizardButtons.Cancel | KeyRecoveryWizardButtons.Back;
            iFolderAcc.Text = wizard.DomainSelectionPage.SelectedDomain.Name + wizard.DomainSelectionPage.SelectedDomain.Host;
            string domainID = wizard.DomainSelectionPage.SelectedDomain.ID;
            DomainInformation domainInfo = (DomainInformation)this.simiasWebService.GetDomainInformation(domainID);
            userName.Text = domainInfo.MemberName;
            newPassphrase.Focus();
            UpdateSensitivity();
        }
        private void newPassphrase_TextChanged(object sender, EventArgs e)
        {
            UpdateSensitivity();
        }
        private void confirmPassphrase_TextChanged(object sender, EventArgs e)
        {
            UpdateSensitivity();
        }
        private void UpdateSensitivity()
        {
            if (this.newPassphrase.Text.Length > 0 && this.confirmPassphrase.Text.Length > 0 && this.userName.Text.Length > 0 && this.password.Text.Length > 0)
                   ((KeyRecoveryWizard)this.Parent).WizardButtons = KeyRecoveryWizardButtons.Cancel | KeyRecoveryWizardButtons.Back | KeyRecoveryWizardButtons.Next;
            else
                ((KeyRecoveryWizard)this.Parent).WizardButtons = KeyRecoveryWizardButtons.Cancel | KeyRecoveryWizardButtons.Back;
        }
        internal override int ValidatePage(int currentIndex)
        {
            DomainInformation[] domains;
            DomainInformation selectedDomainInfo = null;
            domains = this.simiasWebService.GetDomains(true);
            if (this.newPassphrase.Text != this.confirmPassphrase.Text)
            {
                MessageBox.Show(TrayApp.Properties.Resources.passphraseNotEqualError, TrayApp.Properties.Resources.resetPassphraseError,MessageBoxButtons.OK,MessageBoxIcon.Error);
                return currentIndex;
            }
            foreach (DomainInformation di in domains)
            {
                if (di.Authenticated && di.ID == wizard.DomainSelectionPage.SelectedDomain.ID)
                {
                    selectedDomainInfo = (DomainInformation)di;
                }
            }
            bool result = false;
            Domain domain = new Domain(selectedDomainInfo);
            try
            {
                Status status = this.simiasWebService.LogoutFromRemoteDomain(domain.ID);
                if (status.statusCode == StatusCodes.Success)
                {
                    status = this.simiasWebService.LoginToRemoteDomain(domain.ID, this.password.Text);
                    if (status.statusCode != StatusCodes.Success)
                    {
                        MessageBox.Show(Resources.loginError,TrayApp.Properties.Resources.authenticateError,MessageBoxButtons.OK,MessageBoxIcon.Error);
                       (Novell.FormsTrayApp.FormsTrayApp.globalProp()).refreshAll();
                        return -999;
                    }
                    result = true;
                }
            }
            catch (Exception)
            {
                MessageBox.Show(Resources.loginError, TrayApp.Properties.Resources.authenticateError, MessageBoxButtons.OK, MessageBoxIcon.Error);
                (Novell.FormsTrayApp.FormsTrayApp.globalProp()).refreshAll();
                 return -999;
            }
            if (result)
            {
                string memberUID = selectedDomainInfo.MemberUserID;
                try
                {
                    this.simiasWebService.ExportRecoverImport(selectedDomainInfo.ID, memberUID, this.newPassphrase.Text);
                    this.simiasWebService.StorePassPhrase(selectedDomainInfo.ID,
                                                              this.newPassphrase.Text,
                                                              CredentialType.Basic,
                                                             this.simiasWebService.GetRememberOption(selectedDomainInfo.ID));
                }
                catch (Exception )
                {
                   MessageBox.Show(Resources.recoveryError,
                     Resources.resetPassphraseError,MessageBoxButtons.OK,MessageBoxIcon.Error);
                     return -999;
                }
            }
                currentIndex = wizard.MaxPages - 4;
                return base.ValidatePage(currentIndex);
        }
        private void userName_TextChanged(object sender, EventArgs e)
        {
            UpdateSensitivity();
        }
        private void password_TextChanged(object sender, EventArgs e)
        {
            UpdateSensitivity();
        }
    }
}
