

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
            iFolderAcc.Text = wizard.DomainSelectionPage.SelectedDomain.Name;



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

            if (this.newPassphrase.Text.Length > 0 && this.confirmPassphrase.Text.Length > 0 && this.newPassphrase.Text == this.confirmPassphrase.Text && this.userName.Text.Length > 0 && this.password.Text.Length > 0)

                ((KeyRecoveryWizard)this.Parent).WizardButtons = KeyRecoveryWizardButtons.Cancel | KeyRecoveryWizardButtons.Back | KeyRecoveryWizardButtons.Next;

            else
                ((KeyRecoveryWizard)this.Parent).WizardButtons = KeyRecoveryWizardButtons.Cancel | KeyRecoveryWizardButtons.Back;


        }





        internal override int ValidatePage(int currentIndex)
        {

            DomainInformation[] domains;
            DomainInformation selectedDomainInfo = null;
            domains = this.simiasWebService.GetDomains(true);
            Status passPhraseStatus = null;
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
                        MessageBox.Show(Resources.authenticateError);
                        return -999;

                    }

                    result = true;

                }
            }
            catch (Exception e)

            {

                    return -999;


            }




            if (result)
            {

                string memberUID = selectedDomainInfo.MemberUserID;
                string publicKey = null;

                try
                {
                    publicKey = this.ifolderWebService.GetDefaultServerPublicKey(selectedDomainInfo.ID, memberUID);
                    this.simiasWebService.ExportRecoverImport(selectedDomainInfo.ID, memberUID, this.newPassphrase.Text);
                    passPhraseStatus = null;

                    passPhraseStatus = this.simiasWebService.SetPassPhrase(selectedDomainInfo.ID, this.newPassphrase.Text, "DEFAULT", publicKey);
                    result = true;
                }
                catch (Exception ex)
                {

                    MyMessageBox mmb = new MyMessageBox(Resources.recoveryError,
                     Resources.resetPassphraseError, null, MyMessageBoxButtons.OK);
                    mmb.ShowDialog();
                    if (mmb.DialogResult == DialogResult.OK)
                    {
                        return -999;
                    }
                }


            }

            if (passPhraseStatus.statusCode != StatusCodes.Success)
            {
                MyMessageBox mmb = new MyMessageBox(Resources.recoveryError,
                        Resources.resetPassphraseError, null, MyMessageBoxButtons.OK);
                mmb.ShowDialog();
                if (mmb.DialogResult == DialogResult.OK)
                {
                    return currentIndex;
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
