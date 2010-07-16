
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;

using Novell.iFolder.Web;
using Novell.FormsTrayApp;
using Novell.iFolderCom;
using Simias.Client;
using Simias.Client.Authentication;
using Simias.Client.Event;
using System.Security.Cryptography;
using System.Security.Cryptography.X509Certificates;
using System.Xml;
using System.IO;


namespace Novell.Wizard
{
    public partial class SinglePageWizard : Novell.Wizard.InteriorPageTemplate
    {

        private DomainItem selectedDomain = null;
        private string recoveryAgent;
        private iFolderWebService ifWebService = null;
        private SimiasWebService simiasWebService = null;
        private KeyRecoveryWizard wizard;

        private Button browse;
        private string domainID;
        private string inputFilePath;
        private string outputFilePath;




        private System.ComponentModel.IContainer components = null;






        public SinglePageWizard(iFolderWebService ifws, SimiasWebService simws)
        {
            InitializeComponent();
            this.ifWebService = ifws;
            this.simiasWebService = simws;


        }






        internal override void ActivatePage(int previousIndex)
        {
            base.ActivatePage(previousIndex);
            wizard = (KeyRecoveryWizard)this.Parent;

            selectedDomain = wizard.DomainSelectionPage.SelectedDomain;
            this.accountBox.Text = selectedDomain.Name;


            ((KeyRecoveryWizard)this.Parent).WizardButtons = KeyRecoveryWizardButtons.Back | KeyRecoveryWizardButtons.Cancel;
             UpdateSensitivity();
             p12TextBox.Focus();

        }






        internal override int ValidatePage(int currentIndex)
        {
            bool result = Export_func();
            if (result == true)
            {
                result = KeyRecovery_func();
                if (result == true)
                {
                    result = Import_func();
                    if (result == true)
                    {
                    }
                    else
                    {

                        return currentIndex;
                    }
                }
                else
                {

                    return currentIndex;
                }
            }
            else
            {

                return currentIndex;
            }

            currentIndex = wizard.MaxPages - 4;
            return base.ValidatePage(currentIndex);
        }


        internal override int DeactivatePage()
        {

            try
            {
                File.Delete(inputFilePath);
                File.Delete(outputFilePath);
            }

            catch(Exception e)
            {}

            return base.DeactivatePage();

        }
        private void GetDefaultPath(string domainName)
        {
            string appdata = System.Environment.GetEnvironmentVariable("TEMP");
            int i = appdata.LastIndexOf("\\");
            appdata = appdata.Substring(0, i);
            inputFilePath = appdata + "\\" + domainName + "_encry.xml";
            outputFilePath = appdata + "\\" + domainName + "_decry.xml";
        }
        private bool Export_func()
        {
            try
            {
                GetDefaultPath(selectedDomain.Name);
                this.simiasWebService.ExportiFoldersCryptoKeys(selectedDomain.ID, inputFilePath);
            }
            catch (Exception ex)
            {
                MyMessageBox mmb = new MyMessageBox(TrayApp.Properties.Resources.unableToExportMesg, TrayApp.Properties.Resources.wizardText, null, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
                mmb.ShowDialog();
                return false;
            }
            return true;
        }
        private void browse_Click(object sender, EventArgs e)
        {
            OpenFileDialog fileDlg = new OpenFileDialog();
            fileDlg.Filter = "PKCS12 Files|*.p12";
            if (fileDlg.ShowDialog() == DialogResult.OK)
            {
                p12TextBox.Text = fileDlg.FileName;
            }
        }
        private bool KeyRecovery_func()
        {
            string certname = "";
            string pass = "";
            certname = Path.GetFullPath(p12TextBox.Text);
            if ((!File.Exists(certname)) || (!certname.EndsWith(".p12")))
            {
                MessageBox.Show("Private key path invalid");
                return false;
            }
            pass = passwdDomainTextBox.Text;
            X509Certificate2 xcert;
            try
            {
                if (pass.Length > 0)
                    xcert = new X509Certificate2(certname, pass);
                else
                    throw new ArgumentNullException("pass");
                Inner_keyRecovery inner = new Inner_keyRecovery();
                bool flag = false;
                inner.ProcessInputKeyFile(this.inputFilePath, this.outputFilePath, pass, xcert, flag, null);
            }
            catch
            {
            }
            return true;
        }
        private bool Import_func()
        {
            try
            {
                string onetimepp = null;
                this.simiasWebService.ImportiFoldersCryptoKeys(selectedDomain.ID, this.newPassphrase.Text, onetimepp, this.outputFilePath);
                bool rememberOption = this.simiasWebService.GetRememberOption(selectedDomain.ID);
                this.simiasWebService.StorePassPhrase(selectedDomain.ID, "", CredentialType.None, false);
                this.simiasWebService.StorePassPhrase(selectedDomain.ID, this.newPassphrase.Text, CredentialType.Basic, rememberOption);
            }
            catch (Exception ex)
            {
                MyMessageBox mmb = new MyMessageBox(TrayApp.Properties.Resources.importErrorMesg, TrayApp.Properties.Resources.wizardText, null, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
                mmb.ShowDialog();
                return false;
            }
            return true;
        }
        private void UpdateSensitivity()
        {
            if (this.newPassphrase.Text.Length > 0 && this.confirmPassphrase.Text.Length > 0 && this.newPassphrase.Text == this.newPassphrase.Text)
                if (this.p12TextBox.Text.Length > 0 && this.passwdDomainTextBox.Text.Length >= 0)
                {
                    ((KeyRecoveryWizard)this.Parent).WizardButtons = KeyRecoveryWizardButtons.Next | KeyRecoveryWizardButtons.Back | KeyRecoveryWizardButtons.Cancel;
                }
                else
                {
                    ((KeyRecoveryWizard)this.Parent).WizardButtons = KeyRecoveryWizardButtons.Back | KeyRecoveryWizardButtons.Cancel;
                }
        }
        private void p12TextBox_TextChanged(object sender, EventArgs e)
        {
            UpdateSensitivity();
        }
        private void passwdDomainTextBox_TextChanged(object sender, EventArgs e)
        {
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
    }
}
