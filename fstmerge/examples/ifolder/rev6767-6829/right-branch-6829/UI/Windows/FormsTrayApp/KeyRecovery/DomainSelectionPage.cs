
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
    public partial class DomainSelectionPage : Novell.Wizard.InteriorPageTemplate
    {
        private iFolderWebService ifWebService = null;
        private SimiasWebService simiasWebService = null;
        private KeyRecoveryWizard wizard;


        public DomainSelectionPage(iFolderWebService ifws, SimiasWebService simws)
        {
            InitializeComponent();
            this.ifWebService = ifws;
            this.simiasWebService = simws;
        }

        public DomainItem SelectedDomain
        {

            get
            {
                DomainItem domainItem = (DomainItem)this.domainComboBox.SelectedItem;


                return domainItem;
            }
        }



        private void GetLoggedInDomains()
        {
            try
            {
                DomainInformation[] domains;
                domains = this.simiasWebService.GetDomains(true);
                foreach (DomainInformation di in domains)
                {

                    if (di.Authenticated)
                    {

                        DomainItem domainItem = new DomainItem(di.Name, di.ID);
                        this.domainComboBox.Items.Add(domainItem);
                        this.domainComboBox.SelectedIndex = 0;
                    }
                }
            }
            catch
            {

            }
        }





        internal override int DeactivatePage()
        {

            return base.DeactivatePage();
        }

        internal override void ActivatePage(int previousIndex)
        {

               base.ActivatePage(previousIndex);
               wizard = (KeyRecoveryWizard)this.Parent;
               GetLoggedInDomains();
               ((KeyRecoveryWizard)this.Parent).WizardButtons = KeyRecoveryWizardButtons.Cancel;
               domainComboBox.Focus();
                UpdateSensitivity();

        }

        private void domainComboBox_SelectedIndexChanged(object sender, EventArgs e)
        {

            UpdateSensitivity();
        }

        private void UpdateSensitivity()
        {

            if ((ifWebService.GetSecurityPolicy(this.SelectedDomain.ID) != 0) && simiasWebService.IsPassPhraseSet(this.SelectedDomain.ID))
            {


                ((KeyRecoveryWizard)this.Parent).WizardButtons = KeyRecoveryWizardButtons.Cancel|KeyRecoveryWizardButtons.Next;
            }

        }


        internal override int ValidatePage(int currentIndex)
        {
            string RAName = this.ifWebService.GetRAName(SelectedDomain.ID);

            if (RAName == "DEFAULT")
                currentIndex = wizard.MaxPages - 9;

            else
                currentIndex = wizard.MaxPages - 7;

            return base.ValidatePage(currentIndex);
        }
    }
}
