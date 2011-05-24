
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;
using TrayApp.Properties;

namespace Novell.Wizard
{
    public partial class SelectionPage: Novell.Wizard.InteriorPageTemplate
    {




        private System.ComponentModel.IContainer components = null;

        private KeyRecoveryWizard wizard;






        public SelectionPage()
        {
            InitializeComponent();

        }





        internal override void ActivatePage(int previousIndex)
        {
            base.ActivatePage(previousIndex);
            wizard = (KeyRecoveryWizard)this.Parent;
            ((KeyRecoveryWizard)this.Parent).WizardButtons = KeyRecoveryWizardButtons.Cancel | KeyRecoveryWizardButtons.Back;
             UpdateSensitivity();
        }




        private void UpdateSensitivity()
        {
            if ((singleWizRadio.Checked == true )|| (exportRadio.Checked == true) || (importRadio.Checked == true))
            {
                ((KeyRecoveryWizard)this.Parent).WizardButtons = KeyRecoveryWizardButtons.Next | KeyRecoveryWizardButtons.Back | KeyRecoveryWizardButtons.Cancel;
            }
        }







        internal override int ValidatePage(int currentIndex)
        {



           if (singleWizRadio.Checked == true)
            {
                currentIndex = wizard.MaxPages - 6;
            }

            else if (exportRadio.Checked == true)
            {
                currentIndex = wizard.MaxPages - 3;
            }

            else if (importRadio.Checked == true)
            {
                currentIndex = wizard.MaxPages - 5;
            }
            return base.ValidatePage(currentIndex);
        }





        internal override int DeactivatePage()
        {

            return base.DeactivatePage();

        }






        private void exportRadio_CheckedChanged(object sender, EventArgs e)
        {
            UpdateSensitivity();
        }






        private void recoverButton_CheckedChanged(object sender, EventArgs e)
        {
            UpdateSensitivity();

        }






        private void importradio_CheckedChanged(object sender, EventArgs e)
        {
            UpdateSensitivity();
        }

    }
}
