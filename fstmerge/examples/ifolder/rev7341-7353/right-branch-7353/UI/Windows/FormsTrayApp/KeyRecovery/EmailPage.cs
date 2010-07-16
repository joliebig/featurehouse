

using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;

namespace Novell.Wizard
{
    public partial class EmailPage : Novell.Wizard.InteriorPageTemplate
    {
        private KeyRecoveryWizard wizard;





        public EmailPage()

        {
           InitializeComponent();
        }





       internal override int DeactivatePage()
  {

   return base.DeactivatePage ();


  }





        internal override void ActivatePage(int previousIndex)
        {
            base.ActivatePage(previousIndex);


           wizard = (KeyRecoveryWizard)this.Parent;
           emailID.Text = wizard.ExportKeyPage.EmailAddress;
           exportPath.Text = wizard.ExportKeyPage.ExportPath;

          ((KeyRecoveryWizard)this.Parent).WizardButtons = KeyRecoveryWizardButtons.Cancel;
        }




        private System.ComponentModel.IContainer components = null;


    }
}
