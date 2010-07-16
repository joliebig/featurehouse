
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;

namespace Novell.Wizard
{
    public partial class InfoPage : Novell.Wizard.InteriorPageTemplate
    {

        private System.ComponentModel.IContainer components = null;






        public InfoPage()
        {
            InitializeComponent();

         }







        internal override void ActivatePage(int previousIndex)
        {
            base.ActivatePage(previousIndex);


            ((KeyRecoveryWizard)this.Parent).WizardButtons = KeyRecoveryWizardButtons.Next | KeyRecoveryWizardButtons.Cancel;

        }






        internal override int ValidatePage(int currentIndex)
        {

            return base.ValidatePage(currentIndex);
        }





        internal override int DeactivatePage()
        {

            return base.DeactivatePage();

        }

    }
}
