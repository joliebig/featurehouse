

using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Novell.FormsTrayApp;

namespace Novell.Wizard
{



 public class MigrationWelcomePage : Novell.Wizard.MigrationWelcomePageTemplate
 {

  private System.ComponentModel.IContainer components = null;
  private static System.Resources.ResourceManager Resource = new System.Resources.ResourceManager(typeof(Novell.FormsTrayApp.FormsTrayApp));





  public MigrationWelcomePage()
  {


   InitializeComponent();


  }






  private void InitializeComponent()
  {





   this.DescriptionText = Resource.GetString("CompletionPageDT");
   this.Name = "MigrationWelcomePage";
   this.WelcomeTitle = Resource.GetString("MigrationWelcomeTitle");

  }
  internal override void ActivatePage(int previousIndex)
  {
   base.ActivatePage (previousIndex);
   ((MigrationWizard)(this.Parent)).MigrationWizardButtons = MigrationWizardButtons.Next | MigrationWizardButtons.Cancel;
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
        internal override int ValidatePage(int currentIndex)
  {
   return base.ValidatePage (currentIndex);
  }
 }
}
