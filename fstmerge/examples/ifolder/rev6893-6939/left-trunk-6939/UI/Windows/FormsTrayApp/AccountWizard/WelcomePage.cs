

using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Novell.FormsTrayApp;

namespace Novell.Wizard
{



 public class WelcomePage : Novell.Wizard.WelcomePageTemplate
 {

  private System.ComponentModel.IContainer components = null;
  private static System.Resources.ResourceManager Resource = new System.Resources.ResourceManager(typeof(Novell.FormsTrayApp.FormsTrayApp));





  public WelcomePage()
  {

   InitializeComponent();


  }






  private void InitializeComponent()
  {



   this.DescriptionText = Resource.GetString("CompletionPageDT");
   this.Name = "WelcomePage";
   this.WelcomeTitle = Resource.GetString("WelcomePageTitle");

  }
  internal override void ActivatePage(int previousIndex)
  {
   base.ActivatePage (previousIndex);
   ((AccountWizard)(this.Parent)).WizardButtons = WizardButtons.Next | WizardButtons.Cancel;
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
