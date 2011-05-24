

using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;

namespace Novell.InvitationWizard
{



 public class WelcomePage : Novell.InvitationWizard.WelcomePageTemplate
 {

  private System.ComponentModel.IContainer components = null;





  public WelcomePage()
  {

   InitializeComponent();


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






  private void InitializeComponent()
  {



   this.DescriptionText = "Description...";
   this.Name = "WelcomePage";
   this.WelcomeTitle = "Welcome to the iFolder Invitation Wizard";

  }



  internal override int ValidatePage(int currentIndex)
  {
   if (((InvitationWizard)(this.Parent)).Subscription != null)
    return currentIndex + 2;

   return base.ValidatePage (currentIndex);
  }

  internal override void ActivatePage(int previousIndex)
  {

   ((InvitationWizard)(this.Parent)).WizardButtons = WizardButtons.Next | WizardButtons.Cancel;

   base.ActivatePage (previousIndex);
  }

 }
}
