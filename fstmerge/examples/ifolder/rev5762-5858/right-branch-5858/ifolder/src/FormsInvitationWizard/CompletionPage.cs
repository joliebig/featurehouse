

using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;

namespace Novell.InvitationWizard
{



 public class CompletionPage : Novell.InvitationWizard.WelcomePageTemplate
 {

  private System.ComponentModel.IContainer components = null;





  public CompletionPage()
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



   this.Name = "CompletionPage";
   this.WelcomeTitle = "Completing the iFolder Invitation Wizard";

  }



  internal override void ActivatePage(int previousIndex)
  {
   this.DescriptionText = ((InvitationWizard)(this.Parent)).SummaryText;
   this.ActionText = "Click Finish to process your response, or click Back to modify the settings. Click Cancel to exit without responding.";
   base.ActivatePage (previousIndex);
  }

 }
}
