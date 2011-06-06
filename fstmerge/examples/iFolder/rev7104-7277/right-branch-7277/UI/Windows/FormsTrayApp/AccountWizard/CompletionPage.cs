using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Novell.FormsTrayApp;
namespace Novell.Wizard
{
 public class CompletionPage : Novell.Wizard.WelcomePageTemplate
 {
  private System.ComponentModel.IContainer components = null;
  private static System.Resources.ResourceManager Resource = new System.Resources.ResourceManager(typeof(Novell.FormsTrayApp.FormsTrayApp));
  public CompletionPage()
  {
   InitializeComponent();
  }
  private void InitializeComponent()
  {
   this.Name = "CompletionPage";
   this.WelcomeTitle = Resource.GetString("CompletionPageWT");
  }
        internal override void ActivatePage(int previousIndex)
  {
   base.ActivatePage (previousIndex);
   ((AccountWizard)this.Parent).WizardButtons = WizardButtons.Next;
   this.DescriptionText = ((AccountWizard)(this.Parent)).SummaryText;
   this.ActionText = Resource.GetString("CompletionPageAT");
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
 }
}
