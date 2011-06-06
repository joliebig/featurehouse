using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Novell.FormsTrayApp;
namespace Novell.Wizard
{
 public class MigrationCompletionPage : Novell.Wizard.MigrationWelcomePageTemplate
 {
  private System.ComponentModel.IContainer components = null;
  private static System.Resources.ResourceManager Resource = new System.Resources.ResourceManager(typeof(Novell.FormsTrayApp.FormsTrayApp));
  public MigrationCompletionPage()
  {
   InitializeComponent();
  }
  private void InitializeComponent()
  {
   this.Name = "CompletionPage";
   this.WelcomeTitle = Resource.GetString("MigrationCompletionWT");
  }
  internal override void ActivatePage(int previousIndex)
  {
   ((MigrationWizard)(this.Parent)).MigrationWizardButtons = MigrationWizardButtons.Next;
   base.ActivatePage (previousIndex);
   this.DescriptionText = ((MigrationWizard)(this.Parent)).SummaryText;
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
