using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Windows.Forms;
namespace Novell.Wizard
{
 public class BaseWizardPage : System.Windows.Forms.UserControl
 {
  private System.ComponentModel.Container components = null;
  protected int previousIndex = 0;
  public BaseWizardPage()
  {
   InitializeComponent();
  }
  protected override void Dispose( bool disposing )
  {
   if( disposing )
   {
    if(components != null)
    {
     components.Dispose();
    }
   }
   base.Dispose( disposing );
  }
  private void InitializeComponent()
  {
            this.SuspendLayout();
            this.BackColor = System.Drawing.SystemColors.Control;
            this.Name = "BaseWizardPage";
            this.Size = new System.Drawing.Size(496, 304);
            this.ResumeLayout(false);
  }
  internal virtual int ValidatePage(int currentIndex)
  {
   return ++currentIndex;
  }
  internal virtual void ActivatePage(int previousIndex)
  {
   if (previousIndex > 0)
    this.previousIndex = previousIndex;
   this.Show();
  }
  internal virtual int DeactivatePage()
  {
   this.Hide();
   return this.previousIndex;
  }
 }
}
