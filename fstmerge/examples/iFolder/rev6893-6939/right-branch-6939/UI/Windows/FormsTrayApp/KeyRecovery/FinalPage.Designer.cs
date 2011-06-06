using System;
using System.Windows.Forms;
using System.Drawing;
using System.ComponentModel;
using System.Collections;
namespace Novell.Wizard
{
 public partial class FinalPage
 {
  private System.Windows.Forms.Label label1;
  private void InitializeComponent()
        {
            this.label1 = new System.Windows.Forms.Label();
            this.SuspendLayout();
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(25, 105);
            this.label1.MaximumSize = new System.Drawing.Size(457, 0);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(426, 26);
            this.label1.TabIndex = 2;
            this.label1.Text = "Congratulations!! You have successfully reset the passphrase.Now,you can use the " +
                "new passphrase to access your data.";
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.label1);
            this.Name = "FinalPage";
            this.Size = new System.Drawing.Size(480, 307);
            this.Controls.SetChildIndex(this.label1, 0);
            this.ResumeLayout(false);
            this.PerformLayout();
        }
  protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }
 }
}
