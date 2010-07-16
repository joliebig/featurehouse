using System;
using System.Windows.Forms;
using System.Drawing;
using System.ComponentModel;
using System.Collections;

namespace Novell.Wizard
{
 public partial class EmailPage
 {

  private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Label label2;

  private void InitializeComponent()
        {
            this.label1 = new System.Windows.Forms.Label();
            this.label2 = new System.Windows.Forms.Label();
            this.label4 = new System.Windows.Forms.Label();
            this.exportPath = new System.Windows.Forms.Label();
            this.emailID = new System.Windows.Forms.Label();
            this.SuspendLayout();



            this.label1.AutoSize = true;
            this.label1.Cursor = System.Windows.Forms.Cursors.Default;
            this.label1.Location = new System.Drawing.Point(25, 87);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(336, 13);
            this.label1.TabIndex = 2;
            this.label1.Text = TrayApp.Properties.Resources.emailPageFirst;



            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(26, 135);
            this.label2.MaximumSize = new System.Drawing.Size(457, 0);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(455, 26);
            this.label2.TabIndex = 3;
            this.label2.Text = TrayApp.Properties.Resources.emailPageSecond;



            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(74, 181);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(69, 13);
            this.label4.TabIndex = 5;
            this.label4.Text = TrayApp.Properties.Resources.emailAddress;



            this.exportPath.AutoSize = true;
            this.exportPath.Location = new System.Drawing.Point(107, 112);
            this.exportPath.Name = "exportPath";
            this.exportPath.Size = new System.Drawing.Size(84, 13);
            this.exportPath.TabIndex = 7;
            this.exportPath.Text = "exportPathLabel";



            this.emailID.AutoSize = true;
            this.emailID.Location = new System.Drawing.Point(196, 181);
            this.emailID.Name = "emailID";
            this.emailID.Size = new System.Drawing.Size(95, 13);
            this.emailID.TabIndex = 8;
            this.emailID.Text = "emailAddressLabel";



            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.emailID);
            this.Controls.Add(this.exportPath);
            this.Controls.Add(this.label4);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.label2);
            this.Name = "EmailPage";
            this.Controls.SetChildIndex(this.label2, 0);
            this.Controls.SetChildIndex(this.label1, 0);
            this.Controls.SetChildIndex(this.label4, 0);
            this.Controls.SetChildIndex(this.exportPath, 0);
            this.Controls.SetChildIndex(this.emailID, 0);
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

        private Label label4;
        private Label exportPath;
        private Label emailID;
 }
}
