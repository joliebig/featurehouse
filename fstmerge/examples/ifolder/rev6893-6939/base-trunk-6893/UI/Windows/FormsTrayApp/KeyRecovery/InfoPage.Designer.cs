using System;
using System.Windows.Forms;
using System.Drawing;
using System.ComponentModel;
using System.Collections;

namespace Novell.Wizard
{
 public partial class InfoPage
 {


        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.Label label5;

  private void InitializeComponent()
        {
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(InfoPage));
            this.label2 = new System.Windows.Forms.Label();
            this.label5 = new System.Windows.Forms.Label();
            this.SuspendLayout();



            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(17, 88);
            this.label2.MaximumSize = new System.Drawing.Size(457, 0);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(447, 26);
            this.label2.TabIndex = 3;
            this.label2.Text = TrayApp.Properties.Resources.infoPageFirst;



            this.label5.AutoSize = true;
            this.label5.Location = new System.Drawing.Point(17, 133);
            this.label5.MaximumSize = new System.Drawing.Size(457, 0);
            this.label5.Name = "label5";
            this.label5.Size = new System.Drawing.Size(456, 39);
            this.label5.TabIndex = 6;
            this.label5.Text = TrayApp.Properties.Resources.infoPageSecond;



            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.label5);
            this.Controls.Add(this.label2);
            this.Name = "InfoPage";
            this.Size = new System.Drawing.Size(517, 340);
            this.Controls.SetChildIndex(this.label2, 0);
            this.Controls.SetChildIndex(this.label5, 0);
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
