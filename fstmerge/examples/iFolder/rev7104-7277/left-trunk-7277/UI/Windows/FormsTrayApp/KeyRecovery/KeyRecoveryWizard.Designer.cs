using System;
using System.Windows.Forms;
using System.Drawing;
using System.ComponentModel;
using System.Collections;
namespace Novell.Wizard
{
 public partial class KeyRecoveryWizard
 {
  private void InitializeComponent()
        {
            this.cancel = new System.Windows.Forms.Button();
            this.next = new System.Windows.Forms.Button();
            this.back = new System.Windows.Forms.Button();
            this.btnHelp = new System.Windows.Forms.Button();
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.SuspendLayout();
            this.cancel.FlatStyle = System.Windows.Forms.FlatStyle.System;
            this.cancel.ImeMode = System.Windows.Forms.ImeMode.NoControl;
            this.cancel.Location = new System.Drawing.Point(416, 318);
            this.cancel.Name = TrayApp.Properties.Resources.cancel;
            this.cancel.Size = new System.Drawing.Size(72, 23);
            this.cancel.TabIndex = 3;
            this.cancel.Text = TrayApp.Properties.Resources.cancelText;
            this.cancel.Click += new EventHandler(cancel_Click);
            this.next.FlatStyle = System.Windows.Forms.FlatStyle.System;
            this.next.ImeMode = System.Windows.Forms.ImeMode.NoControl;
            this.next.Location = new System.Drawing.Point(333, 318);
            this.next.Name = TrayApp.Properties.Resources.next;
            this.next.Size = new System.Drawing.Size(72, 23);
            this.next.TabIndex = 2;
            this.next.Text = TrayApp.Properties.Resources.nextText + " >";
            this.next.Click += new System.EventHandler(this.next_Click);
            this.back.FlatStyle = System.Windows.Forms.FlatStyle.System;
            this.back.ImeMode = System.Windows.Forms.ImeMode.NoControl;
            this.back.Location = new System.Drawing.Point(253, 318);
            this.back.Name = TrayApp.Properties.Resources.back;
            this.back.Size = new System.Drawing.Size(72, 23);
            this.back.TabIndex = 1;
            this.back.Text = "< " + TrayApp.Properties.Resources.backText;
            this.back.Click += new System.EventHandler(this.back_Click);
            this.btnHelp.FlatStyle = System.Windows.Forms.FlatStyle.System;
            this.btnHelp.ImeMode = System.Windows.Forms.ImeMode.NoControl;
            this.btnHelp.Location = new System.Drawing.Point(16, 318);
            this.btnHelp.Name = TrayApp.Properties.Resources.help;
            this.btnHelp.Size = new System.Drawing.Size(72, 23);
            this.btnHelp.TabIndex = 1;
            this.btnHelp.Text = TrayApp.Properties.Resources.helpText;
            this.btnHelp.Click += new System.EventHandler(this.help_Click);
            this.groupBox1.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)
                | System.Windows.Forms.AnchorStyles.Right)));
            this.groupBox1.Location = new System.Drawing.Point(0, 302);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(496, 4);
            this.groupBox1.TabIndex = 4;
            this.groupBox1.TabStop = false;
            this.groupBox1.Text = "";
            this.AcceptButton = this.next;
            this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
            this.ClientSize = new System.Drawing.Size(496, 348);
            this.ControlBox = false;
            this.Controls.Add(this.groupBox1);
            this.Controls.Add(this.btnHelp);
            this.Controls.Add(this.back);
            this.Controls.Add(this.next);
            this.Controls.Add(this.cancel);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle;
            this.ImeMode = System.Windows.Forms.ImeMode.NoControl;
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = TrayApp.Properties.Resources.wizardName;
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = TrayApp.Properties.Resources.wizardText;
            this.next.Select();
            this.ResumeLayout(false);
        }
  protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                if (components != null)
                {
                    components.Dispose();
                }
            }
            base.Dispose(disposing);
        }
 }
}
