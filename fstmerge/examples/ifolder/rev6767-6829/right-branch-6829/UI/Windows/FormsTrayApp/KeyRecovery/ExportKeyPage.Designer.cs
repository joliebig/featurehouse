using System;
using System.Windows.Forms;
using System.Drawing;
using System.ComponentModel;
using System.Collections;

namespace Novell.Wizard
{
 public partial class ExportKeyPage : Novell.Wizard.InteriorPageTemplate
 {


  private void InitializeComponent()
        {
            this.label2 = new System.Windows.Forms.Label();
            this.label3 = new System.Windows.Forms.Label();
            this.filePath = new System.Windows.Forms.TextBox();
            this.BrowseButton = new System.Windows.Forms.Button();
            this.recoveryAgentLabel = new System.Windows.Forms.Label();
            this.recoveryAgent = new System.Windows.Forms.TextBox();
            this.label1 = new System.Windows.Forms.Label();
            this.accountBox = new System.Windows.Forms.TextBox();
            this.SuspendLayout();



            this.label2.AutoSize = true;
            this.label2.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label2.Location = new System.Drawing.Point(37, 105);
            this.label2.Margin = new System.Windows.Forms.Padding(4, 0, 4, 0);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(86, 13);
            this.label2.TabIndex = 1;
            this.label2.Text = TrayApp.Properties.Resources.iFolderAcc;



            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(22, 168);
            this.label3.Margin = new System.Windows.Forms.Padding(4, 0, 4, 0);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(146, 13);
            this.label3.TabIndex = 3;
            this.label3.Text = TrayApp.Properties.Resources.exportPageFilePath;



            this.filePath.Location = new System.Drawing.Point(175, 161);
            this.filePath.Name = "filePath";
            this.filePath.Size = new System.Drawing.Size(223, 20);
            this.filePath.TabIndex = 4;
            this.filePath.TextChanged += new System.EventHandler(this.filePath_TextChanged);



            this.BrowseButton.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.BrowseButton.Location = new System.Drawing.Point(413, 161);
            this.BrowseButton.Name = "BrowseButton";
            this.BrowseButton.Size = new System.Drawing.Size(57, 20);
            this.BrowseButton.TabIndex = 5;
            this.BrowseButton.Text = global::TrayApp.Properties.Resources.browseText;
            this.BrowseButton.UseVisualStyleBackColor = true;
            this.BrowseButton.Click += new System.EventHandler(this.BrowseButton_Click);



            this.recoveryAgentLabel.AutoSize = true;
            this.recoveryAgentLabel.Location = new System.Drawing.Point(37, 136);
            this.recoveryAgentLabel.Name = "recoveryAgentLabel";
            this.recoveryAgentLabel.Size = new System.Drawing.Size(89, 13);
            this.recoveryAgentLabel.TabIndex = 6;
            this.recoveryAgentLabel.Text = TrayApp.Properties.Resources.exportPageRA;



            this.recoveryAgent.Location = new System.Drawing.Point(174, 129);
            this.recoveryAgent.Name = "recoveryAgent";
            this.recoveryAgent.ReadOnly = true;
            this.recoveryAgent.Size = new System.Drawing.Size(223, 20);
            this.recoveryAgent.TabIndex = 8;



            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(34, 72);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(301, 13);
            this.label1.TabIndex = 9;
            this.label1.Text = TrayApp.Properties.Resources.exportPageFirst;



            this.accountBox.Location = new System.Drawing.Point(174, 98);
            this.accountBox.Name = "accountBox";
            this.accountBox.ReadOnly = true;
            this.accountBox.Size = new System.Drawing.Size(223, 20);
            this.accountBox.TabIndex = 10;



            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.accountBox);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.recoveryAgent);
            this.Controls.Add(this.recoveryAgentLabel);
            this.Controls.Add(this.BrowseButton);
            this.Controls.Add(this.filePath);
            this.Controls.Add(this.label3);
            this.Controls.Add(this.label2);
            this.Name = "ExportKeyPage";
            this.Size = new System.Drawing.Size(500, 271);
            this.Controls.SetChildIndex(this.label2, 0);
            this.Controls.SetChildIndex(this.label3, 0);
            this.Controls.SetChildIndex(this.filePath, 0);
            this.Controls.SetChildIndex(this.BrowseButton, 0);
            this.Controls.SetChildIndex(this.recoveryAgentLabel, 0);
            this.Controls.SetChildIndex(this.recoveryAgent, 0);
            this.Controls.SetChildIndex(this.label1, 0);
            this.Controls.SetChildIndex(this.accountBox, 0);
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

        private TextBox accountBox;
 }
}
