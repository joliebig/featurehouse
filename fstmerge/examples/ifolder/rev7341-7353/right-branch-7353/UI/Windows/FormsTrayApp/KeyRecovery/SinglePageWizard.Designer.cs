using System;
using System.Windows.Forms;
using System.Drawing;
using System.ComponentModel;
using System.Collections;

namespace Novell.Wizard
{
 public partial class SinglePageWizard
 {


        private System.Windows.Forms.Label account1Label;
        private System.Windows.Forms.Label pi2label;
        private System.Windows.Forms.Label domainPasswdLabel;
        private System.Windows.Forms.Label newPassphraseLabel;
        private System.Windows.Forms.Label confirmPassphraseLabel;
        private System.Windows.Forms.TextBox p12TextBox;
        private System.Windows.Forms.TextBox passwdDomainTextBox;
        private System.Windows.Forms.TextBox newPassphrase;
        private System.Windows.Forms.TextBox confirmPassphrase;


  private void InitializeComponent()
        {
            this.account1Label = new System.Windows.Forms.Label();
            this.pi2label = new System.Windows.Forms.Label();
            this.domainPasswdLabel = new System.Windows.Forms.Label();
            this.newPassphraseLabel = new System.Windows.Forms.Label();
            this.confirmPassphraseLabel = new System.Windows.Forms.Label();
            this.p12TextBox = new System.Windows.Forms.TextBox();
            this.passwdDomainTextBox = new System.Windows.Forms.TextBox();
            this.newPassphrase = new System.Windows.Forms.TextBox();
            this.confirmPassphrase = new System.Windows.Forms.TextBox();
            this.browse = new System.Windows.Forms.Button();
            this.accountBox = new System.Windows.Forms.TextBox();
            this.label1 = new System.Windows.Forms.Label();
            this.SuspendLayout();



            this.account1Label.AutoSize = true;
            this.account1Label.Location = new System.Drawing.Point(43, 99);
            this.account1Label.Name = "account1Label";
            this.account1Label.Size = new System.Drawing.Size(86, 13);
            this.account1Label.TabIndex = 0;
            this.account1Label.Text = TrayApp.Properties.Resources.iFolderAcc;



            this.pi2label.AutoSize = true;
            this.pi2label.Location = new System.Drawing.Point(43, 127);
            this.pi2label.Name = "pi2label";
            this.pi2label.Size = new System.Drawing.Size(117, 13);
            this.pi2label.TabIndex = 1;
            this.pi2label.Text = TrayApp.Properties.Resources.singleWizP12File;



            this.domainPasswdLabel.AutoSize = true;
            this.domainPasswdLabel.Location = new System.Drawing.Point(43, 162);
            this.domainPasswdLabel.Name = "domainPasswdLabel";
            this.domainPasswdLabel.Size = new System.Drawing.Size(105, 13);
            this.domainPasswdLabel.TabIndex = 2;
            this.domainPasswdLabel.Text = TrayApp.Properties.Resources.singleWizPasswd;



            this.newPassphraseLabel.AutoSize = true;
            this.newPassphraseLabel.Location = new System.Drawing.Point(43, 192);
            this.newPassphraseLabel.Name = "newPassphraseLabel";
            this.newPassphraseLabel.Size = new System.Drawing.Size(89, 13);
            this.newPassphraseLabel.TabIndex = 3;
            this.newPassphraseLabel.Text = TrayApp.Properties.Resources.newPassphrase;



            this.confirmPassphraseLabel.AutoSize = true;
            this.confirmPassphraseLabel.Location = new System.Drawing.Point(43, 223);
            this.confirmPassphraseLabel.Name = "confirmPassphraseLabel";
            this.confirmPassphraseLabel.Size = new System.Drawing.Size(102, 13);
            this.confirmPassphraseLabel.TabIndex = 4;
            this.confirmPassphraseLabel.Text = TrayApp.Properties.Resources.confirmPassphrase;



            this.p12TextBox.Location = new System.Drawing.Point(206, 123);
            this.p12TextBox.Name = "p12TextBox";
            this.p12TextBox.Size = new System.Drawing.Size(199, 20);
            this.p12TextBox.TabIndex = 6;
            this.p12TextBox.TextChanged += new System.EventHandler(this.p12TextBox_TextChanged);



            this.passwdDomainTextBox.Location = new System.Drawing.Point(206, 155);
            this.passwdDomainTextBox.Name = "passwdDomainTextBox";
            this.passwdDomainTextBox.PasswordChar = '*';
            this.passwdDomainTextBox.Size = new System.Drawing.Size(198, 20);
            this.passwdDomainTextBox.TabIndex = 7;
            this.passwdDomainTextBox.TextChanged += new System.EventHandler(this.passwdDomainTextBox_TextChanged);



            this.newPassphrase.Location = new System.Drawing.Point(205, 185);
            this.newPassphrase.Name = "newPassphrase";
            this.newPassphrase.PasswordChar = '*';
            this.newPassphrase.Size = new System.Drawing.Size(199, 20);
            this.newPassphrase.TabIndex = 8;
            this.newPassphrase.TextChanged += new System.EventHandler(this.newPassphrase_TextChanged);



            this.confirmPassphrase.Location = new System.Drawing.Point(205, 216);
            this.confirmPassphrase.Name = "confirmPassphrase";
            this.confirmPassphrase.PasswordChar = '*';
            this.confirmPassphrase.Size = new System.Drawing.Size(198, 20);
            this.confirmPassphrase.TabIndex = 9;
            this.confirmPassphrase.TextChanged += new System.EventHandler(this.confirmPassphrase_TextChanged);



            this.browse.Location = new System.Drawing.Point(411, 123);
            this.browse.Name = "browse";
            this.browse.Size = new System.Drawing.Size(54, 20);
            this.browse.TabIndex = 10;
            this.browse.Text = global::TrayApp.Properties.Resources.browseText;
            this.browse.UseVisualStyleBackColor = true;
            this.browse.Click += new System.EventHandler(this.browse_Click);



            this.accountBox.Location = new System.Drawing.Point(206, 92);
            this.accountBox.Name = "accountBox";
            this.accountBox.ReadOnly = true;
            this.accountBox.Size = new System.Drawing.Size(198, 20);
            this.accountBox.TabIndex = 11;



            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(54, 66);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(277, 13);
            this.label1.TabIndex = 12;
            this.label1.Text = TrayApp.Properties.Resources.singleWizPageFirst;




            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.label1);
            this.Controls.Add(this.accountBox);
            this.Controls.Add(this.browse);
            this.Controls.Add(this.confirmPassphrase);
            this.Controls.Add(this.newPassphrase);
            this.Controls.Add(this.passwdDomainTextBox);
            this.Controls.Add(this.p12TextBox);
            this.Controls.Add(this.confirmPassphraseLabel);
            this.Controls.Add(this.newPassphraseLabel);
            this.Controls.Add(this.domainPasswdLabel);
            this.Controls.Add(this.pi2label);
            this.Controls.Add(this.account1Label);
            this.Name = "SinglePageWizard";
            this.Size = new System.Drawing.Size(505, 290);
            this.Controls.SetChildIndex(this.account1Label, 0);
            this.Controls.SetChildIndex(this.pi2label, 0);
            this.Controls.SetChildIndex(this.domainPasswdLabel, 0);
            this.Controls.SetChildIndex(this.newPassphraseLabel, 0);
            this.Controls.SetChildIndex(this.confirmPassphraseLabel, 0);
            this.Controls.SetChildIndex(this.p12TextBox, 0);
            this.Controls.SetChildIndex(this.passwdDomainTextBox, 0);
            this.Controls.SetChildIndex(this.newPassphrase, 0);
            this.Controls.SetChildIndex(this.confirmPassphrase, 0);
            this.Controls.SetChildIndex(this.browse, 0);
            this.Controls.SetChildIndex(this.accountBox, 0);
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

        private TextBox accountBox;
        private Label label1;
 }
}
