namespace Novell.Wizard
{
    partial class EnterPassphrasePage
    {
        private System.ComponentModel.IContainer components = null;
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }
        private void InitializeComponent()
        {
            this.label1 = new System.Windows.Forms.Label();
            this.label2 = new System.Windows.Forms.Label();
            this.newPassphrase = new System.Windows.Forms.TextBox();
            this.confirmPassphrase = new System.Windows.Forms.TextBox();
            this.label3 = new System.Windows.Forms.Label();
            this.iFolderAcc = new System.Windows.Forms.TextBox();
            this.loginErrorLabel = new System.Windows.Forms.Label();
            this.userNameLabel = new System.Windows.Forms.Label();
            this.userName = new System.Windows.Forms.TextBox();
            this.passwordLabel = new System.Windows.Forms.Label();
            this.password = new System.Windows.Forms.TextBox();
            this.SuspendLayout();
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(48, 97);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(86, 13);
            this.label1.TabIndex = 2;
            this.label1.Text = TrayApp.Properties.Resources.iFolderAcc;
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(48, 131);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(89, 13);
            this.label2.TabIndex = 4;
            this.label2.Text = TrayApp.Properties.Resources.newPassphrase;
            this.newPassphrase.Location = new System.Drawing.Point(188, 124);
            this.newPassphrase.Name = "newPassphrase";
            this.newPassphrase.PasswordChar = '*';
            this.newPassphrase.Size = new System.Drawing.Size(209, 20);
            this.newPassphrase.TabIndex = 5;
            this.newPassphrase.TextChanged += new System.EventHandler(this.newPassphrase_TextChanged);
            this.confirmPassphrase.Location = new System.Drawing.Point(188, 159);
            this.confirmPassphrase.Name = "confirmPassphrase";
            this.confirmPassphrase.PasswordChar = '*';
            this.confirmPassphrase.Size = new System.Drawing.Size(211, 20);
            this.confirmPassphrase.TabIndex = 6;
            this.confirmPassphrase.TextChanged += new System.EventHandler(this.confirmPassphrase_TextChanged);
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(48, 166);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(102, 13);
            this.label3.TabIndex = 7;
            this.label3.Text = TrayApp.Properties.Resources.confirmPassphrase;
            this.iFolderAcc.Location = new System.Drawing.Point(188, 90);
            this.iFolderAcc.Name = "iFolderAcc";
            this.iFolderAcc.ReadOnly = true;
            this.iFolderAcc.Size = new System.Drawing.Size(211, 20);
            this.iFolderAcc.TabIndex = 8;
            this.loginErrorLabel.AutoSize = true;
            this.loginErrorLabel.Location = new System.Drawing.Point(56, 207);
            this.loginErrorLabel.Name = "loginErrorLabel";
            this.loginErrorLabel.Size = new System.Drawing.Size(0, 13);
            this.loginErrorLabel.TabIndex = 9;
            this.userNameLabel.AutoSize = true;
            this.userNameLabel.Location = new System.Drawing.Point(48, 201);
            this.userNameLabel.Name = "userNameLabel";
            this.userNameLabel.Size = new System.Drawing.Size(61, 13);
            this.userNameLabel.TabIndex = 10;
            this.userNameLabel.Text = TrayApp.Properties.Resources.userNameText;
            this.userName.Location = new System.Drawing.Point(188, 194);
            this.userName.Name = "userName";
            this.userName.ReadOnly = true;
            this.userName.Size = new System.Drawing.Size(211, 20);
            this.userName.TabIndex = 11;
            this.userName.TextChanged += new System.EventHandler(this.userName_TextChanged);
            this.passwordLabel.AutoSize = true;
            this.passwordLabel.Location = new System.Drawing.Point(48, 234);
            this.passwordLabel.Name = "passwordLabel";
            this.passwordLabel.Size = new System.Drawing.Size(56, 13);
            this.passwordLabel.TabIndex = 12;
            this.passwordLabel.Text = TrayApp.Properties.Resources.passwordText;
            this.password.Location = new System.Drawing.Point(188, 234);
            this.password.Name = "password";
            this.password.PasswordChar = '*';
            this.password.Size = new System.Drawing.Size(211, 20);
            this.password.TabIndex = 13;
            this.password.TextChanged += new System.EventHandler(this.password_TextChanged);
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.password);
            this.Controls.Add(this.passwordLabel);
            this.Controls.Add(this.userName);
            this.Controls.Add(this.userNameLabel);
            this.Controls.Add(this.loginErrorLabel);
            this.Controls.Add(this.newPassphrase);
            this.Controls.Add(this.label2);
            this.Controls.Add(this.iFolderAcc);
            this.Controls.Add(this.label3);
            this.Controls.Add(this.confirmPassphrase);
            this.Controls.Add(this.label1);
            this.Name = "EnterPassphrasePage";
            this.Controls.SetChildIndex(this.label1, 0);
            this.Controls.SetChildIndex(this.confirmPassphrase, 0);
            this.Controls.SetChildIndex(this.label3, 0);
            this.Controls.SetChildIndex(this.iFolderAcc, 0);
            this.Controls.SetChildIndex(this.label2, 0);
            this.Controls.SetChildIndex(this.newPassphrase, 0);
            this.Controls.SetChildIndex(this.loginErrorLabel, 0);
            this.Controls.SetChildIndex(this.userNameLabel, 0);
            this.Controls.SetChildIndex(this.userName, 0);
            this.Controls.SetChildIndex(this.passwordLabel, 0);
            this.Controls.SetChildIndex(this.password, 0);
            this.ResumeLayout(false);
            this.PerformLayout();
        }
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.TextBox newPassphrase;
        private System.Windows.Forms.TextBox confirmPassphrase;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.TextBox iFolderAcc;
        private System.Windows.Forms.Label loginErrorLabel;
        private System.Windows.Forms.Label userNameLabel;
        private System.Windows.Forms.TextBox userName;
        private System.Windows.Forms.Label passwordLabel;
        private System.Windows.Forms.TextBox password;
    }
}
