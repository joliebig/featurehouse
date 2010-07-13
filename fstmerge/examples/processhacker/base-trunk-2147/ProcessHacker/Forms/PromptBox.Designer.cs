namespace ProcessHacker
{
    partial class PromptBox
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
            this.labelValue = new System.Windows.Forms.Label();
            this.textValue = new System.Windows.Forms.TextBox();
            this.buttonOK = new System.Windows.Forms.Button();
            this.buttonCancel = new System.Windows.Forms.Button();
            this.SuspendLayout();



            this.labelValue.AutoSize = true;
            this.labelValue.Location = new System.Drawing.Point(12, 15);
            this.labelValue.Name = "labelValue";
            this.labelValue.Size = new System.Drawing.Size(37, 13);
            this.labelValue.TabIndex = 3;
            this.labelValue.Text = "Value:";



            this.textValue.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.textValue.Location = new System.Drawing.Point(55, 12);
            this.textValue.Name = "textValue";
            this.textValue.Size = new System.Drawing.Size(319, 20);
            this.textValue.TabIndex = 0;



            this.buttonOK.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.buttonOK.FlatStyle = System.Windows.Forms.FlatStyle.System;
            this.buttonOK.Location = new System.Drawing.Point(218, 38);
            this.buttonOK.Name = "buttonOK";
            this.buttonOK.Size = new System.Drawing.Size(75, 23);
            this.buttonOK.TabIndex = 1;
            this.buttonOK.Text = "&OK";
            this.buttonOK.UseVisualStyleBackColor = true;
            this.buttonOK.Click += new System.EventHandler(this.buttonOK_Click);



            this.buttonCancel.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.buttonCancel.FlatStyle = System.Windows.Forms.FlatStyle.System;
            this.buttonCancel.Location = new System.Drawing.Point(299, 38);
            this.buttonCancel.Name = "buttonCancel";
            this.buttonCancel.Size = new System.Drawing.Size(75, 23);
            this.buttonCancel.TabIndex = 2;
            this.buttonCancel.Text = "&Cancel";
            this.buttonCancel.UseVisualStyleBackColor = true;
            this.buttonCancel.Click += new System.EventHandler(this.buttonCancel_Click);



            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(386, 73);
            this.ControlBox = false;
            this.Controls.Add(this.buttonCancel);
            this.Controls.Add(this.buttonOK);
            this.Controls.Add(this.textValue);
            this.Controls.Add(this.labelValue);
            this.Name = "PromptBox";
            this.ShowIcon = false;
            this.ShowInTaskbar = false;
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "Enter Value";
            this.ResumeLayout(false);
            this.PerformLayout();

        }



        private System.Windows.Forms.Label labelValue;
        private System.Windows.Forms.TextBox textValue;
        private System.Windows.Forms.Button buttonOK;
        private System.Windows.Forms.Button buttonCancel;
    }
}
