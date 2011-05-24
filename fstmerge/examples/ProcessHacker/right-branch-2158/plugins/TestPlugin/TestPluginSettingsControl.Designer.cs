namespace TestPlugin
{
    partial class TestPluginSettingsControl
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
            this.buttonTest = new System.Windows.Forms.Button();
            this.textSomeText = new System.Windows.Forms.TextBox();
            this.SuspendLayout();



            this.buttonTest.Location = new System.Drawing.Point(3, 29);
            this.buttonTest.Name = "buttonTest";
            this.buttonTest.Size = new System.Drawing.Size(75, 23);
            this.buttonTest.TabIndex = 0;
            this.buttonTest.Text = "Test";
            this.buttonTest.UseVisualStyleBackColor = true;
            this.buttonTest.Click += new System.EventHandler(this.buttonTest_Click);



            this.textSomeText.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.textSomeText.Location = new System.Drawing.Point(3, 3);
            this.textSomeText.Name = "textSomeText";
            this.textSomeText.Size = new System.Drawing.Size(397, 20);
            this.textSomeText.TabIndex = 1;
            this.textSomeText.Text = "Enter text here and it will be saved";



            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.textSomeText);
            this.Controls.Add(this.buttonTest);
            this.Name = "TestPluginSettingsControl";
            this.Size = new System.Drawing.Size(403, 238);
            this.ResumeLayout(false);
            this.PerformLayout();

        }



        private System.Windows.Forms.Button buttonTest;
        private System.Windows.Forms.TextBox textSomeText;
    }
}
