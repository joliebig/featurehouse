namespace ProcessHacker.Components
{
    partial class ColorModifier
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
            this.panelColor = new System.Windows.Forms.Panel();
            this.SuspendLayout();



            this.panelColor.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.panelColor.Dock = System.Windows.Forms.DockStyle.Fill;
            this.panelColor.Location = new System.Drawing.Point(0, 0);
            this.panelColor.Name = "panelColor";
            this.panelColor.Size = new System.Drawing.Size(40, 20);
            this.panelColor.TabIndex = 0;
            this.panelColor.MouseLeave += new System.EventHandler(this.panelColor_MouseLeave);
            this.panelColor.Click += new System.EventHandler(this.panelColor_Click);
            this.panelColor.MouseEnter += new System.EventHandler(this.panelColor_MouseEnter);



            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.panelColor);
            this.Name = "ColorModifier";
            this.Size = new System.Drawing.Size(40, 20);
            this.ResumeLayout(false);

        }



        private System.Windows.Forms.Panel panelColor;
    }
}
