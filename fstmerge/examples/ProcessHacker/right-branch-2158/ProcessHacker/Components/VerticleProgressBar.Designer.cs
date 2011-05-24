namespace ProcessHacker.Components
{
    partial class VerticleProgressBar
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
            this.SuspendLayout();



            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Name = "VerticleProgressBar";
            this.Size = new System.Drawing.Size(20, 150);
            this.Paint += new System.Windows.Forms.PaintEventHandler(this.VerticleProgressBar_Paint);
            this.ResumeLayout(false);

        }


    }
}
