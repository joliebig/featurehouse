namespace ProcessHacker.Components
{
    partial class Plotter
    {



        private System.ComponentModel.IContainer components = null;





        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }

            if (_managedBackBuffer != NO_MANAGED_BACK_BUFFER)
                _managedBackBuffer.Dispose();

            base.Dispose(disposing);
        }







        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            this.toolTip = new System.Windows.Forms.ToolTip(this.components);
            this.SuspendLayout();



            this.toolTip.AutomaticDelay = 0;
            this.toolTip.ShowAlways = true;



            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Name = "Plotter";
            this.Size = new System.Drawing.Size(150, 163);
            this.MouseLeave += new System.EventHandler(this.Plotter_MouseLeave);
            this.Paint += new System.Windows.Forms.PaintEventHandler(this.Plotter_Paint);
            this.MouseMove += new System.Windows.Forms.MouseEventHandler(this.Plotter_MouseMove);
            this.Resize += new System.EventHandler(this.Plotter_Resize);
            this.MouseEnter += new System.EventHandler(this.Plotter_MouseEnter);
            this.ResumeLayout(false);

        }



        private System.Windows.Forms.ToolTip toolTip;
    }
}
