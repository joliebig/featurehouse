using System.Windows.Forms;
using System.Drawing;
namespace ProcessHacker.Components
{
    partial class Indicator
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



            this.BackColor = System.Drawing.Color.Black;
            this.ForeColor = System.Drawing.Color.Lime;
            this.Name = "Indicator";
            this.Size = new System.Drawing.Size(72, 74);
            this.ResumeLayout(false);

        }


    }
}
