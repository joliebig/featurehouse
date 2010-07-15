namespace ProcessHacker.Components
{
    partial class EventPairProperties
    {



        private System.ComponentModel.IContainer components = null;





        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }

            _eventPairHandle.Dereference(disposing);

            base.Dispose(disposing);
        }







        private void InitializeComponent()
        {
            this.buttonSetHigh = new System.Windows.Forms.Button();
            this.buttonSetLow = new System.Windows.Forms.Button();
            this.SuspendLayout();



            this.buttonSetHigh.FlatStyle = System.Windows.Forms.FlatStyle.System;
            this.buttonSetHigh.Location = new System.Drawing.Point(6, 6);
            this.buttonSetHigh.Name = "buttonSetHigh";
            this.buttonSetHigh.Size = new System.Drawing.Size(75, 23);
            this.buttonSetHigh.TabIndex = 0;
            this.buttonSetHigh.Text = "Set High";
            this.buttonSetHigh.UseVisualStyleBackColor = true;
            this.buttonSetHigh.Click += new System.EventHandler(this.buttonSetHigh_Click);



            this.buttonSetLow.FlatStyle = System.Windows.Forms.FlatStyle.System;
            this.buttonSetLow.Location = new System.Drawing.Point(87, 6);
            this.buttonSetLow.Name = "buttonSetLow";
            this.buttonSetLow.Size = new System.Drawing.Size(75, 23);
            this.buttonSetLow.TabIndex = 0;
            this.buttonSetLow.Text = "Set Low";
            this.buttonSetLow.UseVisualStyleBackColor = true;
            this.buttonSetLow.Click += new System.EventHandler(this.buttonSetLow_Click);



            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.buttonSetLow);
            this.Controls.Add(this.buttonSetHigh);
            this.Name = "EventPairProperties";
            this.Padding = new System.Windows.Forms.Padding(3);
            this.Size = new System.Drawing.Size(212, 63);
            this.ResumeLayout(false);

        }



        private System.Windows.Forms.Button buttonSetHigh;
        private System.Windows.Forms.Button buttonSetLow;
    }
}
