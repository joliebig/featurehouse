namespace MeGUI.core.gui
{
    partial class AudioEncodingWindow
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.audioEncodingTab1 = new MeGUI.core.gui.AudioEncodingTab();
            this.button1 = new System.Windows.Forms.Button();
            this.SuspendLayout();
            // 
            // audioEncodingTab1
            // 
            this.audioEncodingTab1.Dock = System.Windows.Forms.DockStyle.Top;
            this.audioEncodingTab1.Location = new System.Drawing.Point(0, 0);
            this.audioEncodingTab1.Name = "audioEncodingTab1";
            this.audioEncodingTab1.QueueButtonText = "Update";
            this.audioEncodingTab1.Size = new System.Drawing.Size(429, 173);
            this.audioEncodingTab1.TabIndex = 0;
            // 
            // button1
            // 
            this.button1.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.button1.AutoSize = true;
            this.button1.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.button1.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.button1.Location = new System.Drawing.Point(306, 146);
            this.button1.Name = "button1";
            this.button1.Size = new System.Drawing.Size(50, 23);
            this.button1.TabIndex = 1;
            this.button1.Text = "Cancel";
            this.button1.UseVisualStyleBackColor = true;
            // 
            // AudioEncodingWindow
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(429, 180);
            this.Controls.Add(this.button1);
            this.Controls.Add(this.audioEncodingTab1);
            this.MaximumSize = new System.Drawing.Size(1000, 214);
            this.MinimumSize = new System.Drawing.Size(437, 214);
            this.Name = "AudioEncodingWindow";
            this.Text = "AudioEncodingWindow";
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        

        private AudioEncodingTab audioEncodingTab1;
        private System.Windows.Forms.Button button1;
    }
}