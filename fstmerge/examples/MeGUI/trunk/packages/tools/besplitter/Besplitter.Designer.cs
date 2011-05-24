namespace MeGUI.packages.tools.besplitter
{
    partial class Besplitter
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
            this.label1 = new System.Windows.Forms.Label();
            this.label2 = new System.Windows.Forms.Label();
            this.label3 = new System.Windows.Forms.Label();
            this.input = new MeGUI.FileBar();
            this.cuts = new MeGUI.FileBar();
            this.output = new MeGUI.FileBar();
            this.goButton = new System.Windows.Forms.Button();
            this.helpButton1 = new MeGUI.core.gui.HelpButton();
            this.SuspendLayout();
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(12, 9);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(47, 13);
            this.label1.TabIndex = 0;
            this.label1.Text = "Input file";
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(12, 54);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(39, 13);
            this.label2.TabIndex = 1;
            this.label2.Text = "Cut file";
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(12, 99);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(55, 13);
            this.label3.TabIndex = 2;
            this.label3.Text = "Output file";
            // 
            // input
            // 
            this.input.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.input.Filename = "";
            this.input.Filter = "";
            this.input.FolderMode = false;
            this.input.Location = new System.Drawing.Point(12, 25);
            this.input.Name = "input";
            this.input.ReadOnly = true;
            this.input.SaveMode = false;
            this.input.Size = new System.Drawing.Size(285, 26);
            this.input.TabIndex = 3;
            this.input.Title = null;
            this.input.FileSelected += new MeGUI.FileBarEventHandler(this.input_FileSelected);
            // 
            // cuts
            // 
            this.cuts.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.cuts.Filename = "";
            this.cuts.Filter = "MeGUI cutlist files (*.clt)|*.clt";
            this.cuts.FolderMode = false;
            this.cuts.Location = new System.Drawing.Point(12, 70);
            this.cuts.Name = "cuts";
            this.cuts.ReadOnly = true;
            this.cuts.SaveMode = false;
            this.cuts.Size = new System.Drawing.Size(285, 26);
            this.cuts.TabIndex = 4;
            this.cuts.Title = null;
            // 
            // output
            // 
            this.output.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.output.Filename = "";
            this.output.Filter = null;
            this.output.FolderMode = false;
            this.output.Location = new System.Drawing.Point(12, 115);
            this.output.Name = "output";
            this.output.ReadOnly = false;
            this.output.SaveMode = true;
            this.output.Size = new System.Drawing.Size(285, 26);
            this.output.TabIndex = 5;
            this.output.Title = null;
            // 
            // goButton
            // 
            this.goButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.goButton.AutoSize = true;
            this.goButton.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.goButton.Location = new System.Drawing.Point(232, 156);
            this.goButton.Name = "goButton";
            this.goButton.Size = new System.Drawing.Size(65, 23);
            this.goButton.TabIndex = 6;
            this.goButton.Text = "Create job";
            this.goButton.UseVisualStyleBackColor = true;
            this.goButton.Click += new System.EventHandler(this.goButton_Click);
            // 
            // helpButton1
            // 
            this.helpButton1.ArticleName = "Audio cutter";
            this.helpButton1.AutoSize = true;
            this.helpButton1.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.helpButton1.Location = new System.Drawing.Point(12, 156);
            this.helpButton1.Name = "helpButton1";
            this.helpButton1.Size = new System.Drawing.Size(39, 23);
            this.helpButton1.TabIndex = 7;
            // 
            // Besplitter
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(309, 191);
            this.Controls.Add(this.helpButton1);
            this.Controls.Add(this.goButton);
            this.Controls.Add(this.output);
            this.Controls.Add(this.cuts);
            this.Controls.Add(this.input);
            this.Controls.Add(this.label3);
            this.Controls.Add(this.label2);
            this.Controls.Add(this.label1);
            this.Name = "Besplitter";
            this.Text = "MeGUI - Audio Cutter";
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        

        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.Label label3;
        private FileBar input;
        private FileBar cuts;
        private FileBar output;
        private System.Windows.Forms.Button goButton;
        private MeGUI.core.gui.HelpButton helpButton1;
    }
}