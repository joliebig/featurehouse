namespace MeGUI
{
    partial class VobSubIndexWindow
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

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.inputGroupbox = new System.Windows.Forms.GroupBox();
            this.input = new MeGUI.FileBar();
            this.pgc = new System.Windows.Forms.NumericUpDown();
            this.pgcLabel = new System.Windows.Forms.Label();
            this.inputLabel = new System.Windows.Forms.Label();
            this.outputGroupbox = new System.Windows.Forms.GroupBox();
            this.output = new MeGUI.FileBar();
            this.nameLabel = new System.Windows.Forms.Label();
            this.subtitleGroupbox = new System.Windows.Forms.GroupBox();
            this.subtitleTracks = new System.Windows.Forms.CheckedListBox();
            this.demuxSelectedTracks = new System.Windows.Forms.RadioButton();
            this.keepAllTracks = new System.Windows.Forms.RadioButton();
            this.closeOnQueue = new System.Windows.Forms.CheckBox();
            this.queueButton = new System.Windows.Forms.Button();
            this.helpButton1 = new MeGUI.core.gui.HelpButton();
            this.inputGroupbox.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.pgc)).BeginInit();
            this.outputGroupbox.SuspendLayout();
            this.subtitleGroupbox.SuspendLayout();
            this.SuspendLayout();
            // 
            // inputGroupbox
            // 
            this.inputGroupbox.Controls.Add(this.input);
            this.inputGroupbox.Controls.Add(this.pgc);
            this.inputGroupbox.Controls.Add(this.pgcLabel);
            this.inputGroupbox.Controls.Add(this.inputLabel);
            this.inputGroupbox.Location = new System.Drawing.Point(2, 2);
            this.inputGroupbox.Name = "inputGroupbox";
            this.inputGroupbox.Size = new System.Drawing.Size(424, 70);
            this.inputGroupbox.TabIndex = 1;
            this.inputGroupbox.TabStop = false;
            this.inputGroupbox.Text = "Input";
            // 
            // input
            // 
            this.input.Filename = "";
            this.input.Filter = "IFO Files|*.ifo";
            this.input.FilterIndex = 0;
            this.input.FolderMode = false;
            this.input.Location = new System.Drawing.Point(120, 13);
            this.input.Name = "input";
            this.input.ReadOnly = true;
            this.input.SaveMode = false;
            this.input.Size = new System.Drawing.Size(286, 26);
            this.input.TabIndex = 5;
            this.input.Title = null;
            this.input.FileSelected += new MeGUI.FileBarEventHandler(this.input_FileSelected);
            // 
            // pgc
            // 
            this.pgc.Location = new System.Drawing.Point(120, 45);
            this.pgc.Maximum = new decimal(new int[] {
            99,
            0,
            0,
            0});
            this.pgc.Minimum = new decimal(new int[] {
            1,
            0,
            0,
            0});
            this.pgc.Name = "pgc";
            this.pgc.Size = new System.Drawing.Size(50, 21);
            this.pgc.TabIndex = 4;
            this.pgc.Value = new decimal(new int[] {
            1,
            0,
            0,
            0});
            // 
            // pgcLabel
            // 
            this.pgcLabel.AutoSize = true;
            this.pgcLabel.Location = new System.Drawing.Point(16, 47);
            this.pgcLabel.Name = "pgcLabel";
            this.pgcLabel.Size = new System.Drawing.Size(27, 13);
            this.pgcLabel.TabIndex = 3;
            this.pgcLabel.Text = "PGC";
            // 
            // inputLabel
            // 
            this.inputLabel.Location = new System.Drawing.Point(16, 20);
            this.inputLabel.Name = "inputLabel";
            this.inputLabel.Size = new System.Drawing.Size(100, 13);
            this.inputLabel.TabIndex = 0;
            this.inputLabel.Text = "Input";
            // 
            // outputGroupbox
            // 
            this.outputGroupbox.Controls.Add(this.output);
            this.outputGroupbox.Controls.Add(this.nameLabel);
            this.outputGroupbox.Location = new System.Drawing.Point(2, 309);
            this.outputGroupbox.Name = "outputGroupbox";
            this.outputGroupbox.Size = new System.Drawing.Size(424, 49);
            this.outputGroupbox.TabIndex = 13;
            this.outputGroupbox.TabStop = false;
            this.outputGroupbox.Text = "Output";
            // 
            // output
            // 
            this.output.Filename = "";
            this.output.Filter = "VobSub Files|*.idx";
            this.output.FilterIndex = 0;
            this.output.FolderMode = false;
            this.output.Location = new System.Drawing.Point(120, 17);
            this.output.Name = "output";
            this.output.ReadOnly = true;
            this.output.SaveMode = true;
            this.output.Size = new System.Drawing.Size(286, 26);
            this.output.TabIndex = 5;
            this.output.Title = "Choose an output file";
            this.output.FileSelected += new MeGUI.FileBarEventHandler(this.output_FileSelected);
            // 
            // nameLabel
            // 
            this.nameLabel.Location = new System.Drawing.Point(16, 20);
            this.nameLabel.Name = "nameLabel";
            this.nameLabel.Size = new System.Drawing.Size(100, 13);
            this.nameLabel.TabIndex = 3;
            this.nameLabel.Text = "Ouput";
            // 
            // subtitleGroupbox
            // 
            this.subtitleGroupbox.Controls.Add(this.subtitleTracks);
            this.subtitleGroupbox.Controls.Add(this.demuxSelectedTracks);
            this.subtitleGroupbox.Controls.Add(this.keepAllTracks);
            this.subtitleGroupbox.Location = new System.Drawing.Point(2, 78);
            this.subtitleGroupbox.Name = "subtitleGroupbox";
            this.subtitleGroupbox.Size = new System.Drawing.Size(424, 225);
            this.subtitleGroupbox.TabIndex = 14;
            this.subtitleGroupbox.TabStop = false;
            this.subtitleGroupbox.Text = "Subtitles";
            // 
            // subtitleTracks
            // 
            this.subtitleTracks.CheckOnClick = true;
            this.subtitleTracks.FormattingEnabled = true;
            this.subtitleTracks.Location = new System.Drawing.Point(50, 72);
            this.subtitleTracks.Name = "subtitleTracks";
            this.subtitleTracks.Size = new System.Drawing.Size(356, 148);
            this.subtitleTracks.TabIndex = 9;
            // 
            // demuxSelectedTracks
            // 
            this.demuxSelectedTracks.Checked = true;
            this.demuxSelectedTracks.Location = new System.Drawing.Point(10, 46);
            this.demuxSelectedTracks.Name = "demuxSelectedTracks";
            this.demuxSelectedTracks.Size = new System.Drawing.Size(336, 24);
            this.demuxSelectedTracks.TabIndex = 8;
            this.demuxSelectedTracks.TabStop = true;
            this.demuxSelectedTracks.Text = "Select Subtitle Streams";
            // 
            // keepAllTracks
            // 
            this.keepAllTracks.Location = new System.Drawing.Point(10, 20);
            this.keepAllTracks.Name = "keepAllTracks";
            this.keepAllTracks.Size = new System.Drawing.Size(160, 24);
            this.keepAllTracks.TabIndex = 7;
            this.keepAllTracks.Text = "Keep all Subtitle tracks";
            // 
            // closeOnQueue
            // 
            this.closeOnQueue.Location = new System.Drawing.Point(272, 364);
            this.closeOnQueue.Name = "closeOnQueue";
            this.closeOnQueue.Size = new System.Drawing.Size(72, 24);
            this.closeOnQueue.TabIndex = 16;
            this.closeOnQueue.Text = "and close";
            // 
            // queueButton
            // 
            this.queueButton.Location = new System.Drawing.Point(352, 364);
            this.queueButton.Name = "queueButton";
            this.queueButton.Size = new System.Drawing.Size(74, 23);
            this.queueButton.TabIndex = 15;
            this.queueButton.Text = "Queue";
            this.queueButton.Click += new System.EventHandler(this.queueButton_Click);
            // 
            // helpButton1
            // 
            this.helpButton1.ArticleName = "Vobsub indexer window";
            this.helpButton1.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.helpButton1.Location = new System.Drawing.Point(7, 364);
            this.helpButton1.Name = "helpButton1";
            this.helpButton1.Size = new System.Drawing.Size(47, 23);
            this.helpButton1.TabIndex = 17;
            // 
            // VobSubIndexWindow
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(434, 393);
            this.Controls.Add(this.helpButton1);
            this.Controls.Add(this.closeOnQueue);
            this.Controls.Add(this.queueButton);
            this.Controls.Add(this.subtitleGroupbox);
            this.Controls.Add(this.outputGroupbox);
            this.Controls.Add(this.inputGroupbox);
            this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "VobSubIndexWindow";
            this.ShowInTaskbar = false;
            this.Text = "MeGUI - VobSub Indexer";
            this.inputGroupbox.ResumeLayout(false);
            this.inputGroupbox.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.pgc)).EndInit();
            this.outputGroupbox.ResumeLayout(false);
            this.subtitleGroupbox.ResumeLayout(false);
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.GroupBox inputGroupbox;
        private System.Windows.Forms.Label inputLabel;
        private System.Windows.Forms.GroupBox outputGroupbox;
        private System.Windows.Forms.Label nameLabel;
        private System.Windows.Forms.GroupBox subtitleGroupbox;
        private System.Windows.Forms.RadioButton demuxSelectedTracks;
        private System.Windows.Forms.RadioButton keepAllTracks;
        private System.Windows.Forms.CheckBox closeOnQueue;
        private System.Windows.Forms.Button queueButton;
        private System.Windows.Forms.Label pgcLabel;
        private System.Windows.Forms.NumericUpDown pgc;
        private System.Windows.Forms.CheckedListBox subtitleTracks;
        private MeGUI.core.gui.HelpButton helpButton1;
        private FileBar input;
        private FileBar output;

    }
}