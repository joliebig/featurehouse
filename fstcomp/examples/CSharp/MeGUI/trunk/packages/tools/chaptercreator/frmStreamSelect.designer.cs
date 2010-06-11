namespace MeGUI
{
  partial class frmStreamSelect
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
        this.btnOK = new System.Windows.Forms.Button();
        this.listBox1 = new System.Windows.Forms.ListBox();
        this.SuspendLayout();
        // 
        // btnOK
        // 
        this.btnOK.Anchor = System.Windows.Forms.AnchorStyles.Bottom;
        this.btnOK.Location = new System.Drawing.Point(182, 264);
        this.btnOK.Name = "btnOK";
        this.btnOK.Size = new System.Drawing.Size(76, 22);
        this.btnOK.TabIndex = 1;
        this.btnOK.Text = "OK";
        this.btnOK.UseVisualStyleBackColor = true;
        this.btnOK.Click += new System.EventHandler(this.btnOK_Click);
        // 
        // listBox1
        // 
        this.listBox1.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                    | System.Windows.Forms.AnchorStyles.Left)
                    | System.Windows.Forms.AnchorStyles.Right)));
        this.listBox1.FormattingEnabled = true;
        this.listBox1.Location = new System.Drawing.Point(13, 15);
        this.listBox1.Name = "listBox1";
        this.listBox1.Size = new System.Drawing.Size(423, 238);
        this.listBox1.TabIndex = 2;
        this.listBox1.DoubleClick += new System.EventHandler(this.btnOK_Click);
        // 
        // frmStreamSelect
        // 
        this.AcceptButton = this.btnOK;
        this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
        this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
        this.ClientSize = new System.Drawing.Size(448, 298);
        this.ControlBox = false;
        this.Controls.Add(this.listBox1);
        this.Controls.Add(this.btnOK);
        this.MaximizeBox = false;
        this.MinimizeBox = false;
        this.Name = "frmStreamSelect";
        this.ShowIcon = false;
        this.ShowInTaskbar = false;
        this.SizeGripStyle = System.Windows.Forms.SizeGripStyle.Show;
        this.Text = "Select your list";
        this.ResumeLayout(false);

    }

    #endregion

    private System.Windows.Forms.Button btnOK;
    private System.Windows.Forms.ListBox listBox1;
  }
}