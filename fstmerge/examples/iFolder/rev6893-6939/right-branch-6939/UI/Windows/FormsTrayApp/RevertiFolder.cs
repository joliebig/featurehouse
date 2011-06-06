using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;
using System.IO;
using Novell.iFolder.Web;
using Novell.iFolderCom;
namespace Novell.FormsTrayApp
{
 public class RevertiFolder : System.Windows.Forms.Form
 {
  private System.Windows.Forms.PictureBox icon;
  private System.Windows.Forms.Label label1;
  private System.Windows.Forms.Label label2;
  public System.Windows.Forms.CheckBox removeFromServer;
  private System.Windows.Forms.Button no;
  private System.Windows.Forms.Button yes;
        private System.Windows.Forms.Button help;
  private System.ComponentModel.Container components = null;
  public RevertiFolder()
  {
   InitializeComponent();
   icon.Image = Novell.Win32Util.Win32Window.IconToAlphaBitmap(SystemIcons.Question);
  }
  protected override void Dispose( bool disposing )
  {
   if( disposing )
   {
    if(components != null)
    {
     components.Dispose();
    }
   }
   base.Dispose( disposing );
  }
  private void InitializeComponent()
  {
   System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(RevertiFolder));
   this.icon = new System.Windows.Forms.PictureBox();
   this.label1 = new System.Windows.Forms.Label();
   this.label2 = new System.Windows.Forms.Label();
   this.removeFromServer = new System.Windows.Forms.CheckBox();
   this.no = new System.Windows.Forms.Button();
   this.yes = new System.Windows.Forms.Button();
            this.help = new System.Windows.Forms.Button();
   this.SuspendLayout();
   this.icon.AccessibleDescription = resources.GetString("icon.AccessibleDescription");
   this.icon.AccessibleName = resources.GetString("icon.AccessibleName");
   this.icon.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("icon.Anchor")));
   this.icon.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("icon.BackgroundImage")));
   this.icon.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("icon.Dock")));
   this.icon.Enabled = ((bool)(resources.GetObject("icon.Enabled")));
   this.icon.Font = ((System.Drawing.Font)(resources.GetObject("icon.Font")));
   this.icon.Image = ((System.Drawing.Image)(resources.GetObject("icon.Image")));
   this.icon.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("icon.ImeMode")));
   this.icon.Location = ((System.Drawing.Point)(resources.GetObject("icon.Location")));
   this.icon.Name = "icon";
   this.icon.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("icon.RightToLeft")));
   this.icon.Size = ((System.Drawing.Size)(resources.GetObject("icon.Size")));
   this.icon.SizeMode = ((System.Windows.Forms.PictureBoxSizeMode)(resources.GetObject("icon.SizeMode")));
   this.icon.TabIndex = ((int)(resources.GetObject("icon.TabIndex")));
   this.icon.TabStop = false;
   this.icon.Text = resources.GetString("icon.Text");
   this.icon.Visible = ((bool)(resources.GetObject("icon.Visible")));
   this.label1.AccessibleDescription = resources.GetString("label1.AccessibleDescription");
   this.label1.AccessibleName = resources.GetString("label1.AccessibleName");
   this.label1.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("label1.Anchor")));
   this.label1.AutoSize = ((bool)(resources.GetObject("label1.AutoSize")));
   this.label1.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("label1.Dock")));
   this.label1.Enabled = ((bool)(resources.GetObject("label1.Enabled")));
   this.label1.Font = ((System.Drawing.Font)(resources.GetObject("label1.Font")));
   this.label1.Image = ((System.Drawing.Image)(resources.GetObject("label1.Image")));
   this.label1.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label1.ImageAlign")));
   this.label1.ImageIndex = ((int)(resources.GetObject("label1.ImageIndex")));
   this.label1.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("label1.ImeMode")));
   this.label1.Location = ((System.Drawing.Point)(resources.GetObject("label1.Location")));
   this.label1.Name = "label1";
   this.label1.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("label1.RightToLeft")));
   this.label1.Size = ((System.Drawing.Size)(resources.GetObject("label1.Size")));
   this.label1.TabIndex = ((int)(resources.GetObject("label1.TabIndex")));
   this.label1.Text = resources.GetString("label1.Text");
   this.label1.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label1.TextAlign")));
   this.label1.Visible = ((bool)(resources.GetObject("label1.Visible")));
   this.label2.AccessibleDescription = resources.GetString("label2.AccessibleDescription");
   this.label2.AccessibleName = resources.GetString("label2.AccessibleName");
   this.label2.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("label2.Anchor")));
   this.label2.AutoSize = ((bool)(resources.GetObject("label2.AutoSize")));
   this.label2.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("label2.Dock")));
   this.label2.Enabled = ((bool)(resources.GetObject("label2.Enabled")));
   this.label2.Font = ((System.Drawing.Font)(resources.GetObject("label2.Font")));
   this.label2.Image = ((System.Drawing.Image)(resources.GetObject("label2.Image")));
   this.label2.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label2.ImageAlign")));
   this.label2.ImageIndex = ((int)(resources.GetObject("label2.ImageIndex")));
   this.label2.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("label2.ImeMode")));
   this.label2.Location = ((System.Drawing.Point)(resources.GetObject("label2.Location")));
   this.label2.Name = "label2";
   this.label2.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("label2.RightToLeft")));
   this.label2.Size = ((System.Drawing.Size)(resources.GetObject("label2.Size")));
   this.label2.TabIndex = ((int)(resources.GetObject("label2.TabIndex")));
   this.label2.Text = resources.GetString("label2.Text");
   this.label2.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label2.TextAlign")));
   this.label2.Visible = ((bool)(resources.GetObject("label2.Visible")));
   this.removeFromServer.AccessibleDescription = resources.GetString("removeFromServer.AccessibleDescription");
   this.removeFromServer.AccessibleName = resources.GetString("removeFromServer.AccessibleName");
   this.removeFromServer.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("removeFromServer.Anchor")));
   this.removeFromServer.Appearance = ((System.Windows.Forms.Appearance)(resources.GetObject("removeFromServer.Appearance")));
   this.removeFromServer.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("removeFromServer.BackgroundImage")));
   this.removeFromServer.CheckAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("removeFromServer.CheckAlign")));
   this.removeFromServer.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("removeFromServer.Dock")));
   this.removeFromServer.Enabled = ((bool)(resources.GetObject("removeFromServer.Enabled")));
   this.removeFromServer.FlatStyle = ((System.Windows.Forms.FlatStyle)(resources.GetObject("removeFromServer.FlatStyle")));
   this.removeFromServer.Font = ((System.Drawing.Font)(resources.GetObject("removeFromServer.Font")));
   this.removeFromServer.Image = ((System.Drawing.Image)(resources.GetObject("removeFromServer.Image")));
   this.removeFromServer.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("removeFromServer.ImageAlign")));
   this.removeFromServer.ImageIndex = ((int)(resources.GetObject("removeFromServer.ImageIndex")));
   this.removeFromServer.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("removeFromServer.ImeMode")));
   this.removeFromServer.Location = ((System.Drawing.Point)(resources.GetObject("removeFromServer.Location")));
   this.removeFromServer.Name = "removeFromServer";
   this.removeFromServer.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("removeFromServer.RightToLeft")));
   this.removeFromServer.Size = ((System.Drawing.Size)(resources.GetObject("removeFromServer.Size")));
   this.removeFromServer.TabIndex = ((int)(resources.GetObject("removeFromServer.TabIndex")));
   this.removeFromServer.Text = resources.GetString("removeFromServer.Text");
   this.removeFromServer.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("removeFromServer.TextAlign")));
   this.removeFromServer.Visible = ((bool)(resources.GetObject("removeFromServer.Visible")));
   this.no.AccessibleDescription = resources.GetString("no.AccessibleDescription");
   this.no.AccessibleName = resources.GetString("no.AccessibleName");
   this.no.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("no.Anchor")));
   this.no.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("no.BackgroundImage")));
   this.no.DialogResult = System.Windows.Forms.DialogResult.No;
   this.no.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("no.Dock")));
   this.no.Enabled = ((bool)(resources.GetObject("no.Enabled")));
   this.no.FlatStyle = ((System.Windows.Forms.FlatStyle)(resources.GetObject("no.FlatStyle")));
   this.no.Font = ((System.Drawing.Font)(resources.GetObject("no.Font")));
   this.no.Image = ((System.Drawing.Image)(resources.GetObject("no.Image")));
   this.no.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("no.ImageAlign")));
   this.no.ImageIndex = ((int)(resources.GetObject("no.ImageIndex")));
   this.no.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("no.ImeMode")));
   this.no.Location = ((System.Drawing.Point)(resources.GetObject("no.Location")));
   this.no.Name = "no";
   this.no.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("no.RightToLeft")));
   this.no.Size = ((System.Drawing.Size)(resources.GetObject("no.Size")));
   this.no.TabIndex = ((int)(resources.GetObject("no.TabIndex")));
   this.no.Text = resources.GetString("no.Text");
   this.no.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("no.TextAlign")));
   this.no.Visible = ((bool)(resources.GetObject("no.Visible")));
   this.yes.AccessibleDescription = resources.GetString("yes.AccessibleDescription");
   this.yes.AccessibleName = resources.GetString("yes.AccessibleName");
   this.yes.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("yes.Anchor")));
   this.yes.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("yes.BackgroundImage")));
   this.yes.DialogResult = System.Windows.Forms.DialogResult.Yes;
   this.yes.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("yes.Dock")));
   this.yes.Enabled = ((bool)(resources.GetObject("yes.Enabled")));
   this.yes.FlatStyle = ((System.Windows.Forms.FlatStyle)(resources.GetObject("yes.FlatStyle")));
   this.yes.Font = ((System.Drawing.Font)(resources.GetObject("yes.Font")));
   this.yes.Image = ((System.Drawing.Image)(resources.GetObject("yes.Image")));
   this.yes.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("yes.ImageAlign")));
   this.yes.ImageIndex = ((int)(resources.GetObject("yes.ImageIndex")));
   this.yes.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("yes.ImeMode")));
   this.yes.Location = ((System.Drawing.Point)(resources.GetObject("yes.Location")));
   this.yes.Name = "yes";
   this.yes.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("yes.RightToLeft")));
   this.yes.Size = ((System.Drawing.Size)(resources.GetObject("yes.Size")));
   this.yes.TabIndex = ((int)(resources.GetObject("yes.TabIndex")));
   this.yes.Text = resources.GetString("yes.Text");
   this.yes.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("yes.TextAlign")));
   this.yes.Visible = ((bool)(resources.GetObject("yes.Visible")));
            this.help.AccessibleDescription = resources.GetString("help.AccessibleDescription");
            this.help.AccessibleName = resources.GetString("help.AccessibleName");
            this.help.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("help.Anchor")));
            this.help.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("help.BackgroundImage")));
            this.help.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("help.Dock")));
            this.help.Enabled = ((bool)(resources.GetObject("help.Enabled")));
            this.help.FlatStyle = ((System.Windows.Forms.FlatStyle)(resources.GetObject("help.FlatStyle")));
            this.help.Font = ((System.Drawing.Font)(resources.GetObject("help.Font")));
            this.help.Image = ((System.Drawing.Image)(resources.GetObject("help.Image")));
            this.help.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("help.ImageAlign")));
            this.help.ImageIndex = ((int)(resources.GetObject("help.ImageIndex")));
            this.help.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("help.ImeMode")));
            this.help.Location = ((System.Drawing.Point)(resources.GetObject("help.Location")));
            this.help.Name = "help";
            this.help.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("help.RightToLeft")));
            this.help.Size = ((System.Drawing.Size)(resources.GetObject("help.Size")));
            this.help.TabIndex = ((int)(resources.GetObject("help.TabIndex")));
            this.help.Text = resources.GetString("help.Text");
            this.help.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("help.TextAlign")));
            this.help.Visible = ((bool)(resources.GetObject("help.Visible")));
            this.help.Click += new System.EventHandler(this.help_Click);
   this.AcceptButton = this.no;
   this.AccessibleDescription = resources.GetString("$this.AccessibleDescription");
   this.AccessibleName = resources.GetString("$this.AccessibleName");
   this.AutoScaleBaseSize = ((System.Drawing.Size)(resources.GetObject("$this.AutoScaleBaseSize")));
   this.AutoScroll = ((bool)(resources.GetObject("$this.AutoScroll")));
   this.AutoScrollMargin = ((System.Drawing.Size)(resources.GetObject("$this.AutoScrollMargin")));
   this.AutoScrollMinSize = ((System.Drawing.Size)(resources.GetObject("$this.AutoScrollMinSize")));
   this.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("$this.BackgroundImage")));
   this.ClientSize = ((System.Drawing.Size)(resources.GetObject("$this.ClientSize")));
   this.Controls.Add(this.no);
   this.Controls.Add(this.yes);
   this.Controls.Add(this.removeFromServer);
   this.Controls.Add(this.label2);
   this.Controls.Add(this.label1);
   this.Controls.Add(this.icon);
            this.Controls.Add(this.help);
   this.Enabled = ((bool)(resources.GetObject("$this.Enabled")));
   this.Font = ((System.Drawing.Font)(resources.GetObject("$this.Font")));
   this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
   this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
   this.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("$this.ImeMode")));
   this.Location = ((System.Drawing.Point)(resources.GetObject("$this.Location")));
   this.MaximizeBox = false;
   this.MaximumSize = ((System.Drawing.Size)(resources.GetObject("$this.MaximumSize")));
   this.MinimizeBox = false;
   this.MinimumSize = ((System.Drawing.Size)(resources.GetObject("$this.MinimumSize")));
   this.Name = "RevertiFolder";
   this.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("$this.RightToLeft")));
   this.ShowInTaskbar = false;
   this.StartPosition = ((System.Windows.Forms.FormStartPosition)(resources.GetObject("$this.StartPosition")));
   this.Text = resources.GetString("$this.Text");
   this.ResumeLayout(false);
  }
  public bool RemoveFromServer
  {
   get { return this.removeFromServer.Checked; }
  }
        private void help_Click(object sender, System.EventArgs e)
        {
            string helpFile = Path.Combine(Path.Combine(Path.Combine(Application.StartupPath, "help"), iFolderAdvanced.GetLanguageDirectory()), @"reverting.html");
            new iFolderComponent().ShowHelp(Application.StartupPath, helpFile);
        }
 }
}
