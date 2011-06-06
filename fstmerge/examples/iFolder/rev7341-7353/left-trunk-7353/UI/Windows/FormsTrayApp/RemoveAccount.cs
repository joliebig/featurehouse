using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;
using Novell.iFolderCom;
namespace Novell.FormsTrayApp
{
 public class RemoveAccount : System.Windows.Forms.Form
 {
  System.Resources.ResourceManager resourceManager = new System.Resources.ResourceManager(typeof(RemoveAccount));
  private System.Windows.Forms.Label label1;
  private System.Windows.Forms.Label label2;
  private System.Windows.Forms.Label label3;
  private System.Windows.Forms.Label label4;
  private System.Windows.Forms.Label server;
  private System.Windows.Forms.Label host;
  private System.Windows.Forms.Label user;
  private System.Windows.Forms.CheckBox removeAll;
  private System.Windows.Forms.Button yes;
  private System.Windows.Forms.Button no;
  private System.Windows.Forms.PictureBox icon;
  private System.ComponentModel.Container components = null;
  public RemoveAccount(DomainInformation domainInfo)
  {
   InitializeComponent();
   server.Text = domainInfo.Name;
   host.Text = domainInfo.Host;
   user.Text = domainInfo.MemberName;
   icon.Image = Novell.Win32Util.Win32Window.IconToAlphaBitmap(SystemIcons.Question);
   removeAll.Enabled = domainInfo.Authenticated;
   int delta;
   int temp;
   Graphics g = label1.CreateGraphics();
   try
   {
    SizeF textSize = g.MeasureString(label1.Text, label1.Font);
    delta = (int)Math.Ceiling(textSize.Width) - label1.Width;
   }
   finally
   {
    g.Dispose();
   }
   SizeF label2Size;
   g = label2.CreateGraphics();
   try
   {
    label2Size = g.MeasureString(label2.Text, label2.Font);
   }
   finally
   {
    g.Dispose();
   }
   SizeF label3Size;
   g = label3.CreateGraphics();
   try
   {
    label3Size = g.MeasureString(label3.Text, label3.Font);
   }
   finally
   {
    g.Dispose();
   }
   SizeF label4Size;
   g = label4.CreateGraphics();
   try
   {
    label4Size = g.MeasureString(label4.Text, label4.Font);
   }
   finally
   {
    g.Dispose();
   }
   if (label2Size.Width > label3Size.Width &&
    label2Size.Width > label4Size.Width)
   {
    temp = (int)Math.Ceiling(label2Size.Width) - label2.Width;
   }
   else if (label3Size.Width > label2Size.Width &&
    label3Size.Width > label4Size.Width)
   {
    temp = (int)Math.Ceiling(label3Size.Width) - label3.Width;
   }
   else
   {
    temp = (int)Math.Ceiling(label4Size.Width) - label4.Width;
   }
   if (temp > 0)
   {
    label2.Width = label3.Width = label4.Width += temp + 8;
    server.Left = host.Left = user.Left = label2.Left + label2.Width + 8;
   }
   if (temp > delta)
   {
    delta = temp;
   }
   g = server.CreateGraphics();
   try
   {
    SizeF serverSize = g.MeasureString(server.Text, server.Font);
    g.Dispose();
    g = host.CreateGraphics();
    SizeF hostSize = g.MeasureString(host.Text, host.Font);
    g.Dispose();
    g = user.CreateGraphics();
    SizeF userSize = g.MeasureString(user.Text, user.Font);
    if ((serverSize.Width > hostSize.Width) &&
     (serverSize.Width > userSize.Width))
    {
     temp = (int)Math.Ceiling(serverSize.Width) - server.Width;
    }
    else if ((hostSize.Width > serverSize.Width) &&
     (hostSize.Width > userSize.Width))
    {
     temp = (int)Math.Ceiling(hostSize.Width) - host.Width;
    }
    else
    {
     temp = (int)Math.Ceiling(userSize.Width) - user.Width;
    }
   }
   finally
   {
    g.Dispose();
   }
   if (temp > delta)
   {
    delta = temp;
   }
   g = removeAll.CreateGraphics();
   try
   {
    SizeF removeAllSize = g.MeasureString(removeAll.Text, removeAll.Font);
    temp = (int)Math.Ceiling(removeAllSize.Width) - removeAll.Width + 18;
   }
   finally
   {
    g.Dispose();
   }
   if (temp > delta)
   {
    delta = temp;
   }
   if (delta > 0)
   {
    this.Width += delta;
   }
   CenterToParent();
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
   System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(RemoveAccount));
   this.icon = new System.Windows.Forms.PictureBox();
   this.label1 = new System.Windows.Forms.Label();
   this.label2 = new System.Windows.Forms.Label();
   this.label3 = new System.Windows.Forms.Label();
   this.label4 = new System.Windows.Forms.Label();
   this.server = new System.Windows.Forms.Label();
   this.host = new System.Windows.Forms.Label();
   this.user = new System.Windows.Forms.Label();
   this.removeAll = new System.Windows.Forms.CheckBox();
   this.yes = new System.Windows.Forms.Button();
   this.no = new System.Windows.Forms.Button();
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
   this.label3.AccessibleDescription = resources.GetString("label3.AccessibleDescription");
   this.label3.AccessibleName = resources.GetString("label3.AccessibleName");
   this.label3.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("label3.Anchor")));
   this.label3.AutoSize = ((bool)(resources.GetObject("label3.AutoSize")));
   this.label3.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("label3.Dock")));
   this.label3.Enabled = ((bool)(resources.GetObject("label3.Enabled")));
   this.label3.Font = ((System.Drawing.Font)(resources.GetObject("label3.Font")));
   this.label3.Image = ((System.Drawing.Image)(resources.GetObject("label3.Image")));
   this.label3.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label3.ImageAlign")));
   this.label3.ImageIndex = ((int)(resources.GetObject("label3.ImageIndex")));
   this.label3.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("label3.ImeMode")));
   this.label3.Location = ((System.Drawing.Point)(resources.GetObject("label3.Location")));
   this.label3.Name = "label3";
   this.label3.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("label3.RightToLeft")));
   this.label3.Size = ((System.Drawing.Size)(resources.GetObject("label3.Size")));
   this.label3.TabIndex = ((int)(resources.GetObject("label3.TabIndex")));
   this.label3.Text = resources.GetString("label3.Text");
   this.label3.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label3.TextAlign")));
   this.label3.Visible = ((bool)(resources.GetObject("label3.Visible")));
   this.label4.AccessibleDescription = resources.GetString("label4.AccessibleDescription");
   this.label4.AccessibleName = resources.GetString("label4.AccessibleName");
   this.label4.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("label4.Anchor")));
   this.label4.AutoSize = ((bool)(resources.GetObject("label4.AutoSize")));
   this.label4.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("label4.Dock")));
   this.label4.Enabled = ((bool)(resources.GetObject("label4.Enabled")));
   this.label4.Font = ((System.Drawing.Font)(resources.GetObject("label4.Font")));
   this.label4.Image = ((System.Drawing.Image)(resources.GetObject("label4.Image")));
   this.label4.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label4.ImageAlign")));
   this.label4.ImageIndex = ((int)(resources.GetObject("label4.ImageIndex")));
   this.label4.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("label4.ImeMode")));
   this.label4.Location = ((System.Drawing.Point)(resources.GetObject("label4.Location")));
   this.label4.Name = "label4";
   this.label4.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("label4.RightToLeft")));
   this.label4.Size = ((System.Drawing.Size)(resources.GetObject("label4.Size")));
   this.label4.TabIndex = ((int)(resources.GetObject("label4.TabIndex")));
   this.label4.Text = resources.GetString("label4.Text");
   this.label4.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label4.TextAlign")));
   this.label4.Visible = ((bool)(resources.GetObject("label4.Visible")));
   this.server.AccessibleDescription = resources.GetString("server.AccessibleDescription");
   this.server.AccessibleName = resources.GetString("server.AccessibleName");
   this.server.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("server.Anchor")));
   this.server.AutoSize = ((bool)(resources.GetObject("server.AutoSize")));
   this.server.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("server.Dock")));
   this.server.Enabled = ((bool)(resources.GetObject("server.Enabled")));
   this.server.Font = ((System.Drawing.Font)(resources.GetObject("server.Font")));
   this.server.Image = ((System.Drawing.Image)(resources.GetObject("server.Image")));
   this.server.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("server.ImageAlign")));
   this.server.ImageIndex = ((int)(resources.GetObject("server.ImageIndex")));
   this.server.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("server.ImeMode")));
   this.server.Location = ((System.Drawing.Point)(resources.GetObject("server.Location")));
   this.server.Name = "server";
   this.server.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("server.RightToLeft")));
   this.server.Size = ((System.Drawing.Size)(resources.GetObject("server.Size")));
   this.server.TabIndex = ((int)(resources.GetObject("server.TabIndex")));
   this.server.Text = resources.GetString("server.Text");
   this.server.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("server.TextAlign")));
   this.server.Visible = ((bool)(resources.GetObject("server.Visible")));
   this.host.AccessibleDescription = resources.GetString("host.AccessibleDescription");
   this.host.AccessibleName = resources.GetString("host.AccessibleName");
   this.host.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("host.Anchor")));
   this.host.AutoSize = ((bool)(resources.GetObject("host.AutoSize")));
   this.host.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("host.Dock")));
   this.host.Enabled = ((bool)(resources.GetObject("host.Enabled")));
   this.host.Font = ((System.Drawing.Font)(resources.GetObject("host.Font")));
   this.host.Image = ((System.Drawing.Image)(resources.GetObject("host.Image")));
   this.host.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("host.ImageAlign")));
   this.host.ImageIndex = ((int)(resources.GetObject("host.ImageIndex")));
   this.host.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("host.ImeMode")));
   this.host.Location = ((System.Drawing.Point)(resources.GetObject("host.Location")));
   this.host.Name = "host";
   this.host.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("host.RightToLeft")));
   this.host.Size = ((System.Drawing.Size)(resources.GetObject("host.Size")));
   this.host.TabIndex = ((int)(resources.GetObject("host.TabIndex")));
   this.host.Text = resources.GetString("host.Text");
   this.host.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("host.TextAlign")));
   this.host.Visible = ((bool)(resources.GetObject("host.Visible")));
   this.user.AccessibleDescription = resources.GetString("user.AccessibleDescription");
   this.user.AccessibleName = resources.GetString("user.AccessibleName");
   this.user.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("user.Anchor")));
   this.user.AutoSize = ((bool)(resources.GetObject("user.AutoSize")));
   this.user.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("user.Dock")));
   this.user.Enabled = ((bool)(resources.GetObject("user.Enabled")));
   this.user.Font = ((System.Drawing.Font)(resources.GetObject("user.Font")));
   this.user.Image = ((System.Drawing.Image)(resources.GetObject("user.Image")));
   this.user.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("user.ImageAlign")));
   this.user.ImageIndex = ((int)(resources.GetObject("user.ImageIndex")));
   this.user.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("user.ImeMode")));
   this.user.Location = ((System.Drawing.Point)(resources.GetObject("user.Location")));
   this.user.Name = "user";
   this.user.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("user.RightToLeft")));
   this.user.Size = ((System.Drawing.Size)(resources.GetObject("user.Size")));
   this.user.TabIndex = ((int)(resources.GetObject("user.TabIndex")));
   this.user.Text = resources.GetString("user.Text");
   this.user.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("user.TextAlign")));
   this.user.Visible = ((bool)(resources.GetObject("user.Visible")));
   this.removeAll.AccessibleDescription = resources.GetString("removeAll.AccessibleDescription");
   this.removeAll.AccessibleName = resources.GetString("removeAll.AccessibleName");
   this.removeAll.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("removeAll.Anchor")));
   this.removeAll.Appearance = ((System.Windows.Forms.Appearance)(resources.GetObject("removeAll.Appearance")));
   this.removeAll.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("removeAll.BackgroundImage")));
   this.removeAll.CheckAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("removeAll.CheckAlign")));
   this.removeAll.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("removeAll.Dock")));
   this.removeAll.Enabled = ((bool)(resources.GetObject("removeAll.Enabled")));
   this.removeAll.FlatStyle = ((System.Windows.Forms.FlatStyle)(resources.GetObject("removeAll.FlatStyle")));
   this.removeAll.Font = ((System.Drawing.Font)(resources.GetObject("removeAll.Font")));
   this.removeAll.Image = ((System.Drawing.Image)(resources.GetObject("removeAll.Image")));
   this.removeAll.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("removeAll.ImageAlign")));
   this.removeAll.ImageIndex = ((int)(resources.GetObject("removeAll.ImageIndex")));
   this.removeAll.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("removeAll.ImeMode")));
   this.removeAll.Location = ((System.Drawing.Point)(resources.GetObject("removeAll.Location")));
   this.removeAll.Name = "removeAll";
   this.removeAll.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("removeAll.RightToLeft")));
   this.removeAll.Size = ((System.Drawing.Size)(resources.GetObject("removeAll.Size")));
   this.removeAll.TabIndex = ((int)(resources.GetObject("removeAll.TabIndex")));
   this.removeAll.Text = resources.GetString("removeAll.Text");
   this.removeAll.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("removeAll.TextAlign")));
   this.removeAll.Visible = ((bool)(resources.GetObject("removeAll.Visible")));
   this.removeAll.CheckedChanged += new System.EventHandler(this.removeAll_CheckedChanged);
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
   this.Controls.Add(this.removeAll);
   this.Controls.Add(this.user);
   this.Controls.Add(this.host);
   this.Controls.Add(this.server);
   this.Controls.Add(this.label4);
   this.Controls.Add(this.label3);
   this.Controls.Add(this.label2);
   this.Controls.Add(this.label1);
   this.Controls.Add(this.icon);
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
   this.Name = "RemoveAccount";
   this.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("$this.RightToLeft")));
   this.ShowInTaskbar = false;
   this.StartPosition = ((System.Windows.Forms.FormStartPosition)(resources.GetObject("$this.StartPosition")));
   this.Text = resources.GetString("$this.Text");
   this.ResumeLayout(false);
  }
  public bool RemoveAll
  {
   get { return removeAll.Checked; }
  }
        private void removeAll_CheckedChanged(object sender, System.EventArgs e)
  {
   if (removeAll.Checked)
   {
    string message = resourceManager.GetString("removeAccount1") + "\n\n" +
     resourceManager.GetString("removeAccount2");
    MyMessageBox mmb = new MyMessageBox(message, resourceManager.GetString("warning"), string.Empty, MyMessageBoxButtons.OKCancel, MyMessageBoxIcon.Warning, MyMessageBoxDefaultButton.Button2);
    if (mmb.ShowDialog() == DialogResult.Cancel)
    {
     removeAll.Checked = false;
    }
   }
  }
 }
}
