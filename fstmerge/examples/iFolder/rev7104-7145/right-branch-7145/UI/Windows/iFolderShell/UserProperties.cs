using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;
using System.Runtime.InteropServices;
namespace Novell.iFolderCom
{
 [ComVisible(false)]
 public class UserProperties : System.Windows.Forms.Form
 {
  private System.Windows.Forms.GroupBox accessButtons;
  private System.Windows.Forms.RadioButton readOnly;
  private System.Windows.Forms.RadioButton readWrite;
  private System.Windows.Forms.RadioButton fullControl;
  private System.Windows.Forms.CheckBox owner;
  private System.Windows.Forms.Button ok;
  private System.Windows.Forms.Button cancel;
  private bool ownerCanBeSet;
  private bool canBeOwner;
  private System.ComponentModel.Container components = null;
  public UserProperties()
  {
   InitializeComponent();
   this.StartPosition = FormStartPosition.CenterParent;
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
   System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(UserProperties));
   this.accessButtons = new System.Windows.Forms.GroupBox();
   this.readOnly = new System.Windows.Forms.RadioButton();
   this.readWrite = new System.Windows.Forms.RadioButton();
   this.fullControl = new System.Windows.Forms.RadioButton();
   this.owner = new System.Windows.Forms.CheckBox();
   this.ok = new System.Windows.Forms.Button();
   this.cancel = new System.Windows.Forms.Button();
   this.accessButtons.SuspendLayout();
   this.SuspendLayout();
   this.accessButtons.AccessibleDescription = resources.GetString("accessButtons.AccessibleDescription");
   this.accessButtons.AccessibleName = resources.GetString("accessButtons.AccessibleName");
   this.accessButtons.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("accessButtons.Anchor")));
   this.accessButtons.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("accessButtons.BackgroundImage")));
   this.accessButtons.Controls.Add(this.readOnly);
   this.accessButtons.Controls.Add(this.readWrite);
   this.accessButtons.Controls.Add(this.fullControl);
   this.accessButtons.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("accessButtons.Dock")));
   this.accessButtons.Enabled = ((bool)(resources.GetObject("accessButtons.Enabled")));
   this.accessButtons.FlatStyle = System.Windows.Forms.FlatStyle.System;
   this.accessButtons.Font = ((System.Drawing.Font)(resources.GetObject("accessButtons.Font")));
   this.accessButtons.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("accessButtons.ImeMode")));
   this.accessButtons.Location = ((System.Drawing.Point)(resources.GetObject("accessButtons.Location")));
   this.accessButtons.Name = "accessButtons";
   this.accessButtons.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("accessButtons.RightToLeft")));
   this.accessButtons.Size = ((System.Drawing.Size)(resources.GetObject("accessButtons.Size")));
   this.accessButtons.TabIndex = ((int)(resources.GetObject("accessButtons.TabIndex")));
   this.accessButtons.TabStop = false;
   this.accessButtons.Text = resources.GetString("accessButtons.Text");
   this.accessButtons.Visible = ((bool)(resources.GetObject("accessButtons.Visible")));
   this.readOnly.AccessibleDescription = resources.GetString("readOnly.AccessibleDescription");
   this.readOnly.AccessibleName = resources.GetString("readOnly.AccessibleName");
   this.readOnly.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("readOnly.Anchor")));
   this.readOnly.Appearance = ((System.Windows.Forms.Appearance)(resources.GetObject("readOnly.Appearance")));
   this.readOnly.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("readOnly.BackgroundImage")));
   this.readOnly.CheckAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("readOnly.CheckAlign")));
   this.readOnly.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("readOnly.Dock")));
   this.readOnly.Enabled = ((bool)(resources.GetObject("readOnly.Enabled")));
   this.readOnly.FlatStyle = ((System.Windows.Forms.FlatStyle)(resources.GetObject("readOnly.FlatStyle")));
   this.readOnly.Font = ((System.Drawing.Font)(resources.GetObject("readOnly.Font")));
   this.readOnly.Image = ((System.Drawing.Image)(resources.GetObject("readOnly.Image")));
   this.readOnly.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("readOnly.ImageAlign")));
   this.readOnly.ImageIndex = ((int)(resources.GetObject("readOnly.ImageIndex")));
   this.readOnly.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("readOnly.ImeMode")));
   this.readOnly.Location = ((System.Drawing.Point)(resources.GetObject("readOnly.Location")));
   this.readOnly.Name = "readOnly";
   this.readOnly.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("readOnly.RightToLeft")));
   this.readOnly.Size = ((System.Drawing.Size)(resources.GetObject("readOnly.Size")));
   this.readOnly.TabIndex = ((int)(resources.GetObject("readOnly.TabIndex")));
   this.readOnly.Text = resources.GetString("readOnly.Text");
   this.readOnly.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("readOnly.TextAlign")));
   this.readOnly.Visible = ((bool)(resources.GetObject("readOnly.Visible")));
   this.readWrite.AccessibleDescription = resources.GetString("readWrite.AccessibleDescription");
   this.readWrite.AccessibleName = resources.GetString("readWrite.AccessibleName");
   this.readWrite.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("readWrite.Anchor")));
   this.readWrite.Appearance = ((System.Windows.Forms.Appearance)(resources.GetObject("readWrite.Appearance")));
   this.readWrite.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("readWrite.BackgroundImage")));
   this.readWrite.CheckAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("readWrite.CheckAlign")));
   this.readWrite.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("readWrite.Dock")));
   this.readWrite.Enabled = ((bool)(resources.GetObject("readWrite.Enabled")));
   this.readWrite.FlatStyle = ((System.Windows.Forms.FlatStyle)(resources.GetObject("readWrite.FlatStyle")));
   this.readWrite.Font = ((System.Drawing.Font)(resources.GetObject("readWrite.Font")));
   this.readWrite.Image = ((System.Drawing.Image)(resources.GetObject("readWrite.Image")));
   this.readWrite.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("readWrite.ImageAlign")));
   this.readWrite.ImageIndex = ((int)(resources.GetObject("readWrite.ImageIndex")));
   this.readWrite.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("readWrite.ImeMode")));
   this.readWrite.Location = ((System.Drawing.Point)(resources.GetObject("readWrite.Location")));
   this.readWrite.Name = "readWrite";
   this.readWrite.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("readWrite.RightToLeft")));
   this.readWrite.Size = ((System.Drawing.Size)(resources.GetObject("readWrite.Size")));
   this.readWrite.TabIndex = ((int)(resources.GetObject("readWrite.TabIndex")));
   this.readWrite.Text = resources.GetString("readWrite.Text");
   this.readWrite.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("readWrite.TextAlign")));
   this.readWrite.Visible = ((bool)(resources.GetObject("readWrite.Visible")));
   this.fullControl.AccessibleDescription = resources.GetString("fullControl.AccessibleDescription");
   this.fullControl.AccessibleName = resources.GetString("fullControl.AccessibleName");
   this.fullControl.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("fullControl.Anchor")));
   this.fullControl.Appearance = ((System.Windows.Forms.Appearance)(resources.GetObject("fullControl.Appearance")));
   this.fullControl.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("fullControl.BackgroundImage")));
   this.fullControl.CheckAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("fullControl.CheckAlign")));
   this.fullControl.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("fullControl.Dock")));
   this.fullControl.Enabled = ((bool)(resources.GetObject("fullControl.Enabled")));
   this.fullControl.FlatStyle = ((System.Windows.Forms.FlatStyle)(resources.GetObject("fullControl.FlatStyle")));
   this.fullControl.Font = ((System.Drawing.Font)(resources.GetObject("fullControl.Font")));
   this.fullControl.Image = ((System.Drawing.Image)(resources.GetObject("fullControl.Image")));
   this.fullControl.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("fullControl.ImageAlign")));
   this.fullControl.ImageIndex = ((int)(resources.GetObject("fullControl.ImageIndex")));
   this.fullControl.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("fullControl.ImeMode")));
   this.fullControl.Location = ((System.Drawing.Point)(resources.GetObject("fullControl.Location")));
   this.fullControl.Name = "fullControl";
   this.fullControl.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("fullControl.RightToLeft")));
   this.fullControl.Size = ((System.Drawing.Size)(resources.GetObject("fullControl.Size")));
   this.fullControl.TabIndex = ((int)(resources.GetObject("fullControl.TabIndex")));
   this.fullControl.Text = resources.GetString("fullControl.Text");
   this.fullControl.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("fullControl.TextAlign")));
   this.fullControl.Visible = ((bool)(resources.GetObject("fullControl.Visible")));
   this.owner.AccessibleDescription = resources.GetString("owner.AccessibleDescription");
   this.owner.AccessibleName = resources.GetString("owner.AccessibleName");
   this.owner.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("owner.Anchor")));
   this.owner.Appearance = ((System.Windows.Forms.Appearance)(resources.GetObject("owner.Appearance")));
   this.owner.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("owner.BackgroundImage")));
   this.owner.CheckAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("owner.CheckAlign")));
   this.owner.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("owner.Dock")));
   this.owner.Enabled = ((bool)(resources.GetObject("owner.Enabled")));
   this.owner.FlatStyle = ((System.Windows.Forms.FlatStyle)(resources.GetObject("owner.FlatStyle")));
   this.owner.Font = ((System.Drawing.Font)(resources.GetObject("owner.Font")));
   this.owner.Image = ((System.Drawing.Image)(resources.GetObject("owner.Image")));
   this.owner.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("owner.ImageAlign")));
   this.owner.ImageIndex = ((int)(resources.GetObject("owner.ImageIndex")));
   this.owner.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("owner.ImeMode")));
   this.owner.Location = ((System.Drawing.Point)(resources.GetObject("owner.Location")));
   this.owner.Name = "owner";
   this.owner.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("owner.RightToLeft")));
   this.owner.Size = ((System.Drawing.Size)(resources.GetObject("owner.Size")));
   this.owner.TabIndex = ((int)(resources.GetObject("owner.TabIndex")));
   this.owner.Text = resources.GetString("owner.Text");
   this.owner.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("owner.TextAlign")));
   this.owner.Visible = ((bool)(resources.GetObject("owner.Visible")));
   this.owner.CheckedChanged += new System.EventHandler(this.owner_CheckedChanged);
   this.ok.AccessibleDescription = resources.GetString("ok.AccessibleDescription");
   this.ok.AccessibleName = resources.GetString("ok.AccessibleName");
   this.ok.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("ok.Anchor")));
   this.ok.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("ok.BackgroundImage")));
   this.ok.DialogResult = System.Windows.Forms.DialogResult.OK;
   this.ok.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("ok.Dock")));
   this.ok.Enabled = ((bool)(resources.GetObject("ok.Enabled")));
   this.ok.FlatStyle = ((System.Windows.Forms.FlatStyle)(resources.GetObject("ok.FlatStyle")));
   this.ok.Font = ((System.Drawing.Font)(resources.GetObject("ok.Font")));
   this.ok.Image = ((System.Drawing.Image)(resources.GetObject("ok.Image")));
   this.ok.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("ok.ImageAlign")));
   this.ok.ImageIndex = ((int)(resources.GetObject("ok.ImageIndex")));
   this.ok.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("ok.ImeMode")));
   this.ok.Location = ((System.Drawing.Point)(resources.GetObject("ok.Location")));
   this.ok.Name = "ok";
   this.ok.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("ok.RightToLeft")));
   this.ok.Size = ((System.Drawing.Size)(resources.GetObject("ok.Size")));
   this.ok.TabIndex = ((int)(resources.GetObject("ok.TabIndex")));
   this.ok.Text = resources.GetString("ok.Text");
   this.ok.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("ok.TextAlign")));
   this.ok.Visible = ((bool)(resources.GetObject("ok.Visible")));
   this.cancel.AccessibleDescription = resources.GetString("cancel.AccessibleDescription");
   this.cancel.AccessibleName = resources.GetString("cancel.AccessibleName");
   this.cancel.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("cancel.Anchor")));
   this.cancel.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("cancel.BackgroundImage")));
   this.cancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
   this.cancel.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("cancel.Dock")));
   this.cancel.Enabled = ((bool)(resources.GetObject("cancel.Enabled")));
   this.cancel.FlatStyle = ((System.Windows.Forms.FlatStyle)(resources.GetObject("cancel.FlatStyle")));
   this.cancel.Font = ((System.Drawing.Font)(resources.GetObject("cancel.Font")));
   this.cancel.Image = ((System.Drawing.Image)(resources.GetObject("cancel.Image")));
   this.cancel.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("cancel.ImageAlign")));
   this.cancel.ImageIndex = ((int)(resources.GetObject("cancel.ImageIndex")));
   this.cancel.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("cancel.ImeMode")));
   this.cancel.Location = ((System.Drawing.Point)(resources.GetObject("cancel.Location")));
   this.cancel.Name = "cancel";
   this.cancel.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("cancel.RightToLeft")));
   this.cancel.Size = ((System.Drawing.Size)(resources.GetObject("cancel.Size")));
   this.cancel.TabIndex = ((int)(resources.GetObject("cancel.TabIndex")));
   this.cancel.Text = resources.GetString("cancel.Text");
   this.cancel.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("cancel.TextAlign")));
   this.cancel.Visible = ((bool)(resources.GetObject("cancel.Visible")));
   this.AcceptButton = this.ok;
   this.AccessibleDescription = resources.GetString("$this.AccessibleDescription");
   this.AccessibleName = resources.GetString("$this.AccessibleName");
   this.AutoScaleBaseSize = ((System.Drawing.Size)(resources.GetObject("$this.AutoScaleBaseSize")));
   this.AutoScroll = ((bool)(resources.GetObject("$this.AutoScroll")));
   this.AutoScrollMargin = ((System.Drawing.Size)(resources.GetObject("$this.AutoScrollMargin")));
   this.AutoScrollMinSize = ((System.Drawing.Size)(resources.GetObject("$this.AutoScrollMinSize")));
   this.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("$this.BackgroundImage")));
   this.CancelButton = this.cancel;
   this.ClientSize = ((System.Drawing.Size)(resources.GetObject("$this.ClientSize")));
   this.Controls.Add(this.cancel);
   this.Controls.Add(this.ok);
   this.Controls.Add(this.accessButtons);
   this.Controls.Add(this.owner);
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
   this.Name = "UserProperties";
   this.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("$this.RightToLeft")));
   this.StartPosition = ((System.Windows.Forms.FormStartPosition)(resources.GetObject("$this.StartPosition")));
   this.Text = resources.GetString("$this.Text");
   this.Load += new System.EventHandler(this.TestForm_Load);
   this.accessButtons.ResumeLayout(false);
   this.ResumeLayout(false);
  }
  private void TestForm_Load(object sender, System.EventArgs e)
  {
   owner.Enabled = canBeOwner && ownerCanBeSet && !owner.Checked;
  }
  private void owner_CheckedChanged(object sender, System.EventArgs e)
  {
   accessButtons.Enabled = !owner.Checked;
   if (owner.Checked)
   {
    fullControl.Checked = true;
   }
  }
  public string Title
  {
   set
   {
    this.Text = value;
   }
  }
  public string Rights
  {
   get
   {
    if (fullControl.Checked)
    {
     return "Admin";
    }
    else if (readWrite.Checked)
    {
     return "ReadWrite";
    }
    else
    {
     return "ReadOnly";
    }
   }
   set
   {
    switch (value)
    {
     case "Admin":
      fullControl.Checked = true;
      break;
     case "ReadWrite":
      readWrite.Checked = true;
      break;
     case "ReadOnly":
      readOnly.Checked = true;
      break;
    }
   }
  }
  public bool CanBeOwner
  {
   set { canBeOwner = value; }
  }
  public bool IsOwner
  {
   get { return owner.Checked; }
   set { owner.Checked = value; }
  }
  public bool OwnerCanBeSet
  {
   set { ownerCanBeSet = value; }
  }
 }
}
