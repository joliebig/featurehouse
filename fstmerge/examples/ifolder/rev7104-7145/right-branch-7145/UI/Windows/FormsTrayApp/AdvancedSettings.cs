

using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;
using System.Net;

namespace Novell.FormsTrayApp
{



 public class AdvancedSettings : System.Windows.Forms.Form
 {
  private System.Windows.Forms.GroupBox groupBox5;
  private System.Windows.Forms.TextBox proxy;
  private System.Windows.Forms.NumericUpDown port;
  private System.Windows.Forms.CheckBox useProxy;
  private System.Windows.Forms.Label label7;
  private System.Windows.Forms.Label label4;
  private System.Windows.Forms.Button ok;
  private System.Windows.Forms.Button cancel;



  private System.ComponentModel.Container components = null;




  public AdvancedSettings()
  {



   InitializeComponent();


   port.Minimum = IPEndPoint.MinPort;
   port.Maximum = IPEndPoint.MaxPort;
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
   System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(AdvancedSettings));
   this.groupBox5 = new System.Windows.Forms.GroupBox();
   this.proxy = new System.Windows.Forms.TextBox();
   this.port = new System.Windows.Forms.NumericUpDown();
   this.useProxy = new System.Windows.Forms.CheckBox();
   this.label7 = new System.Windows.Forms.Label();
   this.label4 = new System.Windows.Forms.Label();
   this.ok = new System.Windows.Forms.Button();
   this.cancel = new System.Windows.Forms.Button();
   this.groupBox5.SuspendLayout();
   ((System.ComponentModel.ISupportInitialize)(this.port)).BeginInit();
   this.SuspendLayout();



   this.groupBox5.AccessibleDescription = resources.GetString("groupBox5.AccessibleDescription");
   this.groupBox5.AccessibleName = resources.GetString("groupBox5.AccessibleName");
   this.groupBox5.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("groupBox5.Anchor")));
   this.groupBox5.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("groupBox5.BackgroundImage")));
   this.groupBox5.Controls.Add(this.proxy);
   this.groupBox5.Controls.Add(this.port);
   this.groupBox5.Controls.Add(this.useProxy);
   this.groupBox5.Controls.Add(this.label7);
   this.groupBox5.Controls.Add(this.label4);
   this.groupBox5.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("groupBox5.Dock")));
   this.groupBox5.Enabled = ((bool)(resources.GetObject("groupBox5.Enabled")));
   this.groupBox5.FlatStyle = System.Windows.Forms.FlatStyle.System;
   this.groupBox5.Font = ((System.Drawing.Font)(resources.GetObject("groupBox5.Font")));
   this.groupBox5.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("groupBox5.ImeMode")));
   this.groupBox5.Location = ((System.Drawing.Point)(resources.GetObject("groupBox5.Location")));
   this.groupBox5.Name = "groupBox5";
   this.groupBox5.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("groupBox5.RightToLeft")));
   this.groupBox5.Size = ((System.Drawing.Size)(resources.GetObject("groupBox5.Size")));
   this.groupBox5.TabIndex = ((int)(resources.GetObject("groupBox5.TabIndex")));
   this.groupBox5.TabStop = false;
   this.groupBox5.Text = resources.GetString("groupBox5.Text");
   this.groupBox5.Visible = ((bool)(resources.GetObject("groupBox5.Visible")));



   this.proxy.AccessibleDescription = resources.GetString("proxy.AccessibleDescription");
   this.proxy.AccessibleName = resources.GetString("proxy.AccessibleName");
   this.proxy.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("proxy.Anchor")));
   this.proxy.AutoSize = ((bool)(resources.GetObject("proxy.AutoSize")));
   this.proxy.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("proxy.BackgroundImage")));
   this.proxy.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("proxy.Dock")));
   this.proxy.Enabled = ((bool)(resources.GetObject("proxy.Enabled")));
   this.proxy.Font = ((System.Drawing.Font)(resources.GetObject("proxy.Font")));
   this.proxy.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("proxy.ImeMode")));
   this.proxy.Location = ((System.Drawing.Point)(resources.GetObject("proxy.Location")));
   this.proxy.MaxLength = ((int)(resources.GetObject("proxy.MaxLength")));
   this.proxy.Multiline = ((bool)(resources.GetObject("proxy.Multiline")));
   this.proxy.Name = "proxy";
   this.proxy.PasswordChar = ((char)(resources.GetObject("proxy.PasswordChar")));
   this.proxy.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("proxy.RightToLeft")));
   this.proxy.ScrollBars = ((System.Windows.Forms.ScrollBars)(resources.GetObject("proxy.ScrollBars")));
   this.proxy.Size = ((System.Drawing.Size)(resources.GetObject("proxy.Size")));
   this.proxy.TabIndex = ((int)(resources.GetObject("proxy.TabIndex")));
   this.proxy.Text = resources.GetString("proxy.Text");
   this.proxy.TextAlign = ((System.Windows.Forms.HorizontalAlignment)(resources.GetObject("proxy.TextAlign")));
   this.proxy.Visible = ((bool)(resources.GetObject("proxy.Visible")));
   this.proxy.WordWrap = ((bool)(resources.GetObject("proxy.WordWrap")));



   this.port.AccessibleDescription = resources.GetString("port.AccessibleDescription");
   this.port.AccessibleName = resources.GetString("port.AccessibleName");
   this.port.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("port.Anchor")));
   this.port.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("port.Dock")));
   this.port.Enabled = ((bool)(resources.GetObject("port.Enabled")));
   this.port.Font = ((System.Drawing.Font)(resources.GetObject("port.Font")));
   this.port.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("port.ImeMode")));
   this.port.Location = ((System.Drawing.Point)(resources.GetObject("port.Location")));
   this.port.Name = "port";
   this.port.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("port.RightToLeft")));
   this.port.Size = ((System.Drawing.Size)(resources.GetObject("port.Size")));
   this.port.TabIndex = ((int)(resources.GetObject("port.TabIndex")));
   this.port.TextAlign = ((System.Windows.Forms.HorizontalAlignment)(resources.GetObject("port.TextAlign")));
   this.port.ThousandsSeparator = ((bool)(resources.GetObject("port.ThousandsSeparator")));
   this.port.UpDownAlign = ((System.Windows.Forms.LeftRightAlignment)(resources.GetObject("port.UpDownAlign")));
   this.port.Visible = ((bool)(resources.GetObject("port.Visible")));



   this.useProxy.AccessibleDescription = resources.GetString("useProxy.AccessibleDescription");
   this.useProxy.AccessibleName = resources.GetString("useProxy.AccessibleName");
   this.useProxy.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("useProxy.Anchor")));
   this.useProxy.Appearance = ((System.Windows.Forms.Appearance)(resources.GetObject("useProxy.Appearance")));
   this.useProxy.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("useProxy.BackgroundImage")));
   this.useProxy.CheckAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("useProxy.CheckAlign")));
   this.useProxy.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("useProxy.Dock")));
   this.useProxy.Enabled = ((bool)(resources.GetObject("useProxy.Enabled")));
   this.useProxy.FlatStyle = ((System.Windows.Forms.FlatStyle)(resources.GetObject("useProxy.FlatStyle")));
   this.useProxy.Font = ((System.Drawing.Font)(resources.GetObject("useProxy.Font")));
   this.useProxy.Image = ((System.Drawing.Image)(resources.GetObject("useProxy.Image")));
   this.useProxy.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("useProxy.ImageAlign")));
   this.useProxy.ImageIndex = ((int)(resources.GetObject("useProxy.ImageIndex")));
   this.useProxy.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("useProxy.ImeMode")));
   this.useProxy.Location = ((System.Drawing.Point)(resources.GetObject("useProxy.Location")));
   this.useProxy.Name = "useProxy";
   this.useProxy.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("useProxy.RightToLeft")));
   this.useProxy.Size = ((System.Drawing.Size)(resources.GetObject("useProxy.Size")));
   this.useProxy.TabIndex = ((int)(resources.GetObject("useProxy.TabIndex")));
   this.useProxy.Text = resources.GetString("useProxy.Text");
   this.useProxy.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("useProxy.TextAlign")));
   this.useProxy.Visible = ((bool)(resources.GetObject("useProxy.Visible")));



   this.label7.AccessibleDescription = resources.GetString("label7.AccessibleDescription");
   this.label7.AccessibleName = resources.GetString("label7.AccessibleName");
   this.label7.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("label7.Anchor")));
   this.label7.AutoSize = ((bool)(resources.GetObject("label7.AutoSize")));
   this.label7.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("label7.Dock")));
   this.label7.Enabled = ((bool)(resources.GetObject("label7.Enabled")));
   this.label7.Font = ((System.Drawing.Font)(resources.GetObject("label7.Font")));
   this.label7.Image = ((System.Drawing.Image)(resources.GetObject("label7.Image")));
   this.label7.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label7.ImageAlign")));
   this.label7.ImageIndex = ((int)(resources.GetObject("label7.ImageIndex")));
   this.label7.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("label7.ImeMode")));
   this.label7.Location = ((System.Drawing.Point)(resources.GetObject("label7.Location")));
   this.label7.Name = "label7";
   this.label7.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("label7.RightToLeft")));
   this.label7.Size = ((System.Drawing.Size)(resources.GetObject("label7.Size")));
   this.label7.TabIndex = ((int)(resources.GetObject("label7.TabIndex")));
   this.label7.Text = resources.GetString("label7.Text");
   this.label7.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label7.TextAlign")));
   this.label7.Visible = ((bool)(resources.GetObject("label7.Visible")));



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
   this.ok.Click += new System.EventHandler(this.ok_Click);



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
   this.Controls.Add(this.groupBox5);
   this.Enabled = ((bool)(resources.GetObject("$this.Enabled")));
   this.Font = ((System.Drawing.Font)(resources.GetObject("$this.Font")));
   this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
   this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
   this.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("$this.ImeMode")));
   this.Location = ((System.Drawing.Point)(resources.GetObject("$this.Location")));
   this.MaximumSize = ((System.Drawing.Size)(resources.GetObject("$this.MaximumSize")));
   this.MinimumSize = ((System.Drawing.Size)(resources.GetObject("$this.MinimumSize")));
   this.Name = "AdvancedSettings";
   this.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("$this.RightToLeft")));
   this.StartPosition = ((System.Windows.Forms.FormStartPosition)(resources.GetObject("$this.StartPosition")));
   this.Text = resources.GetString("$this.Text");
   this.Load += new System.EventHandler(this.AdvancedSettings_Load);
   this.groupBox5.ResumeLayout(false);
   ((System.ComponentModel.ISupportInitialize)(this.port)).EndInit();
   this.ResumeLayout(false);

  }





        private void AdvancedSettings_Load(object sender, System.EventArgs e)
  {

  }




        private void ok_Click(object sender, System.EventArgs e)
  {
   Cursor.Current = Cursors.WaitCursor;



   Cursor.Current = Cursors.Default;
  }
 }
}
