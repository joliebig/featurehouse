
using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;
using System.Runtime.InteropServices;
using System.Text.RegularExpressions;
using Novell.Win32Util;

namespace Novell.iFolderCom
{



 [ComVisible(false)]
 public enum MyMessageBoxButtons
 {



  OK,




  OKCancel,




  YesNo
 }




 [ComVisible(false)]
 public enum MyMessageBoxDefaultButton
 {



  Button1,




  Button2
 }




 [ComVisible(false)]
 public enum MyMessageBoxIcon
 {



  None,




  Information,




  Question,




  Error,




  Warning
 }




 [ComVisible(false)]
 public class MyMessageBox : System.Windows.Forms.Form
 {

  private System.Resources.ResourceManager resourceManager = new System.Resources.ResourceManager(typeof(MyMessageBox));
  private const float maxWidth = 600;
  private System.Windows.Forms.Button yes;
  private System.Windows.Forms.Button no;
  private System.Windows.Forms.Label message;
  private System.Windows.Forms.PictureBox messageIcon;
  private System.Windows.Forms.Button ok;
  private System.Windows.Forms.Button details;
  private bool showDetails = false;
  private MyMessageBoxDefaultButton defaultButton;
  private System.Windows.Forms.TextBox detailsBox;
  private System.Windows.Forms.Button cancel;



  private System.ComponentModel.Container components = null;





  private MyMessageBox()
  {



   InitializeComponent();
  }





  public MyMessageBox(string message) :
   this (message, string.Empty, string.Empty, MyMessageBoxButtons.OK, MyMessageBoxIcon.None, MyMessageBoxDefaultButton.Button1)
  {
  }






  public MyMessageBox(string message, string caption) :
   this (message, caption, string.Empty, MyMessageBoxButtons.OK, MyMessageBoxIcon.None, MyMessageBoxDefaultButton.Button1)
  {
  }







  public MyMessageBox(string message, string caption, string details) :
   this (message, caption, details, MyMessageBoxButtons.OK, MyMessageBoxIcon.None, MyMessageBoxDefaultButton.Button1)
  {
  }
  public MyMessageBox(string message, string caption, string details, MyMessageBoxButtons buttons) :
   this (message, caption, details, buttons, MyMessageBoxIcon.None, MyMessageBoxDefaultButton.Button1)
  {
  }
  public MyMessageBox(string message, string caption, string details, MyMessageBoxButtons buttons, MyMessageBoxIcon icon) :
   this (message, caption, details, buttons, icon, MyMessageBoxDefaultButton.Button1)
  {
  }
  public MyMessageBox(string message, string caption, string details, MyMessageBoxButtons buttons, MyMessageBoxIcon icon, MyMessageBoxDefaultButton defaultButton) :
   this ()
  {
   this.message.Text = message;
   this.Text = caption;
   detailsBox.Text = details.Replace("\n", "\r\n");
   this.details.Visible = !details.Equals(string.Empty);
   switch (buttons)
   {
    case MyMessageBoxButtons.OK:
     ok.Visible = true;
     break;
    case MyMessageBoxButtons.OKCancel:
     ok.Visible = cancel.Visible = true;
     break;
    case MyMessageBoxButtons.YesNo:
     yes.Visible = no.Visible = true;
     break;
   }
   switch (icon)
   {
    case MyMessageBoxIcon.Information:
     messageIcon.Image = Win32Window.IconToAlphaBitmap(SystemIcons.Information);
     break;
    case MyMessageBoxIcon.Question:
     messageIcon.Image = Win32Window.IconToAlphaBitmap(SystemIcons.Question);
     break;
    case MyMessageBoxIcon.Error:
     messageIcon.Image = Win32Window.IconToAlphaBitmap(SystemIcons.Error);
     break;
    case MyMessageBoxIcon.Warning:
     messageIcon.Image = Win32Window.IconToAlphaBitmap(SystemIcons.Warning);
     break;
   }
   this.defaultButton = defaultButton;
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
   System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(MyMessageBox));
   this.yes = new System.Windows.Forms.Button();
   this.no = new System.Windows.Forms.Button();
   this.message = new System.Windows.Forms.Label();
   this.messageIcon = new System.Windows.Forms.PictureBox();
   this.ok = new System.Windows.Forms.Button();
   this.details = new System.Windows.Forms.Button();
   this.detailsBox = new System.Windows.Forms.TextBox();
   this.cancel = new System.Windows.Forms.Button();
   this.SuspendLayout();
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
   this.message.AccessibleDescription = resources.GetString("message.AccessibleDescription");
   this.message.AccessibleName = resources.GetString("message.AccessibleName");
   this.message.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("message.Anchor")));
   this.message.AutoSize = ((bool)(resources.GetObject("message.AutoSize")));
   this.message.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("message.Dock")));
   this.message.Enabled = ((bool)(resources.GetObject("message.Enabled")));
   this.message.Font = ((System.Drawing.Font)(resources.GetObject("message.Font")));
   this.message.Image = ((System.Drawing.Image)(resources.GetObject("message.Image")));
   this.message.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("message.ImageAlign")));
   this.message.ImageIndex = ((int)(resources.GetObject("message.ImageIndex")));
   this.message.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("message.ImeMode")));
   this.message.Location = ((System.Drawing.Point)(resources.GetObject("message.Location")));
   this.message.Name = "message";
   this.message.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("message.RightToLeft")));
   this.message.Size = ((System.Drawing.Size)(resources.GetObject("message.Size")));
   this.message.TabIndex = ((int)(resources.GetObject("message.TabIndex")));
   this.message.Text = resources.GetString("message.Text");
   this.message.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("message.TextAlign")));
   this.message.Visible = ((bool)(resources.GetObject("message.Visible")));
   this.messageIcon.AccessibleDescription = resources.GetString("messageIcon.AccessibleDescription");
   this.messageIcon.AccessibleName = resources.GetString("messageIcon.AccessibleName");
   this.messageIcon.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("messageIcon.Anchor")));
   this.messageIcon.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("messageIcon.BackgroundImage")));
   this.messageIcon.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("messageIcon.Dock")));
   this.messageIcon.Enabled = ((bool)(resources.GetObject("messageIcon.Enabled")));
   this.messageIcon.Font = ((System.Drawing.Font)(resources.GetObject("messageIcon.Font")));
   this.messageIcon.Image = ((System.Drawing.Image)(resources.GetObject("messageIcon.Image")));
   this.messageIcon.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("messageIcon.ImeMode")));
   this.messageIcon.Location = ((System.Drawing.Point)(resources.GetObject("messageIcon.Location")));
   this.messageIcon.Name = "messageIcon";
   this.messageIcon.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("messageIcon.RightToLeft")));
   this.messageIcon.Size = ((System.Drawing.Size)(resources.GetObject("messageIcon.Size")));
   this.messageIcon.SizeMode = ((System.Windows.Forms.PictureBoxSizeMode)(resources.GetObject("messageIcon.SizeMode")));
   this.messageIcon.TabIndex = ((int)(resources.GetObject("messageIcon.TabIndex")));
   this.messageIcon.TabStop = false;
   this.messageIcon.Text = resources.GetString("messageIcon.Text");
   this.messageIcon.Visible = ((bool)(resources.GetObject("messageIcon.Visible")));
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
   this.details.AccessibleDescription = resources.GetString("details.AccessibleDescription");
   this.details.AccessibleName = resources.GetString("details.AccessibleName");
   this.details.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("details.Anchor")));
   this.details.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("details.BackgroundImage")));
   this.details.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("details.Dock")));
   this.details.Enabled = ((bool)(resources.GetObject("details.Enabled")));
   this.details.FlatStyle = ((System.Windows.Forms.FlatStyle)(resources.GetObject("details.FlatStyle")));
   this.details.Font = ((System.Drawing.Font)(resources.GetObject("details.Font")));
   this.details.Image = ((System.Drawing.Image)(resources.GetObject("details.Image")));
   this.details.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("details.ImageAlign")));
   this.details.ImageIndex = ((int)(resources.GetObject("details.ImageIndex")));
   this.details.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("details.ImeMode")));
   this.details.Location = ((System.Drawing.Point)(resources.GetObject("details.Location")));
   this.details.Name = "details";
   this.details.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("details.RightToLeft")));
   this.details.Size = ((System.Drawing.Size)(resources.GetObject("details.Size")));
   this.details.TabIndex = ((int)(resources.GetObject("details.TabIndex")));
   this.details.Text = resources.GetString("details.Text");
   this.details.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("details.TextAlign")));
   this.details.Visible = ((bool)(resources.GetObject("details.Visible")));
   this.details.Click += new System.EventHandler(this.details_Click);
   this.detailsBox.AccessibleDescription = resources.GetString("detailsBox.AccessibleDescription");
   this.detailsBox.AccessibleName = resources.GetString("detailsBox.AccessibleName");
   this.detailsBox.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("detailsBox.Anchor")));
   this.detailsBox.AutoSize = ((bool)(resources.GetObject("detailsBox.AutoSize")));
   this.detailsBox.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("detailsBox.BackgroundImage")));
   this.detailsBox.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("detailsBox.Dock")));
   this.detailsBox.Enabled = ((bool)(resources.GetObject("detailsBox.Enabled")));
   this.detailsBox.Font = ((System.Drawing.Font)(resources.GetObject("detailsBox.Font")));
   this.detailsBox.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("detailsBox.ImeMode")));
   this.detailsBox.Location = ((System.Drawing.Point)(resources.GetObject("detailsBox.Location")));
   this.detailsBox.MaxLength = ((int)(resources.GetObject("detailsBox.MaxLength")));
   this.detailsBox.Multiline = ((bool)(resources.GetObject("detailsBox.Multiline")));
   this.detailsBox.Name = "detailsBox";
   this.detailsBox.PasswordChar = ((char)(resources.GetObject("detailsBox.PasswordChar")));
   this.detailsBox.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("detailsBox.RightToLeft")));
   this.detailsBox.ScrollBars = ((System.Windows.Forms.ScrollBars)(resources.GetObject("detailsBox.ScrollBars")));
   this.detailsBox.Size = ((System.Drawing.Size)(resources.GetObject("detailsBox.Size")));
   this.detailsBox.TabIndex = ((int)(resources.GetObject("detailsBox.TabIndex")));
   this.detailsBox.Text = resources.GetString("detailsBox.Text");
   this.detailsBox.TextAlign = ((System.Windows.Forms.HorizontalAlignment)(resources.GetObject("detailsBox.TextAlign")));
   this.detailsBox.Visible = ((bool)(resources.GetObject("detailsBox.Visible")));
   this.detailsBox.WordWrap = ((bool)(resources.GetObject("detailsBox.WordWrap")));
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
   this.AccessibleDescription = resources.GetString("$this.AccessibleDescription");
   this.AccessibleName = resources.GetString("$this.AccessibleName");
   this.AutoScaleBaseSize = ((System.Drawing.Size)(resources.GetObject("$this.AutoScaleBaseSize")));
   this.AutoScroll = ((bool)(resources.GetObject("$this.AutoScroll")));
   this.AutoScrollMargin = ((System.Drawing.Size)(resources.GetObject("$this.AutoScrollMargin")));
   this.AutoScrollMinSize = ((System.Drawing.Size)(resources.GetObject("$this.AutoScrollMinSize")));
   this.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("$this.BackgroundImage")));
   this.ClientSize = ((System.Drawing.Size)(resources.GetObject("$this.ClientSize")));
   this.Controls.Add(this.cancel);
   this.Controls.Add(this.detailsBox);
   this.Controls.Add(this.details);
   this.Controls.Add(this.ok);
   this.Controls.Add(this.messageIcon);
   this.Controls.Add(this.message);
   this.Controls.Add(this.no);
   this.Controls.Add(this.yes);
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
   this.Name = "MyMessageBox";
   this.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("$this.RightToLeft")));
   this.ShowInTaskbar = false;
   this.StartPosition = ((System.Windows.Forms.FormStartPosition)(resources.GetObject("$this.StartPosition")));
   this.Text = resources.GetString("$this.Text");
   this.Load += new System.EventHandler(this.MyMessageBox_Load);
   this.Activated += new System.EventHandler(this.MyMessageBox_Activated);
   this.Paint += new System.Windows.Forms.PaintEventHandler(this.MyMessageBox_Paint);
   this.ResumeLayout(false);
  }
        private void MyMessageBox_Paint(object sender, System.Windows.Forms.PaintEventArgs e)
  {
   if (showDetails)
   {
    this.Height = detailsBox.Bottom + (this.Height - ClientRectangle.Height) + 8;
   }
   else
   {
    this.Height = detailsBox.Top + (this.Height - ClientRectangle.Height);
   }
  }
        private void details_Click(object sender, System.EventArgs e)
  {
   showDetails = !showDetails;
   details.Text = showDetails ? resourceManager.GetString("hideDetails") : resourceManager.GetString("details.Text");
   this.Invalidate(true);
  }
        private void MyMessageBox_Load(object sender, System.EventArgs e)
  {
   if (yes.Visible)
   {
    IntPtr hMenu = GetSystemMenu(this.Handle, false);
    EnableMenuItem(hMenu, SC_CLOSE, MF_BYCOMMAND | MF_GRAYED);
   }
   resizeControl();
   if (StartPosition == FormStartPosition.CenterScreen)
   {
    CenterToScreen();
   }
   else
   {
    CenterToParent();
   }
  }
        private void MyMessageBox_Activated(object sender, System.EventArgs e)
  {
   switch (defaultButton)
   {
    case MyMessageBoxDefaultButton.Button1:
     if (ok.Visible)
     {
      this.AcceptButton = ok;
      ok.Focus();
     }
     else
     {
      this.AcceptButton = yes;
      yes.Focus();
     }
     break;
    case MyMessageBoxDefaultButton.Button2:
     if (cancel.Visible)
     {
      this.AcceptButton = cancel;
      cancel.Focus();
     }
     else
     {
      this.AcceptButton = no;
      no.Focus();
     }
     break;
   }
  }
  private void resizeButton(Button button)
  {
   Graphics g = button.CreateGraphics();
   try
   {
    Point p = button.Location;
    int width = button.Width;
    SizeF size = g.MeasureString(button.Text, button.Font);
    button.Width = (int)Math.Ceiling(size.Width) + 20;
    if ((button.Anchor & AnchorStyles.Right) == AnchorStyles.Right)
    {
     button.Left = p.X - (button.Width - width);
    }
   }
   finally
   {
    g.Dispose();
   }
  }
  private void resizeControl()
  {
   float width = 0;
   float height = 0;
   double lines = 1;
   SizeF size = new SizeF(0, 0);
   Graphics g = message.CreateGraphics();
   Regex regex = new Regex("[\n]+");
   foreach (string s in regex.Split(message.Text))
   {
    size = g.MeasureString(s, message.Font);
    width = Math.Max((size.Width / maxWidth) > 1 ? maxWidth : size.Width, width);
    lines = Math.Ceiling(size.Width / width);
    height += (float)lines * size.Height;
   }
   g.Dispose();
   int index = 0;
   int newlineCount = 0;
   while ((index = message.Text.IndexOf("\n", index)) != -1)
   {
    while (message.Text[++index] == '\n')
    {
     newlineCount++;
     if (index > message.Text.Length - 1)
      break;
    }
   }
   height += (size.Height * newlineCount) + (float)(2 * lines);
   message.Size = new Size((int)Math.Ceiling(width), (int)Math.Ceiling(height));
   if (details.Visible)
   {
    resizeButton(details);
   }
   int msgWidth = message.Width + message.Left + 4;
   this.Width = Math.Max(msgWidth, details.Visible ? 2 * (details.Width + 14) + ok.Width : 0);
   ok.Top = message.Bottom + 12;
   if (cancel.Visible)
   {
    ok.Left = (ClientRectangle.Width / 2) - (ok.Width + 4);
    cancel.Left = ok.Right + 4;
    cancel.Top = ok.Top;
   }
   else
   {
    ok.Left = (ClientRectangle.Width - ok.Width) / 2;
   }
   if (yes.Visible)
   {
    yes.Left = (ClientRectangle.Width / 2) - (yes.Width + 4);
    no.Left = yes.Right + 4;
    yes.Top = no.Top = ok.Top;
    this.Height = ok.Bottom + (this.Height - ClientRectangle.Height) + 12;
   }
   details.Top = ok.Top;
   details.Left = ClientRectangle.Right - (details.Width + 8);
   detailsBox.Top = ok.Bottom + 8;
   detailsBox.Width = ClientRectangle.Right - (detailsBox.Left * 2);
  }
  private const uint SC_CLOSE = 0xF060;
  private const uint MF_BYCOMMAND = 0;
  private const uint MF_GRAYED = 1;
  [DllImport("user32.dll")]
  static extern bool EnableMenuItem(IntPtr hMenu, uint uIDEnableItem, uint uEnable);
  [DllImport("user32.dll")]
  static extern IntPtr GetSystemMenu(IntPtr hWnd, bool bRevert);
 }
}
