using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;
using System.Runtime.InteropServices;
using System.IO;
using System.Net;
using Novell.Win32Util;
namespace Novell.iFolderCom
{
 [ComVisible(false)]
 public class NewiFolder : System.Windows.Forms.Form
 {
  private System.Windows.Forms.Button close;
  private System.Windows.Forms.PictureBox iFolderEmblem;
  private System.Windows.Forms.CheckBox dontAsk;
  private string folderName;
  private string loadPath;
  private const int SHOP_FILEPATH = 0x2;
  private System.Windows.Forms.LinkLabel iFolderOverview;
  private System.Windows.Forms.Button button1;
  private bool migrate;
  private static System.Resources.ResourceManager resourceManager = new System.Resources.ResourceManager(typeof(NewiFolder));
  private System.ComponentModel.Container components = null;
  public NewiFolder()
  {
   InitializeComponent();
   int start = iFolderOverview.Text.IndexOf( "[hyperlink]" );
   if (start != -1)
   {
    System.Resources.ResourceManager resourceManager = new System.Resources.ResourceManager(typeof(NewiFolder));
    string s = resourceManager.GetString( "[hyperlink]" );
    iFolderOverview.Text = iFolderOverview.Text.Replace( "[hyperlink]", s );
    iFolderOverview.LinkArea = new LinkArea(start, s.Length );
   }
   else
   {
    iFolderOverview.LinkArea = new LinkArea(0, 0);
   }
   this.StartPosition = FormStartPosition.CenterScreen;
   migrate = false;
  }
  public void ShowMigrationPrompt()
  {
   this.iFolderOverview.Text = resourceManager.GetString("MigrationLabel.Text");
   this.Text = resourceManager.GetString("MigrationTitle.Text");
   this.button1.Visible = true;
   this.close.Text = resourceManager.GetString("OK");
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
   System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(NewiFolder));
   this.close = new System.Windows.Forms.Button();
   this.iFolderEmblem = new System.Windows.Forms.PictureBox();
   this.dontAsk = new System.Windows.Forms.CheckBox();
   this.iFolderOverview = new System.Windows.Forms.LinkLabel();
   this.button1 = new System.Windows.Forms.Button();
   this.SuspendLayout();
   this.close.AccessibleDescription = resources.GetString("close.AccessibleDescription");
   this.close.AccessibleName = resources.GetString("close.AccessibleName");
   this.close.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("close.Anchor")));
   this.close.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("close.BackgroundImage")));
   this.close.DialogResult = System.Windows.Forms.DialogResult.OK;
   this.close.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("close.Dock")));
   this.close.Enabled = ((bool)(resources.GetObject("close.Enabled")));
   this.close.FlatStyle = ((System.Windows.Forms.FlatStyle)(resources.GetObject("close.FlatStyle")));
   this.close.Font = ((System.Drawing.Font)(resources.GetObject("close.Font")));
   this.close.Image = ((System.Drawing.Image)(resources.GetObject("close.Image")));
   this.close.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("close.ImageAlign")));
   this.close.ImageIndex = ((int)(resources.GetObject("close.ImageIndex")));
   this.close.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("close.ImeMode")));
   this.close.Location = ((System.Drawing.Point)(resources.GetObject("close.Location")));
   this.close.Name = "close";
   this.close.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("close.RightToLeft")));
   this.close.Size = ((System.Drawing.Size)(resources.GetObject("close.Size")));
   this.close.TabIndex = ((int)(resources.GetObject("close.TabIndex")));
   this.close.Text = resources.GetString("close.Text");
   this.close.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("close.TextAlign")));
   this.close.Visible = ((bool)(resources.GetObject("close.Visible")));
   this.close.Click += new System.EventHandler(this.close_Click);
   this.iFolderEmblem.AccessibleDescription = resources.GetString("iFolderEmblem.AccessibleDescription");
   this.iFolderEmblem.AccessibleName = resources.GetString("iFolderEmblem.AccessibleName");
   this.iFolderEmblem.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("iFolderEmblem.Anchor")));
   this.iFolderEmblem.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("iFolderEmblem.BackgroundImage")));
   this.iFolderEmblem.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("iFolderEmblem.Dock")));
   this.iFolderEmblem.Enabled = ((bool)(resources.GetObject("iFolderEmblem.Enabled")));
   this.iFolderEmblem.Font = ((System.Drawing.Font)(resources.GetObject("iFolderEmblem.Font")));
   this.iFolderEmblem.Image = ((System.Drawing.Image)(resources.GetObject("iFolderEmblem.Image")));
   this.iFolderEmblem.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("iFolderEmblem.ImeMode")));
   this.iFolderEmblem.Location = ((System.Drawing.Point)(resources.GetObject("iFolderEmblem.Location")));
   this.iFolderEmblem.Name = "iFolderEmblem";
   this.iFolderEmblem.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("iFolderEmblem.RightToLeft")));
   this.iFolderEmblem.Size = ((System.Drawing.Size)(resources.GetObject("iFolderEmblem.Size")));
   this.iFolderEmblem.SizeMode = ((System.Windows.Forms.PictureBoxSizeMode)(resources.GetObject("iFolderEmblem.SizeMode")));
   this.iFolderEmblem.TabIndex = ((int)(resources.GetObject("iFolderEmblem.TabIndex")));
   this.iFolderEmblem.TabStop = false;
   this.iFolderEmblem.Text = resources.GetString("iFolderEmblem.Text");
   this.iFolderEmblem.Visible = ((bool)(resources.GetObject("iFolderEmblem.Visible")));
   this.iFolderEmblem.Paint += new System.Windows.Forms.PaintEventHandler(this.iFolderEmblem_Paint);
   this.dontAsk.AccessibleDescription = resources.GetString("dontAsk.AccessibleDescription");
   this.dontAsk.AccessibleName = resources.GetString("dontAsk.AccessibleName");
   this.dontAsk.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("dontAsk.Anchor")));
   this.dontAsk.Appearance = ((System.Windows.Forms.Appearance)(resources.GetObject("dontAsk.Appearance")));
   this.dontAsk.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("dontAsk.BackgroundImage")));
   this.dontAsk.CheckAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("dontAsk.CheckAlign")));
   this.dontAsk.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("dontAsk.Dock")));
   this.dontAsk.Enabled = ((bool)(resources.GetObject("dontAsk.Enabled")));
   this.dontAsk.FlatStyle = ((System.Windows.Forms.FlatStyle)(resources.GetObject("dontAsk.FlatStyle")));
   this.dontAsk.Font = ((System.Drawing.Font)(resources.GetObject("dontAsk.Font")));
   this.dontAsk.Image = ((System.Drawing.Image)(resources.GetObject("dontAsk.Image")));
   this.dontAsk.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("dontAsk.ImageAlign")));
   this.dontAsk.ImageIndex = ((int)(resources.GetObject("dontAsk.ImageIndex")));
   this.dontAsk.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("dontAsk.ImeMode")));
   this.dontAsk.Location = ((System.Drawing.Point)(resources.GetObject("dontAsk.Location")));
   this.dontAsk.Name = "dontAsk";
   this.dontAsk.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("dontAsk.RightToLeft")));
   this.dontAsk.Size = ((System.Drawing.Size)(resources.GetObject("dontAsk.Size")));
   this.dontAsk.TabIndex = ((int)(resources.GetObject("dontAsk.TabIndex")));
   this.dontAsk.Text = resources.GetString("dontAsk.Text");
   this.dontAsk.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("dontAsk.TextAlign")));
   this.dontAsk.Visible = ((bool)(resources.GetObject("dontAsk.Visible")));
   this.iFolderOverview.AccessibleDescription = resources.GetString("iFolderOverview.AccessibleDescription");
   this.iFolderOverview.AccessibleName = resources.GetString("iFolderOverview.AccessibleName");
   this.iFolderOverview.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("iFolderOverview.Anchor")));
   this.iFolderOverview.AutoSize = ((bool)(resources.GetObject("iFolderOverview.AutoSize")));
   this.iFolderOverview.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("iFolderOverview.Dock")));
   this.iFolderOverview.Enabled = ((bool)(resources.GetObject("iFolderOverview.Enabled")));
   this.iFolderOverview.Font = ((System.Drawing.Font)(resources.GetObject("iFolderOverview.Font")));
   this.iFolderOverview.Image = ((System.Drawing.Image)(resources.GetObject("iFolderOverview.Image")));
   this.iFolderOverview.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("iFolderOverview.ImageAlign")));
   this.iFolderOverview.ImageIndex = ((int)(resources.GetObject("iFolderOverview.ImageIndex")));
   this.iFolderOverview.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("iFolderOverview.ImeMode")));
   this.iFolderOverview.LinkArea = ((System.Windows.Forms.LinkArea)(resources.GetObject("iFolderOverview.LinkArea")));
   this.iFolderOverview.Location = ((System.Drawing.Point)(resources.GetObject("iFolderOverview.Location")));
   this.iFolderOverview.Name = "iFolderOverview";
   this.iFolderOverview.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("iFolderOverview.RightToLeft")));
   this.iFolderOverview.Size = ((System.Drawing.Size)(resources.GetObject("iFolderOverview.Size")));
   this.iFolderOverview.TabIndex = ((int)(resources.GetObject("iFolderOverview.TabIndex")));
   this.iFolderOverview.TabStop = true;
   this.iFolderOverview.Text = resources.GetString("iFolderOverview.Text");
   this.iFolderOverview.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("iFolderOverview.TextAlign")));
   this.iFolderOverview.Visible = ((bool)(resources.GetObject("iFolderOverview.Visible")));
   this.iFolderOverview.LinkClicked += new System.Windows.Forms.LinkLabelLinkClickedEventHandler(this.iFolderOverview_LinkClicked);
   this.button1.AccessibleDescription = resources.GetString("button1.AccessibleDescription");
   this.button1.AccessibleName = resources.GetString("button1.AccessibleName");
   this.button1.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("button1.Anchor")));
   this.button1.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("button1.BackgroundImage")));
   this.button1.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("button1.Dock")));
   this.button1.Enabled = ((bool)(resources.GetObject("button1.Enabled")));
   this.button1.FlatStyle = ((System.Windows.Forms.FlatStyle)(resources.GetObject("button1.FlatStyle")));
   this.button1.Font = ((System.Drawing.Font)(resources.GetObject("button1.Font")));
   this.button1.Image = ((System.Drawing.Image)(resources.GetObject("button1.Image")));
   this.button1.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("button1.ImageAlign")));
   this.button1.ImageIndex = ((int)(resources.GetObject("button1.ImageIndex")));
   this.button1.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("button1.ImeMode")));
   this.button1.Location = ((System.Drawing.Point)(resources.GetObject("button1.Location")));
   this.button1.Name = "button1";
   this.button1.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("button1.RightToLeft")));
   this.button1.Size = ((System.Drawing.Size)(resources.GetObject("button1.Size")));
   this.button1.TabIndex = ((int)(resources.GetObject("button1.TabIndex")));
   this.button1.Text = resources.GetString("button1.Text");
   this.button1.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("button1.TextAlign")));
   this.button1.Visible = ((bool)(resources.GetObject("button1.Visible")));
   this.button1.Click += new EventHandler(button1_Click);
   this.AcceptButton = this.close;
   this.AccessibleDescription = resources.GetString("$this.AccessibleDescription");
   this.AccessibleName = resources.GetString("$this.AccessibleName");
   this.AutoScaleBaseSize = ((System.Drawing.Size)(resources.GetObject("$this.AutoScaleBaseSize")));
   this.AutoScroll = ((bool)(resources.GetObject("$this.AutoScroll")));
   this.AutoScrollMargin = ((System.Drawing.Size)(resources.GetObject("$this.AutoScrollMargin")));
   this.AutoScrollMinSize = ((System.Drawing.Size)(resources.GetObject("$this.AutoScrollMinSize")));
   this.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("$this.BackgroundImage")));
   this.ClientSize = ((System.Drawing.Size)(resources.GetObject("$this.ClientSize")));
   this.Controls.Add(this.button1);
   this.Controls.Add(this.iFolderOverview);
   this.Controls.Add(this.dontAsk);
   this.Controls.Add(this.iFolderEmblem);
   this.Controls.Add(this.close);
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
   this.Name = "NewiFolder";
   this.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("$this.RightToLeft")));
   this.StartPosition = ((System.Windows.Forms.FormStartPosition)(resources.GetObject("$this.StartPosition")));
   this.Text = resources.GetString("$this.Text");
   this.Load += new System.EventHandler(this.NewiFolder_Load);
   this.ResumeLayout(false);
  }
  public string FolderName
  {
   get
   {
    return folderName;
   }
   set
   {
    folderName = value;
   }
  }
  public string LoadPath
  {
   get
   {
    return loadPath;
   }
   set
   {
    loadPath = value;
   }
  }
  public bool DontAsk
  {
   get
   {
    return this.dontAsk.Checked;
   }
  }
  public bool Migrate
  {
   get
   {
    return this.migrate;
   }
  }
  private void NewiFolder_Load(object sender, System.EventArgs e)
  {
   try
   {
    this.Icon = new Icon(Path.Combine(loadPath, @"res\ifolder_16.ico"));
   }
   catch {}
  }
        private void iFolderOverview_LinkClicked(object sender, System.Windows.Forms.LinkLabelLinkClickedEventArgs e)
  {
   new iFolderComponent().ShowHelp(LoadPath, @"myifolders.html");
  }
  private void close_Click(object sender, System.EventArgs e)
  {
   if (dontAsk.Checked && this.button1.Visible == false)
   {
    iFolderComponent.DisplayConfirmationEnabled = !dontAsk.Checked;
   }
   else if( this.button1.Visible == true )
   {
    migrate = true;
   }
   this.Close();
  }
        private void iFolderEmblem_Paint(object sender, System.Windows.Forms.PaintEventArgs e)
  {
   try
   {
    IFSHFILEINFO fi;
    IntPtr ret = Win32Window.ShGetFileInfo(
     FolderName,
     Win32Window.FILE_ATTRIBUTE_DIRECTORY,
     out fi,
     342,
     Win32Window.SHGFI_ICON | Win32Window.SHGFI_USEFILEATTRIBUTES);
    if (ret != IntPtr.Zero)
    {
     Bitmap bmap = Win32Window.IconToAlphaBitmap(Icon.FromHandle(fi.hIcon));
     e.Graphics.DrawImage(bmap, 0, 0);
     IntPtr hIcon = Win32Window.LoadImageFromFile(
      0,
      Path.Combine(loadPath, @"res\ifolder_16.ico"),
      Win32Window.IMAGE_ICON,
      32,
      32,
      Win32Window.LR_LOADFROMFILE);
     bmap = Win32Window.IconToAlphaBitmap(Icon.FromHandle(hIcon));
     e.Graphics.DrawImage(bmap, 0, 0);
    }
   }
   catch{}
  }
        private void button1_Click(object sender, EventArgs e)
  {
   this.Close();
  }
 }
}
