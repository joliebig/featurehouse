using System;
using System.IO;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;
using Microsoft.Win32;
using Novell.iFolder.Web;
using Novell.Wizard;
using Novell.iFolderCom;
namespace Novell.FormsTrayApp
{
 public class MigrationWindow : System.Windows.Forms.Form
 {
  private System.Windows.Forms.ListView listView1;
  private System.Windows.Forms.Button btnCancel;
  private System.Windows.Forms.Button btnMigrate;
  private System.Windows.Forms.ColumnHeader columnHeader1;
  private System.Windows.Forms.ColumnHeader columnHeader2;
  private bool windowActive;
  private static System.Resources.ResourceManager resourceManager = new System.Resources.ResourceManager(typeof(MigrationWindow));
  private static System.Resources.ResourceManager Resource = new System.Resources.ResourceManager(typeof(FormsTrayApp));
  private iFolderWebService ifWebService;
  private SimiasWebService simiasWebService;
  private System.Windows.Forms.ColumnHeader encryptionStatus;
        private string ifolderName;
        private string userName;
        private bool merge;
        private string location;
        public string iFolderLocation
        {
            get
            {
                return this.location;
            }
            set
            {
                this.location = value;
            }
        }
        public string iFolderName
        {
            get
            {
                return this.ifolderName;
            }
            set
            {
                this.ifolderName = value;
            }
        }
        public string UserName
        {
            get
            {
                return this.userName;
            }
            set
            {
                this.userName = value;
            }
        }
        public bool Merge
        {
            get
            {
                return this.merge;
            }
            set
            {
                this.merge = value;
                if (this.merge == true)
                {
                    System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(GlobalProperties));
                    this.btnMigrate.Text = resources.GetString("Merge.Text");
                }
            }
        }
  private System.ComponentModel.Container components = null;
  public MigrationWindow(iFolderWebService ifWebService, SimiasWebService simiasWebService)
  {
   windowActive = false;
   this.ifWebService = ifWebService;
   this.simiasWebService = simiasWebService;
   InitializeComponent();
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
   System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(MigrationWindow));
   this.listView1 = new System.Windows.Forms.ListView();
   this.columnHeader1 = new System.Windows.Forms.ColumnHeader();
   this.columnHeader2 = new System.Windows.Forms.ColumnHeader();
   this.btnCancel = new System.Windows.Forms.Button();
   this.btnMigrate = new System.Windows.Forms.Button();
   this.encryptionStatus = new System.Windows.Forms.ColumnHeader();
   this.SuspendLayout();
   this.listView1.AccessibleDescription = resources.GetString("listView1.AccessibleDescription");
   this.listView1.AccessibleName = resources.GetString("listView1.AccessibleName");
   this.listView1.Alignment = ((System.Windows.Forms.ListViewAlignment)(resources.GetObject("listView1.Alignment")));
   this.listView1.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("listView1.Anchor")));
   this.listView1.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("listView1.BackgroundImage")));
   this.listView1.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
                      this.columnHeader1,
                      this.columnHeader2,
                      this.encryptionStatus});
   this.listView1.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("listView1.Dock")));
   this.listView1.Enabled = ((bool)(resources.GetObject("listView1.Enabled")));
   this.listView1.Font = ((System.Drawing.Font)(resources.GetObject("listView1.Font")));
   this.listView1.FullRowSelect = true;
   this.listView1.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("listView1.ImeMode")));
   this.listView1.LabelWrap = ((bool)(resources.GetObject("listView1.LabelWrap")));
   this.listView1.Location = ((System.Drawing.Point)(resources.GetObject("listView1.Location")));
   this.listView1.MultiSelect = false;
   this.listView1.Name = "listView1";
   this.listView1.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("listView1.RightToLeft")));
   this.listView1.Size = ((System.Drawing.Size)(resources.GetObject("listView1.Size")));
   this.listView1.TabIndex = ((int)(resources.GetObject("listView1.TabIndex")));
   this.listView1.Text = resources.GetString("listView1.Text");
   this.listView1.View = System.Windows.Forms.View.Details;
   this.listView1.Visible = ((bool)(resources.GetObject("listView1.Visible")));
   this.listView1.SelectedIndexChanged += new System.EventHandler(this.listView1_SelectedIndexChanged);
   this.columnHeader1.Text = resources.GetString("columnHeader1.Text");
   this.columnHeader1.TextAlign = ((System.Windows.Forms.HorizontalAlignment)(resources.GetObject("columnHeader1.TextAlign")));
   this.columnHeader1.Width = ((int)(resources.GetObject("columnHeader1.Width")));
   this.columnHeader2.Text = resources.GetString("columnHeader2.Text");
   this.columnHeader2.TextAlign = ((System.Windows.Forms.HorizontalAlignment)(resources.GetObject("columnHeader2.TextAlign")));
   this.columnHeader2.Width = ((int)(resources.GetObject("columnHeader2.Width")));
   this.btnCancel.AccessibleDescription = resources.GetString("btnCancel.AccessibleDescription");
   this.btnCancel.AccessibleName = resources.GetString("btnCancel.AccessibleName");
   this.btnCancel.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("btnCancel.Anchor")));
   this.btnCancel.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("btnCancel.BackgroundImage")));
   this.btnCancel.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("btnCancel.Dock")));
   this.btnCancel.Enabled = ((bool)(resources.GetObject("btnCancel.Enabled")));
   this.btnCancel.FlatStyle = ((System.Windows.Forms.FlatStyle)(resources.GetObject("btnCancel.FlatStyle")));
   this.btnCancel.Font = ((System.Drawing.Font)(resources.GetObject("btnCancel.Font")));
   this.btnCancel.Image = ((System.Drawing.Image)(resources.GetObject("btnCancel.Image")));
   this.btnCancel.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("btnCancel.ImageAlign")));
   this.btnCancel.ImageIndex = ((int)(resources.GetObject("btnCancel.ImageIndex")));
   this.btnCancel.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("btnCancel.ImeMode")));
   this.btnCancel.Location = ((System.Drawing.Point)(resources.GetObject("btnCancel.Location")));
   this.btnCancel.Name = "btnCancel";
   this.btnCancel.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("btnCancel.RightToLeft")));
   this.btnCancel.Size = ((System.Drawing.Size)(resources.GetObject("btnCancel.Size")));
   this.btnCancel.TabIndex = ((int)(resources.GetObject("btnCancel.TabIndex")));
   this.btnCancel.Text = resources.GetString("btnCancel.Text");
   this.btnCancel.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("btnCancel.TextAlign")));
   this.btnCancel.Visible = ((bool)(resources.GetObject("btnCancel.Visible")));
   this.btnCancel.Click += new System.EventHandler(this.btnCancel_Click);
   this.btnMigrate.AccessibleDescription = resources.GetString("btnMigrate.AccessibleDescription");
   this.btnMigrate.AccessibleName = resources.GetString("btnMigrate.AccessibleName");
   this.btnMigrate.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("btnMigrate.Anchor")));
   this.btnMigrate.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("btnMigrate.BackgroundImage")));
   this.btnMigrate.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("btnMigrate.Dock")));
   this.btnMigrate.Enabled = ((bool)(resources.GetObject("btnMigrate.Enabled")));
   this.btnMigrate.FlatStyle = ((System.Windows.Forms.FlatStyle)(resources.GetObject("btnMigrate.FlatStyle")));
   this.btnMigrate.Font = ((System.Drawing.Font)(resources.GetObject("btnMigrate.Font")));
   this.btnMigrate.Image = ((System.Drawing.Image)(resources.GetObject("btnMigrate.Image")));
   this.btnMigrate.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("btnMigrate.ImageAlign")));
   this.btnMigrate.ImageIndex = ((int)(resources.GetObject("btnMigrate.ImageIndex")));
   this.btnMigrate.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("btnMigrate.ImeMode")));
   this.btnMigrate.Location = ((System.Drawing.Point)(resources.GetObject("btnMigrate.Location")));
   this.btnMigrate.Name = "btnMigrate";
   this.btnMigrate.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("btnMigrate.RightToLeft")));
   this.btnMigrate.Size = ((System.Drawing.Size)(resources.GetObject("btnMigrate.Size")));
   this.btnMigrate.TabIndex = ((int)(resources.GetObject("btnMigrate.TabIndex")));
   this.btnMigrate.Text = resources.GetString("btnMigrate.Text");
   this.btnMigrate.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("btnMigrate.TextAlign")));
   this.btnMigrate.Visible = ((bool)(resources.GetObject("btnMigrate.Visible")));
   this.btnMigrate.Click += new System.EventHandler(this.btnMigrate_Click);
   this.encryptionStatus.Text = resources.GetString("encryptionStatus.Text");
   this.encryptionStatus.TextAlign = ((System.Windows.Forms.HorizontalAlignment)(resources.GetObject("encryptionStatus.TextAlign")));
   this.encryptionStatus.Width = ((int)(resources.GetObject("encryptionStatus.Width")));
   this.AccessibleDescription = resources.GetString("$this.AccessibleDescription");
   this.AccessibleName = resources.GetString("$this.AccessibleName");
   this.AutoScaleBaseSize = ((System.Drawing.Size)(resources.GetObject("$this.AutoScaleBaseSize")));
   this.AutoScroll = ((bool)(resources.GetObject("$this.AutoScroll")));
   this.AutoScrollMargin = ((System.Drawing.Size)(resources.GetObject("$this.AutoScrollMargin")));
   this.AutoScrollMinSize = ((System.Drawing.Size)(resources.GetObject("$this.AutoScrollMinSize")));
   this.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("$this.BackgroundImage")));
   this.ClientSize = ((System.Drawing.Size)(resources.GetObject("$this.ClientSize")));
   this.Controls.Add(this.btnMigrate);
   this.Controls.Add(this.btnCancel);
   this.Controls.Add(this.listView1);
   this.Enabled = ((bool)(resources.GetObject("$this.Enabled")));
   this.Font = ((System.Drawing.Font)(resources.GetObject("$this.Font")));
   this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
   this.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("$this.ImeMode")));
   this.Location = ((System.Drawing.Point)(resources.GetObject("$this.Location")));
   this.MaximumSize = ((System.Drawing.Size)(resources.GetObject("$this.MaximumSize")));
   this.MinimumSize = ((System.Drawing.Size)(resources.GetObject("$this.MinimumSize")));
   this.Name = "MigrationWindow";
   this.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("$this.RightToLeft")));
   this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
   this.Text = resources.GetString("$this.Text");
   this.Load += new System.EventHandler(this.MigrationWindow_Load);
   this.ResumeLayout(false);
  }
        private void MigrationWindow_Load(object sender, System.EventArgs e)
  {
   this.Icon = new Icon(System.IO.Path.Combine(Application.StartupPath, @"res\ifolder_16.ico"));
   AddMigrationDetails();
  }
  public void AddMigrationDetails()
  {
   string iFolderRegistryKey = @"Software\Novell iFolder";
   RegistryKey iFolderKey = Registry.LocalMachine.OpenSubKey(iFolderRegistryKey);
   if(iFolderKey == null)
   {
    MessageBox.Show(Resource.GetString("ifolder2NotPresent"), resourceManager.GetString("$this.Text"), MessageBoxButtons.OK);
    this.Dispose();
    return;
   }
   string[] AllKeys = new string[iFolderKey.SubKeyCount];
   string User;
   AllKeys = iFolderKey.GetSubKeyNames();
   this.listView1.Items.Clear();
   for(int i=0; i< AllKeys.Length; i++)
   {
    ListViewItem lvi;
    User = iFolderRegistryKey + "\\" + AllKeys[i];
    RegistryKey UserKey = Registry.LocalMachine.OpenSubKey(User);
    if (UserKey == null)
     return;
    if( UserKey.GetValue("FolderPath") != null)
    {
     RegistryKey encrKey = UserKey.OpenSubKey("Home");
     string encrStatus = Resource.GetString("NotEncrypted");
     if( encrKey!= null)
     {
      Object obj = encrKey.GetValue("EncryptionStatus", null);
                        if (obj != null)
                        {
                            if ((int)obj != 0)
                                encrStatus = Resource.GetString("Encrypted");
                        }
     }
                    String iFolderPath = (string)UserKey.GetValue("FolderPath");
                    if (Directory.Exists(iFolderPath) == false)
                        continue;
     lvi = new ListViewItem( new string[]{AllKeys[i], (string)UserKey.GetValue("FolderPath"), encrStatus});
     listView1.Items.Add(lvi);
     lvi.Selected = true;
    }
    UserKey.Close();
   }
   iFolderKey.Close();
   if( this.listView1.SelectedItems.Count <= 0)
   {
    if( !this.windowActive )
     MessageBox.Show(Resource.GetString("ifolder2NotPresent"), resourceManager.GetString("$this.Text"), MessageBoxButtons.OK);
    this.Dispose(true);
   }
   this.windowActive = true;
  }
        private void btnMigrate_Click(object sender, System.EventArgs e)
  {
   ListViewItem lvi = this.listView1.SelectedItems[0];
            if (lvi == null)
                return;
   bool encr = false;
   if( lvi.SubItems[2].Text == Resource.GetString("Encrypted"))
    encr = true;
            if (this.Merge == true)
            {
                this.iFolderLocation = lvi.SubItems[1].Text;
                this.UserName = lvi.SubItems[0].Text;
                DirectoryInfo dir = new DirectoryInfo(this.iFolderLocation);
                if (dir.Exists == false)
                {
                    System.Resources.ResourceManager resManager = new System.Resources.ResourceManager(typeof(GlobalProperties));
                    MyMessageBox mmb = new MyMessageBox(resManager.GetString("FolderDoesNotExistError"), resManager.GetString("FolderDoesNotExistErrorTitle"), resManager.GetString("FolderDoesNotExistErrorDesc"), MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
                    mmb.ShowDialog();
                    this.iFolderLocation = null;
                }
                else if (dir.Exists && dir.Name.Equals(this.iFolderName, StringComparison.CurrentCultureIgnoreCase) == false)
                {
                    MyMessageBox mmb = new MyMessageBox(Resource.GetString("MigrationRenamePrompt.Text"), Resource.GetString("MigrationAlert"), "", MyMessageBoxButtons.YesNo, MyMessageBoxIcon.Question);
                    DialogResult res = mmb.ShowDialog();
                    if (res == DialogResult.Yes)
                    {
                        try
                        {
                                dir.MoveTo(Path.Combine(dir.Parent.FullName, this.iFolderName));
                                this.iFolderLocation = Path.Combine(dir.Parent.FullName, this.iFolderName);
                        }
                        catch (Exception ex)
                        {
                            MyMessageBox mmb1 = new MyMessageBox("Unable to rename the selected location.", "Migration Alert", ex.Message, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
                            mmb1.ShowDialog();
                            this.iFolderLocation = null;
                            this.UserName = null;
                            return;
                        }
                    }
                    else
                    {
                        this.iFolderLocation = null;
                        this.UserName = null;
                        return;
                    }
                }
                else if (dir.Name.Equals(this.iFolderName, StringComparison.CurrentCultureIgnoreCase))
                {
                    this.iFolderLocation = Path.Combine(dir.Parent.FullName, this.iFolderName);
                }
                this.Dispose(true);
                return;
            }
   MigrationWizard migrationWizard = new MigrationWizard( lvi.SubItems[0].Text, lvi.SubItems[1].Text, encr, ifWebService, this.simiasWebService);
   if ( migrationWizard.ShowDialog() == DialogResult.OK )
   {
   }
   migrationWizard.Dispose();
   AddMigrationDetails();
  }
        private void btnCancel_Click(object sender, System.EventArgs e)
  {
            this.iFolderLocation = null;
            this.UserName = null;
   this.Dispose(true);
  }
        private void listView1_SelectedIndexChanged(object sender, EventArgs e)
  {
   if ((this.listView1.SelectedItems.Count == 1) &&
     (this.listView1.Items.Count > 0))
    {
     ListViewItem lvi = this.listView1.SelectedItems[0];
     if (lvi != null)
     {
      this.btnMigrate.Enabled = true;
     }
    }
    else
    {
     this.btnMigrate.Enabled = false;
    }
  }
        public static void RemoveRegistryForUser( string UserName)
        {
    string iFolderRegistryKey = @"Software\Novell iFolder";
    RegistryKey iFolderKey = Registry.LocalMachine.OpenSubKey(iFolderRegistryKey, true);
    try
    {
     iFolderKey.DeleteSubKeyTree(UserName);
    }
    catch(Exception ex)
    {
    }
        }
        public static bool OldiFoldersPresent()
        {
            bool status = false;
            try
            {
                string iFolderRegistryKey = @"Software\Novell iFolder";
                RegistryKey iFolderKey = Registry.LocalMachine.OpenSubKey(iFolderRegistryKey);
                if (iFolderKey == null)
                {
                    return status;
                }
                string[] AllKeys = new string[iFolderKey.SubKeyCount];
                string User;
                AllKeys = iFolderKey.GetSubKeyNames();
                for (int i = 0; i < AllKeys.Length; i++)
                {
                    User = iFolderRegistryKey + "\\" + AllKeys[i];
                    RegistryKey UserKey = Registry.LocalMachine.OpenSubKey(User);
                    if (UserKey == null)
                        break;
                    if (UserKey.GetValue("FolderPath") != null)
                    {
                        RegistryKey encrKey = UserKey.OpenSubKey("Home");
                        string encrStatus = Resource.GetString("NotEncrypted");
                        if (encrKey != null)
                        {
                            Object obj = encrKey.GetValue("EncryptionStatus", null);
                            if (obj != null && (int)obj != 0)
                                encrStatus = Resource.GetString("Encrypted");
                        }
                        String iFolderPath = (string)UserKey.GetValue("FolderPath");
                        if (Directory.Exists(iFolderPath) == false)
                            continue;
                        status = true;
                        break;
                    }
                    UserKey.Close();
                }
                iFolderKey.Close();
            }
            catch (Exception ex)
            {
            }
            return status;
        }
 }
}
