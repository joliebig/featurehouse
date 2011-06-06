using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;
using System.Xml;
using Novell.iFolderCom;
using System.IO;
using Novell.iFolder.Web;
namespace Novell.FormsTrayApp
{
 public class ResetPassphrase : System.Windows.Forms.Form
 {
  private System.Windows.Forms.Panel panel;
  private System.Windows.Forms.PictureBox waterMark;
  private System.Windows.Forms.Label accountLabel;
  private System.Windows.Forms.Label passphraseLabel;
  private System.Windows.Forms.Label newPassphraseLabel;
  private System.Windows.Forms.Label retypePassphraseLabel;
  private System.Windows.Forms.Label recoveryAgentLabel;
  private System.Windows.Forms.ComboBox DomainComboBox;
  private System.Windows.Forms.TextBox passPhrase;
  private System.Windows.Forms.TextBox newPassphrase;
  private System.Windows.Forms.TextBox retypePassphrase;
  private System.Windows.Forms.ComboBox recoveryAgentCombo;
  private System.Windows.Forms.CheckBox rememberPassphrase;
  private System.Windows.Forms.Button btnCancel;
        private System.Windows.Forms.Button buttonHelp;
        private System.Windows.Forms.Button btnReset;
  private SimiasWebService simws;
        private iFolderWebService ifws;
  private string domainID;
  private DomainItem selectedDomain;
  private bool success;
  private static System.Resources.ResourceManager Resource = new System.Resources.ResourceManager(typeof(FormsTrayApp));
  private System.Windows.Forms.PictureBox pictureBox;
  private System.ComponentModel.Container components = null;
  public SimiasWebService simiasWebservice
  {
   get
   {
    return this.simws;
   }
   set
   {
    this.simws = value;
   }
  }
  public string DomainID
  {
   get
   {
    DomainItem domainItem = (DomainItem)this.DomainComboBox.SelectedItem;
    this.domainID = domainItem.ID;
    return this.domainID;
   }
  }
  public bool Success
  {
   get
   {
    return this.success;
   }
  }
  public int DomainCount
  {
   get
   {
    return this.DomainComboBox.Items.Count;
   }
  }
  public ResetPassphrase(SimiasWebService simws, iFolderWebService ifws)
  {
   InitializeComponent();
   this.simws = simws;
            this.ifws = ifws;
   GetLoggedInDomains();
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(ResetPassphrase));
            this.panel = new System.Windows.Forms.Panel();
            this.pictureBox = new System.Windows.Forms.PictureBox();
            this.waterMark = new System.Windows.Forms.PictureBox();
            this.accountLabel = new System.Windows.Forms.Label();
            this.passphraseLabel = new System.Windows.Forms.Label();
            this.newPassphraseLabel = new System.Windows.Forms.Label();
            this.retypePassphraseLabel = new System.Windows.Forms.Label();
            this.recoveryAgentLabel = new System.Windows.Forms.Label();
            this.DomainComboBox = new System.Windows.Forms.ComboBox();
            this.passPhrase = new System.Windows.Forms.TextBox();
            this.newPassphrase = new System.Windows.Forms.TextBox();
            this.retypePassphrase = new System.Windows.Forms.TextBox();
            this.recoveryAgentCombo = new System.Windows.Forms.ComboBox();
            this.rememberPassphrase = new System.Windows.Forms.CheckBox();
            this.btnCancel = new System.Windows.Forms.Button();
            this.btnReset = new System.Windows.Forms.Button();
            this.buttonHelp = new System.Windows.Forms.Button();
            this.panel.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.pictureBox)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.waterMark)).BeginInit();
            this.SuspendLayout();
            this.panel.BackColor = System.Drawing.Color.Transparent;
            this.panel.Controls.Add(this.pictureBox);
            this.panel.Controls.Add(this.waterMark);
            resources.ApplyResources(this.panel, "panel");
            this.panel.Name = "panel";
            resources.ApplyResources(this.pictureBox, "pictureBox");
            this.pictureBox.Name = "pictureBox";
            this.pictureBox.TabStop = false;
            this.waterMark.BackColor = System.Drawing.Color.Transparent;
            resources.ApplyResources(this.waterMark, "waterMark");
            this.waterMark.Name = "waterMark";
            this.waterMark.TabStop = false;
            resources.ApplyResources(this.accountLabel, "accountLabel");
            this.accountLabel.Name = "accountLabel";
            resources.ApplyResources(this.passphraseLabel, "passphraseLabel");
            this.passphraseLabel.Name = "passphraseLabel";
            resources.ApplyResources(this.newPassphraseLabel, "newPassphraseLabel");
            this.newPassphraseLabel.Name = "newPassphraseLabel";
            this.newPassphraseLabel.Click += new System.EventHandler(this.newPassphraseLabel_Click);
            resources.ApplyResources(this.retypePassphraseLabel, "retypePassphraseLabel");
            this.retypePassphraseLabel.Name = "retypePassphraseLabel";
            resources.ApplyResources(this.recoveryAgentLabel, "recoveryAgentLabel");
            this.recoveryAgentLabel.Name = "recoveryAgentLabel";
            resources.ApplyResources(this.DomainComboBox, "DomainComboBox");
            this.DomainComboBox.Name = "DomainComboBox";
            this.DomainComboBox.SelectedIndexChanged += new System.EventHandler(this.DomainComboBox_SelectedIndexChanged);
            resources.ApplyResources(this.passPhrase, "passPhrase");
            this.passPhrase.Name = "passPhrase";
            this.passPhrase.TextChanged += new System.EventHandler(this.passPhrase_TextChanged);
            resources.ApplyResources(this.newPassphrase, "newPassphrase");
            this.newPassphrase.Name = "newPassphrase";
            this.newPassphrase.TextChanged += new System.EventHandler(this.newPassphrase_TextChanged);
            resources.ApplyResources(this.retypePassphrase, "retypePassphrase");
            this.retypePassphrase.Name = "retypePassphrase";
            this.retypePassphrase.TextChanged += new System.EventHandler(this.retypePassphrase_TextChanged);
            resources.ApplyResources(this.recoveryAgentCombo, "recoveryAgentCombo");
            this.recoveryAgentCombo.Name = "recoveryAgentCombo";
            resources.ApplyResources(this.rememberPassphrase, "rememberPassphrase");
            this.rememberPassphrase.Name = "rememberPassphrase";
            resources.ApplyResources(this.btnCancel, "btnCancel");
            this.btnCancel.Name = "btnCancel";
            this.btnCancel.Click += new System.EventHandler(this.btnCancel_Click);
            resources.ApplyResources(this.btnReset, "btnReset");
            this.btnReset.Name = "btnReset";
            this.btnReset.Click += new System.EventHandler(this.btnReset_Click);
            resources.ApplyResources(this.buttonHelp, "buttonHelp");
            this.buttonHelp.Name = "buttonHelp";
            this.buttonHelp.Click += new System.EventHandler(this.buttonHelp_Click);
            resources.ApplyResources(this, "$this");
            this.Controls.Add(this.btnReset);
            this.Controls.Add(this.btnCancel);
            this.Controls.Add(this.buttonHelp);
            this.Controls.Add(this.rememberPassphrase);
            this.Controls.Add(this.recoveryAgentCombo);
            this.Controls.Add(this.retypePassphrase);
            this.Controls.Add(this.newPassphrase);
            this.Controls.Add(this.passPhrase);
            this.Controls.Add(this.DomainComboBox);
            this.Controls.Add(this.recoveryAgentLabel);
            this.Controls.Add(this.retypePassphraseLabel);
            this.Controls.Add(this.newPassphraseLabel);
            this.Controls.Add(this.passphraseLabel);
            this.Controls.Add(this.accountLabel);
            this.Controls.Add(this.panel);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.MaximizeBox = false;
            this.Name = "ResetPassphrase";
            this.Load += new System.EventHandler(this.ResetPassphrase_Load);
            this.panel.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.pictureBox)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.waterMark)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();
  }
        private void newPassphrase_TextChanged(object sender, System.EventArgs e)
  {
   UpdateSensitivity();
  }
        private void newPassphraseLabel_Click(object sender, System.EventArgs e)
  {
  }
        private void ResetPassphrase_Load(object sender, System.EventArgs e)
  {
   this.Icon = new Icon(System.IO.Path.Combine(Application.StartupPath, @"res\ifolder_16.ico"));
   this.waterMark.Image = Image.FromFile(System.IO.Path.Combine(Application.StartupPath, @"res\ifolder-banner.png"));
   this.pictureBox.SizeMode = PictureBoxSizeMode.StretchImage;
   this.pictureBox.Image = Image.FromFile(System.IO.Path.Combine(Application.StartupPath, @"res\ifolder-banner-scaler.png"));
   this.btnReset.Enabled = false;
   this.btnCancel.Select();
   try
   {
    if( this.DomainComboBox.Items.Count > 0)
    {
     if (selectedDomain != null)
     {
      this.DomainComboBox.SelectedItem = selectedDomain;
     }
     else
      this.DomainComboBox.SelectedIndex = 0;
     PopulateRecoveryAgentList();
                    UpdateUI();
    }
   }
   catch
   {
   }
  }
        private void UpdateUI()
        {
            if ( (ifws.GetSecurityPolicy(DomainID) != 0 && simiasWebservice.IsPassPhraseSet(DomainID) ))
            {
                this.passPhrase.Enabled = this.newPassphrase.Enabled = this.retypePassphrase.Enabled = recoveryAgentCombo.Enabled = rememberPassphrase.Enabled = true;
            }
            else
            {
                this.passPhrase.Enabled = this.newPassphrase.Enabled = this.retypePassphrase.Enabled = recoveryAgentCombo.Enabled = rememberPassphrase.Enabled = false;
            }
        }
        private void btnReset_Click(object sender, System.EventArgs e)
  {
   try
   {
    DomainItem domainItem = (DomainItem)this.DomainComboBox.SelectedItem;
    this.domainID = domainItem.ID;
    System.Resources.ResourceManager resManager = new System.Resources.ResourceManager(typeof(Connecting));
    string publicKey = null;
    string ragent = null;
                Status status = null;
                try
                {
                    status = simws.ValidatePassPhrase(this.domainID, this.passPhrase.Text);
                }
                catch (Exception ex)
                {
                    System.Resources.ResourceManager resMgr = new System.Resources.ResourceManager(typeof(VerifyPassphraseDialog));
                    MessageBox.Show(resMgr.GetString("ValidatePPError"), ex.Message);
                }
                if (status.statusCode == StatusCodes.PassPhraseInvalid)
                {
                    MessageBox.Show(Resource.GetString("InvalidCurrentPPText") , Resource.GetString("ResetTitle") );
                    this.success = false;
                    return;
                }
                if (this.recoveryAgentCombo.SelectedItem != null && (string)this.recoveryAgentCombo.SelectedItem != TrayApp.Properties.Resources.serverDefaultRA)
                {
                    byte[] CertificateObj = this.simws.GetRACertificateOnClient(this.DomainID, (string)this.recoveryAgentCombo.SelectedItem);
                    System.Security.Cryptography.X509Certificates.X509Certificate cert = new System.Security.Cryptography.X509Certificates.X509Certificate(CertificateObj);
                    MyMessageBox mmb = new MyMessageBox(string.Format(resManager.GetString("verifyCert"), (string)this.recoveryAgentCombo.SelectedItem), resManager.GetString("verifyCertTitle"), cert.ToString(true), MyMessageBoxButtons.YesNo, MyMessageBoxIcon.Question, MyMessageBoxDefaultButton.Button2);
                    DialogResult messageDialogResult = mmb.ShowDialog();
                    mmb.Dispose();
                    mmb.Close();
                    if (messageDialogResult != DialogResult.Yes)
                        return;
                    else
                    {
                        ragent = (string)this.recoveryAgentCombo.SelectedItem;
                        publicKey = Convert.ToBase64String(cert.GetPublicKey());
                    }
                }
                else
                {
                    ragent = "DEFAULT";
                    DomainInformation domainInfo = (DomainInformation)this.simws.GetDomainInformation(this.DomainID);
                    string memberUID = domainInfo.MemberUserID;
                    publicKey = this.ifws.GetDefaultServerPublicKey(this.DomainID, memberUID);
                }
    status = this.simws.ReSetPassPhrase(this.DomainID, this.passPhrase.Text , this.newPassphrase.Text, ragent, publicKey);
    if( status.statusCode == StatusCodes.Success)
    {
     simws.StorePassPhrase(this.DomainID, "", CredentialType.None, false);
     simws.StorePassPhrase(this.DomainID, this.newPassphrase.Text, CredentialType.Basic, this.rememberPassphrase.Checked);
     MyMessageBox mb = new MyMessageBox(string.Format(Resource.GetString("ResetSuccess")), Resource.GetString("ResetTitle"), "", MyMessageBoxButtons.OK, MyMessageBoxIcon.Information);
     mb.ShowDialog();
     mb.Dispose();
     this.success = true;
     this.Dispose();
     this.Close();
    }
    else
    {
     MessageBox.Show(Resource.GetString("ResetError") , Resource.GetString("ResetTitle") );
     this.success = false;
    }
   }
   catch(Exception ex)
   {
    MessageBox.Show(Resource.GetString("ResetError") , Resource.GetString(ex.Message) );
    this.success = false;
   }
  }
        private void passPhrase_TextChanged(object sender, System.EventArgs e)
  {
   UpdateSensitivity();
  }
        private void retypePassphrase_TextChanged(object sender, System.EventArgs e)
  {
   UpdateSensitivity();
  }
        private void btnCancel_Click(object sender, System.EventArgs e)
  {
   this.success = false;
   this.Dispose();
   this.Close();
  }
        private void buttonHelp_Click(object sender, EventArgs e)
        {
            string helpFile = Path.Combine(Path.Combine(Path.Combine(Application.StartupPath, "help"), iFolderAdvanced.GetLanguageDirectory()), @"managingpassphrse.html");
            new iFolderComponent().ShowHelp(Application.StartupPath, helpFile);
        }
  private void UpdateSensitivity()
  {
   if( this.passPhrase.Text.Length > 0 &&
    this.newPassphrase.Text.Length > 0 &&
    this.newPassphrase.Text == this.retypePassphrase.Text)
    this.btnReset.Enabled = true;
   else
    this.btnReset.Enabled = false;
  }
        private void pictureBox1_Click(object sender, System.EventArgs e)
  {
  }
  private void DomainComboBox_SelectedIndexChanged(object sender, System.EventArgs e)
  {
   PopulateRecoveryAgentList();
            UpdateUI();
  }
  private void PopulateRecoveryAgentList()
  {
   this.recoveryAgentCombo.Items.Clear();
   try
   {
    string[] rAgents= this.simws.GetRAListOnClient(this.DomainID);
    foreach( string rAgent in rAgents)
    {
     this.recoveryAgentCombo.Items.Add( rAgent );
    }
   }
   catch(Exception ex)
   {
   }
   this.recoveryAgentCombo.Items.Add(TrayApp.Properties.Resources.serverDefaultRA);
  }
  private void GetLoggedInDomains()
  {
   try
   {
    DomainInformation[] domains;
    domains = this.simiasWebservice.GetDomains(true);
    foreach (DomainInformation di in domains)
    {
     if( di.Authenticated)
     {
      DomainItem domainItem = new DomainItem(di.Name, di.ID);
      this.DomainComboBox.Items.Add(domainItem);
     }
    }
   }
   catch(Exception ex)
   {
   }
  }
 }
}
