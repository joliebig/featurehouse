

using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using System.Xml;
using System.IO;
using Novell.iFolderCom;
using Novell.FormsTrayApp;
using Novell.iFolder.Web;

namespace Novell.Wizard
{



 public class MigrationIdentityPage : Novell.Wizard.MigrationInteriorPageTemplate
 {



  enum SecurityState
  {

   encryption = 1,
   enforceEncryption = 2,
   SSL = 4,
   enforceSSL = 8
  }

  private System.Windows.Forms.Label label1;
  private System.Windows.Forms.Label label3;
  private System.Windows.Forms.Label label4;


  private System.Windows.Forms.RadioButton encryptionCB;
  private System.Windows.Forms.RadioButton sharedCB;
  private System.Windows.Forms.ComboBox Domains;
  private System.ComponentModel.IContainer components = null;
  private static System.Resources.ResourceManager Resource = new System.Resources.ResourceManager(typeof(Novell.FormsTrayApp.FormsTrayApp));

  private iFolderWebService ifws;
  private DomainItem selectedDomain;





  public MigrationIdentityPage(iFolderWebService ifws)
  {

   this.ifws = ifws;
   InitializeComponent();

  }






  private void InitializeComponent()
  {
   this.label1 = new System.Windows.Forms.Label();
   this.label3 = new System.Windows.Forms.Label();
   this.label4 = new System.Windows.Forms.Label();
   this.Domains = new ComboBox();
   this.encryptionCB = new RadioButton();
   this.sharedCB = new RadioButton();
   this.SuspendLayout();



   this.label1.Location = new System.Drawing.Point(40, 96);
   this.label1.Name = "label1";
   this.label1.Size = new System.Drawing.Size(416, 16);
   this.label1.TabIndex = 0;
   this.label1.Text = Resource.GetString("SelectDomain");



   this.label3.Location = new System.Drawing.Point(80, 122);
   this.label3.Name = "label3";
   this.label3.Size = new System.Drawing.Size(90, 16);
   this.label3.TabIndex = 1;
   this.label3.Text = Resource.GetString("DomainName");





   this.Domains.Location = new Point(170, 119);
   this.Domains.Name = "Domains";
   this.Domains.Size = new Size(260,16);
   this.Domains.SelectedIndexChanged +=new EventHandler(Domains_SelectedIndexChanged);




   this.label4.Location = new System.Drawing.Point(40, 160);
   this.label4.Name = "label4";
   this.label4.Size = new System.Drawing.Size(416, 16);
   this.label4.TabIndex = 3;
   this.label4.Text = Resource.GetString("SelectSecurityOptions");





   this.encryptionCB.Location = new Point(80, 186);
   this.encryptionCB.Name = "encryptionCB";
   this.encryptionCB.Text = Resource.GetString("EncryptedText");
   this.encryptionCB.Size = new Size(390, 16);





   this.sharedCB.Location = new Point(80, 212);
   this.sharedCB.Name = "sharedCB";
   this.sharedCB.Text = Resource.GetString("SharableText");
   this.sharedCB.Size = new Size(390, 16);




   this.Controls.Add(this.label4);
   this.Controls.Add(this.label3);
   this.Controls.Add(this.label1);
   this.Controls.Add(this.Domains);
   this.Controls.Add(this.encryptionCB);
   this.Controls.Add(this.sharedCB);
   this.HeaderSubTitle = "";
   this.HeaderTitle = "";
   this.Name = "IdentityPage";
   this.Controls.SetChildIndex(this.label1, 0);
   this.Controls.SetChildIndex(this.label3, 0);

   this.Controls.SetChildIndex(this.label4, 0);
   this.Load += new EventHandler(MigrationIdentityPage_Load);


   this.ResumeLayout(false);

  }
  internal override void ActivatePage(int previousIndex)
  {
   ((MigrationWizard)this.Parent).MigrationWizardButtons = MigrationWizardButtons.Next | MigrationWizardButtons.Back | MigrationWizardButtons.Cancel;
   base.ActivatePage (previousIndex);
  }
  protected override void Dispose( bool disposing )
  {
   if( disposing )
   {
    if (components != null)
    {
     components.Dispose();
    }
   }
   base.Dispose( disposing );
  }
        internal override int ValidatePage(int currentIndex)
  {
   MigrationWizard wiz = (MigrationWizard)this.Parent;
   return base.ValidatePage (currentIndex);
  }
        public string DomainName
  {
   get
   {
    DomainItem domain = (DomainItem) Domains.SelectedItem;
    return domain.Name;
   }
  }
  public DomainItem domain
  {
   get
   {
    return (DomainItem)Domains.SelectedItem;
   }
  }
  public bool Encrypion
  {
   get
   {
    return this.encryptionCB.Checked;
   }
  }
  public bool SSL
  {
   get
   {
    return sharedCB.Checked;
   }
  }
        private void MigrationIdentityPage_Load(object sender, EventArgs e)
  {
   if (Domains.Items.Count == 0)
   {
    try
    {
     XmlDocument DomainsDoc = new XmlDocument();
     DomainsDoc.Load(Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData), "domain.list"));
     XmlElement element = (XmlElement)DomainsDoc.SelectSingleNode("/domains");
     XmlElement defaultDomainElement = (XmlElement)DomainsDoc.SelectSingleNode("/domains/defaultDomain");
     string defaultID = defaultDomainElement.GetAttribute("ID");
     XmlNodeList nodeList = element.GetElementsByTagName("domain");
     foreach (XmlNode node in nodeList)
     {
      string name = ((XmlElement)node).GetAttribute("name");
      string id = ((XmlElement)node).GetAttribute("ID");
      DomainItem domainItem = new DomainItem(name, id);
      Domains.Items.Add(domainItem);
      if (id.Equals(defaultID))
      {
       selectedDomain = domainItem;
      }
     }
     if (selectedDomain != null)
      Domains.SelectedItem = selectedDomain;
     else
      Domains.SelectedIndex = 0;
    }
    catch
    {
    }
   }
   else
   {
    if (selectedDomain != null)
    {
     Domains.SelectedItem = selectedDomain;
    }
    else if (Domains.Items.Count > 0)
    {
     Domains.SelectedIndex = 0;
    }
   }
  }
        private void Domains_SelectedIndexChanged(object sender, EventArgs e)
  {
   DomainItem domain = (DomainItem) Domains.SelectedItem;
   int SecurityPolicy = ifws.GetSecurityPolicy(domain.ID);
   this.encryptionCB.Checked = true;
   this.encryptionCB.Enabled = this.sharedCB.Enabled = false;
   this.sharedCB.Checked = false;
   if(SecurityPolicy !=0)
   {
    if( (SecurityPolicy & (int)SecurityState.encryption) == (int) SecurityState.encryption)
    {
     if( (SecurityPolicy & (int)SecurityState.enforceEncryption) == (int) SecurityState.enforceEncryption)
      encryptionCB.Checked = true;
     else
     {
      encryptionCB.Enabled = true;
      sharedCB.Enabled = true;
     }
    }
    else
     sharedCB.Checked = true;
   }
   else
    sharedCB.Checked = true;
  }
 }
}
