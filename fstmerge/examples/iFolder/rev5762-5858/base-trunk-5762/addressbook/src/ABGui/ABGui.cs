using System;
using System.IO;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;
using System.Data;
using Simias.Storage;
using Novell.AddressBook;
namespace ABGui
{
 public class ABGui : System.Windows.Forms.Form
 {
  private ArrayList contactList = null;
  private string listBy;
  private System.Windows.Forms.ListBox contacts;
  private System.Windows.Forms.Button newContact;
  private System.Windows.Forms.Label contactID;
  private System.Windows.Forms.Label contactFirst;
  private System.Windows.Forms.Label contactFamily;
  private System.Windows.Forms.Button saveContact;
  private System.ComponentModel.Container components = null;
  private System.Windows.Forms.TextBox editID;
  private System.Windows.Forms.TextBox editFirst;
  private System.Windows.Forms.TextBox editLast;
  private Manager abManager = null;
  private System.Windows.Forms.Label labelUsername;
  private System.Windows.Forms.TextBox editUsername;
  private AddressBook addressBook = null;
  private ContactList abContactList = null;
  private System.Windows.Forms.Label labelSearch;
  private System.Windows.Forms.TextBox editSearch;
  private System.Windows.Forms.ComboBox queryCombo;
  private System.Windows.Forms.Button exportButton;
  private System.Windows.Forms.Button importPhoto;
  private System.Windows.Forms.PictureBox contactPicture;
  private System.Windows.Forms.TabControl addressControl;
  private System.Windows.Forms.TabPage homePage;
  private System.Windows.Forms.TabPage businessPage;
  private System.Windows.Forms.Label hMailLabel;
  private System.Windows.Forms.Label h;
  private System.Windows.Forms.Label hStreetLabel;
  private System.Windows.Forms.Label hCityLabel;
  private System.Windows.Forms.Label hStateLabel;
  private System.Windows.Forms.Label hZipLabel;
  private System.Windows.Forms.Label hCountryLabel;
  private System.Windows.Forms.TextBox hMailEdit;
  private System.Windows.Forms.RichTextBox hStreetEdit;
  private System.Windows.Forms.TextBox hCityEdit;
  private System.Windows.Forms.TextBox hStateEdit;
  private System.Windows.Forms.TextBox hZipEdit;
  private System.Windows.Forms.TextBox hCountryEdit;
  private System.Windows.Forms.Label bMailLabel;
  private System.Windows.Forms.Label bPhoneLabel;
  private System.Windows.Forms.Label bStreetLabel;
  private System.Windows.Forms.Label bCityLabel;
  private System.Windows.Forms.Label bStateLabel;
  private System.Windows.Forms.Label bZipLabel;
  private System.Windows.Forms.Label bCountryLabel;
  private System.Windows.Forms.TextBox bMailEdit;
  private System.Windows.Forms.TextBox bPhoneEdit;
  private System.Windows.Forms.RichTextBox bStreetEdit;
  private System.Windows.Forms.TextBox bCityEdit;
  private System.Windows.Forms.TextBox bStateEdit;
  private System.Windows.Forms.TextBox bZipEdit;
  private System.Windows.Forms.TextBox bCountryEdit;
  private System.Windows.Forms.TextBox hPhoneEdit;
  private System.Windows.Forms.TabPage tabNotes;
  private System.Windows.Forms.RichTextBox notesEdit;
  private System.Windows.Forms.ComboBox comboListBy;
  private System.Windows.Forms.Button vCardImport;
  private static OpenFileDialog openFileDlg;
  public ABGui()
  {
   bool first = true;
   InitializeComponent();
   abManager = Manager.Connect( );
   addressBook = abManager.OpenDefaultAddressBook();
   foreach(AddressBook tmpBook in abManager)
   {
    Console.WriteLine(tmpBook.Name);
    if (tmpBook.Default)
    {
     Console.WriteLine("  default");
    }
   }
   bool isDefault = addressBook.Default;
   this.contacts.DisplayMember = "DisplayName";
   foreach(Contact contact in addressBook)
   {
    this.contacts.Items.Add(new ContactLite(contact.UserName, contact.ID));
   }
   this.contacts.SelectedIndexChanged += new System.EventHandler(this.contacts_SelectedIndexChanged);
   this.contacts.SelectedItem = this.contacts.Items[0];
   this.queryCombo.Items.Add("Begins");
   this.queryCombo.Items.Add("Ends");
   this.queryCombo.Items.Add("Contains");
   this.comboListBy.Text = "Username";
   this.listBy = "Username";
   this.comboListBy.Items.Add("Username");
   this.comboListBy.Items.Add("First");
   this.comboListBy.Items.Add("Last");
   this.comboListBy.Items.Add("E-Mail");
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
  private void InitializeComponent()
  {
   this.contacts = new System.Windows.Forms.ListBox();
   this.newContact = new System.Windows.Forms.Button();
   this.saveContact = new System.Windows.Forms.Button();
   this.contactID = new System.Windows.Forms.Label();
   this.contactFirst = new System.Windows.Forms.Label();
   this.contactFamily = new System.Windows.Forms.Label();
   this.editID = new System.Windows.Forms.TextBox();
   this.editFirst = new System.Windows.Forms.TextBox();
   this.editLast = new System.Windows.Forms.TextBox();
   this.labelUsername = new System.Windows.Forms.Label();
   this.editUsername = new System.Windows.Forms.TextBox();
   this.labelSearch = new System.Windows.Forms.Label();
   this.editSearch = new System.Windows.Forms.TextBox();
   this.queryCombo = new System.Windows.Forms.ComboBox();
   this.exportButton = new System.Windows.Forms.Button();
   this.importPhoto = new System.Windows.Forms.Button();
   this.contactPicture = new System.Windows.Forms.PictureBox();
   this.addressControl = new System.Windows.Forms.TabControl();
   this.businessPage = new System.Windows.Forms.TabPage();
   this.bCountryEdit = new System.Windows.Forms.TextBox();
   this.bZipEdit = new System.Windows.Forms.TextBox();
   this.bStateEdit = new System.Windows.Forms.TextBox();
   this.bCityEdit = new System.Windows.Forms.TextBox();
   this.bStreetEdit = new System.Windows.Forms.RichTextBox();
   this.bPhoneEdit = new System.Windows.Forms.TextBox();
   this.bMailEdit = new System.Windows.Forms.TextBox();
   this.bCountryLabel = new System.Windows.Forms.Label();
   this.bZipLabel = new System.Windows.Forms.Label();
   this.bStateLabel = new System.Windows.Forms.Label();
   this.bCityLabel = new System.Windows.Forms.Label();
   this.bStreetLabel = new System.Windows.Forms.Label();
   this.bPhoneLabel = new System.Windows.Forms.Label();
   this.bMailLabel = new System.Windows.Forms.Label();
   this.homePage = new System.Windows.Forms.TabPage();
   this.hCountryEdit = new System.Windows.Forms.TextBox();
   this.hZipEdit = new System.Windows.Forms.TextBox();
   this.hStateEdit = new System.Windows.Forms.TextBox();
   this.hCityEdit = new System.Windows.Forms.TextBox();
   this.hStreetEdit = new System.Windows.Forms.RichTextBox();
   this.hPhoneEdit = new System.Windows.Forms.TextBox();
   this.hMailEdit = new System.Windows.Forms.TextBox();
   this.hCountryLabel = new System.Windows.Forms.Label();
   this.hZipLabel = new System.Windows.Forms.Label();
   this.hStateLabel = new System.Windows.Forms.Label();
   this.hCityLabel = new System.Windows.Forms.Label();
   this.hStreetLabel = new System.Windows.Forms.Label();
   this.h = new System.Windows.Forms.Label();
   this.hMailLabel = new System.Windows.Forms.Label();
   this.tabNotes = new System.Windows.Forms.TabPage();
   this.notesEdit = new System.Windows.Forms.RichTextBox();
   this.comboListBy = new System.Windows.Forms.ComboBox();
   this.vCardImport = new System.Windows.Forms.Button();
   this.addressControl.SuspendLayout();
   this.businessPage.SuspendLayout();
   this.homePage.SuspendLayout();
   this.tabNotes.SuspendLayout();
   this.SuspendLayout();
   this.contacts.Font = new System.Drawing.Font("Microsoft Sans Serif", 10F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
   this.contacts.ItemHeight = 16;
   this.contacts.Location = new System.Drawing.Point(8, 72);
   this.contacts.Name = "contacts";
   this.contacts.Size = new System.Drawing.Size(264, 420);
   this.contacts.TabIndex = 0;
   this.newContact.Location = new System.Drawing.Point(288, 520);
   this.newContact.Name = "newContact";
   this.newContact.TabIndex = 5;
   this.newContact.Text = "New";
   this.newContact.Click += new System.EventHandler(this.newContact_Click);
   this.saveContact.Location = new System.Drawing.Point(376, 520);
   this.saveContact.Name = "saveContact";
   this.saveContact.TabIndex = 6;
   this.saveContact.Text = "Save";
   this.saveContact.Click += new System.EventHandler(this.saveContact_Click);
   this.contactID.Font = new System.Drawing.Font("Microsoft Sans Serif", 10F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
   this.contactID.Location = new System.Drawing.Point(288, 104);
   this.contactID.Name = "contactID";
   this.contactID.Size = new System.Drawing.Size(40, 23);
   this.contactID.TabIndex = 4;
   this.contactID.Text = "ID:";
   this.contactFirst.Font = new System.Drawing.Font("Microsoft Sans Serif", 10F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
   this.contactFirst.Location = new System.Drawing.Point(288, 168);
   this.contactFirst.Name = "contactFirst";
   this.contactFirst.TabIndex = 5;
   this.contactFirst.Text = "First:";
   this.contactFamily.Font = new System.Drawing.Font("Microsoft Sans Serif", 10F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
   this.contactFamily.Location = new System.Drawing.Point(288, 200);
   this.contactFamily.Name = "contactFamily";
   this.contactFamily.TabIndex = 6;
   this.contactFamily.Text = "Last:";
   this.editID.Font = new System.Drawing.Font("Microsoft Sans Serif", 10F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
   this.editID.Location = new System.Drawing.Point(400, 96);
   this.editID.Name = "editID";
   this.editID.ReadOnly = true;
   this.editID.Size = new System.Drawing.Size(240, 23);
   this.editID.TabIndex = 10;
   this.editID.Text = "";
   this.editFirst.Font = new System.Drawing.Font("Microsoft Sans Serif", 10F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
   this.editFirst.Location = new System.Drawing.Point(400, 160);
   this.editFirst.Name = "editFirst";
   this.editFirst.Size = new System.Drawing.Size(240, 23);
   this.editFirst.TabIndex = 2;
   this.editFirst.Text = "";
   this.editFirst.TextChanged += new System.EventHandler(this.editFirst_TextChanged);
   this.editLast.Font = new System.Drawing.Font("Microsoft Sans Serif", 10F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
   this.editLast.Location = new System.Drawing.Point(400, 192);
   this.editLast.Name = "editLast";
   this.editLast.Size = new System.Drawing.Size(240, 23);
   this.editLast.TabIndex = 3;
   this.editLast.Text = "";
   this.labelUsername.Font = new System.Drawing.Font("Microsoft Sans Serif", 10F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
   this.labelUsername.Location = new System.Drawing.Point(288, 136);
   this.labelUsername.Name = "labelUsername";
   this.labelUsername.TabIndex = 21;
   this.labelUsername.Text = "Username:";
   this.editUsername.Font = new System.Drawing.Font("Microsoft Sans Serif", 10F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
   this.editUsername.Location = new System.Drawing.Point(400, 128);
   this.editUsername.Name = "editUsername";
   this.editUsername.Size = new System.Drawing.Size(240, 23);
   this.editUsername.TabIndex = 1;
   this.editUsername.Text = "";
   this.labelSearch.Font = new System.Drawing.Font("Microsoft Sans Serif", 10F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
   this.labelSearch.Location = new System.Drawing.Point(288, 48);
   this.labelSearch.Name = "labelSearch";
   this.labelSearch.TabIndex = 32;
   this.labelSearch.Text = "Search:";
   this.editSearch.Font = new System.Drawing.Font("Microsoft Sans Serif", 10F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
   this.editSearch.Location = new System.Drawing.Point(400, 40);
   this.editSearch.Name = "editSearch";
   this.editSearch.Size = new System.Drawing.Size(240, 23);
   this.editSearch.TabIndex = 33;
   this.editSearch.Text = "";
   this.editSearch.TextChanged += new System.EventHandler(this.editSearch_TextChanged);
   this.queryCombo.Font = new System.Drawing.Font("Microsoft Sans Serif", 10F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
   this.queryCombo.Location = new System.Drawing.Point(656, 40);
   this.queryCombo.Name = "queryCombo";
   this.queryCombo.Size = new System.Drawing.Size(121, 24);
   this.queryCombo.TabIndex = 34;
   this.queryCombo.Text = "Begins";
   this.exportButton.Location = new System.Drawing.Point(464, 520);
   this.exportButton.Name = "exportButton";
   this.exportButton.Size = new System.Drawing.Size(80, 23);
   this.exportButton.TabIndex = 45;
   this.exportButton.Text = "Export vCard";
   this.exportButton.Click += new System.EventHandler(this.exportButton_Click);
   this.importPhoto.Location = new System.Drawing.Point(656, 520);
   this.importPhoto.Name = "importPhoto";
   this.importPhoto.Size = new System.Drawing.Size(80, 23);
   this.importPhoto.TabIndex = 47;
   this.importPhoto.Text = "Import Photo";
   this.importPhoto.Click += new System.EventHandler(this.importPhoto_Click);
   this.contactPicture.Location = new System.Drawing.Point(664, 96);
   this.contactPicture.Name = "contactPicture";
   this.contactPicture.Size = new System.Drawing.Size(112, 112);
   this.contactPicture.TabIndex = 48;
   this.contactPicture.TabStop = false;
   this.contactPicture.Click += new System.EventHandler(this.contactPicture_Click);
   this.addressControl.Controls.Add(this.businessPage);
   this.addressControl.Controls.Add(this.homePage);
   this.addressControl.Controls.Add(this.tabNotes);
   this.addressControl.Location = new System.Drawing.Point(280, 232);
   this.addressControl.Name = "addressControl";
   this.addressControl.SelectedIndex = 0;
   this.addressControl.Size = new System.Drawing.Size(360, 280);
   this.addressControl.TabIndex = 51;
   this.businessPage.Controls.Add(this.bCountryEdit);
   this.businessPage.Controls.Add(this.bZipEdit);
   this.businessPage.Controls.Add(this.bStateEdit);
   this.businessPage.Controls.Add(this.bCityEdit);
   this.businessPage.Controls.Add(this.bStreetEdit);
   this.businessPage.Controls.Add(this.bPhoneEdit);
   this.businessPage.Controls.Add(this.bMailEdit);
   this.businessPage.Controls.Add(this.bCountryLabel);
   this.businessPage.Controls.Add(this.bZipLabel);
   this.businessPage.Controls.Add(this.bStateLabel);
   this.businessPage.Controls.Add(this.bCityLabel);
   this.businessPage.Controls.Add(this.bStreetLabel);
   this.businessPage.Controls.Add(this.bPhoneLabel);
   this.businessPage.Controls.Add(this.bMailLabel);
   this.businessPage.Font = new System.Drawing.Font("Microsoft Sans Serif", 10F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
   this.businessPage.Location = new System.Drawing.Point(4, 22);
   this.businessPage.Name = "businessPage";
   this.businessPage.Size = new System.Drawing.Size(352, 254);
   this.businessPage.TabIndex = 1;
   this.businessPage.Text = "Business";
   this.businessPage.Click += new System.EventHandler(this.businessPage_Click);
   this.bCountryEdit.Location = new System.Drawing.Point(120, 216);
   this.bCountryEdit.Name = "bCountryEdit";
   this.bCountryEdit.Size = new System.Drawing.Size(216, 23);
   this.bCountryEdit.TabIndex = 13;
   this.bCountryEdit.Text = "";
   this.bZipEdit.Location = new System.Drawing.Point(120, 184);
   this.bZipEdit.Name = "bZipEdit";
   this.bZipEdit.Size = new System.Drawing.Size(216, 23);
   this.bZipEdit.TabIndex = 12;
   this.bZipEdit.Text = "";
   this.bStateEdit.Location = new System.Drawing.Point(120, 152);
   this.bStateEdit.Name = "bStateEdit";
   this.bStateEdit.Size = new System.Drawing.Size(216, 23);
   this.bStateEdit.TabIndex = 11;
   this.bStateEdit.Text = "";
   this.bCityEdit.Location = new System.Drawing.Point(120, 120);
   this.bCityEdit.Name = "bCityEdit";
   this.bCityEdit.Size = new System.Drawing.Size(216, 23);
   this.bCityEdit.TabIndex = 10;
   this.bCityEdit.Text = "";
   this.bStreetEdit.Location = new System.Drawing.Point(120, 72);
   this.bStreetEdit.Name = "bStreetEdit";
   this.bStreetEdit.Size = new System.Drawing.Size(216, 40);
   this.bStreetEdit.TabIndex = 9;
   this.bStreetEdit.Text = "";
   this.bPhoneEdit.Location = new System.Drawing.Point(120, 40);
   this.bPhoneEdit.Name = "bPhoneEdit";
   this.bPhoneEdit.Size = new System.Drawing.Size(216, 23);
   this.bPhoneEdit.TabIndex = 8;
   this.bPhoneEdit.Text = "";
   this.bMailEdit.Location = new System.Drawing.Point(120, 8);
   this.bMailEdit.Name = "bMailEdit";
   this.bMailEdit.Size = new System.Drawing.Size(216, 23);
   this.bMailEdit.TabIndex = 7;
   this.bMailEdit.Text = "";
   this.bCountryLabel.Location = new System.Drawing.Point(8, 224);
   this.bCountryLabel.Name = "bCountryLabel";
   this.bCountryLabel.TabIndex = 6;
   this.bCountryLabel.Text = "Country:";
   this.bZipLabel.Location = new System.Drawing.Point(8, 192);
   this.bZipLabel.Name = "bZipLabel";
   this.bZipLabel.Size = new System.Drawing.Size(96, 16);
   this.bZipLabel.TabIndex = 5;
   this.bZipLabel.Text = "Zip Code:";
   this.bStateLabel.Location = new System.Drawing.Point(8, 160);
   this.bStateLabel.Name = "bStateLabel";
   this.bStateLabel.TabIndex = 4;
   this.bStateLabel.Text = "State/Province:";
   this.bCityLabel.Location = new System.Drawing.Point(8, 128);
   this.bCityLabel.Name = "bCityLabel";
   this.bCityLabel.Size = new System.Drawing.Size(104, 16);
   this.bCityLabel.TabIndex = 3;
   this.bCityLabel.Text = "City:";
   this.bStreetLabel.Location = new System.Drawing.Point(8, 80);
   this.bStreetLabel.Name = "bStreetLabel";
   this.bStreetLabel.TabIndex = 2;
   this.bStreetLabel.Text = "Street:";
   this.bPhoneLabel.Location = new System.Drawing.Point(8, 48);
   this.bPhoneLabel.Name = "bPhoneLabel";
   this.bPhoneLabel.TabIndex = 1;
   this.bPhoneLabel.Text = "Telephone:";
   this.bMailLabel.Location = new System.Drawing.Point(8, 16);
   this.bMailLabel.Name = "bMailLabel";
   this.bMailLabel.TabIndex = 0;
   this.bMailLabel.Text = "E-Mail:";
   this.homePage.Controls.Add(this.hCountryEdit);
   this.homePage.Controls.Add(this.hZipEdit);
   this.homePage.Controls.Add(this.hStateEdit);
   this.homePage.Controls.Add(this.hCityEdit);
   this.homePage.Controls.Add(this.hStreetEdit);
   this.homePage.Controls.Add(this.hPhoneEdit);
   this.homePage.Controls.Add(this.hMailEdit);
   this.homePage.Controls.Add(this.hCountryLabel);
   this.homePage.Controls.Add(this.hZipLabel);
   this.homePage.Controls.Add(this.hStateLabel);
   this.homePage.Controls.Add(this.hCityLabel);
   this.homePage.Controls.Add(this.hStreetLabel);
   this.homePage.Controls.Add(this.h);
   this.homePage.Controls.Add(this.hMailLabel);
   this.homePage.Font = new System.Drawing.Font("Microsoft Sans Serif", 10F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
   this.homePage.Location = new System.Drawing.Point(4, 22);
   this.homePage.Name = "homePage";
   this.homePage.Size = new System.Drawing.Size(352, 254);
   this.homePage.TabIndex = 0;
   this.homePage.Text = "Home";
   this.homePage.Click += new System.EventHandler(this.homePage_Click);
   this.hCountryEdit.Location = new System.Drawing.Point(120, 216);
   this.hCountryEdit.Name = "hCountryEdit";
   this.hCountryEdit.Size = new System.Drawing.Size(216, 23);
   this.hCountryEdit.TabIndex = 13;
   this.hCountryEdit.Text = "";
   this.hZipEdit.Location = new System.Drawing.Point(120, 184);
   this.hZipEdit.Name = "hZipEdit";
   this.hZipEdit.Size = new System.Drawing.Size(216, 23);
   this.hZipEdit.TabIndex = 12;
   this.hZipEdit.Text = "";
   this.hStateEdit.Location = new System.Drawing.Point(120, 152);
   this.hStateEdit.Name = "hStateEdit";
   this.hStateEdit.Size = new System.Drawing.Size(216, 23);
   this.hStateEdit.TabIndex = 11;
   this.hStateEdit.Text = "";
   this.hCityEdit.Location = new System.Drawing.Point(120, 120);
   this.hCityEdit.Name = "hCityEdit";
   this.hCityEdit.Size = new System.Drawing.Size(216, 23);
   this.hCityEdit.TabIndex = 10;
   this.hCityEdit.Text = "";
   this.hStreetEdit.Location = new System.Drawing.Point(120, 72);
   this.hStreetEdit.Name = "hStreetEdit";
   this.hStreetEdit.Size = new System.Drawing.Size(216, 40);
   this.hStreetEdit.TabIndex = 9;
   this.hStreetEdit.Text = "";
   this.hPhoneEdit.Location = new System.Drawing.Point(120, 40);
   this.hPhoneEdit.Name = "hPhoneEdit";
   this.hPhoneEdit.Size = new System.Drawing.Size(216, 23);
   this.hPhoneEdit.TabIndex = 8;
   this.hPhoneEdit.Text = "";
   this.hMailEdit.Location = new System.Drawing.Point(120, 8);
   this.hMailEdit.Name = "hMailEdit";
   this.hMailEdit.Size = new System.Drawing.Size(216, 23);
   this.hMailEdit.TabIndex = 7;
   this.hMailEdit.Text = "";
   this.hCountryLabel.Location = new System.Drawing.Point(8, 224);
   this.hCountryLabel.Name = "hCountryLabel";
   this.hCountryLabel.TabIndex = 6;
   this.hCountryLabel.Text = "Country:";
   this.hZipLabel.Location = new System.Drawing.Point(8, 192);
   this.hZipLabel.Name = "hZipLabel";
   this.hZipLabel.TabIndex = 5;
   this.hZipLabel.Text = "Zip Code:";
   this.hStateLabel.Location = new System.Drawing.Point(8, 160);
   this.hStateLabel.Name = "hStateLabel";
   this.hStateLabel.TabIndex = 4;
   this.hStateLabel.Text = "State/Province:";
   this.hCityLabel.Location = new System.Drawing.Point(8, 128);
   this.hCityLabel.Name = "hCityLabel";
   this.hCityLabel.Size = new System.Drawing.Size(104, 16);
   this.hCityLabel.TabIndex = 3;
   this.hCityLabel.Text = "City:";
   this.hStreetLabel.Location = new System.Drawing.Point(8, 80);
   this.hStreetLabel.Name = "hStreetLabel";
   this.hStreetLabel.TabIndex = 2;
   this.hStreetLabel.Text = "Street:";
   this.h.Location = new System.Drawing.Point(8, 48);
   this.h.Name = "h";
   this.h.Size = new System.Drawing.Size(104, 16);
   this.h.TabIndex = 1;
   this.h.Text = "Telephone:";
   this.hMailLabel.Location = new System.Drawing.Point(8, 16);
   this.hMailLabel.Name = "hMailLabel";
   this.hMailLabel.Size = new System.Drawing.Size(104, 16);
   this.hMailLabel.TabIndex = 0;
   this.hMailLabel.Text = "E-Mail:";
   this.tabNotes.Controls.Add(this.notesEdit);
   this.tabNotes.Location = new System.Drawing.Point(4, 22);
   this.tabNotes.Name = "tabNotes";
   this.tabNotes.Size = new System.Drawing.Size(352, 254);
   this.tabNotes.TabIndex = 2;
   this.tabNotes.Text = "Notes";
   this.tabNotes.Click += new System.EventHandler(this.tabNotes_Click);
   this.notesEdit.Location = new System.Drawing.Point(8, 8);
   this.notesEdit.Name = "notesEdit";
   this.notesEdit.Size = new System.Drawing.Size(336, 240);
   this.notesEdit.TabIndex = 0;
   this.notesEdit.Text = "";
   this.comboListBy.Font = new System.Drawing.Font("Microsoft Sans Serif", 10F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
   this.comboListBy.Location = new System.Drawing.Point(8, 40);
   this.comboListBy.Name = "comboListBy";
   this.comboListBy.Size = new System.Drawing.Size(264, 24);
   this.comboListBy.TabIndex = 52;
   this.comboListBy.SelectedIndexChanged += new System.EventHandler(this.comboBox1_SelectedIndexChanged);
   this.vCardImport.Location = new System.Drawing.Point(560, 520);
   this.vCardImport.Name = "vCardImport";
   this.vCardImport.Size = new System.Drawing.Size(80, 23);
   this.vCardImport.TabIndex = 53;
   this.vCardImport.Text = "Import vCard";
   this.vCardImport.Click += new System.EventHandler(this.vCardImport_Click);
   this.AutoScale = false;
   this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
   this.ClientSize = new System.Drawing.Size(792, 550);
   this.Controls.Add(this.vCardImport);
   this.Controls.Add(this.comboListBy);
   this.Controls.Add(this.addressControl);
   this.Controls.Add(this.contactPicture);
   this.Controls.Add(this.importPhoto);
   this.Controls.Add(this.exportButton);
   this.Controls.Add(this.queryCombo);
   this.Controls.Add(this.editSearch);
   this.Controls.Add(this.labelSearch);
   this.Controls.Add(this.editUsername);
   this.Controls.Add(this.labelUsername);
   this.Controls.Add(this.editLast);
   this.Controls.Add(this.editFirst);
   this.Controls.Add(this.editID);
   this.Controls.Add(this.contactFamily);
   this.Controls.Add(this.contactFirst);
   this.Controls.Add(this.contactID);
   this.Controls.Add(this.saveContact);
   this.Controls.Add(this.newContact);
   this.Controls.Add(this.contacts);
   this.Name = "ABGui";
   this.Text = "Address Book Test Application";
   this.addressControl.ResumeLayout(false);
   this.businessPage.ResumeLayout(false);
   this.homePage.ResumeLayout(false);
   this.tabNotes.ResumeLayout(false);
   this.ResumeLayout(false);
  }
  [STAThread]
  static void Main()
  {
   Application.Run(new ABGui());
  }
  private void contacts_SelectedIndexChanged(object sender, System.EventArgs e)
  {
   ContactLite cLite = (ContactLite) contacts.Items[contacts.SelectedIndex];
   Contact contact = addressBook.GetContact(cLite.ID);
   editUsername.Text = contact.UserName;
   editID.Text = contact.ID;
   try
   {
    Name prefName = contact.GetPreferredName();
    editFirst.Text = prefName.Given;
    editLast.Text = prefName.Family;
   }
   catch
   {
    editFirst.Text = "";
    editLast.Text = "";
   }
   try
   {
    notesEdit.Text = contact.Note;
   }
   catch
   {
    notesEdit.Text = "";
   }
   bool results = LoadAddresses(contact);
   results = LoadContactPicture(contact);
   if (results == false)
   {
    contactPicture.Visible = false;
   }
  }
  private void newContact_Click(object sender, System.EventArgs e)
  {
   editFirst.Clear();
   editLast.Clear();
   editUsername.Clear();
   editID.Clear();
   editSearch.Clear();
   bMailEdit.Clear();
   bPhoneEdit.Clear();
   bStreetEdit.Clear();
   bCityEdit.Clear();
   bStateEdit.Clear();
   bZipEdit.Clear();
   bCountryEdit.Clear();
   hMailEdit.Clear();
   hPhoneEdit.Clear();
   hStreetEdit.Clear();
   hCityEdit.Clear();
   hStateEdit.Clear();
   hZipEdit.Clear();
   hCountryEdit.Clear();
   notesEdit.Clear();
   contactPicture.Visible = false;
  }
  private void saveContact_Click(object sender, System.EventArgs e)
  {
   string username = null;
   string first = null;
   string last = null;
   string email = null;
   username = editUsername.Text;
   first = editFirst.Text;
   last = editLast.Text;
   email = bMailEdit.Text;
   if (username != null)
   {
    if (editID.Text == null || editID.Text == "")
    {
     Contact contact = new Contact();
     contact.UserName = username;
     if (first != null && last != null)
     {
      try
      {
       Name prefName = new Name(first, last);
       prefName.Preferred = true;
       contact.AddName(prefName);
      }
      catch{}
     }
     if (bMailEdit.Text != null && bMailEdit.Text != "")
     {
      contact.EMail = bMailEdit.Text;
     }
     if (hMailEdit.Text != null && hMailEdit.Text != "")
     {
      Email tmpMail = new Email((EmailTypes.internet | EmailTypes.personal), hMailEdit.Text);
      tmpMail.Preferred = false;
      contact.AddEmailAddress(tmpMail);
     }
     if (bPhoneEdit.Text != null && bPhoneEdit.Text != "")
     {
      Telephone tmpPhone = new Telephone(bPhoneEdit.Text);
      tmpPhone.Types = (PhoneTypes.work | PhoneTypes.voice);
      tmpPhone.Preferred = true;
      contact.AddTelephoneNumber(tmpPhone);
     }
     if (hPhoneEdit.Text != null && hPhoneEdit.Text != "")
     {
      Telephone tmpPhone = new Telephone(hPhoneEdit.Text);
      tmpPhone.Types = (PhoneTypes.home | PhoneTypes.voice);
      contact.AddTelephoneNumber(tmpPhone);
     }
     if (bZipEdit.Text != null && bZipEdit.Text != "")
     {
      SaveWorkAddress(contact);
     }
     if (hZipEdit.Text != null && hZipEdit.Text != "")
     {
      SaveHomeAddress(contact);
     }
     if (notesEdit.Text != null && notesEdit.Text != "")
     {
      contact.Note = notesEdit.Text;
     }
     addressBook.AddContact(contact);
     contact.Commit();
     editID.Text = contact.ID;
     ContactLite cLite = new ContactLite(contact.UserName, contact.ID);
     this.contacts.Items.Add(cLite);
     this.contacts.SelectedItem = cLite;
    }
    else
    {
     Contact contact = addressBook.GetContact(editID.Text);
     try
     {
      try
      {
       foreach(Email tmpEmail in contact.GetEmailAddresses())
       {
        tmpEmail.Delete();
       }
      }
      catch{}
      if (bMailEdit.Text != null && bMailEdit.Text != "")
      {
       contact.EMail = bMailEdit.Text;
      }
      if (hMailEdit.Text != null && hMailEdit.Text != "")
      {
       Email tmpMail = new Email((EmailTypes.internet | EmailTypes.personal), hMailEdit.Text);
       tmpMail.Preferred = false;
       contact.AddEmailAddress(tmpMail);
      }
     }
     catch(System.NullReferenceException)
     {
     }
     try
     {
      try
      {
       foreach(Telephone tmpPhone in contact.GetTelephoneNumbers())
       {
        tmpPhone.Delete();
       }
      }
      catch{}
      if (bPhoneEdit.Text != null && bPhoneEdit.Text != "")
      {
       Telephone tmpPhone = new Telephone(bPhoneEdit.Text);
       tmpPhone.Types = (PhoneTypes.preferred | PhoneTypes.work | PhoneTypes.voice);
       contact.AddTelephoneNumber(tmpPhone);
      }
      if (hPhoneEdit.Text != null && hPhoneEdit.Text != "")
      {
       Telephone tmpPhone = new Telephone(hPhoneEdit.Text);
       tmpPhone.Types = (PhoneTypes.home | PhoneTypes.voice);
       contact.AddTelephoneNumber(tmpPhone);
      }
     }
     catch{}
     if (first != null && last != null)
     {
      Name prefName;
      try
      {
       prefName = contact.GetPreferredName();
       if (prefName.Given != first)
       {
        prefName.Given = first;
       }
       if (prefName.Family != last)
       {
        prefName.Family = last;
       }
       if (prefName.Preferred != true)
       {
        prefName.Preferred = true;
       }
      }
      catch
      {
       try
       {
        prefName = new Name(first, last);
        prefName.Preferred = true;
        contact.AddName(prefName);
       }
       catch{}
      }
     }
     if (contact.UserName != username)
     {
      try
      {
       this.contacts.Items.Remove(contact.UserName);
      }
      catch
      {
       this.contacts.Refresh();
      }
      try
      {
       this.contacts.Items.Add(username);
      }
      catch{}
      contact.UserName = username;
      this.contacts.Refresh();
     }
     if (notesEdit.Text != null && notesEdit.Text != "")
     {
      contact.Note = notesEdit.Text;
     }
     if (bZipEdit.Text != null && bZipEdit.Text != "")
     {
      SaveWorkAddress(contact);
     }
     if (hZipEdit.Text != null && hZipEdit.Text != "")
     {
      SaveHomeAddress(contact);
     }
     contact.Commit();
    }
   }
  }
  private void editFirst_TextChanged(object sender, System.EventArgs e)
  {
  }
  private void editSearch_TextChanged(object sender, System.EventArgs e)
  {
   if (editSearch.Text == "")
   {
    return;
   }
   string empty = "";
   contacts.Items.Clear();
   if (editSearch.Text == "*")
   {
    Cursor.Current = Cursors.WaitCursor;
    contacts.BeginUpdate();
    foreach(Contact contact2 in addressBook)
    {
     switch(listBy)
     {
      case "Username":
      {
       ContactLite cLite = new ContactLite(contact2.UserName, contact2.ID);
       this.contacts.Items.Add(cLite);
       break;
      }
      case "E-Mail":
      {
       if (contact2.EMail != "")
       {
        ContactLite cLite = new ContactLite(contact2.EMail, contact2.ID);
        this.contacts.Items.Add(cLite);
       }
       break;
      }
      case "First":
      {
       try
       {
        Name name = contact2.GetPreferredName();
        if (name.Given != "")
        {
         ContactLite cLite = new ContactLite(name.Given, contact2.ID);
         this.contacts.Items.Add(cLite);
        }
       }
       catch{}
       break;
      }
      case "Last":
      {
       try
       {
        Name name = contact2.GetPreferredName();
        if (name.Family != "")
        {
         ContactLite cLite = new ContactLite(name.Family, contact2.ID);
         this.contacts.Items.Add(cLite);
        }
       }
       catch{}
       break;
      }
      default:
      {
       ContactLite cLite = new ContactLite(contact2.UserName, contact2.ID);
       this.contacts.Items.Add(cLite);
       break;
      }
     }
    }
    Cursor.Current = Cursors.Default;
    contacts.EndUpdate();
   }
   else
   {
    Property.Operator queryType;
    Cursor.Current = Cursors.WaitCursor;
    contacts.BeginUpdate();
    string query = this.queryCombo.Text;
    if (query == "Begins")
    {
     queryType = Property.Operator.Begins;
    }
    else
    if (query == "Ends")
    {
     queryType = Property.Operator.Ends;
    }
    else
    if (query == "Contains")
    {
     queryType = Property.Operator.Contains;
    }
    else
    {
     queryType = Property.Operator.Begins;
    }
    IABList searchResults;
    if (listBy == "Username")
    {
     searchResults = (IABList) addressBook.SearchUsername(editSearch.Text, queryType);
    }
    else
    if (listBy == "E-Mail")
    {
     searchResults = addressBook.SearchEmail(editSearch.Text, queryType);
    }
    else
    if (listBy == "First")
    {
     searchResults = addressBook.SearchFirstName(editSearch.Text, queryType);
    }
    else
    if (listBy == "Last")
    {
     searchResults = addressBook.SearchLastName(editSearch.Text, queryType);
    }
    else
    {
     searchResults = addressBook.SearchUsername(editSearch.Text, queryType);
    }
    bool first = true;
    try
    {
     foreach(Contact contact3 in searchResults)
     {
      switch(listBy)
      {
       case "Username":
       {
        ContactLite cLite = new ContactLite(contact3.UserName, contact3.ID);
        this.contacts.Items.Add(cLite);
        break;
       }
       case "E-Mail":
       {
        if (contact3.EMail != "")
        {
         ContactLite cLite = new ContactLite(contact3.EMail, contact3.ID);
         this.contacts.Items.Add(cLite);
        }
        break;
       }
       case "First":
       {
        try
        {
         Name name = contact3.GetPreferredName();
         if (name.Given != "")
         {
          ContactLite cLite = new ContactLite(name.Given, contact3.ID);
          this.contacts.Items.Add(cLite);
         }
        }
        catch{}
        break;
       }
       case "Last":
       {
        try
        {
         Name name = contact3.GetPreferredName();
         if (name.Family != "")
         {
          ContactLite cLite = new ContactLite(name.Family, contact3.ID);
          this.contacts.Items.Add(cLite);
         }
        }
        catch{}
        break;
       }
       default:
       {
        ContactLite cLite = new ContactLite(contact3.UserName, contact3.ID);
        this.contacts.Items.Add(cLite);
        break;
       }
      }
     }
     ContactLite cLite1 = (ContactLite) contacts.Items[0];
     Contact contact = addressBook.GetContact(cLite1.ID);
     editUsername.Text = contact.UserName;
     editID.Text = contact.ID;
     try
     {
      Name prefName = contact.GetPreferredName();
      editFirst.Text = prefName.Given;
      editLast.Text = prefName.Family;
     }
     catch
     {
      editFirst.Text = "";
      editLast.Text = "";
     }
     try
     {
      notesEdit.Text = contact.Note;
     }
     catch
     {
      notesEdit.Text = "";
     }
     LoadAddresses(contact);
     if( LoadContactPicture(contact) == false)
     {
      contactPicture.Visible = false;
     }
    }
    catch{}
    contacts.EndUpdate();
    Cursor.Current = Cursors.Default;
   }
  }
  private void label1_Click(object sender, System.EventArgs e)
  {
  }
  private void exportButton_Click(object sender, System.EventArgs e)
  {
   Contact contact = addressBook.GetContact(editID.Text);
   if (contact != null)
   {
    SaveFileDialog saveDlg = new SaveFileDialog();
    string s = Path.GetDirectoryName(Path.GetDirectoryName(Directory.GetCurrentDirectory()));
    saveDlg.InitialDirectory = s;
    saveDlg.AddExtension = true;
    saveDlg.Filter = "vcf files (*.vcf)|*.vcf" ;
    saveDlg.FileName = "VCARD_FOR_" + contact.FN;
    if(saveDlg.ShowDialog() == DialogResult.OK)
    {
     contact.ExportVCard(saveDlg.FileName);
     MessageBox.Show("vCard exported!");
    }
   }
  }
  private bool LoadAddresses(Contact contact)
  {
   bool foundHome = false;
   bool foundWork = false;
   bool foundHomeMail = false;
   bool foundWorkMail = false;
   bool foundHomePhone = false;
   bool foundWorkPhone = false;
   try
   {
    foreach(Email tmpMail in contact.GetEmailAddresses())
    {
     if ((tmpMail.Types & EmailTypes.work) == EmailTypes.work)
     {
      bMailEdit.Text = tmpMail.Address;
      foundWorkMail = true;
     }
     else
      if ((tmpMail.Types & EmailTypes.personal) == EmailTypes.personal)
     {
      hMailEdit.Text = tmpMail.Address;
      foundHomeMail = true;
     }
    }
   }
   catch{}
   if (foundHomeMail == false)
   {
    hMailEdit.Clear();
   }
   if (foundWorkMail == false)
   {
    bMailEdit.Clear();
   }
   try
   {
    foreach(Telephone tmpPhone in contact.GetTelephoneNumbers())
    {
     if ((tmpPhone.Types & PhoneTypes.work) == PhoneTypes.work)
     {
      bPhoneEdit.Text = tmpPhone.Number;
      foundWorkPhone = true;
     }
     else
     if ((tmpPhone.Types & PhoneTypes.home) == PhoneTypes.home)
     {
      hPhoneEdit.Text = tmpPhone.Number;
      foundHomePhone = true;
     }
    }
   }
   catch{}
   if (foundHomePhone == false)
   {
    hPhoneEdit.Clear();
   }
   if (foundWorkPhone == false)
   {
    bPhoneEdit.Clear();
   }
   foreach(Address addr in contact.GetAddresses())
   {
    if((addr.Types & AddressTypes.work) == AddressTypes.work)
    {
     bStreetEdit.Text = addr.Street;
     bCityEdit.Text = addr.Locality;
     bStateEdit.Text = addr.Region;
     bZipEdit.Text = addr.PostalCode;
     bCountryEdit.Text = addr.Country;
     foundWork = true;
    }
    else
    if((addr.Types & AddressTypes.home) == AddressTypes.home)
    {
     hStreetEdit.Text = addr.Street;
     hCityEdit.Text = addr.Locality;
     hStateEdit.Text = addr.Region;
     hZipEdit.Text = addr.PostalCode;
     hCountryEdit.Text = addr.Country;
     foundHome = true;
    }
   }
   if(foundHome == false)
   {
    hStreetEdit.Text = "";
    hCityEdit.Text = "";
    hStateEdit.Text = "";
    hZipEdit.Text = "";
    hCountryEdit.Text = "";
   }
   if(foundWork == false)
   {
    bStreetEdit.Text = "";
    bCityEdit.Text = "";
    bStateEdit.Text = "";
    bZipEdit.Text = "";
    bCountryEdit.Text = "";
   }
   return(true);
  }
  private bool LoadContactPicture(Contact contact)
  {
   Stream srcStream = null;
   try
   {
    srcStream = contact.ExportPhoto();
    if (srcStream != null)
    {
     contactPicture.Image = System.Drawing.Image.FromStream(srcStream);
     srcStream.Close();
     contactPicture.SizeMode = PictureBoxSizeMode.StretchImage;
     contactPicture.Visible = true;
     contactPicture.Refresh();
     return(true);
    }
   }
   catch
   {
    if (srcStream != null)
    {
     srcStream.Close();
    }
   }
   return(false);
  }
  private bool SaveHomeAddress(Contact contact)
  {
   bool results = true;
   Address tmpAddress = null;
   if (hZipEdit.Text != null && hZipEdit.Text != "")
   {
    bool homeExists = false;
    try
    {
     IEnumerator addrEnum = contact.GetAddresses().GetEnumerator();
     while(addrEnum.MoveNext())
     {
      tmpAddress = (Address) addrEnum.Current;
      if ((tmpAddress.Types & AddressTypes.home) == AddressTypes.home)
      {
       homeExists = true;
       break;
      }
     }
     if (homeExists == true)
     {
      if (tmpAddress.Street != hStreetEdit.Text)
      {
       tmpAddress.Street = hStreetEdit.Text;
      }
      if (tmpAddress.Locality != hCityEdit.Text)
      {
       tmpAddress.Locality = hCityEdit.Text;
      }
      if (tmpAddress.Region != hStateEdit.Text)
      {
       tmpAddress.Region = hStateEdit.Text;
      }
      if (tmpAddress.PostalCode != hZipEdit.Text)
      {
       tmpAddress.PostalCode = hZipEdit.Text;
      }
      if (tmpAddress.Country != hCountryEdit.Text)
      {
       tmpAddress.Country = hCountryEdit.Text;
      }
      tmpAddress.Commit();
     }
     else
     {
      tmpAddress = new Address(hZipEdit.Text);
      tmpAddress.Preferred = false;
      tmpAddress.Types = AddressTypes.home | AddressTypes.dom;
      tmpAddress.Street = hStreetEdit.Text;
      tmpAddress.Locality = hCityEdit.Text;
      tmpAddress.Region = hStateEdit.Text;
      tmpAddress.Country = hCountryEdit.Text;
      contact.AddAddress(tmpAddress);
     }
    }
    catch{}
   }
   return(results);
  }
  private bool SaveWorkAddress(Contact contact)
  {
   bool results = true;
   Address tmpAddress = null;
   if (bZipEdit.Text != null && bZipEdit.Text != "")
   {
    bool workExists = false;
    try
    {
     IEnumerator addrEnum = contact.GetAddresses().GetEnumerator();
     while(addrEnum.MoveNext())
     {
      tmpAddress = (Address) addrEnum.Current;
      if((tmpAddress.Types & AddressTypes.work) == AddressTypes.work)
      {
       workExists = true;
       break;
      }
     }
     if (workExists == true)
     {
      if (tmpAddress.Street != bStreetEdit.Text)
      {
       tmpAddress.Street = bStreetEdit.Text;
      }
      if (tmpAddress.Locality != bCityEdit.Text)
      {
       tmpAddress.Locality = bCityEdit.Text;
      }
      if (tmpAddress.Region != bStateEdit.Text)
      {
       tmpAddress.Region = bStateEdit.Text;
      }
      if (tmpAddress.PostalCode != bZipEdit.Text)
      {
       tmpAddress.PostalCode = bZipEdit.Text;
      }
      if (tmpAddress.Country != bCountryEdit.Text)
      {
       tmpAddress.Country = bCountryEdit.Text;
      }
      tmpAddress.Commit();
     }
     else
     {
      tmpAddress = new Address(bZipEdit.Text);
      tmpAddress.Preferred = true;
      tmpAddress.Street = bStreetEdit.Text;
      tmpAddress.Locality = bCityEdit.Text;
      tmpAddress.Region = bStateEdit.Text;
      tmpAddress.Country = bCountryEdit.Text;
      contact.AddAddress(tmpAddress);
     }
    }
    catch{}
   }
   return(results);
  }
  private void importPhoto_Click(object sender, System.EventArgs e)
  {
   openFileDlg = new OpenFileDialog();
   string s = Path.GetDirectoryName(Path.GetDirectoryName(Directory.GetCurrentDirectory()));
   openFileDlg.InitialDirectory = s;
   openFileDlg.AddExtension = true;
   openFileDlg.Filter = "jpg files (*.jpg)|*.jpg" ;
   if(openFileDlg.ShowDialog() == DialogResult.OK)
   {
    if (editID.Text != null && editID.Text != "")
    {
     bool results;
     Contact contact = addressBook.GetContact(editID.Text);
     results = contact.ImportPhoto(openFileDlg.FileName);
     if (results == true)
     {
      LoadContactPicture(contact);
     }
     else
     {
      contactPicture.Visible = false;
     }
    }
   }
  }
  public static void openFileDlg_OK(object sender, System.ComponentModel.CancelEventArgs e)
  {
   string fileName = openFileDlg.FileName;
   Console.WriteLine(fileName);
   Stream t = new FileStream(fileName, FileMode.Open);
   BinaryReader binaryRead = new BinaryReader(t);
   binaryRead.BaseStream.Position = 0;
   int i = 0;
   while(true)
   {
    i = binaryRead.BaseStream.ReadByte();
    if(i == -1)
    {
     break;
    }
   }
   binaryRead.Close();
   t.Close();
  }
  private void contactPicture_Click(object sender, System.EventArgs e)
  {
  }
  private void homePage_Click(object sender, System.EventArgs e)
  {
  }
  private void businessPage_Click(object sender, System.EventArgs e)
  {
  }
  private void tabNotes_Click(object sender, System.EventArgs e)
  {
  }
  private void comboBox1_SelectedIndexChanged(object sender, System.EventArgs e)
  {
   if (this.comboListBy.Text != listBy)
   {
    listBy = this.comboListBy.Text;
    contacts.Items.Clear();
    if (listBy == "Username")
    {
     foreach(Contact contact in addressBook)
     {
      this.contacts.Items.Add(new ContactLite(contact.UserName, contact.ID));
     }
    }
    else
    if (listBy == "First")
    {
     foreach(Contact contact in addressBook)
     {
      try
      {
       Name name = contact.GetPreferredName();
       if (name.Given != "")
       {
        this.contacts.Items.Add(new ContactLite(name.Given, contact.ID));
       }
      }
      catch{}
     }
    }
    else
    if (listBy == "Last")
    {
     foreach(Contact contact in addressBook)
     {
      try
      {
       Name name = contact.GetPreferredName();
       if (name.Family != "")
       {
        this.contacts.Items.Add(new ContactLite(name.Family, contact.ID));
       }
      }
      catch{}
     }
    }
    else
    if (listBy == "E-Mail")
    {
     foreach(Contact contact in addressBook)
     {
      if (contact.EMail != null && contact.EMail != "")
      {
       this.contacts.Items.Add(new ContactLite(contact.EMail, contact.ID));
      }
     }
    }
    this.contacts.SelectedItem = this.contacts.Items[0];
   }
  }
  private void vCardImport_Click(object sender, System.EventArgs e)
  {
   OpenFileDialog openFileDlg = new OpenFileDialog();
   string s = Path.GetDirectoryName(Path.GetDirectoryName(Directory.GetCurrentDirectory()));
   openFileDlg.InitialDirectory = s;
   openFileDlg.AddExtension = true;
   openFileDlg.Filter = "vcf files (*.vcf)|*.vcf" ;
   if(openFileDlg.ShowDialog() == DialogResult.OK)
   {
    Contact newContact = addressBook.ImportVCard(openFileDlg.FileName);
    if (newContact != null)
    {
     IABList emails = newContact.GetEmailAddresses();
     bool changedOther = false;
     foreach(Email tmpMail in emails)
     {
      if ((tmpMail.Types & EmailTypes.work) == EmailTypes.work)
      {
       tmpMail.Preferred = true;
      }
      else
      if (changedOther == false && (tmpMail.Types & EmailTypes.other) == EmailTypes.other)
      {
       EmailTypes tmp = tmpMail.Types;
       tmp &= ~EmailTypes.other;
       tmp |= EmailTypes.personal;
       tmpMail.Types = tmp;
       changedOther = true;
      }
     }
     IABList phoneNumbers = newContact.GetTelephoneNumbers();
     foreach(Telephone tmpPhone in phoneNumbers)
     {
      if (tmpPhone.Preferred == false)
      {
       tmpPhone.Preferred = true;
       break;
      }
     }
     newContact.Commit();
     ContactLite cLite = new ContactLite(newContact.UserName, newContact.ID);
     this.contacts.Items.Add(cLite);
     this.contacts.SelectedItem = cLite;
    }
   }
  }
 }
 public class ContactLite
 {
  private string displayname;
  private string id;
  public string DisplayName
  {
   get
   {
    return(displayname);
   }
  }
  public string ID
  {
   get
   {
    return(id);
   }
  }
  public ContactLite(string name, string id)
  {
   this.displayname = name;
   this.id = id;
  }
 }
}
