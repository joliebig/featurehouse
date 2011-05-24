

using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;
using System.IO;
using Novell.AddressBook;
using Simias;

namespace Novell.iFolder.FormsBookLib
{



 public class ContactEditor : System.Windows.Forms.Form
 {

  private static readonly ISimiasLog logger = SimiasLogManager.GetLogger(typeof(ContactEditor));
  private const string applicationIcon = @"res\ifolder_contact_card.ico";
  private System.Windows.Forms.TabControl tabControl1;
  private System.Windows.Forms.TabPage tabPage1;
  private System.Windows.Forms.Button cancel;
  private System.Windows.Forms.Button ok;
  private System.Windows.Forms.Button fullNameButton;
  private System.Windows.Forms.TextBox fullName;
  private System.Windows.Forms.Label label1;
  private System.Windows.Forms.PictureBox pictureContact;
  private System.Windows.Forms.GroupBox groupBox1;
  private System.Windows.Forms.Label label2;
  private System.Windows.Forms.DomainUpDown emailLabel1;
  private System.Windows.Forms.TextBox email1;
  private System.Windows.Forms.TextBox email4;
  private System.Windows.Forms.DomainUpDown emailLabel4;
  private System.Windows.Forms.TextBox email2;
  private System.Windows.Forms.DomainUpDown emailLabel2;
  private System.Windows.Forms.TextBox email3;
  private System.Windows.Forms.DomainUpDown emailLabel3;
  private System.Windows.Forms.CheckBox htmlMail;
  private System.Windows.Forms.Label label3;
  private System.Windows.Forms.TextBox phone1;
  private System.Windows.Forms.DomainUpDown phoneLabel1;
  private System.Windows.Forms.TextBox phone2;
  private System.Windows.Forms.DomainUpDown phoneLabel2;
  private System.Windows.Forms.TextBox phone3;
  private System.Windows.Forms.DomainUpDown phoneLabel3;
  private System.Windows.Forms.TextBox phone4;
  private System.Windows.Forms.DomainUpDown phoneLabel4;
  private System.Windows.Forms.Label label4;
  private System.Windows.Forms.DomainUpDown imLabel1;
  private System.Windows.Forms.TextBox im1;
  private System.Windows.Forms.Label label5;
  private System.Windows.Forms.DomainUpDown im1Location;
  private System.Windows.Forms.DomainUpDown im2Location;
  private System.Windows.Forms.Label label6;
  private System.Windows.Forms.TextBox im2;
  private System.Windows.Forms.DomainUpDown imLabel2;
  private System.Windows.Forms.DomainUpDown im3Location;
  private System.Windows.Forms.Label label7;
  private System.Windows.Forms.TextBox im3;
  private System.Windows.Forms.DomainUpDown imLabel3;
  private System.Windows.Forms.TabPage tabPage2;
  private System.Windows.Forms.TabPage tabPage3;
  private System.Windows.Forms.Label label8;
  private System.Windows.Forms.Label label9;
  private System.Windows.Forms.Label label10;
  private System.Windows.Forms.Label label11;
  private System.Windows.Forms.Label label12;
  private System.Windows.Forms.TextBox webcam;
  private System.Windows.Forms.Label label13;
  private System.Windows.Forms.Label label14;
  private System.Windows.Forms.Label label15;
  private System.Windows.Forms.Label label16;
  private System.Windows.Forms.TextBox profession;
  private System.Windows.Forms.Label label17;
  private System.Windows.Forms.Label label18;
  private System.Windows.Forms.TextBox company;
  private System.Windows.Forms.TextBox manager;
  private System.Windows.Forms.TextBox department;
  private System.Windows.Forms.Label label20;
  private System.Windows.Forms.Label label21;
  private System.Windows.Forms.TextBox birthday;
  private System.Windows.Forms.DateTimePicker birthdayPicker;
  private System.Windows.Forms.TextBox notes;
  private System.Windows.Forms.Label label23;
  private System.Windows.Forms.Label label24;
  private System.Windows.Forms.Label label25;
  private System.Windows.Forms.Label label26;
  private System.Windows.Forms.Label label27;
  private System.Windows.Forms.Label label28;
  private System.Windows.Forms.Label label29;
  private System.Windows.Forms.Label label30;
  private System.Windows.Forms.Label label31;
  private System.Windows.Forms.Label label34;
  private System.Windows.Forms.Label label35;
  private System.Windows.Forms.Label label36;
  private System.Windows.Forms.Label label37;
  private System.Windows.Forms.Label label40;
  private System.Windows.Forms.Label label41;
  private System.Windows.Forms.Label label42;
  private System.Windows.Forms.Label label43;
  private System.Windows.Forms.Label label44;
  private System.Windows.Forms.TextBox jobTitle;
  private System.Windows.Forms.TextBox nickname;
  private System.Windows.Forms.TextBox otherState;
  private System.Windows.Forms.TextBox otherZip;
  private System.Windows.Forms.TextBox otherCity;
  private System.Windows.Forms.TextBox otherAddress2;
  private System.Windows.Forms.TextBox otherStreet;
  private System.Windows.Forms.TextBox homeState;
  private System.Windows.Forms.TextBox homeZip;
  private System.Windows.Forms.TextBox homeCity;
  private System.Windows.Forms.TextBox homeAddress2;
  private System.Windows.Forms.TextBox homeStreet;
  private System.Windows.Forms.TextBox workState;
  private System.Windows.Forms.TextBox workZip;
  private System.Windows.Forms.TextBox workCity;
  private System.Windows.Forms.TextBox workAddress2;
  private System.Windows.Forms.TextBox workStreet;
  private System.Windows.Forms.TextBox calendar;
  private System.Windows.Forms.TextBox blog;
  private System.Windows.Forms.TextBox homePage;
  private System.Windows.Forms.Label label39;
  private System.Windows.Forms.Label label38;
  private System.Windows.Forms.Label label33;
  private System.Windows.Forms.Label label32;
  private System.Windows.Forms.ComboBox workCountry;
  private System.Windows.Forms.ComboBox homeCountry;
  private System.Windows.Forms.ComboBox otherCountry;

  private Novell.AddressBook.AddressBook addressBook = null;
  private Contact contact = null;
  private bool newContact = false;
  private Name name;
  private bool nameValidated;
  private Email eMail1;
  private Email eMail2;
  private Email eMail3;
  private Email eMail4;
  private Telephone telephone1;
  private Telephone telephone2;
  private Telephone telephone3;
  private Telephone telephone4;
  private IM iM1;
  private IM iM2;
  private IM iM3;
  private Address workAddress;
  private Address homeAddress;
  private Address otherAddress;
  private string loadPath;
  private int emailFixedSpace;
  private int phoneFixedSpace;
  private int jobFixedSpace;
  private int addrFixedSpace;
  private System.Windows.Forms.HelpProvider helpProvider1;




  private System.ComponentModel.Container components = null;





  public ContactEditor()
  {



   InitializeComponent();


   emailFixedSpace = this.Size.Width - email1.Size.Width - email2.Size.Width;
   phoneFixedSpace = this.Size.Width - phone1.Size.Width - phone2.Size.Width;
   jobFixedSpace = this.Size.Width - profession.Size.Width - jobTitle.Size.Width;
   addrFixedSpace = this.Size.Width - workCity.Size.Width - workZip.Size.Width;


   object[] countries = new object[] {
              "United States",
              "Afghanistan",
              "Albania",
              "Algeria",
              "American Samoa",
              "Andorra",
              "Angola",
              "Anguilla",
              "Antarctica",
              "Antigua And Barbuda",
              "Argentina",
              "Armenia",
              "Aruba",
              "Australia",
              "Austria",
              "Azerbaijan"};


   workCountry.Items.AddRange(countries);
   homeCountry.Items.AddRange(countries);
   otherCountry.Items.AddRange(countries);
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
   this.tabControl1 = new System.Windows.Forms.TabControl();
   this.tabPage1 = new System.Windows.Forms.TabPage();
   this.im3Location = new System.Windows.Forms.DomainUpDown();
   this.label7 = new System.Windows.Forms.Label();
   this.im3 = new System.Windows.Forms.TextBox();
   this.imLabel3 = new System.Windows.Forms.DomainUpDown();
   this.im2Location = new System.Windows.Forms.DomainUpDown();
   this.label6 = new System.Windows.Forms.Label();
   this.im2 = new System.Windows.Forms.TextBox();
   this.imLabel2 = new System.Windows.Forms.DomainUpDown();
   this.im1Location = new System.Windows.Forms.DomainUpDown();
   this.label5 = new System.Windows.Forms.Label();
   this.im1 = new System.Windows.Forms.TextBox();
   this.imLabel1 = new System.Windows.Forms.DomainUpDown();
   this.label4 = new System.Windows.Forms.Label();
   this.phone4 = new System.Windows.Forms.TextBox();
   this.phoneLabel4 = new System.Windows.Forms.DomainUpDown();
   this.phone3 = new System.Windows.Forms.TextBox();
   this.phoneLabel3 = new System.Windows.Forms.DomainUpDown();
   this.phone2 = new System.Windows.Forms.TextBox();
   this.phoneLabel2 = new System.Windows.Forms.DomainUpDown();
   this.phone1 = new System.Windows.Forms.TextBox();
   this.phoneLabel1 = new System.Windows.Forms.DomainUpDown();
   this.label3 = new System.Windows.Forms.Label();
   this.htmlMail = new System.Windows.Forms.CheckBox();
   this.email4 = new System.Windows.Forms.TextBox();
   this.emailLabel4 = new System.Windows.Forms.DomainUpDown();
   this.email3 = new System.Windows.Forms.TextBox();
   this.emailLabel3 = new System.Windows.Forms.DomainUpDown();
   this.email2 = new System.Windows.Forms.TextBox();
   this.emailLabel2 = new System.Windows.Forms.DomainUpDown();
   this.email1 = new System.Windows.Forms.TextBox();
   this.emailLabel1 = new System.Windows.Forms.DomainUpDown();
   this.label2 = new System.Windows.Forms.Label();
   this.groupBox1 = new System.Windows.Forms.GroupBox();
   this.pictureContact = new System.Windows.Forms.PictureBox();
   this.nickname = new System.Windows.Forms.TextBox();
   this.label1 = new System.Windows.Forms.Label();
   this.fullName = new System.Windows.Forms.TextBox();
   this.fullNameButton = new System.Windows.Forms.Button();
   this.tabPage2 = new System.Windows.Forms.TabPage();
   this.notes = new System.Windows.Forms.TextBox();
   this.label23 = new System.Windows.Forms.Label();
   this.birthday = new System.Windows.Forms.TextBox();
   this.birthdayPicker = new System.Windows.Forms.DateTimePicker();
   this.label21 = new System.Windows.Forms.Label();
   this.label20 = new System.Windows.Forms.Label();
   this.department = new System.Windows.Forms.TextBox();
   this.manager = new System.Windows.Forms.TextBox();
   this.company = new System.Windows.Forms.TextBox();
   this.jobTitle = new System.Windows.Forms.TextBox();
   this.label18 = new System.Windows.Forms.Label();
   this.label17 = new System.Windows.Forms.Label();
   this.profession = new System.Windows.Forms.TextBox();
   this.label16 = new System.Windows.Forms.Label();
   this.label15 = new System.Windows.Forms.Label();
   this.label14 = new System.Windows.Forms.Label();
   this.label13 = new System.Windows.Forms.Label();
   this.webcam = new System.Windows.Forms.TextBox();
   this.calendar = new System.Windows.Forms.TextBox();
   this.blog = new System.Windows.Forms.TextBox();
   this.homePage = new System.Windows.Forms.TextBox();
   this.label12 = new System.Windows.Forms.Label();
   this.label11 = new System.Windows.Forms.Label();
   this.label10 = new System.Windows.Forms.Label();
   this.label9 = new System.Windows.Forms.Label();
   this.label8 = new System.Windows.Forms.Label();
   this.tabPage3 = new System.Windows.Forms.TabPage();
   this.otherCountry = new System.Windows.Forms.ComboBox();
   this.homeCountry = new System.Windows.Forms.ComboBox();
   this.workCountry = new System.Windows.Forms.ComboBox();
   this.label44 = new System.Windows.Forms.Label();
   this.label39 = new System.Windows.Forms.Label();
   this.otherState = new System.Windows.Forms.TextBox();
   this.otherZip = new System.Windows.Forms.TextBox();
   this.label38 = new System.Windows.Forms.Label();
   this.otherCity = new System.Windows.Forms.TextBox();
   this.otherAddress2 = new System.Windows.Forms.TextBox();
   this.otherStreet = new System.Windows.Forms.TextBox();
   this.label40 = new System.Windows.Forms.Label();
   this.label41 = new System.Windows.Forms.Label();
   this.label42 = new System.Windows.Forms.Label();
   this.label43 = new System.Windows.Forms.Label();
   this.label33 = new System.Windows.Forms.Label();
   this.homeState = new System.Windows.Forms.TextBox();
   this.homeZip = new System.Windows.Forms.TextBox();
   this.label32 = new System.Windows.Forms.Label();
   this.homeCity = new System.Windows.Forms.TextBox();
   this.homeAddress2 = new System.Windows.Forms.TextBox();
   this.homeStreet = new System.Windows.Forms.TextBox();
   this.label34 = new System.Windows.Forms.Label();
   this.label35 = new System.Windows.Forms.Label();
   this.label36 = new System.Windows.Forms.Label();
   this.label37 = new System.Windows.Forms.Label();
   this.label31 = new System.Windows.Forms.Label();
   this.label30 = new System.Windows.Forms.Label();
   this.workState = new System.Windows.Forms.TextBox();
   this.workZip = new System.Windows.Forms.TextBox();
   this.label29 = new System.Windows.Forms.Label();
   this.workCity = new System.Windows.Forms.TextBox();
   this.workAddress2 = new System.Windows.Forms.TextBox();
   this.workStreet = new System.Windows.Forms.TextBox();
   this.label28 = new System.Windows.Forms.Label();
   this.label27 = new System.Windows.Forms.Label();
   this.label26 = new System.Windows.Forms.Label();
   this.label25 = new System.Windows.Forms.Label();
   this.label24 = new System.Windows.Forms.Label();
   this.cancel = new System.Windows.Forms.Button();
   this.ok = new System.Windows.Forms.Button();
   this.helpProvider1 = new System.Windows.Forms.HelpProvider();
   this.tabControl1.SuspendLayout();
   this.tabPage1.SuspendLayout();
   this.tabPage2.SuspendLayout();
   this.tabPage3.SuspendLayout();
   this.SuspendLayout();



   this.tabControl1.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
    | System.Windows.Forms.AnchorStyles.Left)
    | System.Windows.Forms.AnchorStyles.Right)));
   this.tabControl1.Controls.Add(this.tabPage1);
   this.tabControl1.Controls.Add(this.tabPage2);
   this.tabControl1.Controls.Add(this.tabPage3);
   this.tabControl1.Location = new System.Drawing.Point(8, 8);
   this.tabControl1.Name = "tabControl1";
   this.tabControl1.SelectedIndex = 0;
   this.tabControl1.Size = new System.Drawing.Size(616, 560);
   this.tabControl1.TabIndex = 0;



   this.tabPage1.Controls.Add(this.im3Location);
   this.tabPage1.Controls.Add(this.label7);
   this.tabPage1.Controls.Add(this.im3);
   this.tabPage1.Controls.Add(this.imLabel3);
   this.tabPage1.Controls.Add(this.im2Location);
   this.tabPage1.Controls.Add(this.label6);
   this.tabPage1.Controls.Add(this.im2);
   this.tabPage1.Controls.Add(this.imLabel2);
   this.tabPage1.Controls.Add(this.im1Location);
   this.tabPage1.Controls.Add(this.label5);
   this.tabPage1.Controls.Add(this.im1);
   this.tabPage1.Controls.Add(this.imLabel1);
   this.tabPage1.Controls.Add(this.label4);
   this.tabPage1.Controls.Add(this.phone4);
   this.tabPage1.Controls.Add(this.phoneLabel4);
   this.tabPage1.Controls.Add(this.phone3);
   this.tabPage1.Controls.Add(this.phoneLabel3);
   this.tabPage1.Controls.Add(this.phone2);
   this.tabPage1.Controls.Add(this.phoneLabel2);
   this.tabPage1.Controls.Add(this.phone1);
   this.tabPage1.Controls.Add(this.phoneLabel1);
   this.tabPage1.Controls.Add(this.label3);
   this.tabPage1.Controls.Add(this.htmlMail);
   this.tabPage1.Controls.Add(this.email4);
   this.tabPage1.Controls.Add(this.emailLabel4);
   this.tabPage1.Controls.Add(this.email3);
   this.tabPage1.Controls.Add(this.emailLabel3);
   this.tabPage1.Controls.Add(this.email2);
   this.tabPage1.Controls.Add(this.emailLabel2);
   this.tabPage1.Controls.Add(this.email1);
   this.tabPage1.Controls.Add(this.emailLabel1);
   this.tabPage1.Controls.Add(this.label2);
   this.tabPage1.Controls.Add(this.groupBox1);
   this.tabPage1.Controls.Add(this.pictureContact);
   this.tabPage1.Controls.Add(this.nickname);
   this.tabPage1.Controls.Add(this.label1);
   this.tabPage1.Controls.Add(this.fullName);
   this.tabPage1.Controls.Add(this.fullNameButton);
   this.tabPage1.Location = new System.Drawing.Point(4, 22);
   this.tabPage1.Name = "tabPage1";
   this.tabPage1.Size = new System.Drawing.Size(608, 534);
   this.tabPage1.TabIndex = 0;
   this.tabPage1.Text = "Contact";



   this.im3Location.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
   this.helpProvider1.SetHelpString(this.im3Location, "Specifies a location for the instant messaging account.");
   this.im3Location.Items.Add("Work");
   this.im3Location.Items.Add("Home");
   this.im3Location.Items.Add("Other");
   this.im3Location.Location = new System.Drawing.Point(488, 448);
   this.im3Location.Name = "im3Location";
   this.helpProvider1.SetShowHelp(this.im3Location, true);
   this.im3Location.Size = new System.Drawing.Size(96, 20);
   this.im3Location.TabIndex = 37;
   this.im3Location.SelectedItemChanged += new System.EventHandler(this.im3Location_SelectedItemChanged);



   this.label7.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
   this.label7.Location = new System.Drawing.Point(440, 448);
   this.label7.Name = "label7";
   this.label7.Size = new System.Drawing.Size(100, 16);
   this.label7.TabIndex = 36;
   this.label7.Text = "Location:";



   this.im3.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
    | System.Windows.Forms.AnchorStyles.Right)));
   this.helpProvider1.SetHelpString(this.im3, "Specifies an instant messaging account for the contact.");
   this.im3.Location = new System.Drawing.Point(168, 448);
   this.im3.Name = "im3";
   this.helpProvider1.SetShowHelp(this.im3, true);
   this.im3.Size = new System.Drawing.Size(264, 20);
   this.im3.TabIndex = 35;
   this.im3.Text = "";
   this.im3.TextChanged += new System.EventHandler(this.im3_TextChanged);



   this.helpProvider1.SetHelpString(this.imLabel3, "Specifies the provider for the Instant Messaging account.");
   this.imLabel3.Items.Add("AIM");
   this.imLabel3.Items.Add("Jabber");
   this.imLabel3.Items.Add("Yahoo");
   this.imLabel3.Items.Add("GroupWise");
   this.imLabel3.Items.Add("MSN");
   this.imLabel3.Location = new System.Drawing.Point(24, 448);
   this.imLabel3.Name = "imLabel3";
   this.helpProvider1.SetShowHelp(this.imLabel3, true);
   this.imLabel3.Size = new System.Drawing.Size(136, 20);
   this.imLabel3.TabIndex = 34;
   this.imLabel3.TextChanged += new System.EventHandler(this.imLabel3_TextChanged);
   this.imLabel3.SelectedItemChanged += new System.EventHandler(this.imLabel3_TextChanged);



   this.im2Location.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
   this.helpProvider1.SetHelpString(this.im2Location, "Specifies a location for the instant messaging account.");
   this.im2Location.Items.Add("Work");
   this.im2Location.Items.Add("Home");
   this.im2Location.Items.Add("Other");
   this.im2Location.Location = new System.Drawing.Point(488, 416);
   this.im2Location.Name = "im2Location";
   this.helpProvider1.SetShowHelp(this.im2Location, true);
   this.im2Location.Size = new System.Drawing.Size(96, 20);
   this.im2Location.TabIndex = 33;
   this.im2Location.SelectedItemChanged += new System.EventHandler(this.im2Location_SelectedItemChanged);



   this.label6.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
   this.label6.Location = new System.Drawing.Point(440, 416);
   this.label6.Name = "label6";
   this.label6.Size = new System.Drawing.Size(100, 16);
   this.label6.TabIndex = 32;
   this.label6.Text = "Location:";



   this.im2.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
    | System.Windows.Forms.AnchorStyles.Right)));
   this.helpProvider1.SetHelpString(this.im2, "Specifies an instant messaging account for the contact.");
   this.im2.Location = new System.Drawing.Point(168, 416);
   this.im2.Name = "im2";
   this.helpProvider1.SetShowHelp(this.im2, true);
   this.im2.Size = new System.Drawing.Size(264, 20);
   this.im2.TabIndex = 31;
   this.im2.Text = "";
   this.im2.TextChanged += new System.EventHandler(this.im2_TextChanged);



   this.helpProvider1.SetHelpString(this.imLabel2, "Specifies the provider for the Instant Messaging account.");
   this.imLabel2.Items.Add("AIM");
   this.imLabel2.Items.Add("Jabber");
   this.imLabel2.Items.Add("Yahoo");
   this.imLabel2.Items.Add("GroupWise");
   this.imLabel2.Items.Add("MSN");
   this.imLabel2.Location = new System.Drawing.Point(24, 416);
   this.imLabel2.Name = "imLabel2";
   this.helpProvider1.SetShowHelp(this.imLabel2, true);
   this.imLabel2.Size = new System.Drawing.Size(136, 20);
   this.imLabel2.TabIndex = 30;
   this.imLabel2.TextChanged += new System.EventHandler(this.imLabel2_TextChanged);
   this.imLabel2.SelectedItemChanged += new System.EventHandler(this.imLabel2_TextChanged);



   this.im1Location.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
   this.helpProvider1.SetHelpString(this.im1Location, "Specifies a location for the instant messaging account.");
   this.im1Location.Items.Add("Work");
   this.im1Location.Items.Add("Home");
   this.im1Location.Items.Add("Other");
   this.im1Location.Location = new System.Drawing.Point(488, 384);
   this.im1Location.Name = "im1Location";
   this.helpProvider1.SetShowHelp(this.im1Location, true);
   this.im1Location.Size = new System.Drawing.Size(96, 20);
   this.im1Location.TabIndex = 29;
   this.im1Location.SelectedItemChanged += new System.EventHandler(this.im1Location_SelectedItemChanged);



   this.label5.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
   this.label5.Location = new System.Drawing.Point(440, 384);
   this.label5.Name = "label5";
   this.label5.Size = new System.Drawing.Size(100, 16);
   this.label5.TabIndex = 28;
   this.label5.Text = "Location:";



   this.im1.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
    | System.Windows.Forms.AnchorStyles.Right)));
   this.helpProvider1.SetHelpString(this.im1, "Specifies an instant messaging account for the contact.");
   this.im1.Location = new System.Drawing.Point(168, 384);
   this.im1.Name = "im1";
   this.helpProvider1.SetShowHelp(this.im1, true);
   this.im1.Size = new System.Drawing.Size(264, 20);
   this.im1.TabIndex = 27;
   this.im1.Text = "";
   this.im1.TextChanged += new System.EventHandler(this.im1_TextChanged);



   this.helpProvider1.SetHelpString(this.imLabel1, "Specifies the provider for the Instant Messaging account.");
   this.imLabel1.Items.Add("AIM");
   this.imLabel1.Items.Add("Jabber");
   this.imLabel1.Items.Add("Yahoo");
   this.imLabel1.Items.Add("GroupWise");
   this.imLabel1.Items.Add("MSN");
   this.imLabel1.Location = new System.Drawing.Point(24, 384);
   this.imLabel1.Name = "imLabel1";
   this.helpProvider1.SetShowHelp(this.imLabel1, true);
   this.imLabel1.Size = new System.Drawing.Size(136, 20);
   this.imLabel1.TabIndex = 26;
   this.imLabel1.TextChanged += new System.EventHandler(this.imLabel1_TextChanged);
   this.imLabel1.SelectedItemChanged += new System.EventHandler(this.imLabel1_TextChanged);



   this.label4.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.75F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
   this.label4.Location = new System.Drawing.Point(8, 352);
   this.label4.Name = "label4";
   this.label4.Size = new System.Drawing.Size(120, 16);
   this.label4.TabIndex = 25;
   this.label4.Text = "Instant Messaging";



   this.helpProvider1.SetHelpString(this.phone4, "Specifies a telephone number for this contact.");
   this.phone4.Location = new System.Drawing.Point(432, 288);
   this.phone4.Name = "phone4";
   this.helpProvider1.SetShowHelp(this.phone4, true);
   this.phone4.Size = new System.Drawing.Size(152, 20);
   this.phone4.TabIndex = 24;
   this.phone4.Text = "";
   this.phone4.TextChanged += new System.EventHandler(this.phone4_TextChanged);



   this.helpProvider1.SetHelpString(this.phoneLabel4, "Specifies the type for the telephone number.");
   this.phoneLabel4.Items.Add("Business");
   this.phoneLabel4.Items.Add("Business fax");
   this.phoneLabel4.Items.Add("Home");
   this.phoneLabel4.Items.Add("Mobile");
   this.phoneLabel4.Items.Add("Pager");
   this.phoneLabel4.Location = new System.Drawing.Point(304, 288);
   this.phoneLabel4.Name = "phoneLabel4";
   this.helpProvider1.SetShowHelp(this.phoneLabel4, true);
   this.phoneLabel4.TabIndex = 23;
   this.phoneLabel4.SelectedItemChanged += new System.EventHandler(this.phoneLabel4_SelectedItemChanged);



   this.helpProvider1.SetHelpString(this.phone3, "Specifies a telephone number for this contact.");
   this.phone3.Location = new System.Drawing.Point(136, 288);
   this.phone3.Name = "phone3";
   this.helpProvider1.SetShowHelp(this.phone3, true);
   this.phone3.Size = new System.Drawing.Size(160, 20);
   this.phone3.TabIndex = 22;
   this.phone3.Text = "";
   this.phone3.TextChanged += new System.EventHandler(this.phone3_TextChanged);



   this.helpProvider1.SetHelpString(this.phoneLabel3, "Specifies the type for the telephone number.");
   this.phoneLabel3.Items.Add("Business");
   this.phoneLabel3.Items.Add("Business fax");
   this.phoneLabel3.Items.Add("Home");
   this.phoneLabel3.Items.Add("Mobile");
   this.phoneLabel3.Items.Add("Pager");
   this.phoneLabel3.Location = new System.Drawing.Point(24, 288);
   this.phoneLabel3.Name = "phoneLabel3";
   this.helpProvider1.SetShowHelp(this.phoneLabel3, true);
   this.phoneLabel3.Size = new System.Drawing.Size(104, 20);
   this.phoneLabel3.TabIndex = 21;
   this.phoneLabel3.SelectedItemChanged += new System.EventHandler(this.phoneLabel3_SelectedItemChanged);



   this.helpProvider1.SetHelpString(this.phone2, "Specifies a telephone number for this contact.");
   this.phone2.Location = new System.Drawing.Point(432, 256);
   this.phone2.Name = "phone2";
   this.helpProvider1.SetShowHelp(this.phone2, true);
   this.phone2.Size = new System.Drawing.Size(152, 20);
   this.phone2.TabIndex = 20;
   this.phone2.Text = "";
   this.phone2.TextChanged += new System.EventHandler(this.phone2_TextChanged);



   this.helpProvider1.SetHelpString(this.phoneLabel2, "Specifies the type for the telephone number.");
   this.phoneLabel2.Items.Add("Business");
   this.phoneLabel2.Items.Add("Business fax");
   this.phoneLabel2.Items.Add("Home");
   this.phoneLabel2.Items.Add("Mobile");
   this.phoneLabel2.Items.Add("Pager");
   this.phoneLabel2.Location = new System.Drawing.Point(304, 256);
   this.phoneLabel2.Name = "phoneLabel2";
   this.helpProvider1.SetShowHelp(this.phoneLabel2, true);
   this.phoneLabel2.TabIndex = 19;
   this.phoneLabel2.SelectedItemChanged += new System.EventHandler(this.phoneLabel2_SelectedItemChanged);



   this.helpProvider1.SetHelpString(this.phone1, "Specifies a telephone number for this contact.");
   this.phone1.Location = new System.Drawing.Point(136, 256);
   this.phone1.Name = "phone1";
   this.helpProvider1.SetShowHelp(this.phone1, true);
   this.phone1.Size = new System.Drawing.Size(160, 20);
   this.phone1.TabIndex = 18;
   this.phone1.Text = "";
   this.phone1.TextChanged += new System.EventHandler(this.phone1_TextChanged);



   this.helpProvider1.SetHelpString(this.phoneLabel1, "Specifies the type for the telephone number.");
   this.phoneLabel1.Items.Add("Business");
   this.phoneLabel1.Items.Add("Business fax");
   this.phoneLabel1.Items.Add("Home");
   this.phoneLabel1.Items.Add("Mobile");
   this.phoneLabel1.Items.Add("Pager");
   this.phoneLabel1.Location = new System.Drawing.Point(24, 256);
   this.phoneLabel1.Name = "phoneLabel1";
   this.helpProvider1.SetShowHelp(this.phoneLabel1, true);
   this.phoneLabel1.Size = new System.Drawing.Size(104, 20);
   this.phoneLabel1.TabIndex = 17;
   this.phoneLabel1.SelectedItemChanged += new System.EventHandler(this.phoneLabel1_SelectedItemChanged);



   this.label3.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.75F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
   this.label3.Location = new System.Drawing.Point(8, 232);
   this.label3.Name = "label3";
   this.label3.Size = new System.Drawing.Size(100, 16);
   this.label3.TabIndex = 16;
   this.label3.Text = "Telephone";



   this.htmlMail.Location = new System.Drawing.Point(24, 192);
   this.htmlMail.Name = "htmlMail";
   this.htmlMail.Size = new System.Drawing.Size(184, 16);
   this.htmlMail.TabIndex = 15;
   this.htmlMail.Text = "Wants to receive HTML mail";



   this.helpProvider1.SetHelpString(this.email4, "Specifies an email account for the contact.");
   this.email4.Location = new System.Drawing.Point(408, 160);
   this.email4.Name = "email4";
   this.helpProvider1.SetShowHelp(this.email4, true);
   this.email4.Size = new System.Drawing.Size(176, 20);
   this.email4.TabIndex = 14;
   this.email4.Text = "";
   this.email4.TextChanged += new System.EventHandler(this.email4_TextChanged);
   this.email4.Leave += new System.EventHandler(this.email4_Leave);



   this.helpProvider1.SetHelpString(this.emailLabel4, "Specifies the type for the email account.");
   this.emailLabel4.Items.Add("Work");
   this.emailLabel4.Items.Add("Home");
   this.emailLabel4.Items.Add("Other");
   this.emailLabel4.Location = new System.Drawing.Point(312, 160);
   this.emailLabel4.Name = "emailLabel4";
   this.helpProvider1.SetShowHelp(this.emailLabel4, true);
   this.emailLabel4.Size = new System.Drawing.Size(88, 20);
   this.emailLabel4.TabIndex = 13;
   this.emailLabel4.SelectedItemChanged += new System.EventHandler(this.emailLabel4_SelectedItemChanged);



   this.helpProvider1.SetHelpString(this.email3, "Specifies an email account for the contact.");
   this.email3.Location = new System.Drawing.Point(120, 160);
   this.email3.Name = "email3";
   this.helpProvider1.SetShowHelp(this.email3, true);
   this.email3.Size = new System.Drawing.Size(184, 20);
   this.email3.TabIndex = 12;
   this.email3.Text = "";
   this.email3.TextChanged += new System.EventHandler(this.email3_TextChanged);
   this.email3.Leave += new System.EventHandler(this.email3_Leave);



   this.helpProvider1.SetHelpString(this.emailLabel3, "Specifies the type for the email account.");
   this.emailLabel3.Items.Add("Work");
   this.emailLabel3.Items.Add("Home");
   this.emailLabel3.Items.Add("Other");
   this.emailLabel3.Location = new System.Drawing.Point(24, 160);
   this.emailLabel3.Name = "emailLabel3";
   this.helpProvider1.SetShowHelp(this.emailLabel3, true);
   this.emailLabel3.Size = new System.Drawing.Size(88, 20);
   this.emailLabel3.TabIndex = 11;
   this.emailLabel3.SelectedItemChanged += new System.EventHandler(this.emailLabel3_SelectedItemChanged);



   this.helpProvider1.SetHelpString(this.email2, "Specifies an email account for the contact.");
   this.email2.Location = new System.Drawing.Point(408, 128);
   this.email2.Name = "email2";
   this.helpProvider1.SetShowHelp(this.email2, true);
   this.email2.Size = new System.Drawing.Size(176, 20);
   this.email2.TabIndex = 10;
   this.email2.Text = "";
   this.email2.TextChanged += new System.EventHandler(this.email2_TextChanged);
   this.email2.Leave += new System.EventHandler(this.email2_Leave);



   this.helpProvider1.SetHelpString(this.emailLabel2, "Specifies the type for the email account.");
   this.emailLabel2.Items.Add("Work");
   this.emailLabel2.Items.Add("Home");
   this.emailLabel2.Items.Add("Other");
   this.emailLabel2.Location = new System.Drawing.Point(312, 128);
   this.emailLabel2.Name = "emailLabel2";
   this.helpProvider1.SetShowHelp(this.emailLabel2, true);
   this.emailLabel2.Size = new System.Drawing.Size(88, 20);
   this.emailLabel2.TabIndex = 9;
   this.emailLabel2.SelectedItemChanged += new System.EventHandler(this.emailLabel2_SelectedItemChanged);



   this.helpProvider1.SetHelpString(this.email1, "Specifies an email account for the contact.");
   this.email1.Location = new System.Drawing.Point(120, 128);
   this.email1.Name = "email1";
   this.helpProvider1.SetShowHelp(this.email1, true);
   this.email1.Size = new System.Drawing.Size(184, 20);
   this.email1.TabIndex = 8;
   this.email1.Text = "";
   this.email1.TextChanged += new System.EventHandler(this.email1_TextChanged);
   this.email1.Leave += new System.EventHandler(this.email1_Leave);



   this.helpProvider1.SetHelpString(this.emailLabel1, "Specifies the type for the email account.");
   this.emailLabel1.Items.Add("Work");
   this.emailLabel1.Items.Add("Home");
   this.emailLabel1.Items.Add("Other");
   this.emailLabel1.Location = new System.Drawing.Point(24, 128);
   this.emailLabel1.Name = "emailLabel1";
   this.helpProvider1.SetShowHelp(this.emailLabel1, true);
   this.emailLabel1.Size = new System.Drawing.Size(88, 20);
   this.emailLabel1.TabIndex = 7;
   this.emailLabel1.SelectedItemChanged += new System.EventHandler(this.emailLabel1_SelectedItemChanged);



   this.label2.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.75F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
   this.label2.Location = new System.Drawing.Point(8, 100);
   this.label2.Name = "label2";
   this.label2.Size = new System.Drawing.Size(100, 16);
   this.label2.TabIndex = 6;
   this.label2.Text = "Email";



   this.groupBox1.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
    | System.Windows.Forms.AnchorStyles.Right)));
   this.groupBox1.Location = new System.Drawing.Point(8, 88);
   this.groupBox1.Name = "groupBox1";
   this.groupBox1.Size = new System.Drawing.Size(584, 4);
   this.groupBox1.TabIndex = 5;
   this.groupBox1.TabStop = false;



   this.helpProvider1.SetHelpString(this.pictureContact, "Click here to add or change the photo for this contact.");
   this.pictureContact.Location = new System.Drawing.Point(16, 8);
   this.pictureContact.Name = "pictureContact";
   this.helpProvider1.SetShowHelp(this.pictureContact, true);
   this.pictureContact.Size = new System.Drawing.Size(56, 70);
   this.pictureContact.TabIndex = 4;
   this.pictureContact.TabStop = false;
   this.pictureContact.Click += new System.EventHandler(this.pictureContact_Click);



   this.nickname.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
    | System.Windows.Forms.AnchorStyles.Right)));
   this.helpProvider1.SetHelpString(this.nickname, "Specifies the contact\'s nickname.");
   this.nickname.Location = new System.Drawing.Point(168, 48);
   this.nickname.Name = "nickname";
   this.helpProvider1.SetShowHelp(this.nickname, true);
   this.nickname.Size = new System.Drawing.Size(176, 20);
   this.nickname.TabIndex = 3;
   this.nickname.Text = "";



   this.label1.Location = new System.Drawing.Point(104, 48);
   this.label1.Name = "label1";
   this.label1.Size = new System.Drawing.Size(100, 16);
   this.label1.TabIndex = 2;
   this.label1.Text = "Nickname:";



   this.fullName.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
    | System.Windows.Forms.AnchorStyles.Right)));
   this.helpProvider1.SetHelpString(this.fullName, "Specifies the contact\'s full name.");
   this.fullName.Location = new System.Drawing.Point(168, 16);
   this.fullName.Name = "fullName";
   this.helpProvider1.SetShowHelp(this.fullName, true);
   this.fullName.Size = new System.Drawing.Size(416, 20);
   this.fullName.TabIndex = 1;
   this.fullName.Text = "";
   this.fullName.TextChanged += new System.EventHandler(this.fullName_TextChanged);
   this.fullName.Leave += new System.EventHandler(this.fullName_Leave);



   this.fullNameButton.FlatStyle = System.Windows.Forms.FlatStyle.System;
   this.helpProvider1.SetHelpString(this.fullNameButton, "Click to view or change the contact\'s full name.");
   this.fullNameButton.Location = new System.Drawing.Point(80, 14);
   this.fullNameButton.Name = "fullNameButton";
   this.helpProvider1.SetShowHelp(this.fullNameButton, true);
   this.fullNameButton.Size = new System.Drawing.Size(80, 23);
   this.fullNameButton.TabIndex = 0;
   this.fullNameButton.Text = "Full name...";
   this.fullNameButton.Click += new System.EventHandler(this.fullNameButton_Click);



   this.tabPage2.Controls.Add(this.notes);
   this.tabPage2.Controls.Add(this.label23);
   this.tabPage2.Controls.Add(this.birthday);
   this.tabPage2.Controls.Add(this.birthdayPicker);
   this.tabPage2.Controls.Add(this.label21);
   this.tabPage2.Controls.Add(this.label20);
   this.tabPage2.Controls.Add(this.department);
   this.tabPage2.Controls.Add(this.manager);
   this.tabPage2.Controls.Add(this.company);
   this.tabPage2.Controls.Add(this.jobTitle);
   this.tabPage2.Controls.Add(this.label18);
   this.tabPage2.Controls.Add(this.label17);
   this.tabPage2.Controls.Add(this.profession);
   this.tabPage2.Controls.Add(this.label16);
   this.tabPage2.Controls.Add(this.label15);
   this.tabPage2.Controls.Add(this.label14);
   this.tabPage2.Controls.Add(this.label13);
   this.tabPage2.Controls.Add(this.webcam);
   this.tabPage2.Controls.Add(this.calendar);
   this.tabPage2.Controls.Add(this.blog);
   this.tabPage2.Controls.Add(this.homePage);
   this.tabPage2.Controls.Add(this.label12);
   this.tabPage2.Controls.Add(this.label11);
   this.tabPage2.Controls.Add(this.label10);
   this.tabPage2.Controls.Add(this.label9);
   this.tabPage2.Controls.Add(this.label8);
   this.tabPage2.Location = new System.Drawing.Point(4, 22);
   this.tabPage2.Name = "tabPage2";
   this.tabPage2.Size = new System.Drawing.Size(608, 534);
   this.tabPage2.TabIndex = 1;
   this.tabPage2.Text = "Personal Information";



   this.notes.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
    | System.Windows.Forms.AnchorStyles.Right)));
   this.helpProvider1.SetHelpString(this.notes, "Specifies notes for the contact.");
   this.notes.Location = new System.Drawing.Point(80, 352);
   this.notes.Multiline = true;
   this.notes.Name = "notes";
   this.helpProvider1.SetShowHelp(this.notes, true);
   this.notes.Size = new System.Drawing.Size(504, 152);
   this.notes.TabIndex = 25;
   this.notes.Text = "";



   this.label23.Location = new System.Drawing.Point(24, 352);
   this.label23.Name = "label23";
   this.label23.Size = new System.Drawing.Size(100, 16);
   this.label23.TabIndex = 24;
   this.label23.Text = "Notes:";



   this.helpProvider1.SetHelpString(this.birthday, "Specifies the contact\'s birthday.");
   this.birthday.Location = new System.Drawing.Point(80, 320);
   this.birthday.Name = "birthday";
   this.helpProvider1.SetShowHelp(this.birthday, true);
   this.birthday.Size = new System.Drawing.Size(180, 20);
   this.birthday.TabIndex = 22;
   this.birthday.Text = "";
   this.birthday.Leave += new System.EventHandler(this.birthday_Leave);



   this.birthdayPicker.Location = new System.Drawing.Point(112, 320);
   this.birthdayPicker.Name = "birthdayPicker";
   this.birthdayPicker.Size = new System.Drawing.Size(168, 20);
   this.birthdayPicker.TabIndex = 23;
   this.birthdayPicker.ValueChanged += new System.EventHandler(this.birthdayPicker_ValueChanged);



   this.label21.Location = new System.Drawing.Point(24, 320);
   this.label21.Name = "label21";
   this.label21.Size = new System.Drawing.Size(100, 16);
   this.label21.TabIndex = 21;
   this.label21.Text = "Birthday:";



   this.label20.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.75F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
   this.label20.Location = new System.Drawing.Point(16, 288);
   this.label20.Name = "label20";
   this.label20.Size = new System.Drawing.Size(100, 16);
   this.label20.TabIndex = 20;
   this.label20.Text = "Miscellaneous";



   this.helpProvider1.SetHelpString(this.department, "Specifies the contact\'s department.");
   this.department.Location = new System.Drawing.Point(376, 216);
   this.department.Name = "department";
   this.helpProvider1.SetShowHelp(this.department, true);
   this.department.Size = new System.Drawing.Size(208, 20);
   this.department.TabIndex = 17;
   this.department.Text = "";



   this.helpProvider1.SetHelpString(this.manager, "Specifies the contact\'s manager.");
   this.manager.Location = new System.Drawing.Point(88, 240);
   this.manager.Name = "manager";
   this.helpProvider1.SetShowHelp(this.manager, true);
   this.manager.Size = new System.Drawing.Size(208, 20);
   this.manager.TabIndex = 19;
   this.manager.Text = "";



   this.helpProvider1.SetHelpString(this.company, "Specifies the contact\'s company.");
   this.company.Location = new System.Drawing.Point(88, 216);
   this.company.Name = "company";
   this.helpProvider1.SetShowHelp(this.company, true);
   this.company.Size = new System.Drawing.Size(208, 20);
   this.company.TabIndex = 15;
   this.company.Text = "";



   this.helpProvider1.SetHelpString(this.jobTitle, "Specifies the contact\'s job title.");
   this.jobTitle.Location = new System.Drawing.Point(376, 192);
   this.jobTitle.Name = "jobTitle";
   this.helpProvider1.SetShowHelp(this.jobTitle, true);
   this.jobTitle.Size = new System.Drawing.Size(208, 20);
   this.jobTitle.TabIndex = 13;
   this.jobTitle.Text = "";



   this.label18.Location = new System.Drawing.Point(304, 216);
   this.label18.Name = "label18";
   this.label18.Size = new System.Drawing.Size(72, 16);
   this.label18.TabIndex = 16;
   this.label18.Text = "Department:";



   this.label17.Location = new System.Drawing.Point(304, 192);
   this.label17.Name = "label17";
   this.label17.Size = new System.Drawing.Size(72, 16);
   this.label17.TabIndex = 12;
   this.label17.Text = "Job title:";



   this.helpProvider1.SetHelpString(this.profession, "Specifies the contact\'s profession.");
   this.profession.Location = new System.Drawing.Point(88, 192);
   this.profession.Name = "profession";
   this.helpProvider1.SetShowHelp(this.profession, true);
   this.profession.Size = new System.Drawing.Size(208, 20);
   this.profession.TabIndex = 11;
   this.profession.Text = "";



   this.label16.Location = new System.Drawing.Point(24, 240);
   this.label16.Name = "label16";
   this.label16.Size = new System.Drawing.Size(100, 16);
   this.label16.TabIndex = 18;
   this.label16.Text = "Manager:";



   this.label15.Location = new System.Drawing.Point(24, 216);
   this.label15.Name = "label15";
   this.label15.Size = new System.Drawing.Size(100, 16);
   this.label15.TabIndex = 14;
   this.label15.Text = "Company:";



   this.label14.Location = new System.Drawing.Point(24, 192);
   this.label14.Name = "label14";
   this.label14.Size = new System.Drawing.Size(100, 16);
   this.label14.TabIndex = 10;
   this.label14.Text = "Profession:";



   this.label13.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.75F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
   this.label13.Location = new System.Drawing.Point(16, 160);
   this.label13.Name = "label13";
   this.label13.Size = new System.Drawing.Size(100, 16);
   this.label13.TabIndex = 9;
   this.label13.Text = "Job";



   this.webcam.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
    | System.Windows.Forms.AnchorStyles.Right)));
   this.helpProvider1.SetHelpString(this.webcam, "Specifies the address of the contact\'s web camera.");
   this.webcam.Location = new System.Drawing.Point(88, 120);
   this.webcam.Name = "webcam";
   this.helpProvider1.SetShowHelp(this.webcam, true);
   this.webcam.Size = new System.Drawing.Size(496, 20);
   this.webcam.TabIndex = 8;
   this.webcam.Text = "";



   this.calendar.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
    | System.Windows.Forms.AnchorStyles.Right)));
   this.helpProvider1.SetHelpString(this.calendar, "Specifies the address of the contact\'s calendar web page.");
   this.calendar.Location = new System.Drawing.Point(88, 96);
   this.calendar.Name = "calendar";
   this.helpProvider1.SetShowHelp(this.calendar, true);
   this.calendar.Size = new System.Drawing.Size(496, 20);
   this.calendar.TabIndex = 6;
   this.calendar.Text = "";



   this.blog.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
    | System.Windows.Forms.AnchorStyles.Right)));
   this.helpProvider1.SetHelpString(this.blog, "Specifies the address of the contact\'s web log.");
   this.blog.Location = new System.Drawing.Point(88, 72);
   this.blog.Name = "blog";
   this.helpProvider1.SetShowHelp(this.blog, true);
   this.blog.Size = new System.Drawing.Size(496, 20);
   this.blog.TabIndex = 4;
   this.blog.Text = "";



   this.homePage.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
    | System.Windows.Forms.AnchorStyles.Right)));
   this.helpProvider1.SetHelpString(this.homePage, "Specifies the address of the contact\'s home web page.");
   this.homePage.Location = new System.Drawing.Point(88, 48);
   this.homePage.Name = "homePage";
   this.helpProvider1.SetShowHelp(this.homePage, true);
   this.homePage.Size = new System.Drawing.Size(496, 20);
   this.homePage.TabIndex = 2;
   this.homePage.Text = "";



   this.label12.Location = new System.Drawing.Point(24, 120);
   this.label12.Name = "label12";
   this.label12.Size = new System.Drawing.Size(100, 16);
   this.label12.TabIndex = 7;
   this.label12.Text = "Webcam:";



   this.label11.Location = new System.Drawing.Point(24, 96);
   this.label11.Name = "label11";
   this.label11.Size = new System.Drawing.Size(100, 16);
   this.label11.TabIndex = 5;
   this.label11.Text = "Calendar:";



   this.label10.Location = new System.Drawing.Point(24, 72);
   this.label10.Name = "label10";
   this.label10.Size = new System.Drawing.Size(100, 16);
   this.label10.TabIndex = 3;
   this.label10.Text = "Blog:";



   this.label9.Location = new System.Drawing.Point(24, 48);
   this.label9.Name = "label9";
   this.label9.Size = new System.Drawing.Size(100, 16);
   this.label9.TabIndex = 1;
   this.label9.Text = "Home page:";



   this.label8.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.75F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
   this.label8.Location = new System.Drawing.Point(16, 16);
   this.label8.Name = "label8";
   this.label8.Size = new System.Drawing.Size(120, 16);
   this.label8.TabIndex = 0;
   this.label8.Text = "Web Addresses";



   this.tabPage3.Controls.Add(this.otherCountry);
   this.tabPage3.Controls.Add(this.homeCountry);
   this.tabPage3.Controls.Add(this.workCountry);
   this.tabPage3.Controls.Add(this.label44);
   this.tabPage3.Controls.Add(this.label39);
   this.tabPage3.Controls.Add(this.otherState);
   this.tabPage3.Controls.Add(this.otherZip);
   this.tabPage3.Controls.Add(this.label38);
   this.tabPage3.Controls.Add(this.otherCity);
   this.tabPage3.Controls.Add(this.otherAddress2);
   this.tabPage3.Controls.Add(this.otherStreet);
   this.tabPage3.Controls.Add(this.label40);
   this.tabPage3.Controls.Add(this.label41);
   this.tabPage3.Controls.Add(this.label42);
   this.tabPage3.Controls.Add(this.label43);
   this.tabPage3.Controls.Add(this.label33);
   this.tabPage3.Controls.Add(this.homeState);
   this.tabPage3.Controls.Add(this.homeZip);
   this.tabPage3.Controls.Add(this.label32);
   this.tabPage3.Controls.Add(this.homeCity);
   this.tabPage3.Controls.Add(this.homeAddress2);
   this.tabPage3.Controls.Add(this.homeStreet);
   this.tabPage3.Controls.Add(this.label34);
   this.tabPage3.Controls.Add(this.label35);
   this.tabPage3.Controls.Add(this.label36);
   this.tabPage3.Controls.Add(this.label37);
   this.tabPage3.Controls.Add(this.label31);
   this.tabPage3.Controls.Add(this.label30);
   this.tabPage3.Controls.Add(this.workState);
   this.tabPage3.Controls.Add(this.workZip);
   this.tabPage3.Controls.Add(this.label29);
   this.tabPage3.Controls.Add(this.workCity);
   this.tabPage3.Controls.Add(this.workAddress2);
   this.tabPage3.Controls.Add(this.workStreet);
   this.tabPage3.Controls.Add(this.label28);
   this.tabPage3.Controls.Add(this.label27);
   this.tabPage3.Controls.Add(this.label26);
   this.tabPage3.Controls.Add(this.label25);
   this.tabPage3.Controls.Add(this.label24);
   this.tabPage3.Location = new System.Drawing.Point(4, 22);
   this.tabPage3.Name = "tabPage3";
   this.tabPage3.Size = new System.Drawing.Size(608, 534);
   this.tabPage3.TabIndex = 2;
   this.tabPage3.Text = "Mailing Address";



   this.helpProvider1.SetHelpString(this.otherCountry, "Specifies the country for the contact\'s alternate address.");
   this.otherCountry.Location = new System.Drawing.Point(392, 424);
   this.otherCountry.Name = "otherCountry";
   this.helpProvider1.SetShowHelp(this.otherCountry, true);
   this.otherCountry.Size = new System.Drawing.Size(192, 21);
   this.otherCountry.TabIndex = 38;
   this.otherCountry.TextChanged += new System.EventHandler(this.otherCountry_TextChanged);



   this.helpProvider1.SetHelpString(this.homeCountry, "Specifies the country for the contact\'s home address.");
   this.homeCountry.Location = new System.Drawing.Point(392, 272);
   this.homeCountry.Name = "homeCountry";
   this.helpProvider1.SetShowHelp(this.homeCountry, true);
   this.homeCountry.Size = new System.Drawing.Size(192, 21);
   this.homeCountry.TabIndex = 25;
   this.homeCountry.TextChanged += new System.EventHandler(this.homeCountry_TextChanged);



   this.helpProvider1.SetHelpString(this.workCountry, "Specifies the country for the contact\'s work address.");
   this.workCountry.Location = new System.Drawing.Point(392, 120);
   this.workCountry.Name = "workCountry";
   this.helpProvider1.SetShowHelp(this.workCountry, true);
   this.workCountry.Size = new System.Drawing.Size(192, 21);
   this.workCountry.TabIndex = 12;
   this.workCountry.TextChanged += new System.EventHandler(this.workCountry_TextChanged);



   this.label44.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.75F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
   this.label44.Location = new System.Drawing.Point(16, 320);
   this.label44.Name = "label44";
   this.label44.Size = new System.Drawing.Size(100, 16);
   this.label44.TabIndex = 26;
   this.label44.Text = "Other";



   this.label39.Location = new System.Drawing.Point(344, 424);
   this.label39.Name = "label39";
   this.label39.Size = new System.Drawing.Size(48, 16);
   this.label39.TabIndex = 37;
   this.label39.Text = "Country:";



   this.helpProvider1.SetHelpString(this.otherState, "Specifies the state or province for the contact\'s alternate address.");
   this.otherState.Location = new System.Drawing.Point(96, 424);
   this.otherState.Name = "otherState";
   this.helpProvider1.SetShowHelp(this.otherState, true);
   this.otherState.Size = new System.Drawing.Size(192, 20);
   this.otherState.TabIndex = 34;
   this.otherState.Text = "";
   this.otherState.TextChanged += new System.EventHandler(this.otherState_TextChanged);



   this.helpProvider1.SetHelpString(this.otherZip, "Specifies the ZIP or postal code for the contact\'s alternate address.");
   this.otherZip.Location = new System.Drawing.Point(392, 400);
   this.otherZip.Name = "otherZip";
   this.helpProvider1.SetShowHelp(this.otherZip, true);
   this.otherZip.Size = new System.Drawing.Size(192, 20);
   this.otherZip.TabIndex = 36;
   this.otherZip.Text = "";
   this.otherZip.TextChanged += new System.EventHandler(this.otherZip_TextChanged);



   this.label38.Location = new System.Drawing.Point(304, 400);
   this.label38.Name = "label38";
   this.label38.Size = new System.Drawing.Size(88, 16);
   this.label38.TabIndex = 35;
   this.label38.Text = "Zip/Postal code:";



   this.helpProvider1.SetHelpString(this.otherCity, "Specifies the city for the contact\'s alternate address.");
   this.otherCity.Location = new System.Drawing.Point(96, 400);
   this.otherCity.Name = "otherCity";
   this.helpProvider1.SetShowHelp(this.otherCity, true);
   this.otherCity.Size = new System.Drawing.Size(192, 20);
   this.otherCity.TabIndex = 32;
   this.otherCity.Text = "";
   this.otherCity.TextChanged += new System.EventHandler(this.otherCity_TextChanged);



   this.otherAddress2.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
    | System.Windows.Forms.AnchorStyles.Right)));
   this.helpProvider1.SetHelpString(this.otherAddress2, "Specifies extended information for the contact\'s alternate address.");
   this.otherAddress2.Location = new System.Drawing.Point(96, 376);
   this.otherAddress2.Name = "otherAddress2";
   this.helpProvider1.SetShowHelp(this.otherAddress2, true);
   this.otherAddress2.Size = new System.Drawing.Size(488, 20);
   this.otherAddress2.TabIndex = 30;
   this.otherAddress2.Text = "";
   this.otherAddress2.TextChanged += new System.EventHandler(this.otherAddress2_TextChanged);



   this.otherStreet.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
    | System.Windows.Forms.AnchorStyles.Right)));
   this.helpProvider1.SetHelpString(this.otherStreet, "Specifies the street for the contact\'s alternate address.");
   this.otherStreet.Location = new System.Drawing.Point(96, 352);
   this.otherStreet.Name = "otherStreet";
   this.helpProvider1.SetShowHelp(this.otherStreet, true);
   this.otherStreet.Size = new System.Drawing.Size(488, 20);
   this.otherStreet.TabIndex = 28;
   this.otherStreet.Text = "";
   this.otherStreet.TextChanged += new System.EventHandler(this.otherStreet_TextChanged);



   this.label40.Location = new System.Drawing.Point(16, 424);
   this.label40.Name = "label40";
   this.label40.Size = new System.Drawing.Size(100, 16);
   this.label40.TabIndex = 33;
   this.label40.Text = "State/Province:";



   this.label41.Location = new System.Drawing.Point(64, 400);
   this.label41.Name = "label41";
   this.label41.Size = new System.Drawing.Size(100, 16);
   this.label41.TabIndex = 31;
   this.label41.Text = "City:";



   this.label42.Location = new System.Drawing.Point(40, 376);
   this.label42.Name = "label42";
   this.label42.Size = new System.Drawing.Size(100, 16);
   this.label42.TabIndex = 29;
   this.label42.Text = "Address2:";



   this.label43.Location = new System.Drawing.Point(48, 352);
   this.label43.Name = "label43";
   this.label43.Size = new System.Drawing.Size(100, 16);
   this.label43.TabIndex = 27;
   this.label43.Text = "Address:";



   this.label33.Location = new System.Drawing.Point(344, 272);
   this.label33.Name = "label33";
   this.label33.Size = new System.Drawing.Size(48, 16);
   this.label33.TabIndex = 24;
   this.label33.Text = "Country:";



   this.helpProvider1.SetHelpString(this.homeState, "Specifies the state or province for the contact\'s home address.");
   this.homeState.Location = new System.Drawing.Point(96, 272);
   this.homeState.Name = "homeState";
   this.helpProvider1.SetShowHelp(this.homeState, true);
   this.homeState.Size = new System.Drawing.Size(192, 20);
   this.homeState.TabIndex = 21;
   this.homeState.Text = "";
   this.homeState.TextChanged += new System.EventHandler(this.homeState_TextChanged);



   this.helpProvider1.SetHelpString(this.homeZip, "Specifies the ZIP or postal code for the contact\'s home address.");
   this.homeZip.Location = new System.Drawing.Point(392, 248);
   this.homeZip.Name = "homeZip";
   this.helpProvider1.SetShowHelp(this.homeZip, true);
   this.homeZip.Size = new System.Drawing.Size(192, 20);
   this.homeZip.TabIndex = 23;
   this.homeZip.Text = "";
   this.homeZip.TextChanged += new System.EventHandler(this.homeZip_TextChanged);



   this.label32.Location = new System.Drawing.Point(304, 248);
   this.label32.Name = "label32";
   this.label32.Size = new System.Drawing.Size(88, 16);
   this.label32.TabIndex = 22;
   this.label32.Text = "Zip/Postal code:";



   this.helpProvider1.SetHelpString(this.homeCity, "Specifies the city for the contact\'s home address.");
   this.homeCity.Location = new System.Drawing.Point(96, 248);
   this.homeCity.Name = "homeCity";
   this.helpProvider1.SetShowHelp(this.homeCity, true);
   this.homeCity.Size = new System.Drawing.Size(192, 20);
   this.homeCity.TabIndex = 19;
   this.homeCity.Text = "";
   this.homeCity.TextChanged += new System.EventHandler(this.homeCity_TextChanged);



   this.homeAddress2.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
    | System.Windows.Forms.AnchorStyles.Right)));
   this.helpProvider1.SetHelpString(this.homeAddress2, "Specifies extended information for the contact\'s home address.");
   this.homeAddress2.Location = new System.Drawing.Point(96, 224);
   this.homeAddress2.Name = "homeAddress2";
   this.helpProvider1.SetShowHelp(this.homeAddress2, true);
   this.homeAddress2.Size = new System.Drawing.Size(488, 20);
   this.homeAddress2.TabIndex = 17;
   this.homeAddress2.Text = "";
   this.homeAddress2.TextChanged += new System.EventHandler(this.homeAddress2_TextChanged);



   this.homeStreet.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
    | System.Windows.Forms.AnchorStyles.Right)));
   this.helpProvider1.SetHelpString(this.homeStreet, "Specifies the street for the contact\'s home address.");
   this.homeStreet.Location = new System.Drawing.Point(96, 200);
   this.homeStreet.Name = "homeStreet";
   this.helpProvider1.SetShowHelp(this.homeStreet, true);
   this.homeStreet.Size = new System.Drawing.Size(488, 20);
   this.homeStreet.TabIndex = 15;
   this.homeStreet.Text = "";
   this.homeStreet.TextChanged += new System.EventHandler(this.homeStreet_TextChanged);



   this.label34.Location = new System.Drawing.Point(16, 272);
   this.label34.Name = "label34";
   this.label34.Size = new System.Drawing.Size(100, 16);
   this.label34.TabIndex = 20;
   this.label34.Text = "State/Province:";



   this.label35.Location = new System.Drawing.Point(64, 248);
   this.label35.Name = "label35";
   this.label35.Size = new System.Drawing.Size(100, 16);
   this.label35.TabIndex = 18;
   this.label35.Text = "City:";



   this.label36.Location = new System.Drawing.Point(40, 224);
   this.label36.Name = "label36";
   this.label36.Size = new System.Drawing.Size(100, 16);
   this.label36.TabIndex = 16;
   this.label36.Text = "Address2:";



   this.label37.Location = new System.Drawing.Point(48, 200);
   this.label37.Name = "label37";
   this.label37.Size = new System.Drawing.Size(100, 16);
   this.label37.TabIndex = 14;
   this.label37.Text = "Address:";



   this.label31.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.75F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
   this.label31.Location = new System.Drawing.Point(16, 168);
   this.label31.Name = "label31";
   this.label31.Size = new System.Drawing.Size(100, 16);
   this.label31.TabIndex = 13;
   this.label31.Text = "Home";



   this.label30.Location = new System.Drawing.Point(344, 120);
   this.label30.Name = "label30";
   this.label30.Size = new System.Drawing.Size(48, 16);
   this.label30.TabIndex = 11;
   this.label30.Text = "Country:";



   this.helpProvider1.SetHelpString(this.workState, "Specifies the state or province for the contact\'s work address.");
   this.workState.Location = new System.Drawing.Point(96, 120);
   this.workState.Name = "workState";
   this.helpProvider1.SetShowHelp(this.workState, true);
   this.workState.Size = new System.Drawing.Size(192, 20);
   this.workState.TabIndex = 8;
   this.workState.Text = "";
   this.workState.TextChanged += new System.EventHandler(this.workState_TextChanged);



   this.helpProvider1.SetHelpString(this.workZip, "Specifies the ZIP or postal code for the contact\'s work address.");
   this.workZip.Location = new System.Drawing.Point(392, 96);
   this.workZip.Name = "workZip";
   this.helpProvider1.SetShowHelp(this.workZip, true);
   this.workZip.Size = new System.Drawing.Size(192, 20);
   this.workZip.TabIndex = 10;
   this.workZip.Text = "";
   this.workZip.TextChanged += new System.EventHandler(this.workZip_TextChanged);



   this.label29.Location = new System.Drawing.Point(304, 96);
   this.label29.Name = "label29";
   this.label29.Size = new System.Drawing.Size(88, 16);
   this.label29.TabIndex = 9;
   this.label29.Text = "Zip/Postal code:";



   this.helpProvider1.SetHelpString(this.workCity, "Specifies the city for the contact\'s work address.");
   this.workCity.Location = new System.Drawing.Point(96, 96);
   this.workCity.Name = "workCity";
   this.helpProvider1.SetShowHelp(this.workCity, true);
   this.workCity.Size = new System.Drawing.Size(192, 20);
   this.workCity.TabIndex = 6;
   this.workCity.Text = "";
   this.workCity.TextChanged += new System.EventHandler(this.workCity_TextChanged);



   this.workAddress2.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
    | System.Windows.Forms.AnchorStyles.Right)));
   this.helpProvider1.SetHelpString(this.workAddress2, "Specifies extended information for the contact\'s work address.");
   this.workAddress2.Location = new System.Drawing.Point(96, 72);
   this.workAddress2.Name = "workAddress2";
   this.helpProvider1.SetShowHelp(this.workAddress2, true);
   this.workAddress2.Size = new System.Drawing.Size(488, 20);
   this.workAddress2.TabIndex = 4;
   this.workAddress2.Text = "";
   this.workAddress2.TextChanged += new System.EventHandler(this.workAddress2_TextChanged);



   this.workStreet.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
    | System.Windows.Forms.AnchorStyles.Right)));
   this.helpProvider1.SetHelpString(this.workStreet, "Specifies the street for the contact\'s work address.");
   this.workStreet.Location = new System.Drawing.Point(96, 48);
   this.workStreet.Name = "workStreet";
   this.helpProvider1.SetShowHelp(this.workStreet, true);
   this.workStreet.Size = new System.Drawing.Size(488, 20);
   this.workStreet.TabIndex = 2;
   this.workStreet.Text = "";
   this.workStreet.TextChanged += new System.EventHandler(this.workStreet_TextChanged);



   this.label28.Location = new System.Drawing.Point(16, 120);
   this.label28.Name = "label28";
   this.label28.Size = new System.Drawing.Size(100, 16);
   this.label28.TabIndex = 7;
   this.label28.Text = "State/Province:";



   this.label27.Location = new System.Drawing.Point(64, 96);
   this.label27.Name = "label27";
   this.label27.Size = new System.Drawing.Size(100, 16);
   this.label27.TabIndex = 5;
   this.label27.Text = "City:";



   this.label26.Location = new System.Drawing.Point(40, 72);
   this.label26.Name = "label26";
   this.label26.Size = new System.Drawing.Size(100, 16);
   this.label26.TabIndex = 3;
   this.label26.Text = "Address2:";



   this.label25.Location = new System.Drawing.Point(48, 48);
   this.label25.Name = "label25";
   this.label25.Size = new System.Drawing.Size(100, 16);
   this.label25.TabIndex = 1;
   this.label25.Text = "Address:";



   this.label24.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.75F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
   this.label24.Location = new System.Drawing.Point(16, 16);
   this.label24.Name = "label24";
   this.label24.Size = new System.Drawing.Size(100, 16);
   this.label24.TabIndex = 0;
   this.label24.Text = "Work";



   this.cancel.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
   this.cancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
   this.cancel.FlatStyle = System.Windows.Forms.FlatStyle.System;
   this.cancel.Location = new System.Drawing.Point(544, 576);
   this.cancel.Name = "cancel";
   this.cancel.TabIndex = 1;
   this.cancel.Text = "Cancel";



   this.ok.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
   this.ok.DialogResult = System.Windows.Forms.DialogResult.OK;
   this.ok.FlatStyle = System.Windows.Forms.FlatStyle.System;
   this.ok.Location = new System.Drawing.Point(464, 576);
   this.ok.Name = "ok";
   this.ok.TabIndex = 2;
   this.ok.Text = "OK";
   this.ok.Click += new System.EventHandler(this.ok_Click);



   this.AcceptButton = this.ok;
   this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
   this.CancelButton = this.cancel;
   this.ClientSize = new System.Drawing.Size(632, 606);
   this.Controls.Add(this.ok);
   this.Controls.Add(this.cancel);
   this.Controls.Add(this.tabControl1);
   this.HelpButton = true;
   this.MaximizeBox = false;
   this.MinimizeBox = false;
   this.MinimumSize = new System.Drawing.Size(640, 640);
   this.Name = "ContactEditor";
   this.ShowInTaskbar = false;
   this.Text = "Contact Editor";
   this.SizeChanged += new System.EventHandler(this.ContactEditor_SizeChanged);
   this.Load += new System.EventHandler(this.ContactEditor_Load);
   this.Activated += new System.EventHandler(this.ContactEditor_Activated);
   this.tabControl1.ResumeLayout(false);
   this.tabPage1.ResumeLayout(false);
   this.tabPage2.ResumeLayout(false);
   this.tabPage3.ResumeLayout(false);
   this.ResumeLayout(false);

  }






  public Contact CurrentContact
  {
   get
   {
    return contact;
   }

   set
   {
    contact = value;
   }
  }




  public Novell.AddressBook.AddressBook CurrentAddressBook
  {
   set
   {
    this.addressBook = value;
   }
  }




  public string LoadPath
  {
   set
   {
    loadPath = value;
   }
  }



  private void LoadAddresses()
  {
   try
   {
    int n = 1;


    foreach(Email tmpMail in contact.GetEmailAddresses())
    {
     DomainUpDown emailLabel;

     if (tmpMail.Preferred)
     {
      eMail1 = tmpMail;
      email1.DataBindings.Add("Text", eMail1, "Address");
      emailLabel = emailLabel1;
     }
     else
     {
      switch (n)
      {
       case 1:
        eMail2 = tmpMail;
        email2.DataBindings.Add("Text", eMail2, "Address");
        emailLabel = emailLabel2;
        break;
       case 2:
        eMail3 = tmpMail;
        email3.DataBindings.Add("Text", eMail3, "Address");
        emailLabel = emailLabel3;
        break;
       default:
        eMail4 = tmpMail;
        email4.DataBindings.Add("Text", eMail4, "Address");
        emailLabel = emailLabel4;
        break;
      }

      n++;
     }

     if ((tmpMail.Types & EmailTypes.work) == EmailTypes.work)
     {
      emailLabel.SelectedIndex = 0;
     }
     else if ((tmpMail.Types & EmailTypes.personal) == EmailTypes.personal)
     {
      emailLabel.SelectedIndex = 1;
     }
     else if ((tmpMail.Types & EmailTypes.other) == EmailTypes.other)
     {
      emailLabel.SelectedIndex = 2;
     }
    }
   }
   catch (SimiasException e)
   {
    e.LogError();
    MessageBox.Show("An error occurred while reading email addresses.  Please see the log file for additional information.", "Email Address Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
   }
   catch (Exception e)
   {
    logger.Debug(e, "Reading email addresses");
    MessageBox.Show("An error occurred while reading email addresses.  Please see the log file for additional information.", "Email Address Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
   }


   try
   {
    int n = 1;

    foreach(Telephone tmpPhone in contact.GetTelephoneNumbers())
    {
     DomainUpDown phoneLabel;

     if (tmpPhone.Preferred)
     {
      telephone1 = tmpPhone;
      phone1.DataBindings.Add("Text", telephone1, "Number");
      phoneLabel = phoneLabel1;
     }
     else
     {
      switch (n)
      {
       case 1:
        telephone2 = tmpPhone;
        phone2.DataBindings.Add("Text", telephone2, "Number");
        phoneLabel = phoneLabel2;
        break;
       case 2:
        telephone3 = tmpPhone;
        phone3.DataBindings.Add("Text", telephone3, "Number");
        phoneLabel = phoneLabel3;
        break;
       default:
        telephone4 = tmpPhone;
        phone4.DataBindings.Add("Text", telephone4, "Number");
        phoneLabel = phoneLabel4;
        break;
      }

      n++;
     }


     if ((tmpPhone.Types & (PhoneTypes.work | PhoneTypes.voice)) == (PhoneTypes.work | PhoneTypes.voice))
     {
      phoneLabel.SelectedIndex = 0;
     }
     else if ((tmpPhone.Types & (PhoneTypes.work | PhoneTypes.fax)) == (PhoneTypes.work | PhoneTypes.fax))
     {
      phoneLabel.SelectedIndex = 1;
     }
     else if ((tmpPhone.Types & (PhoneTypes.home | PhoneTypes.voice)) == (PhoneTypes.home | PhoneTypes.voice))
     {
      phoneLabel.SelectedIndex = 2;
     }
     else if ((tmpPhone.Types & PhoneTypes.cell) == PhoneTypes.cell)
     {
      phoneLabel.SelectedIndex = 3;
     }
     else if ((tmpPhone.Types & PhoneTypes.pager) == PhoneTypes.pager)
     {
      phoneLabel.SelectedIndex = 4;
     }
    }
   }
   catch (SimiasException e)
   {
    e.LogError();
    MessageBox.Show("An error occurred while reading telephone numbers.  Please see the log file for additional information.", "Telephone Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
   }
   catch (Exception e)
   {
    logger.Debug(e, "Reading telephone numbers");
    MessageBox.Show("An error occurred while reading telephone numbers.  Please see the log file for additional information.", "Telephone Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
   }


   try
   {
    int n = 1;
    foreach (IM im in contact.GetInstantMessageAccounts())
    {
     DomainUpDown imLocation;

     if (im.Preferred)
     {
      iM1 = im;
      im1.DataBindings.Add("Text", iM1, "Address");
      imLocation = im1Location;
      imLabel1.DataBindings.Add("Text", iM1, "Provider");
     }
     else
     {
      switch (n)
      {
       case 1:
        iM2 = im;
        im2.DataBindings.Add("Text", iM2, "Address");
        imLocation = im2Location;
        imLabel2.DataBindings.Add("Text", iM2, "Provider");
        break;
       default:
        iM3 = im;
        im3.DataBindings.Add("Text", iM3, "Address");
        imLocation = im3Location;
        imLabel3.DataBindings.Add("Text", iM3, "Provider");
        break;
      }

      n++;
     }

     if ((im.Types & IMTypes.work) == IMTypes.work)
     {
      imLocation.SelectedIndex = 0;
     }
     else if ((im.Types & IMTypes.home) == IMTypes.home)
     {
      imLocation.SelectedIndex = 1;
     }
     else if ((im.Types & IMTypes.other) == IMTypes.other)
     {
      imLocation.SelectedIndex = 2;
     }
    }
   }
   catch (SimiasException e)
   {
    e.LogError();
    MessageBox.Show("An error occurred while reading IM addresses.  Please see the log file for additional information.", "IM Address Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
   }
   catch (Exception e)
   {
    logger.Debug(e, "Reading IM addresses");
    MessageBox.Show("An error occurred while reading IM addresses.  Please see the log file for additional information.", "IM Address Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
   }


   try
   {
    foreach(Address addr in contact.GetAddresses())
    {
     if((addr.Types & AddressTypes.work) == AddressTypes.work)
     {
      if (workAddress == null)
      {
       workAddress = addr;
       workStreet.DataBindings.Add("Text", workAddress, "Street");
       workAddress2.DataBindings.Add("Text", workAddress, "ExtendedAddress");
       workCity.DataBindings.Add("Text", workAddress, "Locality");
       workState.DataBindings.Add("Text", workAddress, "Region");
       workZip.DataBindings.Add("Text", workAddress, "PostalCode");
       workCountry.DataBindings.Add("Text", workAddress, "Country");
      }
     }
     else if((addr.Types & AddressTypes.home) == AddressTypes.home)
     {
      if (homeAddress == null)
      {
       homeAddress = addr;
       homeStreet.DataBindings.Add("Text", homeAddress, "Street");
       homeAddress2.DataBindings.Add("Text", homeAddress, "ExtendedAddress");
       homeCity.DataBindings.Add("Text", homeAddress, "Locality");
       homeState.DataBindings.Add("Text", homeAddress, "Region");
       homeZip.DataBindings.Add("Text", homeAddress, "PostalCode");
       homeCountry.DataBindings.Add("Text", homeAddress, "Country");
      }
     }
     else
     {
      if (otherAddress == null)
      {
       otherAddress = addr;
       otherStreet.DataBindings.Add("Text", otherAddress, "Street");
       otherAddress2.DataBindings.Add("Text", otherAddress, "ExtendedAddress");
       otherCity.DataBindings.Add("Text", otherAddress, "Locality");
       otherState.DataBindings.Add("Text", otherAddress, "Region");
       otherZip.DataBindings.Add("Text", otherAddress, "PostalCode");
       otherCountry.DataBindings.Add("Text", otherAddress, "Country");
      }
     }
    }
   }
   catch (SimiasException e)
   {
    e.LogError();
    MessageBox.Show("An error occurred while reading addresses.  Please see the log file for additional information.", "Address Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
   }
   catch (Exception e)
   {
    logger.Debug(e, "Reading addresses");
    MessageBox.Show("An error occurred while reading addresses.  Please see the log file for additional information.", "Address Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
   }

   return;
  }

  private string ConvertDateToString(DateTime dt)
  {
   string date = dt.ToString("s");
   return date.Substring(0, date.IndexOf("T"));
  }

  private void AddNewEmailAddress(ref Email email, ref TextBox editBox)
  {
   email = new Email(0, editBox.Text);
   try
   {
    editBox.DataBindings.Add("Text", email, "Address");
    contact.AddEmailAddress(email);
   }
   catch (SimiasException e)
   {
    e.LogError();
    MessageBox.Show("An error occurred while adding the email addresses.  Please see the log file for additional information.", "Add Email Address Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
   }
   catch (Exception e)
   {
    logger.Debug(e, "Adding email address");
    MessageBox.Show("An error occurred while adding the email addresses.  Please see the log file for additional information.", "Add Email Address Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
   }
  }

  private void SetEmailType(ref Email email, ref DomainUpDown type)
  {

   EmailTypes emailTypes = email.Types & ~(EmailTypes.work | EmailTypes.personal | EmailTypes.other);


   switch (type.SelectedIndex)
   {
    case 0:
     email.Types = emailTypes | EmailTypes.work;
     break;
    case 1:
     email.Types = emailTypes | EmailTypes.personal;
     break;
    default:
     email.Types = emailTypes | EmailTypes.other;
     break;
   }
  }

  private void AddNewTelephone(ref Telephone telephone, ref TextBox editBox)
  {
   telephone = new Telephone(editBox.Text);
   try
   {
    editBox.DataBindings.Add("Text", telephone, "Number");
    contact.AddTelephoneNumber(telephone);
   }
   catch (SimiasException e)
   {
    e.LogError();
    MessageBox.Show("An error occurred while adding the telephone number.  Please see the log file for additional information.", "Add Telephone Number Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
   }
   catch (Exception e)
   {
    logger.Debug(e, "Adding telephone number");
    MessageBox.Show("An error occurred while adding the telephone number.  Please see the log file for additional information.", "Add Telephone Number Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
   }
  }

  private void SetTelephoneType(ref Telephone telephone, ref DomainUpDown type)
  {

   PhoneTypes phoneTypes = telephone.Types & PhoneTypes.preferred;


   switch (type.SelectedIndex)
   {
    case 0:
     telephone.Types = phoneTypes | PhoneTypes.work | PhoneTypes.voice;
     break;
    case 1:
     telephone.Types = phoneTypes | PhoneTypes.work | PhoneTypes.fax;
     break;
    case 2:
     telephone.Types = phoneTypes | PhoneTypes.home | PhoneTypes.voice;
     break;
    case 3:
     telephone.Types = phoneTypes | PhoneTypes.cell;
     break;
    default:
     telephone.Types = phoneTypes | PhoneTypes.pager;
     break;
   }
  }

  private void AddNewIMAccount(ref IM im, ref DomainUpDown labelBox, ref TextBox editBox)
  {
   im = new IM(editBox.Text, labelBox.Text);
   try
   {
    editBox.DataBindings.Add("Text", im, "Address");
    labelBox.DataBindings.Add("Text", im, "Provider");
    contact.AddInstantMessage(im);
   }
   catch (SimiasException e)
   {
    e.LogError();
    MessageBox.Show("An error occurred while adding the IM address.  Please see the log file for additional information.", "Add IM Address Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
   }
   catch (Exception e)
   {
    logger.Debug(e, "Adding IM address");
    MessageBox.Show("An error occurred while adding the IM address.  Please see the log file for additional information.", "Add IM Address Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
   }
  }

  private void SetIMType(ref IM im, ref DomainUpDown type)
  {

   IMTypes imTypes = im.Types & IMTypes.preferred;


   switch (type.SelectedIndex)
   {
    case 0:
     im.Types = imTypes | IMTypes.work;
     break;
    case 1:
     im.Types = imTypes | IMTypes.home;
     break;
    default:
     im.Types = imTypes | IMTypes.other;
     break;
   }
  }

  private void AddNewWorkAddress()
  {
   workAddress.Types = AddressTypes.work;
   workStreet.DataBindings.Add("Text", workAddress, "Street");
   workAddress2.DataBindings.Add("Text", workAddress, "ExtendedAddress");
   workCity.DataBindings.Add("Text", workAddress, "Locality");
   workState.DataBindings.Add("Text", workAddress, "Region");
   workZip.DataBindings.Add("Text", workAddress, "PostalCode");
   workCountry.DataBindings.Add("Text", workAddress, "Country");
  }

  private void AddNewHomeAddress()
  {
   homeAddress.Types = AddressTypes.home;
   homeStreet.DataBindings.Add("Text", homeAddress, "Street");
   homeAddress2.DataBindings.Add("Text", homeAddress, "ExtendedAddress");
   homeCity.DataBindings.Add("Text", homeAddress, "Locality");
   homeState.DataBindings.Add("Text", homeAddress, "Region");
   homeZip.DataBindings.Add("Text", homeAddress, "PostalCode");
   homeCountry.DataBindings.Add("Text", homeAddress, "Country");
  }

  private void AddNewOtherAddress()
  {
   otherAddress.Types = AddressTypes.other;
   otherStreet.DataBindings.Add("Text", otherAddress, "Street");
   otherAddress2.DataBindings.Add("Text", otherAddress, "ExtendedAddress");
   otherCity.DataBindings.Add("Text", otherAddress, "Locality");
   otherState.DataBindings.Add("Text", otherAddress, "Region");
   otherZip.DataBindings.Add("Text", otherAddress, "PostalCode");
   otherCountry.DataBindings.Add("Text", otherAddress, "Country");
  }

  private void SetEditBoxSizeAndLocation(Control box1, Control label1, Control box2, int fixedSpace)
  {

   int offset1 = label1.Left - (box1.Left + box1.Width);
   int offset2 = box2.Left - (label1.Left + label1.Width);


   box1.Width = (this.Width - fixedSpace) / 2;
   box2.Width = box1.Width;


   label1.Left = box1.Left + box1.Width + offset1;
   box2.Left = label1.Left + label1.Width + offset2;
  }



  private void ContactEditor_Load(object sender, System.EventArgs e)
  {

   try
   {
    if (loadPath != null)
    {
     this.Icon = new Icon(Path.Combine(loadPath, applicationIcon));
    }
    else
    {
     this.Icon = new Icon(Path.Combine(Application.StartupPath, applicationIcon));
    }
   }
   catch (Exception ex)
   {
    logger.Debug(ex, "Loading icon");
   }


   emailLabel1.SelectedIndex = 0;
   emailLabel2.SelectedIndex = 1;
   emailLabel3.SelectedIndex = 2;
   emailLabel4.SelectedIndex = 2;
   phoneLabel1.SelectedIndex = 0;
   phoneLabel2.SelectedIndex = 1;
   phoneLabel3.SelectedIndex = 3;
   phoneLabel4.SelectedIndex = 2;
   imLabel1.SelectedIndex = 0;
   imLabel2.SelectedIndex = 1;
   imLabel3.SelectedIndex = 2;
   im1Location.SelectedIndex = 0;
   im2Location.SelectedIndex = 1;
   im3Location.SelectedIndex = 2;


   if (contact == null)
   {
    contact = new Contact();
    newContact = true;


    ok.Enabled = false;
   }

   string basePath = Path.Combine(loadPath != null ? loadPath : Application.StartupPath, "res");

   if (contact != null)
   {



    try
    {
     name = this.contact.GetPreferredName();
     fullName.Text = name.FN;
     nameValidated = true;
    }
    catch (Exception ex)
    {
     logger.Info(ex, "Full name not found");
     fullName.Text = "";
    }

    jobTitle.DataBindings.Add("Text", contact, "Title");
    department.DataBindings.Add("Text", contact, "Organization");
    homePage.DataBindings.Add("Text", contact, "Url");
    blog.DataBindings.Add("Text", contact, "Blog");
    nickname.DataBindings.Add("Text", contact, "Nickname");
    birthday.DataBindings.Add("Text", contact, "Birthday");
    calendar.DataBindings.Add("Text", contact, "Calendar");
    webcam.DataBindings.Add("Text", contact, "Webcam");

    try
    {
     pictureContact.SizeMode = PictureBoxSizeMode.StretchImage;
     pictureContact.Image = Image.FromStream(contact.ExportPhoto());
    }
    catch (Exception ex)
    {
     logger.Debug(ex, "Loading photo");

     try
     {
      pictureContact.Image = Image.FromFile(Path.Combine(basePath, "blankhead.png"));
     }
     catch (Exception ex2)
     {
      logger.Debug(ex2, "Loading default image");
     }
    }

    LoadAddresses();
   }
  }

  private void ok_Click(object sender, System.EventArgs e)
  {
   if (fullName.Focused)
   {

    fullName_Leave(this, e);
   }

   try
   {
    if (newContact)
    {

     if (email1.Text != String.Empty)
     {
      contact.UserName = email1.Text.Substring(0, email1.Text.IndexOf("@"));
     }
     else
     {
      contact.UserName = name.Given + name.Family;
     }

     addressBook.AddContact(contact);
    }

    contact.Commit();
   }
   catch (SimiasException ex)
   {
    ex.LogError();
    MessageBox.Show("An error occurred while adding the contact.  Please see the log file for additional information.", "Add Contact Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
   }
   catch (Exception ex)
   {
    logger.Debug(ex, "Adding contact");
    MessageBox.Show("An error occurred while adding the contact.  Please see the log file for additional information.", "Add Contact Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
   }
  }

  private void pictureContact_Click(object sender, System.EventArgs e)
  {
   OpenFileDialog openFileDialog = new OpenFileDialog();
   openFileDialog.Title = "Add Contact Picture";
   openFileDialog.Filter = "Image Files(*.BMP;*.JPG;*.GIF)|*.BMP;*.JPG;*.GIF";

   if(openFileDialog.ShowDialog() == DialogResult.OK)
   {
    try
    {
     contact.ImportPhoto(openFileDialog.OpenFile());
     pictureContact.Image = Image.FromStream(contact.ExportPhoto());
    }
    catch (SimiasException ex)
    {
     ex.LogError();
     MessageBox.Show("An error occurred while adding the contact photo.  Please see the log file for additional information.", "Add Contact Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
    }
    catch (Exception ex)
    {
     logger.Debug(ex, "Adding photo");
     MessageBox.Show("An error occurred while adding the contact photo.  Please see the log file for additional information.", "Add Contact Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
    }
   }
  }


  private void fullNameButton_Click(object sender, System.EventArgs e)
  {
   FullName fullNameDlg = new FullName();
   bool newName = false;


   if (name == null)
   {
    name = new Name();
    newName = true;
   }

   fullNameDlg.Title = name.Prefix;
   fullNameDlg.FirstName = name.Given;
   fullNameDlg.MiddleName = name.Other;
   fullNameDlg.LastName = name.Family;
   fullNameDlg.Suffix = name.Suffix;

   DialogResult result = fullNameDlg.ShowDialog();
   if (result == DialogResult.OK)
   {

    if (name.Prefix != fullNameDlg.Title)
     name.Prefix = fullNameDlg.Title;
    if (name.Given != fullNameDlg.FirstName)
     name.Given = fullNameDlg.FirstName;
    if (name.Other != fullNameDlg.MiddleName)
     name.Other = fullNameDlg.MiddleName;
    if (name.Family != fullNameDlg.LastName)
     name.Family = fullNameDlg.LastName;
    if (name.Suffix != fullNameDlg.Suffix)
     name.Suffix = fullNameDlg.Suffix;

    if (newName && (contact != null))
    {
     name.Preferred = true;
     try
     {
      contact.AddName(name);
     }
     catch (SimiasException ex)
     {
      ex.LogError();
      MessageBox.Show("An error occurred while adding the name.  Please see the log file for additional information.", "Add Name Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
     }
     catch (Exception ex)
     {
      logger.Debug(ex, "Adding name");
      MessageBox.Show("An error occurred while adding the name.  Please see the log file for additional information.", "Add Name Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
     }
    }

    fullName.Text = name.FN;
    nameValidated = true;
   }
  }

  private void fullName_TextChanged(object sender, System.EventArgs e)
  {
   nameValidated = false;

   if (newContact && (email1.Text == String.Empty))
   {
    ok.Enabled = fullName.Text != String.Empty;
   }
  }

  private void fullName_Leave(object sender, System.EventArgs e)
  {



   if (cancel.Focused)
    return;

   if (name == null)
   {
    name = new Name();
    contact.AddName(name);
   }


   string tmpName = fullName.Text.Trim();


   if (!nameValidated && !FullNameParser.Parse(tmpName, ref name))
   {


    fullNameButton_Click(this, null);
   }
  }



  private void emailLabel1_SelectedItemChanged(object sender, System.EventArgs e)
  {
   if (emailLabel1.Focused)
   {
    if (eMail1 == null)
    {
     AddNewEmailAddress(ref eMail1, ref email1);
    }

    SetEmailType(ref eMail1, ref emailLabel1);
   }
  }

  private void email1_TextChanged(object sender, System.EventArgs e)
  {
   if (eMail1 == null)
   {
    AddNewEmailAddress(ref eMail1, ref email1);
    SetEmailType(ref eMail1, ref emailLabel1);
   }

   if (newContact && (fullName.Text == String.Empty))
   {
    ok.Enabled = email1.Text != String.Empty;
   }
  }

  private void email1_Leave(object sender, System.EventArgs e)
  {


   if (email1.Text == String.Empty)
   {
    if (eMail1 != null)
    {
     eMail1.Delete();
     eMail1 = null;
    }
   }
  }

  private void emailLabel2_SelectedItemChanged(object sender, System.EventArgs e)
  {
   if (emailLabel2.Focused)
   {
    if (eMail2 == null)
    {
     AddNewEmailAddress(ref eMail2, ref email2);
    }

    SetEmailType(ref eMail2, ref emailLabel2);
   }
  }

  private void email2_TextChanged(object sender, System.EventArgs e)
  {
   if (eMail2 == null)
   {
    AddNewEmailAddress(ref eMail2, ref email2);
    SetEmailType(ref eMail2, ref emailLabel2);
   }
  }

  private void email2_Leave(object sender, System.EventArgs e)
  {


   if (email2.Text == String.Empty)
   {
    if (eMail2 != null)
    {
     eMail2.Delete();
     eMail2 = null;
    }
   }
  }

  private void emailLabel3_SelectedItemChanged(object sender, System.EventArgs e)
  {
   if (emailLabel3.Focused)
   {
    if (eMail3 == null)
    {
     AddNewEmailAddress(ref eMail3, ref email3);
    }

    SetEmailType(ref eMail3, ref emailLabel3);
   }
  }

  private void email3_TextChanged(object sender, System.EventArgs e)
  {
   if (eMail3 == null)
   {
    AddNewEmailAddress(ref eMail3, ref email3);
    SetEmailType(ref eMail3, ref emailLabel3);
   }
  }

  private void email3_Leave(object sender, System.EventArgs e)
  {


   if (email3.Text == String.Empty)
   {
    if (eMail3 != null)
    {
     eMail3.Delete();
     eMail3 = null;
    }
   }
  }

  private void emailLabel4_SelectedItemChanged(object sender, System.EventArgs e)
  {
   if (emailLabel4.Focused)
   {
    if (eMail4 == null)
    {
     AddNewEmailAddress(ref eMail4, ref email4);
    }

    SetEmailType(ref eMail4, ref emailLabel4);
   }
  }

  private void email4_TextChanged(object sender, System.EventArgs e)
  {
   if (eMail4 == null)
   {
    AddNewEmailAddress(ref eMail4, ref email4);
    SetEmailType(ref eMail4, ref emailLabel4);
   }
  }

  private void email4_Leave(object sender, System.EventArgs e)
  {


   if (email4.Text == String.Empty)
   {
    if (eMail4 != null)
    {
     eMail4.Delete();
     eMail4 = null;
    }
   }
  }



  private void phoneLabel1_SelectedItemChanged(object sender, System.EventArgs e)
  {
   if (phoneLabel1.Focused)
   {
    if (telephone1 == null)
    {
     AddNewTelephone(ref telephone1, ref phone1);
    }

    SetTelephoneType(ref telephone1, ref phoneLabel1);
   }
  }

  private void phone1_TextChanged(object sender, System.EventArgs e)
  {
   if (telephone1 == null)
   {
    AddNewTelephone(ref telephone1, ref phone1);
    SetTelephoneType(ref telephone1, ref phoneLabel1);
   }
  }

  private void phoneLabel2_SelectedItemChanged(object sender, System.EventArgs e)
  {
   if (phoneLabel2.Focused)
   {
    if (telephone2 == null)
    {
     AddNewTelephone(ref telephone2, ref phone2);
    }

    SetTelephoneType(ref telephone2, ref phoneLabel2);
   }
  }

  private void phone2_TextChanged(object sender, System.EventArgs e)
  {
   if (telephone2 == null)
   {
    AddNewTelephone(ref telephone2, ref phone2);
    SetTelephoneType(ref telephone2, ref phoneLabel2);
   }
  }

  private void phoneLabel3_SelectedItemChanged(object sender, System.EventArgs e)
  {
   if (phoneLabel3.Focused)
   {
    if (telephone3 == null)
    {
     AddNewTelephone(ref telephone3, ref phone3);
    }

    SetTelephoneType(ref telephone3, ref phoneLabel3);
   }
  }

  private void phone3_TextChanged(object sender, System.EventArgs e)
  {
   if (telephone3 == null)
   {
    AddNewTelephone(ref telephone3, ref phone3);
    SetTelephoneType(ref telephone3, ref phoneLabel3);
   }
  }

  private void phoneLabel4_SelectedItemChanged(object sender, System.EventArgs e)
  {
   if (phoneLabel4.Focused)
   {
    if (telephone4 == null)
    {
     AddNewTelephone(ref telephone4, ref phone4);
    }

    SetTelephoneType(ref telephone4, ref phoneLabel4);
   }
  }

  private void phone4_TextChanged(object sender, System.EventArgs e)
  {
   if (telephone4 == null)
   {
    AddNewTelephone(ref telephone4, ref phone4);
    SetTelephoneType(ref telephone4, ref phoneLabel4);
   }
  }



  private void imLabel1_TextChanged(object sender, System.EventArgs e)
  {
   if (imLabel1.Focused)
   {
    if (iM1 == null)
    {
     AddNewIMAccount(ref iM1, ref imLabel1, ref im1);
     SetIMType(ref iM1, ref im1Location);
    }
   }
  }

  private void im1_TextChanged(object sender, System.EventArgs e)
  {
   if (iM1 == null)
   {
    AddNewIMAccount(ref iM1, ref imLabel1, ref im1);
    SetIMType(ref iM1, ref im1Location);
   }
  }

  private void im1Location_SelectedItemChanged(object sender, System.EventArgs e)
  {
   if (im1Location.Focused)
   {
    if (iM1 == null)
    {
     AddNewIMAccount(ref iM1, ref imLabel1, ref im1);
    }

    SetIMType(ref iM1, ref im1Location);
   }
  }

  private void imLabel2_TextChanged(object sender, System.EventArgs e)
  {
   if (imLabel2.Focused)
   {
    if (iM2 == null)
    {
     AddNewIMAccount(ref iM2, ref imLabel2, ref im2);
     SetIMType(ref iM2, ref im2Location);
    }
   }
  }

  private void im2_TextChanged(object sender, System.EventArgs e)
  {
   if (iM2 == null)
   {
    AddNewIMAccount(ref iM2, ref imLabel2, ref im2);
    SetIMType(ref iM2, ref im2Location);
   }
  }

  private void im2Location_SelectedItemChanged(object sender, System.EventArgs e)
  {
   if (im2Location.Focused)
   {
    if (iM2 == null)
    {
     AddNewIMAccount(ref iM2, ref imLabel2, ref im2);
    }

    SetIMType(ref iM2, ref im2Location);
   }
  }

  private void imLabel3_TextChanged(object sender, System.EventArgs e)
  {
   if (imLabel3.Focused)
   {
    if (iM3 == null)
    {
     AddNewIMAccount(ref iM3, ref imLabel3, ref im3);
     SetIMType(ref iM3, ref im3Location);
    }
   }
  }

  private void im3_TextChanged(object sender, System.EventArgs e)
  {
   if (iM3 == null)
   {
    AddNewIMAccount(ref iM3, ref imLabel3, ref im3);
    SetIMType(ref iM3, ref im3Location);
   }
  }

  private void im3Location_SelectedItemChanged(object sender, System.EventArgs e)
  {
   if (im3Location.Focused)
   {
    if (iM3 == null)
    {
     AddNewIMAccount(ref iM3, ref imLabel3, ref im3);
    }

    SetIMType(ref iM3, ref im3Location);
   }
  }


  private void birthdayPicker_ValueChanged(object sender, System.EventArgs e)
  {
   birthday.Text = ConvertDateToString(birthdayPicker.Value);
  }

  private void birthday_Leave(object sender, System.EventArgs e)
  {
   try
   {

    birthdayPicker.Text = birthday.Text;
   }
   catch (Exception ex)
   {
    logger.Debug(ex, "Get birthday");
   }
  }


  private void workStreet_TextChanged(object sender, System.EventArgs e)
  {
   if (workAddress == null)
   {
    workAddress = new Address();
    workAddress.Street = workStreet.Text;
    try
    {
     AddNewWorkAddress();
     contact.AddAddress(workAddress);
    }
    catch (SimiasException ex)
    {
     ex.LogError();
     MessageBox.Show("An error occurred while adding the work address.  Please see the log file for additional information", "Add Work Address Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
    }
    catch (Exception ex)
    {
     logger.Debug(ex, "Adding work address");
     MessageBox.Show("An error occurred while adding the work address.  Please see the log file for additional information", "Add Work Address Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
    }
   }
  }

  private void workAddress2_TextChanged(object sender, System.EventArgs e)
  {
   if (workAddress == null)
   {
    workAddress = new Address();
    workAddress.ExtendedAddress = workAddress2.Text;
    try
    {
     AddNewWorkAddress();
     contact.AddAddress(workAddress);
    }
    catch (SimiasException ex)
    {
     ex.LogError();
     MessageBox.Show("An error occurred while adding the work address.  Please see the log file for additional information", "Add Work Address Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
    }
    catch (Exception ex)
    {
     logger.Debug(ex, "Adding work address");
     MessageBox.Show("An error occurred while adding the work address.  Please see the log file for additional information", "Add Work Address Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
    }
   }
  }

  private void workCity_TextChanged(object sender, System.EventArgs e)
  {
   if (workAddress == null)
   {
    workAddress = new Address();
    workAddress.Locality = workCity.Text;
    try
    {
     AddNewWorkAddress();
     contact.AddAddress(workAddress);
    }
    catch (SimiasException ex)
    {
     ex.LogError();
     MessageBox.Show("An error occurred while adding the work address.  Please see the log file for additional information", "Add Work Address Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
    }
    catch (Exception ex)
    {
     logger.Debug(ex, "Adding work address");
     MessageBox.Show("An error occurred while adding the work address.  Please see the log file for additional information", "Add Work Address Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
    }
   }
  }

  private void workState_TextChanged(object sender, System.EventArgs e)
  {
   if (workAddress == null)
   {
    workAddress = new Address();
    workAddress.Region = workState.Text;
    try
    {
     AddNewWorkAddress();
     contact.AddAddress(workAddress);
    }
    catch (SimiasException ex)
    {
     ex.LogError();
     MessageBox.Show("An error occurred while adding the work address.  Please see the log file for additional information", "Add Work Address Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
    }
    catch (Exception ex)
    {
     logger.Debug(ex, "Adding work address");
     MessageBox.Show("An error occurred while adding the work address.  Please see the log file for additional information", "Add Work Address Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
    }
   }
  }

  private void workZip_TextChanged(object sender, System.EventArgs e)
  {
   if (workAddress == null)
   {
    workAddress = new Address();
    workAddress.PostalCode = workZip.Text;
    try
    {
     AddNewWorkAddress();
     contact.AddAddress(workAddress);
    }
    catch (SimiasException ex)
    {
     ex.LogError();
     MessageBox.Show("An error occurred while adding the work address.  Please see the log file for additional information", "Add Work Address Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
    }
    catch (Exception ex)
    {
     logger.Debug(ex, "Adding work address");
     MessageBox.Show("An error occurred while adding the work address.  Please see the log file for additional information", "Add Work Address Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
    }
   }
  }

  private void workCountry_TextChanged(object sender, System.EventArgs e)
  {
   if (workAddress == null)
   {
    workAddress = new Address();
    workAddress.Country = workCountry.Text;
    try
    {
     AddNewWorkAddress();
     contact.AddAddress(workAddress);
    }
    catch (SimiasException ex)
    {
     ex.LogError();
     MessageBox.Show("An error occurred while adding the work address.  Please see the log file for additional information", "Add Work Address Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
    }
    catch (Exception ex)
    {
     logger.Debug(ex, "Adding work address");
     MessageBox.Show("An error occurred while adding the work address.  Please see the log file for additional information", "Add Work Address Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
    }
   }
  }

  private void homeStreet_TextChanged(object sender, System.EventArgs e)
  {
   if (homeAddress == null)
   {
    homeAddress = new Address();
    homeAddress.Street = homeStreet.Text;
    try
    {
     AddNewHomeAddress();
     contact.AddAddress(homeAddress);
    }
    catch (SimiasException ex)
    {
     ex.LogError();
     MessageBox.Show("An error occurred while adding the home address.  Please see the log file for additional information", "Add Home Address Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
    }
    catch (Exception ex)
    {
     logger.Debug(ex, "Adding home address");
     MessageBox.Show("An error occurred while adding the home address.  Please see the log file for additional information", "Add Home Address Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
    }
   }
  }

  private void homeAddress2_TextChanged(object sender, System.EventArgs e)
  {
   if (homeAddress == null)
   {
    homeAddress = new Address();
    homeAddress.ExtendedAddress = homeAddress2.Text;
    try
    {
     AddNewHomeAddress();
     contact.AddAddress(homeAddress);
    }
    catch (SimiasException ex)
    {
     ex.LogError();
     MessageBox.Show("An error occurred while adding the home address.  Please see the log file for additional information", "Add Home Address Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
    }
    catch (Exception ex)
    {
     logger.Debug(ex, "Adding home address");
     MessageBox.Show("An error occurred while adding the home address.  Please see the log file for additional information", "Add Home Address Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
    }
   }
  }

  private void homeCity_TextChanged(object sender, System.EventArgs e)
  {
   if (homeAddress == null)
   {
    homeAddress = new Address();
    homeAddress.Locality = homeCity.Text;
    try
    {
     AddNewHomeAddress();
     contact.AddAddress(homeAddress);
    }
    catch (SimiasException ex)
    {
     ex.LogError();
     MessageBox.Show("An error occurred while adding the home address.  Please see the log file for additional information", "Add Home Address Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
    }
    catch (Exception ex)
    {
     logger.Debug(ex, "Adding home address");
     MessageBox.Show("An error occurred while adding the home address.  Please see the log file for additional information", "Add Home Address Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
    }
   }
  }

  private void homeState_TextChanged(object sender, System.EventArgs e)
  {
   if (homeAddress == null)
   {
    homeAddress = new Address();
    homeAddress.Region = homeState.Text;
    try
    {
     AddNewHomeAddress();
     contact.AddAddress(homeAddress);
    }
    catch (SimiasException ex)
    {
     ex.LogError();
     MessageBox.Show("An error occurred while adding the home address.  Please see the log file for additional information", "Add Home Address Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
    }
    catch (Exception ex)
    {
     logger.Debug(ex, "Adding home address");
     MessageBox.Show("An error occurred while adding the home address.  Please see the log file for additional information", "Add Home Address Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
    }
   }
  }

  private void homeZip_TextChanged(object sender, System.EventArgs e)
  {
   if (homeAddress == null)
   {
    homeAddress = new Address();
    homeAddress.PostalCode = homeZip.Text;
    try
    {
     AddNewHomeAddress();
     contact.AddAddress(homeAddress);
    }
    catch (SimiasException ex)
    {
     ex.LogError();
     MessageBox.Show("An error occurred while adding the home address.  Please see the log file for additional information", "Add Home Address Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
    }
    catch (Exception ex)
    {
     logger.Debug(ex, "Adding home address");
     MessageBox.Show("An error occurred while adding the home address.  Please see the log file for additional information", "Add Home Address Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
    }
   }
  }

  private void homeCountry_TextChanged(object sender, System.EventArgs e)
  {
   if (homeAddress == null)
   {
    homeAddress = new Address();
    homeAddress.Country = homeCountry.Text;
    try
    {
     AddNewHomeAddress();
     contact.AddAddress(homeAddress);
    }
    catch (SimiasException ex)
    {
     ex.LogError();
     MessageBox.Show("An error occurred while adding the home address.  Please see the log file for additional information", "Add Home Address Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
    }
    catch (Exception ex)
    {
     logger.Debug(ex, "Adding home address");
     MessageBox.Show("An error occurred while adding the home address.  Please see the log file for additional information", "Add Home Address Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
    }
   }
  }

  private void otherStreet_TextChanged(object sender, System.EventArgs e)
  {
   if (otherAddress == null)
   {
    otherAddress = new Address();
    otherAddress.Street = otherStreet.Text;
    try
    {
     AddNewOtherAddress();
     contact.AddAddress(otherAddress);
    }
    catch (SimiasException ex)
    {
     ex.LogError();
     MessageBox.Show("An error occurred while adding the other address.  Please see the log file for additional information", "Add Other Address Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
    }
    catch (Exception ex)
    {
     logger.Debug(ex, "Adding other address");
     MessageBox.Show("An error occurred while adding the other address.  Please see the log file for additional information", "Add Other Address Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
    }
   }
  }

  private void otherAddress2_TextChanged(object sender, System.EventArgs e)
  {
   if (otherAddress == null)
   {
    otherAddress = new Address();
    otherAddress.ExtendedAddress = otherAddress2.Text;
    try
    {
     AddNewOtherAddress();
     contact.AddAddress(otherAddress);
    }
    catch (SimiasException ex)
    {
     ex.LogError();
     MessageBox.Show("An error occurred while adding the other address.  Please see the log file for additional information", "Add Other Address Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
    }
    catch (Exception ex)
    {
     logger.Debug(ex, "Adding other address");
     MessageBox.Show("An error occurred while adding the other address.  Please see the log file for additional information", "Add Other Address Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
    }
   }
  }

  private void otherCity_TextChanged(object sender, System.EventArgs e)
  {
   if (otherAddress == null)
   {
    otherAddress = new Address();
    otherAddress.Locality = otherCity.Text;
    try
    {
     AddNewOtherAddress();
     contact.AddAddress(otherAddress);
    }
    catch (SimiasException ex)
    {
     ex.LogError();
     MessageBox.Show("An error occurred while adding the other address.  Please see the log file for additional information", "Add Other Address Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
    }
    catch (Exception ex)
    {
     logger.Debug(ex, "Adding other address");
     MessageBox.Show("An error occurred while adding the other address.  Please see the log file for additional information", "Add Other Address Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
    }
   }
  }

  private void otherState_TextChanged(object sender, System.EventArgs e)
  {
   if (otherAddress == null)
   {
    otherAddress = new Address();
    otherAddress.Region = otherState.Text;
    try
    {
     AddNewOtherAddress();
     contact.AddAddress(otherAddress);
    }
    catch (SimiasException ex)
    {
     ex.LogError();
     MessageBox.Show("An error occurred while adding the other address.  Please see the log file for additional information", "Add Other Address Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
    }
    catch (Exception ex)
    {
     logger.Debug(ex, "Adding other address");
     MessageBox.Show("An error occurred while adding the other address.  Please see the log file for additional information", "Add Other Address Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
    }
   }
  }

  private void otherZip_TextChanged(object sender, System.EventArgs e)
  {
   if (otherAddress == null)
   {
    otherAddress = new Address();
    otherAddress.PostalCode = otherZip.Text;
    try
    {
     AddNewOtherAddress();
     contact.AddAddress(otherAddress);
    }
    catch (SimiasException ex)
    {
     ex.LogError();
     MessageBox.Show("An error occurred while adding the other address.  Please see the log file for additional information", "Add Other Address Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
    }
    catch (Exception ex)
    {
     logger.Debug(ex, "Adding other address");
     MessageBox.Show("An error occurred while adding the other address.  Please see the log file for additional information", "Add Other Address Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
    }
   }
  }

  private void otherCountry_TextChanged(object sender, System.EventArgs e)
  {
   if (otherAddress == null)
   {
    otherAddress = new Address();
    otherAddress.Country = otherCountry.Text;
    try
    {
     AddNewOtherAddress();
     contact.AddAddress(otherAddress);
    }
    catch (SimiasException ex)
    {
     ex.LogError();
     MessageBox.Show("An error occurred while adding the other address.  Please see the log file for additional information", "Add Other Address Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
    }
    catch (Exception ex)
    {
     logger.Debug(ex, "Adding other address");
     MessageBox.Show("An error occurred while adding the other address.  Please see the log file for additional information", "Add Other Address Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
    }
   }
  }


  private void ContactEditor_SizeChanged(object sender, System.EventArgs e)
  {

   SetEditBoxSizeAndLocation(email1, emailLabel2, email2, emailFixedSpace);
   SetEditBoxSizeAndLocation(email3, emailLabel4, email4, emailFixedSpace);


   SetEditBoxSizeAndLocation(phone1, phoneLabel2, phone2, phoneFixedSpace);
   SetEditBoxSizeAndLocation(phone3, phoneLabel4, phone4, phoneFixedSpace);


   SetEditBoxSizeAndLocation(profession, label17, jobTitle, jobFixedSpace);
   SetEditBoxSizeAndLocation(company, label18, department, jobFixedSpace);
   manager.Size = profession.Size;

   SetEditBoxSizeAndLocation(workCity, label29, workZip, addrFixedSpace);
   SetEditBoxSizeAndLocation(workState, label30, workCountry, addrFixedSpace);
   SetEditBoxSizeAndLocation(homeCity, label32, homeZip, addrFixedSpace);
   SetEditBoxSizeAndLocation(homeState, label33, homeCountry, addrFixedSpace);
   SetEditBoxSizeAndLocation(otherCity, label38, otherZip, addrFixedSpace);
   SetEditBoxSizeAndLocation(otherState, label39, otherCountry, addrFixedSpace);
  }

  private void ContactEditor_Activated(object sender, System.EventArgs e)
  {

   fullName.Focus();
  }

 }
}
