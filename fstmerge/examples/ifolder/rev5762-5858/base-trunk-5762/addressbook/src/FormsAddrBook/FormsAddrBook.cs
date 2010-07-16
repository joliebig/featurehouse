

using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;
using System.IO;
using System.Diagnostics;
using Novell.iFolder.FormsBookLib;
using Novell.AddressBook;
using Simias;

namespace Novell.iFolder.FormsAddrBook
{



 public class FormsAddrBook : System.Windows.Forms.Form
 {


  private static readonly ISimiasLog logger = SimiasLogManager.GetLogger(typeof(FormsAddrBook));
  private Novell.AddressBook.Manager manager = null;
  private Novell.AddressBook.AddressBook selectedBook;
  private System.Windows.Forms.MainMenu mainMenu1;
  private System.Windows.Forms.MenuItem menuFile;
  private System.Windows.Forms.MenuItem menuFileNew;
  private System.Windows.Forms.MenuItem menuFileExit;
  private System.Windows.Forms.MenuItem menuEdit;
  private System.Windows.Forms.MenuItem menuEditContact;
  private System.Windows.Forms.MenuItem menuFileNewAddressBook;
  private System.Windows.Forms.MenuItem menuFileNewContact;
  private System.Windows.Forms.MenuItem menuEditDelete;
  private System.Windows.Forms.MenuItem menuFileDivider;
  private System.Windows.Forms.MenuItem menuEditDivider;
  private System.Windows.Forms.MenuItem menuEditCut;
  private System.Windows.Forms.MenuItem menuEditCopy;
  private System.Windows.Forms.MenuItem menuEditPaste;
  private System.Windows.Forms.MenuItem menuTools;
  private System.Windows.Forms.MenuItem menuToolsImportVCard;
  private System.Windows.Forms.MenuItem menuToolsExportVCard;
  private System.Windows.Forms.MenuItem menuHelp;
  private System.Windows.Forms.MenuItem menuHelpContents;
  private System.Windows.Forms.MenuItem menuHelpAbout;
  private ArrayList selectedContacts;
  private Contact contact;
  private System.Windows.Forms.Panel detailsView;
  private System.Windows.Forms.Splitter splitter1;
  private Novell.iFolder.FormsBookLib.BooksContacts booksContacts;
  private System.Windows.Forms.Panel panel1;
  private System.Windows.Forms.TextBox search;
  private System.Windows.Forms.Label label1;
  private System.Windows.Forms.ToolBar toolBar1;
  private System.Windows.Forms.ToolBarButton toolBarButton1;
  private System.Windows.Forms.ToolBarButton toolBarButton2;
  private System.Windows.Forms.ToolBarButton toolBarButton3;
  private System.Windows.Forms.ToolBarButton toolBarButton4;
  private System.Windows.Forms.ToolBarButton toolBarButton5;
  private System.Windows.Forms.Timer editSearchTimer;
  private System.ComponentModel.IContainer components;





  public FormsAddrBook()
  {



   InitializeComponent();




   manager = Manager.Connect();
   this.booksContacts.CurrentManager = manager;


   try
   {
    this.Icon = new Icon(Path.Combine(Application.StartupPath, @"res\address_app.ico"));
   }
   catch (Exception e)
   {
    logger.Debug(e, "Loading icon");
   }

   this.booksContacts.ContactSelected += new Novell.iFolder.FormsBookLib.BooksContacts.ContactSelectedDelegate(booksContacts_ContactSelected);
   this.booksContacts.BookSelected += new Novell.iFolder.FormsBookLib.BooksContacts.BookSelectedDelegate(booksContacts_BookSelected);
   this.detailsView.Paint += new PaintEventHandler(detailsView_Paint);


   toolBar1.Buttons[1].Enabled = false;
   toolBar1.Buttons[2].Enabled = false;


   toolBar1.Buttons[3].Enabled = false;
   toolBar1.Buttons[4].Enabled = false;

   this.menuFileNew.Popup += new EventHandler(menuFileNew_Popup);
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
   this.components = new System.ComponentModel.Container();
   this.mainMenu1 = new System.Windows.Forms.MainMenu();
   this.menuFile = new System.Windows.Forms.MenuItem();
   this.menuFileNew = new System.Windows.Forms.MenuItem();
   this.menuFileNewAddressBook = new System.Windows.Forms.MenuItem();
   this.menuFileNewContact = new System.Windows.Forms.MenuItem();
   this.menuFileDivider = new System.Windows.Forms.MenuItem();
   this.menuFileExit = new System.Windows.Forms.MenuItem();
   this.menuEdit = new System.Windows.Forms.MenuItem();
   this.menuEditContact = new System.Windows.Forms.MenuItem();
   this.menuEditDivider = new System.Windows.Forms.MenuItem();
   this.menuEditCut = new System.Windows.Forms.MenuItem();
   this.menuEditCopy = new System.Windows.Forms.MenuItem();
   this.menuEditPaste = new System.Windows.Forms.MenuItem();
   this.menuEditDelete = new System.Windows.Forms.MenuItem();
   this.menuTools = new System.Windows.Forms.MenuItem();
   this.menuToolsImportVCard = new System.Windows.Forms.MenuItem();
   this.menuToolsExportVCard = new System.Windows.Forms.MenuItem();
   this.menuHelp = new System.Windows.Forms.MenuItem();
   this.menuHelpContents = new System.Windows.Forms.MenuItem();
   this.menuHelpAbout = new System.Windows.Forms.MenuItem();
   this.detailsView = new System.Windows.Forms.Panel();
   this.splitter1 = new System.Windows.Forms.Splitter();
   this.booksContacts = new Novell.iFolder.FormsBookLib.BooksContacts();
   this.panel1 = new System.Windows.Forms.Panel();
   this.search = new System.Windows.Forms.TextBox();
   this.label1 = new System.Windows.Forms.Label();
   this.toolBar1 = new System.Windows.Forms.ToolBar();
   this.toolBarButton1 = new System.Windows.Forms.ToolBarButton();
   this.toolBarButton2 = new System.Windows.Forms.ToolBarButton();
   this.toolBarButton3 = new System.Windows.Forms.ToolBarButton();
   this.toolBarButton4 = new System.Windows.Forms.ToolBarButton();
   this.toolBarButton5 = new System.Windows.Forms.ToolBarButton();
   this.editSearchTimer = new System.Windows.Forms.Timer(this.components);
   this.panel1.SuspendLayout();
   this.SuspendLayout();



   this.mainMenu1.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
                       this.menuFile,
                       this.menuEdit,
                       this.menuTools,
                       this.menuHelp});



   this.menuFile.Index = 0;
   this.menuFile.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
                      this.menuFileNew,
                      this.menuFileDivider,
                      this.menuFileExit});
   this.menuFile.Text = "&File";



   this.menuFileNew.Index = 0;
   this.menuFileNew.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
                      this.menuFileNewAddressBook,
                      this.menuFileNewContact});
   this.menuFileNew.Text = "&New";



   this.menuFileNewAddressBook.Index = 0;
   this.menuFileNewAddressBook.Text = "Address &Book";
   this.menuFileNewAddressBook.Click += new System.EventHandler(this.menuFileNewAddressBook_Click);



   this.menuFileNewContact.Index = 1;
   this.menuFileNewContact.Text = "&Contact";
   this.menuFileNewContact.Click += new System.EventHandler(this.menuFileNewContact_Click);



   this.menuFileDivider.Index = 1;
   this.menuFileDivider.Text = "-";



   this.menuFileExit.Index = 2;
   this.menuFileExit.Text = "E&xit";
   this.menuFileExit.Click += new System.EventHandler(this.menuFileExit_Click);



   this.menuEdit.Index = 1;
   this.menuEdit.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
                      this.menuEditContact,
                      this.menuEditDivider,
                      this.menuEditCut,
                      this.menuEditCopy,
                      this.menuEditPaste,
                      this.menuEditDelete});
   this.menuEdit.Text = "&Edit";
   this.menuEdit.Popup += new System.EventHandler(this.menuEdit_Popup);



   this.menuEditContact.Index = 0;
   this.menuEditContact.Text = "&Contact";
   this.menuEditContact.Click += new System.EventHandler(this.menuEditContact_Click);



   this.menuEditDivider.Index = 1;
   this.menuEditDivider.Text = "-";



   this.menuEditCut.Index = 2;
   this.menuEditCut.Text = "Cu&t";



   this.menuEditCopy.Index = 3;
   this.menuEditCopy.Text = "&Copy";



   this.menuEditPaste.Index = 4;
   this.menuEditPaste.Text = "&Paste";



   this.menuEditDelete.Index = 5;
   this.menuEditDelete.Text = "&Delete";
   this.menuEditDelete.Click += new System.EventHandler(this.menuEditDelete_Click);



   this.menuTools.Index = 2;
   this.menuTools.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
                       this.menuToolsImportVCard,
                       this.menuToolsExportVCard});
   this.menuTools.Text = "&Tools";
   this.menuTools.Popup += new System.EventHandler(this.menuTools_Popup);



   this.menuToolsImportVCard.Index = 0;
   this.menuToolsImportVCard.Text = "&Import vCard";
   this.menuToolsImportVCard.Click += new System.EventHandler(this.menuToolsImportVCard_Click);



   this.menuToolsExportVCard.Index = 1;
   this.menuToolsExportVCard.Text = "E&xport vCard";
   this.menuToolsExportVCard.Click += new System.EventHandler(this.menuToolsExportVCard_Click);



   this.menuHelp.Index = 3;
   this.menuHelp.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
                      this.menuHelpContents,
                      this.menuHelpAbout});
   this.menuHelp.Text = "&Help";



   this.menuHelpContents.Index = 0;
   this.menuHelpContents.Text = "&Contents";
   this.menuHelpContents.Click += new System.EventHandler(this.menuHelpContents_Click);



   this.menuHelpAbout.Index = 1;
   this.menuHelpAbout.Text = "&About";



   this.detailsView.BackColor = System.Drawing.SystemColors.ActiveCaptionText;
   this.detailsView.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
   this.detailsView.Dock = System.Windows.Forms.DockStyle.Fill;
   this.detailsView.Location = new System.Drawing.Point(307, 0);
   this.detailsView.Name = "detailsView";
   this.detailsView.Size = new System.Drawing.Size(341, 384);
   this.detailsView.TabIndex = 1;



   this.splitter1.Location = new System.Drawing.Point(304, 0);
   this.splitter1.Name = "splitter1";
   this.splitter1.Size = new System.Drawing.Size(3, 384);
   this.splitter1.TabIndex = 3;
   this.splitter1.TabStop = false;



   this.booksContacts.Dock = System.Windows.Forms.DockStyle.Left;
   this.booksContacts.Filter = null;
   this.booksContacts.LoadPath = null;
   this.booksContacts.Location = new System.Drawing.Point(0, 0);
   this.booksContacts.Name = "booksContacts";
   this.booksContacts.Size = new System.Drawing.Size(304, 384);
   this.booksContacts.TabIndex = 2;
   this.booksContacts.ContactDoubleClicked += new Novell.iFolder.FormsBookLib.BooksContacts.ContactDoubleClickedDelegate(this.booksContacts_ContactDoubleClicked);



   this.panel1.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
    | System.Windows.Forms.AnchorStyles.Left)
    | System.Windows.Forms.AnchorStyles.Right)));
   this.panel1.Controls.Add(this.detailsView);
   this.panel1.Controls.Add(this.splitter1);
   this.panel1.Controls.Add(this.booksContacts);
   this.panel1.Location = new System.Drawing.Point(8, 88);
   this.panel1.Name = "panel1";
   this.panel1.Size = new System.Drawing.Size(648, 384);
   this.panel1.TabIndex = 1;



   this.search.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
    | System.Windows.Forms.AnchorStyles.Right)));
   this.search.Location = new System.Drawing.Point(56, 56);
   this.search.Name = "search";
   this.search.Size = new System.Drawing.Size(600, 20);
   this.search.TabIndex = 3;
   this.search.Text = "";
   this.search.TextChanged += new System.EventHandler(this.search_TextChanged);



   this.label1.Location = new System.Drawing.Point(8, 56);
   this.label1.Name = "label1";
   this.label1.Size = new System.Drawing.Size(100, 16);
   this.label1.TabIndex = 4;
   this.label1.Text = "Search:";



   this.toolBar1.Buttons.AddRange(new System.Windows.Forms.ToolBarButton[] {
                      this.toolBarButton1,
                      this.toolBarButton2,
                      this.toolBarButton3,
                      this.toolBarButton4,
                      this.toolBarButton5});
   this.toolBar1.DropDownArrows = true;
   this.toolBar1.Location = new System.Drawing.Point(0, 0);
   this.toolBar1.Name = "toolBar1";
   this.toolBar1.ShowToolTips = true;
   this.toolBar1.Size = new System.Drawing.Size(664, 42);
   this.toolBar1.TabIndex = 5;
   this.toolBar1.ButtonClick += new System.Windows.Forms.ToolBarButtonClickEventHandler(this.toolBar1_ButtonClick);



   this.toolBarButton1.Text = "New Book";
   this.toolBarButton1.ToolTipText = "Create Address Book";



   this.toolBarButton2.Text = "New Group";
   this.toolBarButton2.ToolTipText = "Create Group";



   this.toolBarButton3.Text = "New Contact";
   this.toolBarButton3.ToolTipText = "Create Contact";



   this.toolBarButton4.Text = "Import vCard";
   this.toolBarButton4.ToolTipText = "Import contact from vCard";



   this.toolBarButton5.Text = "Export vCard";
   this.toolBarButton5.ToolTipText = "Export contact to vCard";



   this.editSearchTimer.Interval = 500;
   this.editSearchTimer.Tick += new System.EventHandler(this.editSearchTimer_Tick);



   this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
   this.ClientSize = new System.Drawing.Size(664, 481);
   this.Controls.Add(this.toolBar1);
   this.Controls.Add(this.search);
   this.Controls.Add(this.label1);
   this.Controls.Add(this.panel1);
   this.Menu = this.mainMenu1;
   this.MinimumSize = new System.Drawing.Size(544, 480);
   this.Name = "FormsAddrBook";
   this.Text = "iFolder Address Book";
   this.Load += new System.EventHandler(this.FormsAddrBook_Load);
   this.panel1.ResumeLayout(false);
   this.ResumeLayout(false);

  }


  [STAThread]
  static void Main()
  {
   Application.Run(new FormsAddrBook());
  }


  private void importVCard()
  {
   OpenFileDialog openFileDialog = new OpenFileDialog();

   openFileDialog.Multiselect = true;
   openFileDialog.Filter = "vcf files (*.vcf)|*.vcf";
   openFileDialog.RestoreDirectory = true ;

   if(openFileDialog.ShowDialog() == DialogResult.OK)
   {
    try
    {
     Cursor.Current = Cursors.WaitCursor;
     booksContacts.ContactsBeginUpdate();
     foreach (string file in openFileDialog.FileNames)
     {
      Contact contact = selectedBook.ImportVCard(file);
      booksContacts.AddContactToListView(contact, 1, true);
     }
     booksContacts.ContactsEndUpdate();
     Cursor.Current = Cursors.Default;
    }
    catch (SimiasException e)
    {
     e.LogError();
     MessageBox.Show("An error occurred while importing the vCard.  Please see the log file for additional information", "vCard Import Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
    }
    catch (Exception e)
    {
     logger.Debug(e, "Importing vCard");
     MessageBox.Show("An error occurred while importing the vCard.  Please see the log file for additional information", "vCard Import Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
    }
   }
  }

  private void exportVCard()
  {
   if (booksContacts.SelectedContacts.Count > 1)
   {
    FolderBrowserDialog folderBrowserDialog = new FolderBrowserDialog();
    folderBrowserDialog.Description = "Select the location to save the vCards in.";
    folderBrowserDialog.ShowNewFolderButton = true;

    if (folderBrowserDialog.ShowDialog() == DialogResult.OK)
    {
     Cursor.Current = Cursors.WaitCursor;
     foreach (ListViewItem lvi in booksContacts.SelectedContacts)
     {
      try
      {
       Contact c = (Contact)lvi.Tag;
       Name name = c.GetPreferredName();
       string fileName = Path.Combine(folderBrowserDialog.SelectedPath, name.Given + name.Family + ".vcf");
       DialogResult dialogResult = DialogResult.Yes;
       if (File.Exists(fileName))
       {
        dialogResult = MessageBox.Show(fileName + " already exists.\nDo you want to replace it?", "Export vCard for " + c.FN, MessageBoxButtons.YesNoCancel, MessageBoxIcon.Exclamation);
        if (dialogResult == DialogResult.Cancel)
         break;
       }

       if (dialogResult == DialogResult.Yes)
        c.ExportVCard(fileName);
      }
      catch (SimiasException e)
      {
       e.LogError();
       MessageBox.Show("An error occurred while exporting the vCard.  Please see the log file for additional information", "vCard Export Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
      }
      catch (Exception e)
      {
       logger.Debug(e, "Exporting vCard");
       MessageBox.Show("An error occurred while exporting the vCard.  Please see the log file for additional information", "vCard Export Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
      }
     }

     Cursor.Current = Cursors.Default;
    }
   }
   else
   {
    SaveFileDialog saveFileDialog = new SaveFileDialog();

    saveFileDialog.Filter = "vcf files (*.vcf)|*.vcf";
    saveFileDialog.RestoreDirectory = true;

    try
    {
     saveFileDialog.Title = "Export vCard for " + contact.FN;
     Name name = contact.GetPreferredName();
     saveFileDialog.FileName = name.Given + name.Family;

     if (saveFileDialog.ShowDialog() == DialogResult.OK)
     {
      Cursor.Current = Cursors.WaitCursor;
      contact.ExportVCard(saveFileDialog.FileName);
      Cursor.Current = Cursors.Default;
     }
    }
    catch (SimiasException e)
    {
     e.LogError();
     MessageBox.Show("An error occurred while exporting the vCard.  Please see the log file for additional information", "vCard Export Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
    }
    catch (Exception e)
    {
     logger.Debug(e, "Exporting vCard");
     MessageBox.Show("An error occurred while exporting the vCard.  Please see the log file for additional information", "vCard Export Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
    }
   }
  }



  private void menuFileNew_Popup(object sender, EventArgs e)
  {
   menuFileNewContact.Enabled = this.booksContacts.SelectedAddressBook != null;
  }

  private void menuFileNewAddressBook_Click(object sender, EventArgs e)
  {
   this.booksContacts.CreateAddressBook();
  }

  private void menuFileNewContact_Click(object sender, EventArgs e)
  {
   this.booksContacts.CreateContact();
  }

  private void menuEdit_Popup(object sender, EventArgs e)
  {
   selectedContacts = this.booksContacts.SelectedContacts;
   this.menuEditContact.Enabled = selectedContacts.Count == 1;

   if (selectedContacts.Count > 0)
   {
    this.menuEditDelete.Enabled = true;
   }
   else
   {
    this.menuEditDelete.Enabled = selectedBook != null;
   }
  }

  private void menuEditContact_Click(object sender, EventArgs e)
  {
   this.booksContacts.EditContact();
  }

  private void menuEditDelete_Click(object sender, EventArgs e)
  {
   if (selectedContacts.Count > 0)
   {
    this.booksContacts.DeleteContact();
   }
   else if (selectedBook != null)
   {
    this.booksContacts.DeleteAddressBook();
   }
  }

  private void menuFileExit_Click(object sender, EventArgs e)
  {
            this.Close();
  }

  private void menuHelpContents_Click(object sender, System.EventArgs e)
  {

   string helpPath = Path.Combine(Application.StartupPath, @"help\en\doc\u\data\front.html");

   try
   {
    Process.Start(helpPath);
   }
   catch (Exception ex)
   {
    logger.Debug(ex, "Loading help file");
    MessageBox.Show("Unable to open help file: \n" + helpPath, "Help File Not Found");
   }
  }

  private void booksContacts_ContactSelected(object sender, ContactSelectedEventArgs e)
  {
   if (e.SingleSelected)
   {
    contact = (Contact)((ListViewItem)booksContacts.SelectedContacts[0]).Tag;


    toolBar1.Buttons[4].Enabled = true;
   }
   else if (e.ItemSelected)
   {
    contact = null;


    toolBar1.Buttons[4].Enabled = true;
   }
   else
   {
    contact = null;


    toolBar1.Buttons[4].Enabled = false;
   }


   detailsView.Invalidate();
  }

  private void booksContacts_BookSelected(object sender, EventArgs e)
  {
   selectedBook = booksContacts.SelectedAddressBook;


   toolBar1.Buttons[3].Enabled = selectedBook != null;


   toolBar1.Buttons[2].Enabled = selectedBook != null;
  }

  private void detailsView_Paint(object sender, PaintEventArgs e)
  {

   SolidBrush whiteBrush = new SolidBrush(Color.White);
   e.Graphics.FillRectangle(whiteBrush, e.ClipRectangle);

   if (contact != null)
   {

    Image image;
    Rectangle rect = new Rectangle(8, 8, 60, 75);

    float x = 75;
    float y = 8;
    int delta = 10;
    try
    {
     image = Image.FromStream(contact.ExportPhoto());
     e.Graphics.DrawImage(image, rect);
    }
    catch (Exception ex)
    {
     logger.Debug(ex, "Loading photo");

     try
     {


      string basePath = Path.Combine(Application.StartupPath, "res");

      image = Image.FromFile(Path.Combine(basePath, "blankhead.png"));
      e.Graphics.DrawImage(image, rect);
     }
     catch (Exception ex2)
     {
      logger.Debug(ex2, "Loading default image");
     }
    }


    Font boldFont = new Font(Font.FontFamily, 14, FontStyle.Bold);
    e.Graphics.DrawString(contact.FN, boldFont, SystemBrushes.WindowText, x, y);
    y += boldFont.Size + delta;


    e.Graphics.DrawString(contact.Title, Font, SystemBrushes.WindowText, x, y);

    y = 90;
    try
    {

     StringFormat format = new StringFormat(StringFormatFlags.DirectionRightToLeft);
     foreach(Telephone phone in contact.GetTelephoneNumbers())
     {
      if ((phone.Types & (PhoneTypes.work | PhoneTypes.voice)) == (PhoneTypes.work | PhoneTypes.voice))
      {
       e.Graphics.DrawString("work", Font, SystemBrushes.ControlDark, x-1, y, format);
       e.Graphics.DrawString(phone.Number, Font, SystemBrushes.WindowText, x, y);
       y += Font.Size + 5;
      }
     }

     foreach(Telephone phone in contact.GetTelephoneNumbers())
     {
      if ((phone.Types & (PhoneTypes.work | PhoneTypes.fax)) == (PhoneTypes.work | PhoneTypes.fax))
      {
       e.Graphics.DrawString("fax", Font, SystemBrushes.ControlDark, x-1, y, format);
       e.Graphics.DrawString(phone.Number, Font, SystemBrushes.WindowText, x, y);
       y += Font.Size + 5;
      }
     }

     foreach(Telephone phone in contact.GetTelephoneNumbers())
     {
      if ((phone.Types & PhoneTypes.cell) == PhoneTypes.cell)
      {
       e.Graphics.DrawString("mobile", Font, SystemBrushes.ControlDark, x-1, y, format);
       e.Graphics.DrawString(phone.Number, Font, SystemBrushes.WindowText, x, y);
       y += Font.Size + 5;
      }
     }

     foreach(Telephone phone in contact.GetTelephoneNumbers())
     {
      if ((phone.Types & PhoneTypes.pager) == PhoneTypes.pager)
      {
       e.Graphics.DrawString("pager", Font, SystemBrushes.ControlDark, x-1, y, format);
       e.Graphics.DrawString(phone.Number, Font, SystemBrushes.WindowText, x, y);
       y += Font.Size + 5;
      }
     }

     foreach(Telephone phone in contact.GetTelephoneNumbers())
     {
      if ((phone.Types & (PhoneTypes.home | PhoneTypes.voice)) == (PhoneTypes.home | PhoneTypes.voice))
      {
       e.Graphics.DrawString("home", Font, SystemBrushes.ControlDark, x-1, y, format);
       e.Graphics.DrawString(phone.Number, Font, SystemBrushes.WindowText, x, y);
       y += Font.Size + 5;
      }
     }


     y += delta;
     foreach(Email email in contact.GetEmailAddresses())
     {
      if ((email.Types & EmailTypes.work) == EmailTypes.work)
      {
       e.Graphics.DrawString("work", Font, SystemBrushes.ControlDark, x-1, y, format);
       e.Graphics.DrawString(email.Address, Font, SystemBrushes.WindowText, x, y);
       y += Font.Size + 5;
      }
     }

     foreach(Email email in contact.GetEmailAddresses())
     {
      if ((email.Types & EmailTypes.personal) == EmailTypes.personal)
      {
       e.Graphics.DrawString("home", Font, SystemBrushes.ControlDark, x-1, y, format);
       e.Graphics.DrawString(email.Address, Font, SystemBrushes.WindowText, x, y);
       y += Font.Size + 5;
      }
     }

     foreach(Email email in contact.GetEmailAddresses())
     {
      if ((email.Types & EmailTypes.other) == EmailTypes.other)
      {
       e.Graphics.DrawString("other", Font, SystemBrushes.ControlDark, x-1, y, format);
       e.Graphics.DrawString(email.Address, Font, SystemBrushes.WindowText, x, y);
       y += Font.Size + 5;
      }
     }


     y += delta;
     foreach(IM im in contact.GetInstantMessageAccounts())
     {
      if ((im.Types & IMTypes.work) == IMTypes.work)
      {
       e.Graphics.DrawString("work", Font, SystemBrushes.ControlDark, x-1, y, format);
       e.Graphics.DrawString(im.Address + " (" + im.Provider + ")", Font, SystemBrushes.WindowText, x, y);
       y += Font.Size + 5;
      }
     }

     foreach(IM im in contact.GetInstantMessageAccounts())
     {
      if ((im.Types & IMTypes.home) == IMTypes.home)
      {
       e.Graphics.DrawString("home", Font, SystemBrushes.ControlDark, x-1, y, format);
       e.Graphics.DrawString(im.Address + " (" + im.Provider + ")", Font, SystemBrushes.WindowText, x, y);
       y += Font.Size + 5;
      }
     }

     foreach(IM im in contact.GetInstantMessageAccounts())
     {
      if ((im.Types & IMTypes.other) == IMTypes.other)
      {
       e.Graphics.DrawString("other", Font, SystemBrushes.ControlDark, x-1, y, format);
       e.Graphics.DrawString(im.Address + " (" + im.Provider + ")", Font, SystemBrushes.WindowText, x, y);
       y += Font.Size + 5;
      }
     }


     y += delta;
     foreach(Address addr in contact.GetAddresses())
     {
      if((addr.Types & AddressTypes.work) == AddressTypes.work)
      {
       e.Graphics.DrawString("work", Font, SystemBrushes.ControlDark, x-1, y, format);
       e.Graphics.DrawString(addr.Street, Font, SystemBrushes.WindowText, x, y);
       y += Font.Size + 5;
       if (addr.ExtendedAddress != "")
       {
        e.Graphics.DrawString(addr.ExtendedAddress, Font, SystemBrushes.WindowText, x, y);
        y += Font.Size + 5;
       }

       if (addr.PostalBox != "")
       {
        e.Graphics.DrawString("P.O. Box " + addr.PostalBox, Font, SystemBrushes.WindowText, x, y);
        y += Font.Size + 5;
       }

       if (addr.Locality != "" ||
        addr.Region != "" ||
        addr.PostalCode != "")
       {
        string cityState = addr.Locality + ", " + addr.Region + " " + addr.PostalCode;
        e.Graphics.DrawString(cityState.Trim(), Font, SystemBrushes.WindowText, x, y);
        y += Font.Size + 5;
       }

       if (addr.Country != "")
       {
        e.Graphics.DrawString(addr.Country, Font, SystemBrushes.WindowText, x, y);
        y += Font.Size + 5;
       }

       y += delta;
      }
     }

     foreach(Address addr in contact.GetAddresses())
     {
      if((addr.Types & AddressTypes.home) == AddressTypes.home)
      {
       e.Graphics.DrawString("home", Font, SystemBrushes.ControlDark, x-1, y, format);
       e.Graphics.DrawString(addr.Street, Font, SystemBrushes.WindowText, x, y);
       y += Font.Size + 5;
       if (addr.ExtendedAddress != "")
       {
        e.Graphics.DrawString(addr.ExtendedAddress, Font, SystemBrushes.WindowText, x, y);
        y += Font.Size + 5;
       }

       if (addr.PostalBox != "")
       {
        e.Graphics.DrawString("P.O. Box " + addr.PostalBox, Font, SystemBrushes.WindowText, x, y);
        y += Font.Size + 5;
       }

       if (addr.Locality != "" ||
        addr.Region != "" ||
        addr.PostalCode != "")
       {
        string cityState = addr.Locality + ", " + addr.Region + " " + addr.PostalCode;
        e.Graphics.DrawString(cityState.Trim(), Font, SystemBrushes.WindowText, x, y);
        y += Font.Size + 5;
       }

       if (addr.Country != "")
       {
        e.Graphics.DrawString(addr.Country, Font, SystemBrushes.WindowText, x, y);
        y += Font.Size + 5;
       }

       y += delta;
      }
     }
    }
    catch (SimiasException ex)
    {
     ex.LogError();
     MessageBox.Show("An error occurred while displaying contact data.  Please see the log file for additional information", "Display Contact Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
    }
    catch (Exception ex)
    {
     logger.Debug(ex, "Displaying contact");
     MessageBox.Show("An error occurred while displaying contact data.  Please see the log file for additional information", "Display Contact Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
    }
   }
   else
   {

    e.Graphics.Clear(Color.White);
   }
  }

  private void editSearchTimer_Tick(object sender, System.EventArgs e)
  {


   editSearchTimer.Stop();


   booksContacts.FilterList(search.Text);
  }

  private void search_TextChanged(object sender, System.EventArgs e)
  {

   this.editSearchTimer.Stop();
   this.editSearchTimer.Start();
  }

  private void toolBar1_ButtonClick(object sender, System.Windows.Forms.ToolBarButtonClickEventArgs e)
  {
   switch (e.Button.Text)
   {
    case "New Book":
     booksContacts.CreateAddressBook();
     break;
    case "New Group":
     break;
    case "New Contact":
     booksContacts.CreateContact();
     break;
    case "Import vCard":
     importVCard();
     break;
    case "Export vCard":
     exportVCard();
     break;
    default:
     break;
   }
  }

  private void FormsAddrBook_Load(object sender, System.EventArgs e)
  {
   toolBar1.ImageList = this.booksContacts.ContactImageList;
   toolBar1.Buttons[0].ImageIndex = 2;
   toolBar1.Buttons[1].ImageIndex = 1;
   toolBar1.Buttons[2].ImageIndex = 1;
   toolBar1.Buttons[3].ImageIndex = 3;
   toolBar1.Buttons[4].ImageIndex = 4;
  }

  private void menuTools_Popup(object sender, System.EventArgs e)
  {
   selectedContacts = booksContacts.SelectedContacts;
   menuToolsExportVCard.Enabled = selectedContacts.Count > 0;

   menuToolsImportVCard.Enabled = selectedBook != null;
  }

  private void menuToolsImportVCard_Click(object sender, System.EventArgs e)
  {
   importVCard();
  }

  private void menuToolsExportVCard_Click(object sender, System.EventArgs e)
  {
   exportVCard();
  }

  private void booksContacts_ContactDoubleClicked(object sender, Novell.iFolder.FormsBookLib.ContactDoubleClickedEventArgs e)
  {
   booksContacts.EditContact();
  }

 }
}
