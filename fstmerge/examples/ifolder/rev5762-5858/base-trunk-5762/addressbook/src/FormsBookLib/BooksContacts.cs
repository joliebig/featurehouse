

using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Windows.Forms;
using System.IO;
using Novell.AddressBook;
using Simias;
using Simias.Storage;

namespace Novell.iFolder.FormsBookLib
{



 public class BooksContacts : System.Windows.Forms.UserControl
 {

  private static readonly ISimiasLog logger = SimiasLogManager.GetLogger(typeof(BooksContacts));
  private System.Windows.Forms.Panel panel1;
  private System.Windows.Forms.ListView books;
  private System.Windows.Forms.Splitter splitter1;
  private System.Windows.Forms.ListView contacts;
  private System.Windows.Forms.ContextMenu contactsContextMenu;
  private System.Windows.Forms.MenuItem editContactMenu;
  private System.Windows.Forms.MenuItem createContactMenu;
  private System.Windows.Forms.MenuItem deleteContactMenu;
  private System.Windows.Forms.ContextMenu booksContextMenu;
  private System.Windows.Forms.MenuItem createBookMenu;
  private System.Windows.Forms.MenuItem deleteBookMenu;

  private Collection collection = null;
  private Novell.AddressBook.Manager manager = null;
  private Novell.AddressBook.AddressBook addressBook = null;
  private ArrayList selectedContacts;
  private ArrayList validSelectedContacts;
  private System.ComponentModel.IContainer components;
  private string loadPath;
  private System.Windows.Forms.HelpProvider helpProvider1;
  private string filter;





  public BooksContacts()
  {

   InitializeComponent();




   editContactMenu = new MenuItem("Edit...");
   editContactMenu.Click += new EventHandler(editContact_Click);

   createContactMenu = new MenuItem("&Create...");
   createContactMenu.Click += new EventHandler(createContact_Click);

   deleteContactMenu = new MenuItem("Delete");
   deleteContactMenu.Click += new EventHandler(deleteContact_Click);

   contactsContextMenu = new ContextMenu();
   contactsContextMenu.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {editContactMenu, createContactMenu, deleteContactMenu});
   contacts.ContextMenu = contactsContextMenu;
   contacts.ContextMenu.Popup += new EventHandler(contactsContextMenu_Popup);


   createBookMenu = new MenuItem("Create...");
   createBookMenu.Click += new EventHandler(createBook_Click);

   deleteBookMenu = new MenuItem("Delete");
   deleteBookMenu.Click += new EventHandler(deleteBook_Click);

   booksContextMenu = new ContextMenu();
   booksContextMenu.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {createBookMenu, deleteBookMenu});
   books.ContextMenu = booksContextMenu;
   books.ContextMenu.Popup += new EventHandler(booksContextMenu_Popup);
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
   this.panel1 = new System.Windows.Forms.Panel();
   this.contacts = new System.Windows.Forms.ListView();
   this.splitter1 = new System.Windows.Forms.Splitter();
   this.books = new System.Windows.Forms.ListView();
   this.helpProvider1 = new System.Windows.Forms.HelpProvider();
   this.panel1.SuspendLayout();
   this.SuspendLayout();



   this.panel1.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
    | System.Windows.Forms.AnchorStyles.Left)
    | System.Windows.Forms.AnchorStyles.Right)));
   this.panel1.Controls.Add(this.contacts);
   this.panel1.Controls.Add(this.splitter1);
   this.panel1.Controls.Add(this.books);
   this.panel1.Location = new System.Drawing.Point(0, 0);
   this.panel1.Name = "panel1";
   this.panel1.Size = new System.Drawing.Size(368, 304);
   this.panel1.TabIndex = 2;



   this.contacts.Dock = System.Windows.Forms.DockStyle.Fill;
   this.helpProvider1.SetHelpString(this.contacts, "Displays the contacts found in the selected address book.");
   this.contacts.HideSelection = false;
   this.contacts.Location = new System.Drawing.Point(123, 0);
   this.contacts.Name = "contacts";
   this.helpProvider1.SetShowHelp(this.contacts, true);
   this.contacts.Size = new System.Drawing.Size(245, 304);
   this.contacts.TabIndex = 1;
   this.contacts.KeyDown += new System.Windows.Forms.KeyEventHandler(this.contacts_KeyDown);
   this.contacts.DoubleClick += new System.EventHandler(this.contacts_DoubleClick);
   this.contacts.SelectedIndexChanged += new System.EventHandler(this.contacts_SelectedIndexChanged);



   this.splitter1.Location = new System.Drawing.Point(120, 0);
   this.splitter1.Name = "splitter1";
   this.splitter1.Size = new System.Drawing.Size(3, 304);
   this.splitter1.TabIndex = 1;
   this.splitter1.TabStop = false;
   this.splitter1.SplitterMoved += new System.Windows.Forms.SplitterEventHandler(this.splitter1_SplitterMoved);



   this.books.Dock = System.Windows.Forms.DockStyle.Left;
   this.helpProvider1.SetHelpString(this.books, "Displays the available address books.");
   this.books.HideSelection = false;
   this.books.Location = new System.Drawing.Point(0, 0);
   this.books.Name = "books";
   this.helpProvider1.SetShowHelp(this.books, true);
   this.books.Size = new System.Drawing.Size(120, 304);
   this.books.TabIndex = 0;
   this.books.KeyDown += new System.Windows.Forms.KeyEventHandler(this.books_KeyDown);
   this.books.SelectedIndexChanged += new System.EventHandler(this.books_SelectedIndexChanged);



   this.Controls.Add(this.panel1);
   this.Name = "BooksContacts";
   this.Size = new System.Drawing.Size(368, 304);
   this.Load += new System.EventHandler(this.BooksContacts_Load);
   this.panel1.ResumeLayout(false);
   this.ResumeLayout(false);

  }



  public Collection Collection
  {
   set
   {
    this.collection = value;
   }
  }




  public Novell.AddressBook.Manager CurrentManager
  {
   set
   {
    this.manager = value;
   }
  }




  public Novell.AddressBook.AddressBook SelectedAddressBook
  {
   get
   {
    Novell.AddressBook.AddressBook book = null;
    foreach(ListViewItem lvitem in this.books.SelectedItems)
    {


     book = (Novell.AddressBook.AddressBook)lvitem.Tag;
    }

    return book;
   }
  }




  public ArrayList SelectedContacts
  {
   get
   {
    selectedContacts = new ArrayList();
    foreach (ListViewItem item in contacts.SelectedItems)
    {
     selectedContacts.Add(item);
    }

    return selectedContacts;
   }
  }




  public ArrayList ValidSelectedContacts
  {
   get
   {
    validSelectedContacts = new ArrayList();
    foreach (ListViewItem item in contacts.SelectedItems)
    {
     if (item.ForeColor != Color.Gray)
     {
      validSelectedContacts.Add(item);
     }
    }

    return validSelectedContacts;
   }
  }




  public string LoadPath
  {
   get
   {
    return this.loadPath;
   }

   set
   {
    this.loadPath = value;
   }
  }




  public ImageList ContactImageList
  {
   get
   {
    return contacts.SmallImageList;
   }
  }




  public string Filter
  {
   get
   {
    return filter;
   }

   set
   {
    filter = value;
   }
  }
  public void AddContactToListView(Contact contact, int imageIndex, bool selected)
  {
   if (imageIndex == -1)
   {
    imageIndex = contact.IsCurrentUser ? 0 : 1;
   }
   ListViewItem item;
   if (contact.FN != null && contact.FN != String.Empty)
   {
    item = new ListViewItem(contact.FN, imageIndex);
   }
   else
   {
    item = new ListViewItem(contact.UserName, imageIndex);
   }
   item.Tag = contact;
   contacts.Items.Add(item);
   if (selected)
   {
    contacts.SelectedItems.Clear();
    item.Selected = true;
   }
  }
  public void ContactsBeginUpdate()
  {
   contacts.BeginUpdate();
  }
  public void ContactsEndUpdate()
  {
   contacts.EndUpdate();
  }
  public void CreateAddressBook()
  {
   createBook_Click(this, null);
  }
  public void CreateContact()
  {
   createContact_Click(this, null);
  }
  public void EditContact()
  {
   editContact_Click(this, null);
  }
  public void DeleteAddressBook()
  {
   deleteBook_Click(this, null);
  }
  public void DeleteContact()
  {
   deleteContact_Click(this, null);
  }
  public void FilterList(string filter)
  {
   if (filter != null)
    this.filter = filter;
   contacts.Items.Clear();
   Cursor.Current = Cursors.WaitCursor;
   contacts.BeginUpdate();
   if (Filter != null && Filter != String.Empty)
   {
    Hashtable results = new Hashtable();
    try
    {
     IABList searchResults = addressBook.SearchFirstName(Filter, SearchOp.Contains);
     foreach(Contact c in searchResults)
     {
      results.Add(c.ID, c);
     }
     searchResults = addressBook.SearchLastName(Filter, SearchOp.Contains);
     foreach(Contact c in searchResults)
     {
      if (!results.ContainsKey(c.ID))
       results.Add(c.ID, c);
     }
     searchResults = addressBook.SearchUsername(Filter, SearchOp.Contains);
     foreach(Contact c in searchResults)
     {
      if (!results.ContainsKey(c.ID))
       results.Add(c.ID, c);
     }
     searchResults = addressBook.SearchEmail(Filter, SearchOp.Contains);
     foreach(Contact c in searchResults)
     {
      if (!results.ContainsKey(c.ID))
       results.Add(c.ID, c);
     }
     IDictionaryEnumerator enumerator = results.GetEnumerator();
     while (enumerator.MoveNext())
     {
      Contact c = (Contact) enumerator.Value;
      AddContactToListView(c, -1, false);
     }
    }
    catch (SimiasException e)
    {
     e.LogError();
     MessageBox.Show("An error was encountered while searching.  Please see the log file for additional information.", "Search Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
    }
    catch (Exception e)
    {
     logger.Debug(e, "Searching - filter = {0}", filter);
     MessageBox.Show("An error was encountered while searching.  Please see the log file for additional information.", "Search Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
    }
   }
   else
   {
    foreach (Contact c in addressBook)
    {
     AddContactToListView(c, -1, false);
    }
   }
   contacts.EndUpdate();
   Cursor.Current = Cursors.Default;
   contacts_SelectedIndexChanged(this, null);
  }
  public delegate void ContactSelectedDelegate(object sender, ContactSelectedEventArgs e);
  public event ContactSelectedDelegate ContactSelected;
  public delegate void ContactDoubleClickedDelegate(object sender, ContactDoubleClickedEventArgs e);
  public event ContactDoubleClickedDelegate ContactDoubleClicked;
  public delegate void BookSelectedDelegate(object sender, EventArgs e);
  public event BookSelectedDelegate BookSelected;
  private void BooksContacts_Load(object sender, EventArgs e)
  {
   try
   {
    ImageList booksImageList = new ImageList();
    string basePath = Path.Combine(loadPath != null ? loadPath : Application.StartupPath, "res");
    booksImageList.Images.Add(new Icon(Path.Combine(basePath, "ifolder_add_bk.ico")));
    books.SmallImageList = booksImageList;
    ImageList contactsImageList = new ImageList();
    contactsImageList.Images.Add(new Icon(Path.Combine(basePath, "ifolder_me_card.ico")));
    contactsImageList.Images.Add(new Icon(Path.Combine(basePath, "ifolder_contact_card.ico")));
    contactsImageList.Images.Add(new Icon(Path.Combine(basePath, "ifolder_add_bk.ico")));
    contactsImageList.Images.Add(Image.FromFile(Path.Combine(basePath, "contactimport_16.ico")));
    contactsImageList.Images.Add(Image.FromFile(Path.Combine(basePath, "contactexport_16.ico")));
    contacts.SmallImageList = contactsImageList;
   }
   catch (Exception ex)
   {
    logger.Debug(ex, "Loading images");
   }
   this.books.MultiSelect = false;
   this.books.View = View.Details;
   this.books.Columns.Add("Books", this.books.Size.Width - 4, HorizontalAlignment.Left);
   this.contacts.View = View.Details;
   this.contacts.Columns.Add("Contacts", this.contacts.Size.Width - 4, HorizontalAlignment.Left);
   try
   {
    IEnumerator addrBooks= manager.GetAddressBooks().GetEnumerator();
    while (addrBooks.MoveNext())
    {
     AddressBook.AddressBook book = (AddressBook.AddressBook)addrBooks.Current;
     if ((collection != null) && !collection.Domain.Equals(Domain.WorkGroupDomainID) && !collection.Domain.Equals(book.Domain))
     {
      continue;
     }
     ListViewItem item;
     item = new ListViewItem(book.Name, 0);
     item.Tag = book;
     this.books.Items.Add(item);
    }
    if (books.Items.Count > 0)
    {
     books.Items[0].Selected = true;
    }
   }
   catch (SimiasException ex)
   {
    ex.LogFatal();
    MessageBox.Show("A fatal error occurred during initialization.  Please see the log file for additional information.", "Fatal Error", MessageBoxButtons.OK, MessageBoxIcon.Stop);
   }
   catch (Exception ex)
   {
    logger.Fatal(ex, "Initializing");
    MessageBox.Show("A fatal error occurred during initialization.  Please see the log file for additional information.", "Fatal Error", MessageBoxButtons.OK, MessageBoxIcon.Stop);
   }
  }
  private void books_SelectedIndexChanged(object sender, System.EventArgs e)
  {
   if (this.books.SelectedItems.Count > 0)
   {
    addressBook = (Novell.AddressBook.AddressBook)books.SelectedItems[0].Tag;
    FilterList(null);
   }
   EventArgs args = new EventArgs();
   if (BookSelected != null)
   {
    BookSelected(this, args);
   }
  }
  private void contacts_SelectedIndexChanged(object sender, System.EventArgs e)
  {
   bool validSelected = false;
   bool singleSelected = this.contacts.SelectedItems.Count == 1;
   bool itemSelected = this.contacts.SelectedItems.Count > 0;
   foreach (ListViewItem item in this.contacts.SelectedItems)
   {
    if (item.ForeColor != Color.Gray)
    {
     validSelected = true;
     break;
    }
   }
   ContactSelectedEventArgs args = new ContactSelectedEventArgs(validSelected, itemSelected, singleSelected);
   if (ContactSelected != null)
   {
    ContactSelected(this, args);
   }
  }
  private void contacts_DoubleClick(object sender, EventArgs e)
  {
   Contact contact = null;
   ListViewItem lvitem = null;
   foreach (ListViewItem item in this.contacts.SelectedItems)
   {
    contact = (Contact)item.Tag;
    lvitem = item;
   }
   if (contact != null)
   {
    ContactDoubleClickedEventArgs args = new ContactDoubleClickedEventArgs(this.addressBook, contact, lvitem);
    if (ContactDoubleClicked != null)
    {
     ContactDoubleClicked(this, args);
    }
   }
  }
  private void splitter1_SplitterMoved(object sender, SplitterEventArgs e)
  {
   this.contacts.Columns[0].Width = this.contacts.Size.Width - 4;
   this.books.Columns[0].Width = this.books.Size.Width - 4;
  }
  private void createContact_Click(object sender, System.EventArgs e)
  {
   ContactEditor editor = new ContactEditor();
   editor.LoadPath = LoadPath;
   editor.CurrentAddressBook = this.addressBook;
   DialogResult result = editor.ShowDialog();
   if (result == DialogResult.OK)
   {
    AddContactToListView(editor.CurrentContact, 1, true);
   }
  }
  private void createBook_Click(object sender, System.EventArgs e)
  {
   AddBook addBook = new AddBook();
   DialogResult result = addBook.ShowDialog();
   if (result == DialogResult.OK)
   {
    try
    {
     Novell.AddressBook.AddressBook addrBook = manager.CreateAddressBook(addBook.Name);
     ListViewItem item = new ListViewItem(addrBook.Name, 0);
     item.Tag = addrBook;
     item.Selected = true;
     this.books.Items.Add(item);
    }
    catch (SimiasException ex)
    {
     ex.LogError();
     MessageBox.Show("An error occurred while creating the address book.  Please see the log file for additional information.", "Create Address Book Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
    }
    catch (Exception ex)
    {
     logger.Debug(ex, "Creating address book");
     MessageBox.Show("An error occurred while creating the address book.  Please see the log file for additional information.", "Create Address Book Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
    }
   }
  }
  private void editContact_Click(object sender, EventArgs e)
  {
   ListViewItem selectedItem = null;
   foreach (ListViewItem lvitem in this.contacts.SelectedItems)
   {
    selectedItem = lvitem;
   }
   if (selectedItem != null)
   {
    ContactEditor editor = new ContactEditor();
    editor.LoadPath = LoadPath;
    editor.CurrentAddressBook = this.addressBook;
    editor.CurrentContact = (Contact)selectedItem.Tag;
    DialogResult result = editor.ShowDialog();
    if (result == DialogResult.OK)
    {
     selectedItem.Text = editor.CurrentContact.FN;
    }
   }
  }
  private void deleteContact_Click(object sender, EventArgs e)
  {
   foreach (ListViewItem lvitem in this.contacts.SelectedItems)
   {
    Contact contact = (Contact)lvitem.Tag;
    if (!contact.IsCurrentUser)
    {
     try
     {
      contact.Delete();
      lvitem.Remove();
     }
     catch (SimiasException ex)
     {
      ex.LogError();
      MessageBox.Show("An error occurred while deleting the contact.  Please see the log file for additional information.", "Delete Contact Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
     }
     catch (Exception ex)
     {
      logger.Debug(ex, "Deleting contact");
      MessageBox.Show("An error occurred while deleting the contact.  Please see the log file for additional information.", "Delete Contact Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
     }
    }
    else
    {
     MessageBox.Show("Deleting your own contact is not allowed.");
    }
   }
  }
  private void contactsContextMenu_Popup(object sender, EventArgs e)
  {
   if (this.contacts.SelectedItems.Count == 0)
   {
    this.createContactMenu.Enabled = books.SelectedItems.Count > 0;
    this.editContactMenu.Enabled = false;
    this.deleteContactMenu.Enabled = false;
   }
   else if (this.contacts.SelectedItems.Count == 1)
   {
    this.createContactMenu.Enabled = false;
    this.editContactMenu.Enabled = true;
    this.deleteContactMenu.Enabled = true;
   }
   else
   {
    this.createContactMenu.Enabled = false;
    this.editContactMenu.Enabled = false;
    this.deleteContactMenu.Enabled = true;
   }
  }
  private void deleteBook_Click(object sender, EventArgs e)
  {
   foreach (ListViewItem lvitem in this.books.SelectedItems)
   {
    Novell.AddressBook.AddressBook book = (Novell.AddressBook.AddressBook)lvitem.Tag;
    if (MessageBox.Show("Are you sure you want to delete this address book?", lvitem.Text.ToString(), MessageBoxButtons.YesNo, MessageBoxIcon.Question, MessageBoxDefaultButton.Button2) == DialogResult.Yes)
    {
     try
     {
      book.Commit(book.Delete());
      lvitem.Remove();
     }
     catch (SimiasException ex)
     {
      ex.LogError();
      MessageBox.Show("An error occurred while deleting the address book.  Please see the log file for additional information.", "Delete Address Book Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
     }
     catch (Exception ex)
     {
      logger.Debug(ex, "Deleting address book");
      MessageBox.Show("An error occurred while deleting the address book.  Please see the log file for additional information.", "Delete Address Book Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
     }
    }
   }
  }
  private void booksContextMenu_Popup(object sender, EventArgs e)
  {
   this.createBookMenu.Enabled = this.books.SelectedItems.Count == 0;
   this.deleteBookMenu.Enabled = this.books.SelectedItems.Count != 0;
  }
  private void contacts_KeyDown(object sender, System.Windows.Forms.KeyEventArgs e)
  {
   if (e.KeyCode == Keys.Delete)
   {
    deleteContact_Click(this, null);
   }
  }
  private void books_KeyDown(object sender, System.Windows.Forms.KeyEventArgs e)
  {
   if (e.KeyCode == Keys.Delete)
   {
    deleteBook_Click(this, null);
   }
  }
 }
 public class ContactSelectedEventArgs : EventArgs
 {
  private bool validSelected;
  private bool singleSelected;
  private bool itemSelected;
  public ContactSelectedEventArgs(bool validSelected, bool itemSelected, bool singleSelected)
  {
   this.validSelected = validSelected;
   this.itemSelected = itemSelected;
   this.singleSelected = singleSelected;
  }
  public bool ValidSelected
  {
   get
   {
    return validSelected;
   }
  }
  public bool SingleSelected
  {
   get
   {
    return singleSelected;
   }
  }
  public bool ItemSelected
  {
   get
   {
    return itemSelected;
   }
  }
 }
 public class ContactDoubleClickedEventArgs : EventArgs
 {
  private Contact contact;
  private Novell.AddressBook.AddressBook addressBook;
  private ListViewItem lvitem;
  public ContactDoubleClickedEventArgs(Novell.AddressBook.AddressBook addressBook, Contact contact, ListViewItem lvitem)
  {
   this.contact = contact;
   this.addressBook = addressBook;
   this.lvitem = lvitem;
  }
  public Contact Contact
  {
   get
   {
    return contact;
   }
  }
  public Novell.AddressBook.AddressBook AddressBook
  {
   get
   {
    return addressBook;
   }
  }
  public ListViewItem LVitem
  {
   get
   {
    return lvitem;
   }
  }
 }
}
