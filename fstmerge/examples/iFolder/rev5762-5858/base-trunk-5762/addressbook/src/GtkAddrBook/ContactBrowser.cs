using System;
using Novell.AddressBook;
using Simias.Storage;
using System.Collections;
using System.IO;
using Gtk;
using Gdk;
using Glade;
using GtkSharp;
using GLib;
namespace Novell.AddressBook.UI.gtk
{
 public class ListData
 {
  private readonly Pixbuf pb;
  private readonly string name;
  public ListData(Pixbuf pb, string name)
  {
   this.pb = pb;
   this.name = name;
  }
  public Pixbuf Icon
  {
   get { return pb;}
  }
  public string Name
  {
   get { return name;}
  }
 }
 public class ContactBrowser
 {
  [Glade.Widget] private Gnome.App CBApp = null;
  [Glade.Widget] private TreeView BookTreeView = null;
  [Glade.Widget] internal TreeView ContactTreeView;
  [Glade.Widget] private Gtk.Button CreateContactButton = null;
  [Glade.Widget] private Gtk.Button ImportButton = null;
  [Glade.Widget] private Gtk.Button ExportButton = null;
  [Glade.Widget] private Gtk.Entry SearchEntry;
  [Glade.Widget] internal Gtk.TextView PictureTextView;
  [Glade.Widget] internal Gtk.TextView LabelTextView;
  [Glade.Widget] internal Gtk.TextView ValueTextView;
  [Glade.Widget] internal Gtk.TextView TitleTextView;
  [Glade.Widget] private Gtk.MenuItem NewBookItem = null;
  [Glade.Widget] private Gtk.MenuItem ConvertBookItem = null;
  [Glade.Widget] private Gtk.MenuItem ShareBookItem = null;
  [Glade.Widget] private Gtk.MenuItem DeleteBookItem = null;
  [Glade.Widget] private Gtk.MenuItem BookPropsItem = null;
  [Glade.Widget] private Gtk.MenuItem NewContactItem = null;
  [Glade.Widget] private Gtk.MenuItem MakeMeItem = null;
  [Glade.Widget] private Gtk.MenuItem MakeNotMeItem = null;
  [Glade.Widget] private Gtk.MenuItem DeleteContactItem = null;
  [Glade.Widget] private Gtk.MenuItem NewGroupItem = null;
  [Glade.Widget] private Gtk.MenuItem DeleteGroupItem = null;
  [Glade.Widget] private Gtk.MenuItem ContactPropsItem = null;
  [Glade.Widget] private Gtk.MenuItem ImportVCardItem = null;
  [Glade.Widget] private Gtk.MenuItem ExportVCardItem = null;
  Manager abMan;
  AddressBook curAddrBook;
  ListStore BookTreeStore;
  ListStore ContactTreeStore;
  Pixbuf UserCardPixBuf;
  Pixbuf CurCardPixBuf;
  Pixbuf BookPixBuf;
  Pixbuf BlankHeadPixBuf;
  uint searchTimeoutID;
  public event EventHandler AddrBookClosed;
  public ContactBrowser (Manager abMan)
  {
   this.abMan = abMan;
   Init();
  }
  public ContactBrowser ()
  {
   Init();
  }
  public void Init ()
  {
   if(abMan == null)
   {
    try
    {
     abMan = Manager.Connect( );
    }
    catch(Exception e)
    {
     Console.WriteLine("Unable to connect to Addr Book: " + e);
    }
   }
   Glade.XML gxml = new Glade.XML (Util.GladePath("contact-browser.glade"),
     "CBApp", null);
   gxml.Autoconnect (this);
   BookTreeStore = new ListStore(typeof(AddressBook));
   BookTreeView.Model = BookTreeStore;
   CellRendererPixbuf bcrp = new CellRendererPixbuf();
   TreeViewColumn btvc = new TreeViewColumn();
   btvc.PackStart(bcrp, false);
   btvc.SetCellDataFunc(bcrp, new TreeCellDataFunc(
      BookCellPixbufDataFunc));
   CellRendererText bcrt = new CellRendererText();
   btvc.PackStart(bcrt, false);
   btvc.SetCellDataFunc(bcrt, new TreeCellDataFunc(
      BookCellTextDataFunc));
   btvc.Title = "Books";
   BookTreeView.AppendColumn(btvc);
   BookTreeView.Selection.Changed +=
    new EventHandler(on_book_selection_changed);
   ContactTreeStore = new ListStore(typeof(Contact));
   ContactTreeView.Model = ContactTreeStore;
   CellRendererPixbuf ccrp = new CellRendererPixbuf();
   TreeViewColumn ctvc = new TreeViewColumn();
   ctvc.PackStart(ccrp, false);
   ctvc.SetCellDataFunc(ccrp, new TreeCellDataFunc(
      ContactCellPixbufDataFunc));
   CellRendererText ccrt = new CellRendererText();
   ctvc.PackStart(ccrt, false);
   ctvc.SetCellDataFunc(ccrt, new TreeCellDataFunc(
      ContactCellTextDataFunc));
   ctvc.Title = "Contacts";
   ContactTreeView.AppendColumn(ctvc);
   ContactTreeView.Selection.Changed +=
    new EventHandler(on_contact_selection_changed);
   AddLabelTags();
   AddTitleTags();
   UserCardPixBuf = new Pixbuf(Util.ImagesPath("contact.png"));
   CurCardPixBuf = new Pixbuf(Util.ImagesPath("contact_me.png"));
   BookPixBuf = new Pixbuf(Util.ImagesPath("book.png"));
   BlankHeadPixBuf = new Pixbuf(Util.ImagesPath("blankhead.png"));
   RefreshBooks();
   searchTimeoutID = 0;
   EnableControls(false, false);
  }
  public void RefreshBooks()
  {
   BookTreeStore.Clear();
   if(abMan != null)
   {
    try
    {
     foreach(AddressBook ab in abMan)
     {
      BookTreeStore.AppendValues(ab);
     }
    }
    catch(Exception e)
    {
     Console.WriteLine(
       "Unable to connect to the Address Book: " + e);
    }
   }
  }
  private bool SearchCallback()
  {
   SearchAddrBook();
   return false;
  }
  private void BookCellTextDataFunc (Gtk.TreeViewColumn tree_column,
    Gtk.CellRenderer cell, Gtk.TreeModel tree_model,
    Gtk.TreeIter iter)
  {
   AddressBook book = (AddressBook) BookTreeStore.GetValue(iter,0);
   ((CellRendererText) cell).Text = book.Name;
  }
  private void BookCellPixbufDataFunc (Gtk.TreeViewColumn tree_column,
    Gtk.CellRenderer cell, Gtk.TreeModel tree_model,
    Gtk.TreeIter iter)
  {
   ((CellRendererPixbuf) cell).Pixbuf = BookPixBuf;
  }
  private void ContactCellTextDataFunc (Gtk.TreeViewColumn tree_column,
    Gtk.CellRenderer cell, Gtk.TreeModel tree_model,
    Gtk.TreeIter iter)
  {
   Contact cnt = (Contact) ContactTreeStore.GetValue(iter,0);
   if(cnt != null)
   {
    if( (cnt.FN != null) && (cnt.FN.Length > 0) )
     ((CellRendererText) cell).Text = cnt.FN;
    else
     ((CellRendererText) cell).Text = cnt.UserName;
   }
   else
   {
    ((CellRendererText) cell).Text = "unknown contact";
   }
  }
  private void ContactCellPixbufDataFunc (
    Gtk.TreeViewColumn tree_column, Gtk.CellRenderer cell,
    Gtk.TreeModel tree_model, Gtk.TreeIter iter)
  {
   Contact cnt = (Contact) ContactTreeStore.GetValue(iter,0);
   if(cnt != null)
   {
    if(cnt.IsCurrentUser)
     ((CellRendererPixbuf) cell).Pixbuf = CurCardPixBuf;
    else
     ((CellRendererPixbuf) cell).Pixbuf = UserCardPixBuf;
   }
   else
   {
    ((CellRendererPixbuf) cell).Pixbuf = UserCardPixBuf;
   }
  }
  public void ShowAll()
  {
   CBApp.ShowAll();
  }
  public void on_CBApp_delete (object o, DeleteEventArgs args)
  {
   args.RetVal = true;
   on_quit(o, args);
  }
  private void EnableControls(bool bookSelected, bool contactSelected)
  {
   NewBookItem.Sensitive = true;
   ConvertBookItem.Sensitive = true;
   if(bookSelected)
   {
    ShareBookItem.Sensitive = true;
    DeleteBookItem.Sensitive = true;
    BookPropsItem.Sensitive = true;
    CreateContactButton.Sensitive = true;
    NewContactItem.Sensitive = true;
    NewGroupItem.Sensitive = true;
    ImportButton.Sensitive = true;
    ImportVCardItem.Sensitive = true;
    if(contactSelected)
    {
     MakeMeItem.Sensitive = true;
     MakeNotMeItem.Sensitive = true;
     DeleteContactItem.Sensitive = true;
     DeleteGroupItem.Sensitive = true;
     ContactPropsItem.Sensitive = true;
     ExportVCardItem.Sensitive = true;
     ExportButton.Sensitive = true;
    }
    else
    {
     MakeMeItem.Sensitive = false;
     MakeNotMeItem.Sensitive = false;
     DeleteContactItem.Sensitive = false;
     DeleteGroupItem.Sensitive = false;
     ContactPropsItem.Sensitive = false;
     ExportVCardItem.Sensitive = false;
     ExportButton.Sensitive = false;
    }
   }
   else
   {
    ShareBookItem.Sensitive = false;
    DeleteBookItem.Sensitive = false;
    BookPropsItem.Sensitive = false;
    NewContactItem.Sensitive = false;
    MakeMeItem.Sensitive = false;
    MakeNotMeItem.Sensitive = false;
    DeleteContactItem.Sensitive = false;
    NewGroupItem.Sensitive = false;
    DeleteGroupItem.Sensitive = false;
    ContactPropsItem.Sensitive = false;
    ImportVCardItem.Sensitive = false;
    ExportVCardItem.Sensitive = false;
    CreateContactButton.Sensitive = false;
    ImportButton.Sensitive = false;
    ExportButton.Sensitive = false;
   }
  }
  private void SearchAddrBook()
  {
   ContactTreeStore.Clear();
   if(SearchEntry.Text.Length > 0)
   {
    Hashtable idHash = new Hashtable();
    IABList clist = curAddrBook.SearchFirstName(SearchEntry.Text,
      Simias.Storage.SearchOp.Begins);
    foreach(Contact c in clist)
    {
     idHash.Add(c.ID, c);
    }
    clist = curAddrBook.SearchLastName(SearchEntry.Text,
      Simias.Storage.SearchOp.Begins);
    foreach(Contact c in clist)
    {
     if(!idHash.Contains(c.ID))
      idHash.Add(c.ID, c);
    }
    clist = curAddrBook.SearchUsername(SearchEntry.Text,
      Simias.Storage.SearchOp.Begins);
    foreach(Contact c in clist)
    {
     if(!idHash.Contains(c.ID))
      idHash.Add(c.ID, c);
    }
    clist = curAddrBook.SearchEmail(SearchEntry.Text,
      Simias.Storage.SearchOp.Begins);
    foreach(Contact c in clist)
    {
     if(!idHash.Contains(c.ID))
      idHash.Add(c.ID, c);
    }
    foreach(Contact c in idHash.Values)
    {
     ContactTreeStore.AppendValues(c);
    }
   }
   else
   {
    foreach(Contact c in curAddrBook)
    {
     ContactTreeStore.AppendValues(c);
    }
   }
   TreeSelection tSelect = ContactTreeView.Selection;
   tSelect.SelectPath(new TreePath("0"));
  }
  public void onCreateBook(object o, EventArgs args)
  {
   BookEditor be = new BookEditor();
   be.TransientFor = CBApp;
   int rc = be.Run();
   if((rc == -5) && (abMan != null))
   {
    AddressBook ab = abMan.CreateAddressBook(be.Name);
    BookTreeStore.AppendValues(ab);
   }
  }
  public void onConvertTeamspace(object o, EventArgs args)
  {
   CollectionPicker cp = new CollectionPicker();
   cp.IgnoreType = "AB:AddressBook";
   int rc = cp.Run();
   if(rc == -5)
   {
    Collection col = cp.Collection;
    col.SetType(col, "AB:AddressBook");
    col.Commit();
    RefreshBooks();
   }
  }
  public void onCreateContact(object o, EventArgs args)
  {
   if(CreateContactButton.Sensitive == true)
   {
    ContactEditor ce = new ContactEditor();
    ce.TransientFor = CBApp;
    ce.Contact = new Contact();
    int rc = ce.Run();
    if(rc == -5)
    {
     curAddrBook.AddContact(ce.Contact);
     ce.Contact.Commit();
     ContactTreeStore.AppendValues(ce.Contact);
    }
   }
  }
  public void onImportVCard(object o, EventArgs args)
  {
   FileSelection fs = new FileSelection ("Select VCard to Import...");
   int rc = fs.Run ();
   fs.Hide ();
   if(rc == -5)
   {
    Contact newContact = null;
    StreamReader reader = null;
    bool hasmore = true;
    try
    {
     reader = new StreamReader(fs.Filename);
     if (reader != null)
     {
      while(hasmore)
      {
       newContact = curAddrBook.ImportVCard(reader);
       if(newContact != null)
       {
        ContactTreeStore.AppendValues(newContact);
       }
       try
       {
        String newline = reader.ReadLine();
        if(newline == null)
         hasmore = false;
       }
       catch(Exception e)
       {
        hasmore = false;
       }
      }
     }
    }
    catch(Exception e)
    {
     Console.WriteLine(e);
    }
    finally
    {
     if (reader != null)
     {
      reader.Close();
     }
    }
   }
  }
  public void onExportVCard(object o, EventArgs args)
  {
   TreeSelection tSelect = ContactTreeView.Selection;
   if(tSelect.CountSelectedRows() == 1)
   {
    TreeModel tModel;
    TreeIter iter;
    tSelect.GetSelected(out tModel, out iter);
    Contact cnt = (Contact) tModel.GetValue(iter,0);
    if(cnt != null)
    {
     string fileName = cnt.FN + ".vcf";
     FileSelection fs = new FileSelection ("Export VCard to...");
     fs.Filename = fileName;
     int rc = fs.Run ();
     fs.Hide ();
     if(rc == -5)
     {
      cnt.ExportVCard(fs.Filename);
     }
    }
   }
  }
  public void onCreateGroup(object o, EventArgs args)
  {
  }
  public void on_HelpButton_clicked(object o, EventArgs args)
  {
   Util.ShowHelp("bq6lx19.html", null);
  }
  public void on_about_event(object o, EventArgs args)
  {
   Util.ShowAbout();
  }
  public void on_DeleteButton_clicked(object o, EventArgs args)
  {
  }
  public void DeleteSelectedBooks()
  {
   TreeSelection tSelect = BookTreeView.Selection;
   if(tSelect.CountSelectedRows() == 1)
   {
    TreeModel tModel;
    TreeIter iter;
    tSelect.GetSelected(out tModel, out iter);
    AddressBook ab = (AddressBook) tModel.GetValue(iter,0);
    try
    {
     ab.Delete();
     ab.Commit();
     BookTreeStore.Remove(ref iter);
     ContactTreeStore.Clear();
    }
    catch(ApplicationException e)
    {
     Console.WriteLine(e);
    }
   }
  }
  public void on_book_key_press(object o, KeyPressEventArgs args)
  {
   switch(args.Event.HardwareKeycode)
   {
    case 22:
    case 107:
     {
      TreeSelection tSelect = BookTreeView.Selection;
      if(tSelect.CountSelectedRows() > 0)
      {
       MessageDialog dialog = new MessageDialog(CBApp,
         DialogFlags.Modal |
           DialogFlags.DestroyWithParent,
         MessageType.Question,
         ButtonsType.YesNo,
         "Do you want to delete the selected Address Books?");
       dialog.Title = "Delete Books";
       dialog.TransientFor = CBApp;
       int rc = dialog.Run();
       dialog.Hide();
       if(rc == (int)ResponseType.Yes)
       {
        DeleteSelectedBooks();
       }
      }
      break;
     }
   }
  }
  private void on_contact_selection_changed(object o, EventArgs args)
  {
   TreeSelection tSelect = ContactTreeView.Selection;
   if(tSelect.CountSelectedRows() == 1)
   {
    TreeModel tModel;
    TreeIter iter;
    EnableControls(true, true);
    tSelect.GetSelected(out tModel, out iter);
    Contact c = (Contact) tModel.GetValue(iter,0);
    DisplayContactDetails(c);
    if(c.IsCurrentUser)
    {
     MakeNotMeItem.Sensitive = true;
     MakeMeItem.Sensitive = false;
    }
    else
    {
     MakeMeItem.Sensitive = true;
     MakeNotMeItem.Sensitive = false;
    }
   }
   else
   {
    ClearContactDetails();
    EnableControls(true, false);
   }
  }
  public void on_book_selection_changed(object o, EventArgs args)
  {
   TreeSelection tSelect = BookTreeView.Selection;
   if(tSelect.CountSelectedRows() == 1)
   {
    TreeModel tModel;
    TreeIter iter;
    tSelect.GetSelected(out tModel, out iter);
    curAddrBook = (AddressBook) tModel.GetValue(iter,0);
    EnableControls(true, false);
    ContactTreeStore.Clear();
    foreach(Contact cont in curAddrBook)
    {
     if(cont != null)
     {
      ContactTreeStore.AppendValues(cont);
     }
     else
      Console.WriteLine("We were retuned a NULL contact.");
    }
   }
   else
    EnableControls(false, false);
  }
  public void on_share_clicked(object o, EventArgs args)
  {
   TreeSelection tSelect = BookTreeView.Selection;
   if(tSelect.CountSelectedRows() == 1)
   {
    TreeModel tModel;
    TreeIter iter;
    tSelect.GetSelected(out tModel, out iter);
    curAddrBook = (AddressBook) tModel.GetValue(iter,0);
    CollectionProperties colProp = new CollectionProperties();
    colProp.TransientFor = CBApp;
    colProp.Collection = curAddrBook;
    colProp.ActiveTag = 1;
    colProp.Run();
   }
  }
  public void on_contact_key_press(object o, KeyPressEventArgs args)
  {
   switch(args.Event.HardwareKeycode)
   {
    case 22:
    case 107:
     {
      TreeSelection tSelect = ContactTreeView.Selection;
      if(tSelect.CountSelectedRows() > 0)
      {
       MessageDialog dialog = new MessageDialog(CBApp,
         DialogFlags.Modal |
           DialogFlags.DestroyWithParent,
         MessageType.Question,
         ButtonsType.YesNo,
         "Do you want to delete the selected Contacts?");
       dialog.Response += new ResponseHandler(
         DeleteContactResponse);
       dialog.Title = "Delete Contacts";
       dialog.Show();
      }
      break;
     }
   }
  }
  public void DeleteContactResponse(object sender, ResponseArgs args)
  {
   MessageDialog dialog = (MessageDialog) sender;
   switch((ResponseType)args.ResponseId)
   {
    case ResponseType.Yes:
     DeleteSelectedContacts();
     break;
    default:
     break;
   }
   dialog.Destroy();
  }
  public void DeleteSelectedContacts()
  {
   TreeSelection tSelect = ContactTreeView.Selection;
   if(tSelect.CountSelectedRows() == 1)
   {
    TreeModel tModel;
    TreeIter iter;
    tSelect.GetSelected(out tModel, out iter);
    Contact cnt = (Contact) tModel.GetValue(iter,0);
    if(cnt.IsCurrentUser)
    {
     MessageDialog med = new MessageDialog(CBApp,
       DialogFlags.DestroyWithParent | DialogFlags.Modal,
       MessageType.Error,
       ButtonsType.Close,
       "Deleting yourself ain't right and we ain't gonna let you do it.");
     med.Title = "This ain't right";
     med.Run();
     med.Hide();
     return;
    }
    ContactTreeStore.Remove(ref iter);
    try
    {
     cnt.Delete();
    }
    catch(ApplicationException e)
    {
     Console.WriteLine(e);
    }
   }
  }
  public void on_make_contact_me(object obj, EventArgs args)
  {
   TreeSelection tSelect = ContactTreeView.Selection;
   if(tSelect.CountSelectedRows() == 1)
   {
    TreeModel tModel;
    TreeIter iter;
    tSelect.GetSelected(out tModel, out iter);
    Contact cnt = (Contact) tModel.GetValue(iter,0);
    if(!cnt.IsCurrentUser)
    {
     cnt.IsCurrentUser = true;
     cnt.Commit();
    }
    MakeMeItem.Sensitive = false;
    MakeNotMeItem.Sensitive = true;
   }
  }
  public void revert_me_contact(object obj, EventArgs args)
  {
   TreeSelection tSelect = ContactTreeView.Selection;
   if(tSelect.CountSelectedRows() == 1)
   {
    TreeModel tModel;
    TreeIter iter;
    tSelect.GetSelected(out tModel, out iter);
    Contact cnt = (Contact) tModel.GetValue(iter,0);
    if(cnt.IsCurrentUser)
    {
     cnt.IsCurrentUser = false;
     cnt.Commit();
    }
    MakeMeItem.Sensitive = true;
    MakeNotMeItem.Sensitive = false;
   }
  }
  internal void EditSelectedContact(object obj, EventArgs args)
  {
   TreeSelection tSelect = ContactTreeView.Selection;
   if(tSelect.CountSelectedRows() == 1)
   {
    TreeModel tModel;
    TreeIter iter;
    tSelect.GetSelected(out tModel, out iter);
    Contact c = (Contact) tModel.GetValue(iter,0);
    ContactEditor ce = new ContactEditor();
    ce.TransientFor = CBApp;
    ce.Contact = c;
    int rc = ce.Run();
    if(rc == -5)
    {
     c.Commit();
     on_contact_selection_changed(obj, args);
    }
   }
  }
  internal void on_editContactButton_clicked(object obj, EventArgs args)
  {
   EditSelectedContact(obj, args);
  }
  internal void on_contact_row_activated(object obj,
    RowActivatedArgs args)
  {
   EditSelectedContact(obj, args);
  }
  public void on_quit(object o, EventArgs args)
  {
   CBApp.Hide();
   CBApp.Destroy();
   CBApp = null;
   if(AddrBookClosed != null)
   {
    EventArgs e = new EventArgs();
    AddrBookClosed(this, e);
   }
  }
  public void on_show_picker(object o, EventArgs args)
  {
   ContactPicker cp = new ContactPicker();
   cp.AddrBookManager = abMan;
   cp.Run();
  }
  private void AddTitleTags()
  {
   TextTag tag = new TextTag("title");
   tag.Weight = Pango.Weight.Bold;
   tag.Size = (int) Pango.Scale.PangoScale * 12;
   TitleTextView.Buffer.TagTable.Add(tag);
  }
  private void AddTitles(string title1, string title2, string title3)
  {
   TextIter insertIter, beginIter, endIter;
   int begin, end;
   insertIter = TitleTextView.Buffer.GetIterAtMark(
     TitleTextView.Buffer.InsertMark);
   TitleTextView.Buffer.Insert (insertIter, "\n");
   if(title1 != null)
   {
    begin = TitleTextView.Buffer.CharCount;
    insertIter = TitleTextView.Buffer.GetIterAtMark(
      TitleTextView.Buffer.InsertMark);
    TitleTextView.Buffer.Insert (insertIter, title1+"\n");
    end = TitleTextView.Buffer.CharCount;
    endIter = TitleTextView.Buffer.GetIterAtOffset(end);
    beginIter = TitleTextView.Buffer.GetIterAtOffset(begin);
    TitleTextView.Buffer.ApplyTag ("title", beginIter, endIter);
   }
   if(title2 != null)
   {
    insertIter = TitleTextView.Buffer.GetIterAtMark(
      TitleTextView.Buffer.InsertMark);
    TitleTextView.Buffer.Insert (insertIter, title2+"\n");
   }
   if(title3 != null)
   {
    insertIter = TitleTextView.Buffer.GetIterAtMark(
      TitleTextView.Buffer.InsertMark);
    TitleTextView.Buffer.Insert (insertIter, title3+"\n");
   }
  }
  private void AddLabelTags()
  {
   TextTag tag = new TextTag("label");
   tag.Foreground = "grey";
   tag.Justification = Justification.Right;
   LabelTextView.Buffer.TagTable.Add(tag);
  }
  private void AddLabeledValue(string label, string value)
  {
   AddLabel(label);
   AddValue(value);
  }
  private void AddLabel(string label)
  {
   TextIter insertIter, beginIter, endIter;
   int begin, end;
   if(label == null)
   {
    insertIter = LabelTextView.Buffer.GetIterAtMark(
      LabelTextView.Buffer.InsertMark);
    LabelTextView.Buffer.Insert (insertIter, "\n");
   }
   else
   {
    begin = LabelTextView.Buffer.CharCount;
    insertIter = LabelTextView.Buffer.GetIterAtMark(
      LabelTextView.Buffer.InsertMark);
    LabelTextView.Buffer.Insert (insertIter, label+"\n");
    end = LabelTextView.Buffer.CharCount;
    endIter = LabelTextView.Buffer.GetIterAtOffset(end);
    beginIter = LabelTextView.Buffer.GetIterAtOffset(begin);
    LabelTextView.Buffer.ApplyTag ("label", beginIter, endIter);
   }
  }
  private void AddValue(string value)
  {
   TextIter insertIter;
   if(value == null)
   {
    insertIter = ValueTextView.Buffer.GetIterAtMark(
      ValueTextView.Buffer.InsertMark);
    ValueTextView.Buffer.Insert (insertIter, "\n");
   }
   else
   {
    insertIter = ValueTextView.Buffer.GetIterAtMark(
      ValueTextView.Buffer.InsertMark);
    ValueTextView.Buffer.Insert (insertIter, value + "\n");
   }
  }
  private void AddPicture(Pixbuf pixbuf)
  {
   TextIter insertIter;
   if(pixbuf != null)
   {
   insertIter = PictureTextView.Buffer.GetIterAtMark(
     PictureTextView.Buffer.InsertMark);
   PictureTextView.Buffer.Insert (insertIter, "\n");
   insertIter = PictureTextView.Buffer.GetIterAtMark(
     PictureTextView.Buffer.InsertMark);
   PictureTextView.Buffer.InsertPixbuf (insertIter, pixbuf);
   }
   else
    Console.WriteLine("THE pixbuf was null!");
  }
  private Pixbuf GetScaledPhoto(Contact c, int height)
  {
   Pixbuf pb = null;
   try
   {
    int newWidth, newHeight;
    pb = new Pixbuf(c.ExportPhoto());
    newHeight = height;
    newWidth = height;
    if(pb.Height != pb.Width)
    {
     int perc = (height * 1000) / pb.Height;
     newWidth = pb.Width * perc / 1000;
    }
    pb = pb.ScaleSimple(newWidth, newHeight,
      InterpType.Bilinear);
   }
   catch(Exception e)
   {
    pb = null;
   }
   return pb;
  }
  public void on_search_changed(object o, EventArgs args)
  {
   if(searchTimeoutID != 0)
   {
    Gtk.Timeout.Remove(searchTimeoutID);
    searchTimeoutID = 0;
   }
   searchTimeoutID = Gtk.Timeout.Add(500, new Gtk.Function(
     SearchCallback));
  }
  public void on_search_button_clicked(object o, EventArgs args)
  {
   SearchAddrBook();
  }
  public void ClearContactDetails()
  {
   PictureTextView.Buffer.Clear();
   LabelTextView.Buffer.Clear();
   ValueTextView.Buffer.Clear();
   TitleTextView.Buffer.Clear();
  }
  public void DisplayContactDetails(Contact c)
  {
   bool addBlank = false;
   ClearContactDetails();
   if(c != null)
   {
    Pixbuf cPhoto = GetScaledPhoto(c, 64);
    if(cPhoto != null)
     AddPicture(cPhoto);
    else
     AddPicture(BlankHeadPixBuf);
    if(c.FN != null)
    {
     AddTitles(c.FN, c.Title, null);
    }
    else
    {
     AddTitles(c.UserName, c.Title, null);
    }
    foreach(Telephone phone in c.GetTelephoneNumbers())
    {
     addBlank = true;
     if((phone.Types & PhoneTypes.home) == PhoneTypes.home)
     {
      AddLabeledValue("home", phone.Number);
     }
     if((phone.Types & PhoneTypes.work) == PhoneTypes.work)
     {
      AddLabeledValue("work", phone.Number);
     }
     if((phone.Types & PhoneTypes.other) == PhoneTypes.other)
     {
      AddLabeledValue("other", phone.Number);
     }
     if((phone.Types & PhoneTypes.cell) == PhoneTypes.cell)
     {
      AddLabeledValue("mobile", phone.Number);
     }
     if((phone.Types & PhoneTypes.pager) == PhoneTypes.pager)
     {
      AddLabeledValue("pager", phone.Number);
     }
     if((phone.Types & PhoneTypes.fax) == PhoneTypes.fax)
     {
      AddLabeledValue("fax", phone.Number);
     }
    }
    if(addBlank)
    {
     AddLabeledValue(null, null);
     addBlank = false;
    }
    foreach(Email e in c.GetEmailAddresses())
    {
     addBlank = true;
     if((e.Types & EmailTypes.personal) == EmailTypes.personal)
     {
      AddLabeledValue("home", e.Address);
     }
     if((e.Types & EmailTypes.work) == EmailTypes.work)
     {
      AddLabeledValue("work", e.Address);
     }
     if((e.Types & EmailTypes.other) == EmailTypes.other)
     {
      AddLabeledValue("other", e.Address);
     }
    }
    if(addBlank)
    {
     AddLabeledValue(null, null);
     addBlank = false;
    }
    foreach(IM im in c.GetInstantMessageAccounts())
    {
     addBlank = true;
     if((im.Types & IMTypes.home) == IMTypes.home)
     {
      AddLabeledValue("home",
        im.Address + " (" + im.Provider + ")");
     }
     if((im.Types & IMTypes.work) == IMTypes.work)
     {
      AddLabeledValue("work",
        im.Address + " (" + im.Provider + ")");
     }
     if((im.Types & IMTypes.other) == IMTypes.other)
     {
      AddLabeledValue("other",
        im.Address + " (" + im.Provider + ")");
     }
    }
    if(addBlank)
    {
     AddLabeledValue(null, null);
     addBlank = false;
    }
    foreach(Address addr in c.GetAddresses())
    {
     string addressLine = "";
     if((addr.Types & AddressTypes.home) ==
       AddressTypes.home)
     {
      if(addr.Street != null)
       AddLabeledValue("home", addr.Street);
     }
     else if((addr.Types & AddressTypes.work) ==
       AddressTypes.work)
     {
      if(addr.Street != null)
       AddLabeledValue("work", addr.Street);
     }
     else
     {
      if(addr.Street != null)
       AddLabeledValue("other", addr.Street);
     }
     if((addr.MailStop != null) && (addr.MailStop.Length > 0))
      AddLabeledValue(null, "MS: " + addr.MailStop);
     if(addr.Locality != null)
      addressLine = addr.Locality + " ";
     if(addr.Region != null)
      addressLine += addr.Region + " ";
     if(addr.PostalCode != null)
      addressLine += addr.PostalCode + " ";
     if(addressLine.Length > 0)
      AddLabeledValue(null, addressLine);
     if((addr.Country != null) && (addr.Country.Length > 0))
      AddLabeledValue(null, addr.Country);
     AddLabeledValue(null, null);
    }
   }
  }
 }
}
