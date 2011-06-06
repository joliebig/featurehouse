using System;
using Gtk;
using System.Collections;
using Simias.Client;
namespace Novell.iFolder
{
 public class iFolderUserSelector : Dialog
 {
  private SimiasWebService simws;
  private string domainID;
  private Gdk.Pixbuf UserPixBuf;
  private Gtk.TreeView SelTreeView;
  private Gtk.ListStore SelTreeStore;
  private BigList memberList;
  private MemberListModel memberListModel;
  private Gtk.ComboBox SearchAttribComboBox;
  private Gtk.Entry SearchEntry;
  private Gtk.Button UserAddButton;
  private Gtk.Button UserDelButton;
  private uint searchTimeoutID;
  private Hashtable selectedUsers;
  public const int NumOfMembersToReturnDefault = 25;
  public MemberInfo[] SelectedUsers
  {
   get
   {
    ArrayList list = new ArrayList();
    TreeIter iter;
    if(SelTreeStore.GetIterFirst(out iter))
    {
     do
     {
      MemberInfo member = (MemberInfo)
           SelTreeStore.GetValue(iter,0);
      list.Add(member);
     }
     while(SelTreeStore.IterNext(ref iter));
    }
    return (MemberInfo[]) (list.ToArray(typeof(MemberInfo)));
   }
  }
  public iFolderUserSelector( Gtk.Window parent,
         SimiasWebService SimiasWS,
         string domainID)
   : base()
  {
   this.Title = Util.GS("Select Users");
   if (SimiasWS == null)
    throw new ApplicationException("SimiasWebService was null");
   this.simws = SimiasWS;
   this.domainID = domainID;
   this.HasSeparator = false;
   this.Resizable = true;
   this.Modal = true;
   if(parent != null)
    this.TransientFor = parent;
   InitializeWidgets();
   this.Realized += new EventHandler(OnRealizeWidget);
   searchTimeoutID = 0;
   selectedUsers = new Hashtable();
  }
  ~iFolderUserSelector()
  {
   memberListModel.CloseSearch();
  }
  private void InitializeWidgets()
  {
   this.SetDefaultSize (500, 400);
   this.Icon = new Gdk.Pixbuf(Util.ImagesPath("ifolderuser.png"));
   VBox dialogBox = new VBox();
   dialogBox.Spacing = 10;
   dialogBox.BorderWidth = 10;
   this.VBox.PackStart(dialogBox, true, true, 0);
   Table findTable = new Table(2, 3, false);
   dialogBox.PackStart(findTable, false, false, 0);
   findTable.ColumnSpacing = 20;
   findTable.RowSpacing = 5;
   Label findLabel = new Label(Util.GS("Find:"));
   findLabel.Xalign = 0;
   findTable.Attach(findLabel, 0, 1, 0, 1,
    AttachOptions.Shrink, 0, 0, 0);
   SearchAttribComboBox = ComboBox.NewText();
   SearchAttribComboBox.AppendText(Util.GS("First Name"));
   SearchAttribComboBox.AppendText(Util.GS("Last Name"));
   SearchAttribComboBox.AppendText(Util.GS("Full Name"));
   SearchAttribComboBox.Active = 2;
   SearchAttribComboBox.Changed += new EventHandler(OnSearchAttribComboBoxChanged);
   findTable.Attach(SearchAttribComboBox, 1, 2, 0, 1,
    AttachOptions.Shrink, 0, 0, 0);
   SearchEntry = new Gtk.Entry(Util.GS("<Enter text to find a user>"));
   SearchEntry.SelectRegion(0, -1);
   SearchEntry.CanFocus = true;
   SearchEntry.Changed += new EventHandler(OnSearchEntryChanged);
   findTable.Attach(SearchEntry, 2, 3, 0, 1,
    AttachOptions.Expand | AttachOptions.Fill, 0, 0, 0);
   Label findHelpTextLabel = new Label(Util.GS("(Full or partial name)"));
   findHelpTextLabel.Xalign = 0;
   findTable.Attach(findHelpTextLabel, 2,3,1,2,
    AttachOptions.Expand | AttachOptions.Fill, 0, 0, 0);
   HBox selBox = new HBox();
   selBox.Spacing = 10;
   dialogBox.PackStart(selBox, true, true, 0);
   memberListModel = new MemberListModel(domainID, simws);
   memberList = new BigList(memberListModel);
   ScrolledWindow sw = new ScrolledWindow(memberList.HAdjustment, memberList.VAdjustment);
   sw.ShadowType = Gtk.ShadowType.EtchedIn;
   sw.Add(memberList);
   selBox.PackStart(sw, true, true, 0);
   memberList.ItemSelected += new ItemSelected(OnMemberIndexSelected);
   memberList.ItemActivated += new ItemActivated(OnMemberIndexActivated);
   VBox btnBox = new VBox();
   btnBox.Spacing = 10;
   selBox.PackStart(btnBox, false, true, 0);
   UserAddButton = new Button(Util.GS("_Add >>"));
   btnBox.PackStart(UserAddButton, false, true, 0);
   UserAddButton.Clicked += new EventHandler(OnAddButtonClicked);
   UserDelButton = new Button(Util.GS("_Remove"));
   btnBox.PackStart(UserDelButton, false, true, 0);
   UserDelButton.Clicked += new EventHandler(OnRemoveButtonClicked);
   SelTreeView = new TreeView();
   ScrolledWindow ssw = new ScrolledWindow();
   ssw.ShadowType = Gtk.ShadowType.EtchedIn;
   ssw.Add(SelTreeView);
   selBox.PackStart(ssw, true, true, 0);
   SelTreeStore = new ListStore(typeof(MemberInfo));
   SelTreeView.Model = SelTreeStore;
   CellRendererPixbuf smcrp = new CellRendererPixbuf();
   TreeViewColumn selmemberColumn = new TreeViewColumn();
   selmemberColumn.PackStart(smcrp, false);
   selmemberColumn.SetCellDataFunc(smcrp, new TreeCellDataFunc(
      UserCellPixbufDataFunc));
   CellRendererText smcrt = new CellRendererText();
   selmemberColumn.PackStart(smcrt, false);
   selmemberColumn.SetCellDataFunc(smcrt, new TreeCellDataFunc(
      UserCellTextDataFunc));
   selmemberColumn.Title = Util.GS("Selected Users");
   selmemberColumn.Resizable = true;
   SelTreeView.AppendColumn(selmemberColumn);
   SelTreeView.Selection.Mode = SelectionMode.Multiple;
   SelTreeView.Selection.Changed += new EventHandler(
      OnSelUserSelectionChanged);
   UserPixBuf =
    new Gdk.Pixbuf(Util.ImagesPath("ifolderuser.png"));
   this.AddButton(Stock.Cancel, ResponseType.Cancel);
   this.AddButton(Stock.Ok, ResponseType.Ok);
   this.AddButton(Stock.Help, ResponseType.Help);
   SearchiFolderUsers();
  }
  private void OnRealizeWidget(object o, EventArgs args)
  {
   SearchEntry.HasFocus = true;
  }
  private void UserCellTextDataFunc (Gtk.TreeViewColumn tree_column,
    Gtk.CellRenderer cell, Gtk.TreeModel tree_model,
    Gtk.TreeIter iter)
  {
   MemberInfo member = (MemberInfo) tree_model.GetValue(iter,0);
   if( (member.FullName != null) && (member.FullName.Length > 0) )
   {
    if (memberListModel.IsDuplicateFullName(member.FullName))
     ((CellRendererText) cell).Text = string.Format("{0} ({1})", member.FullName, member.Name);
    else
     ((CellRendererText) cell).Text = member.FullName;
   }
   else
    ((CellRendererText) cell).Text = member.Name;
  }
  private void UserCellPixbufDataFunc (Gtk.TreeViewColumn tree_column,
    Gtk.CellRenderer cell, Gtk.TreeModel tree_model,
    Gtk.TreeIter iter)
  {
   ((CellRendererPixbuf) cell).Pixbuf = UserPixBuf;
  }
  public void OnMemberIndexSelected(int index)
  {
   if (index >= 0)
   {
    UserAddButton.Sensitive = true;
   }
   else
   {
    UserAddButton.Sensitive = false;
   }
  }
  private void OnMemberIndexActivated(int index)
  {
   OnAddButtonClicked(null, null);
  }
  public void OnSelUserSelectionChanged(object o, EventArgs args)
  {
   TreeSelection tSelect = SelTreeView.Selection;
   if(tSelect.CountSelectedRows() > 0)
   {
    UserDelButton.Sensitive = true;
   }
   else
   {
    UserDelButton.Sensitive = false;
   }
  }
  public void OnSearchAttribComboBoxChanged(object o, EventArgs args)
  {
   if (searchTimeoutID != 0)
   {
    GLib.Source.Remove(searchTimeoutID);
    searchTimeoutID = 0;
   }
    SearchiFolderUsers();
  }
  public void OnSearchEntryChanged(object o, EventArgs args)
  {
   if(searchTimeoutID != 0)
   {
    GLib.Source.Remove(searchTimeoutID);
    searchTimeoutID = 0;
   }
   searchTimeoutID = GLib.Timeout.Add(500, new GLib.TimeoutHandler(
      SearchCallback));
  }
  private bool SearchCallback()
  {
   SearchiFolderUsers();
   return false;
  }
  private void SearchiFolderUsers()
  {
   if (this.GdkWindow != null)
   {
    this.GdkWindow.Cursor = new Gdk.Cursor(Gdk.CursorType.Watch);
   }
   UserAddButton.Sensitive = false;
   UserDelButton.Sensitive = false;
   if(SearchEntry.Text.Length > 0 && SearchEntry.Text != Util.GS("<Enter text to find a user>"))
   {
    int searchAttribIndex = SearchAttribComboBox.Active;
    string searchAttribute;
    switch(searchAttribIndex)
    {
     case 1:
      searchAttribute = "Family";
      break;
     case 2:
      searchAttribute = "FN";
      break;
     case 0:
     default:
      searchAttribute = "Given";
      break;
    }
    PerformInitialSearch(searchAttribute, SearchEntry.Text);
   }
   else
   {
    PerformInitialSearch(null, null);
   }
   if (this.GdkWindow != null)
   {
    this.GdkWindow.Cursor = null;
   }
  }
  private void PerformInitialSearch(string searchAttribute, string searchString)
  {
   string searchContext;
   MemberInfo[] memberInfoA;
   int totalMembers;
   if (searchString == null)
   {
    simws.FindFirstMembers(
     domainID,
     NumOfMembersToReturnDefault,
     out searchContext,
     out memberInfoA,
     out totalMembers);
   }
   else
   {
    simws.FindFirstSpecificMembers(
     domainID,
     searchAttribute,
     SearchEntry.Text,
     SearchType.Begins,
     NumOfMembersToReturnDefault,
     out searchContext,
     out memberInfoA,
     out totalMembers);
   }
   memberListModel.Reinitialize(searchContext, memberInfoA, totalMembers);
   memberList.Reload();
   memberList.Refresh();
   if (totalMembers > 0)
   {
    memberList.Selected = 0;
   }
  }
  private void OnAddButtonClicked(object o, EventArgs args)
  {
   int selectedIndex = memberList.Selected;
   if (selectedIndex >= 0)
   {
    MemberInfo memberInfo = null;
    try
    {
     memberInfo = memberListModel.GetMemberInfo(selectedIndex);
    }
    catch(Exception e)
    {
     Console.WriteLine(e.Message);
    }
    if (memberInfo != null)
    {
     if (!selectedUsers.ContainsKey(memberInfo.UserID))
     {
      selectedUsers.Add(memberInfo.UserID, memberInfo);
      SelTreeStore.AppendValues(memberInfo);
     }
    }
   }
  }
  public void OnRemoveButtonClicked(object o, EventArgs args)
  {
   TreeModel tModel;
   Queue iterQueue;
   iterQueue = new Queue();
   TreeSelection tSelect = SelTreeView.Selection;
   Array treePaths = tSelect.GetSelectedRows(out tModel);
   if(tModel != null)
    tModel = null;
   foreach(TreePath tPath in treePaths)
   {
    TreeIter iter;
    if(SelTreeStore.GetIter(out iter, tPath))
    {
     iterQueue.Enqueue(iter);
    }
   }
   while(iterQueue.Count > 0)
   {
    TreeIter iter = (TreeIter) iterQueue.Dequeue();
    MemberInfo member =
      (MemberInfo) SelTreeStore.GetValue(iter,0);
    selectedUsers.Remove(member.UserID);
    SelTreeStore.Remove(ref iter);
   }
  }
 }
 internal class MemberListModel : IListModel
 {
  private string domainID;
  private string searchContext;
  private int total = 0;
  private Hashtable memberInfos;
  private Hashtable memberFullNames;
  private Hashtable duplicateMembers;
  private SimiasWebService simws;
  public MemberListModel(String DomainID, SimiasWebService SimiasWS)
  {
   domainID = DomainID;
   simws = SimiasWS;
   searchContext = null;
   total = 0;
   memberInfos = new Hashtable();
   memberFullNames = new Hashtable();
   duplicateMembers = new Hashtable();
  }
  public string SearchContext
  {
   get
   {
    return searchContext;
   }
  }
  public void Reinitialize(string SearchContext, MemberInfo[] MemberList, int Total)
  {
   CloseSearch();
   memberInfos.Clear();
   memberFullNames.Clear();
   duplicateMembers.Clear();
   searchContext = SearchContext;
   if (MemberList != null)
   {
    for (int i = 0; i < MemberList.Length; i++)
    {
     MemberInfo memberInfo = MemberList[i];
     if (memberFullNames.Contains(memberInfo.FullName))
     {
      string username = (string) memberFullNames[memberInfo.FullName];
      if (!username.Equals(memberInfo.Name))
      {
       duplicateMembers[memberInfo.FullName] = 0;
      }
     }
     else
      memberFullNames[memberInfo.FullName] = memberInfo.Name;
     memberInfos[i] = memberInfo;
    }
    if (MemberList.Length >= Total)
    {
     CloseSearch();
    }
   }
   total = Total;
  }
  public void CloseSearch()
  {
   if (searchContext != null)
   {
    try
    {
     simws.FindCloseMembers(domainID, searchContext);
     searchContext = null;
    }
    catch(Exception e)
    {
    }
   }
  }
  public MemberInfo GetMemberInfo(int index)
  {
   if (index < 0 || index >= total || (total == 0))
   {
    Console.WriteLine("MemberListModel.GetMemberInfo() called with index out of the range or when total == 0");
    throw new Exception("GetValue called when no items are present");
   }
   MemberInfo memberInfoReturn;
   if (memberInfos.Contains(index))
   {
    memberInfoReturn = (MemberInfo)memberInfos[index];
   }
   else
   {
    if (searchContext == null)
    {
     throw new Exception("searchContext was closed too soon");
    }
    try
    {
     MemberInfo[] newMemberList;
     simws.FindSeekMembers(domainID, ref searchContext,
            index, iFolderUserSelector.NumOfMembersToReturnDefault,
            out newMemberList);
     int currentIndex = index;
     foreach(MemberInfo memberInfo in newMemberList)
     {
      if (memberFullNames.Contains(memberInfo.FullName))
      {
       string username = (string) memberFullNames[memberInfo.FullName];
       if (!username.Equals(memberInfo.Name))
       {
        duplicateMembers[memberInfo.FullName] = 0;
       }
      }
      else
       memberFullNames[memberInfo.FullName] = memberInfo.Name;
      memberInfos[currentIndex] = memberInfo;
      currentIndex++;
     }
    }
    catch (Exception e)
    {
     Console.WriteLine("Exception thrown calling simws.FindSeekMembers(): {0}", e.Message);
    }
    memberInfoReturn = (MemberInfo)memberInfos[index];
   }
   if (memberInfoReturn == null)
    throw new Exception("Could not find the specified member");
   return memberInfoReturn;
  }
  public bool IsDuplicateFullName(string FullName)
  {
   return duplicateMembers.Contains(FullName);
  }
  public int Rows
  {
   get
   {
    return total;
   }
  }
  public string GetValue(int row)
  {
   MemberInfo memberInfo = null;
   try
   {
    memberInfo = GetMemberInfo(row);
   }
   catch(Exception e)
   {
    Console.WriteLine(string.Format("{0}: {1}", row, e.Message));
    return Util.GS("Unknown");
   }
   string fullName = memberInfo.FullName;
   if (fullName != null && fullName.Length > 0)
   {
    if (duplicateMembers.Contains(memberInfo.FullName))
     return string.Format("{0} ({1})", fullName, memberInfo.Name);
    else
     return string.Format("{0}", fullName);
   }
   return memberInfo.Name;
  }
  public string GetDescription(int row)
  {
   return GetValue(row);
  }
 }
}
