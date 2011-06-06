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
  private Gtk.TreeView SelTreeView;
  private Gtk.ListStore SelTreeStore;
  private BigList memberList;
  private MemberListModel memberListModel;
  private Gtk.Entry SearchEntry;
  private Gtk.Button CancelSearchButton;
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
   this.Title = Util.GS("Add Users");
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
   KeyPressEvent += new KeyPressEventHandler (KeyPressHandler);
                 KeyReleaseEvent += new KeyReleaseEventHandler(KeyReleaseHandler);
   AddEvents( (int) Gdk.EventMask.KeyPressMask | (int) Gdk.EventMask.KeyReleaseMask );
  }
  ~iFolderUserSelector()
  {
   memberListModel.CloseSearch();
  }
  void KeyPressHandler(object o, KeyPressEventArgs args)
  {
   args.RetVal = true;
   switch(args.Event.Key)
   {
    case Gdk.Key.Control_R:
    case Gdk.Key.Control_L:
     memberList.ctrl_pressed = true;
     break;
   }
  }
  void KeyReleaseHandler(object o, KeyReleaseEventArgs args)
  {
   args.RetVal = true;
   switch(args.Event.Key)
   {
    case Gdk.Key.Control_R:
    case Gdk.Key.Control_L:
     memberList.ctrl_pressed = false;
     break;
   }
  }
  private void InitializeWidgets()
  {
   this.SetDefaultSize (500, 400);
   this.Icon = new Gdk.Pixbuf(Util.ImagesPath("ifolder16.png"));
   VBox dialogBox = new VBox();
   dialogBox.Spacing = 10;
   dialogBox.BorderWidth = 10;
   this.VBox.PackStart(dialogBox, true, true, 0);
   Label l = new Label(
    string.Format("<span size=\"large\" weight=\"bold\">{0}</span>",
        Util.GS("Add users to this iFolder")));
   l.Xalign = 0;
   l.UseMarkup = true;
   dialogBox.PackStart(l, false, false, 0);
   HBox selBox = new HBox(false, 10);
   dialogBox.PackStart(selBox, true, true, 0);
   VBox vbox = new VBox(false, 0);
   selBox.PackStart(vbox, false, false, 0);
   HBox searchHBox = new HBox(false, 4);
   vbox.PackStart(searchHBox, false, false, 0);
   Label findLabel = new Label(Util.GS("_Find:"));
   searchHBox.PackStart(findLabel, false, false, 0);
   findLabel.Xalign = 0;
   SearchEntry = new Entry();
   searchHBox.PackStart(SearchEntry, true, true, 0);
   findLabel.MnemonicWidget = SearchEntry;
   SearchEntry.SelectRegion(0, -1);
   SearchEntry.CanFocus = true;
   SearchEntry.Changed +=
    new EventHandler(OnSearchEntryChanged);
   Image stopImage = new Image(Stock.Stop, Gtk.IconSize.Menu);
   stopImage.SetAlignment(0.5F, 0F);
   CancelSearchButton = new Button(stopImage);
   searchHBox.PackEnd(CancelSearchButton, false, false, 0);
   CancelSearchButton.Relief = ReliefStyle.None;
   CancelSearchButton.Sensitive = false;
   CancelSearchButton.Clicked +=
    new EventHandler(OnCancelSearchButton);
   memberListModel = new MemberListModel(domainID, simws);
   memberList = new BigList(memberListModel);
   ScrolledWindow sw = new ScrolledWindow(memberList.HAdjustment, memberList.VAdjustment);
   sw.ShadowType = Gtk.ShadowType.EtchedIn;
   sw.Add(memberList);
   vbox.PackStart(sw, true, true, 0);
   memberList.ItemSelected += new ItemSelected(OnMemberIndexSelected);
   memberList.ItemActivated += new ItemActivated(OnMemberIndexActivated);
   VBox btnBox = new VBox();
   btnBox.Spacing = 10;
   selBox.PackStart(btnBox, false, false, 0);
   Label spacer = new Label("");
   btnBox.PackStart(spacer, true, true, 0);
   HBox buttonHBox = new HBox(false, 4);
   spacer = new Label("");
   buttonHBox.PackStart(spacer, true, true, 0);
   Label buttonLabel = new Label(Util.GS("_Add"));
   buttonHBox.PackStart(buttonLabel, false, false, 0);
   Image buttonImage = new Image(Stock.GoForward, IconSize.Button);
   buttonHBox.PackStart(buttonImage, false, false, 0);
   spacer = new Label("");
   buttonHBox.PackStart(spacer, true, true, 0);
   UserAddButton = new Button(buttonHBox);
   btnBox.PackStart(UserAddButton, false, true, 0);
   UserAddButton.Clicked += new EventHandler(OnAddButtonClicked);
   buttonHBox = new HBox(false, 4);
   spacer = new Label("");
   buttonHBox.PackStart(spacer, true, true, 0);
   buttonImage = new Image(Stock.GoBack, IconSize.Button);
   buttonHBox.PackStart(buttonImage, false, false, 0);
   buttonLabel = new Label(Util.GS("_Remove"));
   buttonHBox.PackStart(buttonLabel, false, false, 0);
   spacer = new Label("");
   buttonHBox.PackStart(spacer, true, true, 0);
   UserDelButton = new Button(buttonHBox);
   btnBox.PackStart(UserDelButton, false, true, 0);
   UserDelButton.Clicked += new EventHandler(OnRemoveButtonClicked);
   spacer = new Label("");
   btnBox.PackStart(spacer, true, true, 0);
   vbox = new VBox(false, 0);
   selBox.PackStart(vbox, true, true, 0);
   l = new Label(Util.GS("_Users to add:"));
   l.Xalign = 0;
   vbox.PackStart(l, false, false, 0);
   SelTreeView = new TreeView();
   ScrolledWindow ssw = new ScrolledWindow();
   ssw.ShadowType = Gtk.ShadowType.EtchedIn;
   ssw.Add(SelTreeView);
   vbox.PackStart(ssw, true, true, 0);
   ssw.WidthRequest = 200;
   l.MnemonicWidget = SelTreeView;
   SelTreeStore = new ListStore(typeof(MemberInfo));
   SelTreeStore.SetSortFunc(
    0,
    new TreeIterCompareFunc(SelTreeStoreSortFunction));
   SelTreeStore.SetSortColumnId(0, SortType.Ascending);
   SelTreeView.Model = SelTreeStore;
   SelTreeView.HeadersVisible = false;
   TreeViewColumn selmemberColumn = new TreeViewColumn();
   CellRendererText smcrt = new CellRendererText();
   selmemberColumn.PackStart(smcrt, false);
   selmemberColumn.SetCellDataFunc(smcrt, new TreeCellDataFunc(
      UserCellTextDataFunc));
   selmemberColumn.Title = Util.GS("Users to Add...");
   SelTreeView.AppendColumn(selmemberColumn);
   SelTreeView.Selection.Mode = SelectionMode.Multiple;
   SelTreeView.Selection.Changed += new EventHandler(
      OnSelUserSelectionChanged);
   this.AddButton(Stock.Cancel, ResponseType.Cancel);
   this.AddButton(Stock.Ok, ResponseType.Ok);
   this.AddButton(Stock.Help, ResponseType.Help);
   SetResponseSensitive(ResponseType.Ok, false);
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
  private int SelTreeStoreSortFunction(TreeModel model, TreeIter a, TreeIter b)
  {
   MemberInfo memberA = (MemberInfo) SelTreeStore.GetValue(a, 0);
   MemberInfo memberB = (MemberInfo) SelTreeStore.GetValue(b, 0);
   if (memberA == null || memberB == null)
    return 0;
   string nameA = memberA.FullName;
   string nameB = memberB.FullName;
   if (nameA == null || nameA.Length < 1)
    nameA = memberA.Name;
   if (nameB == null || nameB.Length < 1)
    nameB = memberB.Name;
   return string.Compare(nameA, nameB, true);
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
  public void OnSearchEntryChanged(object o, EventArgs args)
  {
   if(searchTimeoutID != 0)
   {
    GLib.Source.Remove(searchTimeoutID);
    searchTimeoutID = 0;
   }
   if (SearchEntry.Text.Length > 0)
    CancelSearchButton.Sensitive = true;
   else
    CancelSearchButton.Sensitive = false;
   searchTimeoutID = GLib.Timeout.Add(500, new GLib.TimeoutHandler(
      SearchCallback));
  }
  private void OnCancelSearchButton(object o, EventArgs args)
  {
   SearchEntry.Text = "";
   SearchEntry.GrabFocus();
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
   if(SearchEntry.Text.Length > 0)
   {
    PerformInitialSearch("FN", SearchEntry.Text);
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
     SearchType.Contains,
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
   int selectedIndex;
   for(int i = memberList.sel_rows_count(); i>0; i--)
   {
    selectedIndex = (int) memberList.getRowAt(i-1);
    if (selectedIndex >= 0)
    {
     MemberInfo memberInfo = null;
     try
     {
      memberInfo = memberListModel.GetMemberInfo(selectedIndex);
     }
     catch(Exception e)
     {
      Debug.PrintLine(e.Message);
     }
     if (memberInfo != null)
     {
      if (!selectedUsers.ContainsKey(memberInfo.UserID))
      {
       selectedUsers.Add(memberInfo.UserID, memberInfo);
       SelTreeStore.AppendValues(memberInfo);
       SetResponseSensitive(ResponseType.Ok, true);
      }
     }
    }
   }
   memberList.clear_sel_rows();
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
   if (SelTreeStore.IterNChildren() == 0)
    SetResponseSensitive(ResponseType.Ok, false);
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
   int nonMemberElements = 0;
   searchContext = SearchContext;
   if (MemberList != null)
   {
    for (int i = 0, j=0; i < MemberList.Length; i++)
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
     if ( memberInfo.IsHost )
     {
             nonMemberElements++;
     }
     else
     {
                 memberInfos[j++] = memberInfo;
     }
    }
    if (MemberList.Length >= Total)
    {
     CloseSearch();
    }
   }
   total = Total - nonMemberElements;
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
    catch(Exception)
    {
    }
   }
  }
  public MemberInfo GetMemberInfo(int index)
  {
   if (index < 0 || index >= total || (total == 0))
   {
    Debug.PrintLine("MemberListModel.GetMemberInfo() called with index out of the range or when total == 0");
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
     Debug.PrintLine(String.Format("Exception thrown calling simws.FindSeekMembers(): {0}", e.Message));
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
    Debug.PrintLine(string.Format("{0}: {1}", row, e.Message));
    return Util.GS("Unknown");
   }
   string fullName = memberInfo.FullName;
   if (fullName != null && fullName.Length > 0)
   {
    return string.Format("{0} ({1})", fullName, memberInfo.Name);
   }
   return memberInfo.Name;
  }
  public string GetDescription(int row)
  {
   return GetValue(row);
  }
 }
}
