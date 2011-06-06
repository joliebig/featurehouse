using Gtk;
using System;
namespace Novell.iFolder
{
 public class DragCreateDialog : Dialog
 {
  private DomainInformation[] domains;
  private string defaultDomainID;
  private ComboBox domainComboBox;
  private string initialPath;
  private string folderName;
  private string folderPath;
  TextView descriptionTextView;
  private Expander optionsExpander;
  public string iFolderPath
  {
   get
   {
    return this.initialPath;
   }
  }
  public string DomainID
  {
   get
   {
    int activeIndex = domainComboBox.Active;
    if (activeIndex >= 0)
     return domains[activeIndex].ID;
    else
     return "0";
   }
  }
  public string Description
  {
   get
   {
    return descriptionTextView.Buffer.Text;
   }
   set
   {
    descriptionTextView.Buffer.Text = value;
   }
  }
  public DragCreateDialog(Gtk.Window parentWindow, DomainInformation[] domainArray, string defaultDomainID, string initialPath)
    : base(Util.GS("Convert to an iFolder..."), parentWindow,
    DialogFlags.Modal | DialogFlags.DestroyWithParent | DialogFlags.NoSeparator,
    Stock.Help, ResponseType.Help, Stock.Cancel, ResponseType.Cancel, Stock.Ok, ResponseType.Ok)
  {
   domains = domainArray;
   this.defaultDomainID = defaultDomainID;
   this.Icon = new Gdk.Pixbuf(Util.ImagesPath("ifolder24.png"));
   this.initialPath = initialPath;
   Widget widgets = SetupWidgets();
   widgets.ShowAll();
   this.VBox.Add(widgets);
  }
  private Widget SetupWidgets()
  {
   VBox vbox = new VBox();
   vbox.BorderWidth = 10;
   vbox.Spacing = 10;
   Table table = new Table(3, 2, false);
   vbox.PackStart(table, true, true, 0);
   table.ColumnSpacing = 12;
   table.RowSpacing = 12;
   int lastSlashPos =
    initialPath.LastIndexOf(System.IO.Path.DirectorySeparatorChar);
   if (lastSlashPos < 0)
   {
    folderName = initialPath;
    folderPath = "";
   }
   else
   {
    folderName = initialPath.Substring(lastSlashPos + 1);
    folderPath = initialPath.Substring(0, lastSlashPos);
   }
   Label label = new Label(string.Format("Name:"));
   label.Xalign = 0;
   table.Attach(label, 0, 1, 0, 1,
       AttachOptions.Shrink | AttachOptions.Fill, 0, 0, 0);
   label = new Label(folderName);
   label.Xalign = 0;
   table.Attach(label, 1, 2, 0, 1,
       AttachOptions.Shrink | AttachOptions.Fill, 0, 0, 0);
   label = new Label(string.Format("Folder:"));
   label.Xalign = 0;
   label.Yalign = 0;
   table.Attach(label, 0, 1, 1, 2,
       AttachOptions.Shrink | AttachOptions.Fill, 0, 0, 0);
   label = new Label(folderPath);
   label.Xalign = 0;
   table.Attach(label, 1, 2, 1, 2,
       AttachOptions.Shrink | AttachOptions.Fill, 0, 0, 0);
   table.Attach(CreateMoreOptionsExpander(defaultDomainID),
       0, 2, 2, 3,
       AttachOptions.Expand | AttachOptions.Fill, 0, 0, 0);
   return vbox;
  }
  private Widget CreateMoreOptionsExpander(string defaultDomainID)
  {
   optionsExpander = new Expander(Util.GS("More options"));
   optionsExpander.Activated += new EventHandler(OnOptionsExpanded);
   Table optionsTable = new Table(2, 3, false);
   optionsExpander.Add(optionsTable);
   optionsTable.ColumnSpacing = 10;
   optionsTable.RowSpacing = 10;
   optionsTable.SetColSpacing(0, 30);
   Label l = new Label(Util.GS("iFolder Account:"));
   l.Xalign = 0;
   optionsTable.Attach(l, 1,2,0,1,
        AttachOptions.Shrink | AttachOptions.Fill, 0,0,0);
   domainComboBox = ComboBox.NewText();
   optionsTable.Attach(domainComboBox, 2,3,0,1,
        AttachOptions.Expand | AttachOptions.Fill, 0,0,0);
   int defaultDomain = 0;
   for (int x = 0; x < domains.Length; x++)
   {
    domainComboBox.AppendText(domains[x].Name);
    if (defaultDomainID != null)
    {
     if (defaultDomainID == domains[x].ID)
      defaultDomain = x;
    }
    else
     defaultDomain = x;
   }
   domainComboBox.Active = defaultDomain;
   l = new Label(Util.GS("Description:"));
   l.Xalign = 0;
   optionsTable.Attach(l, 1,2,1,2,
        AttachOptions.Shrink | AttachOptions.Fill, 0,0,0);
   descriptionTextView = new TextView();
   descriptionTextView.LeftMargin = 4;
   descriptionTextView.RightMargin = 4;
   descriptionTextView.Editable = true;
   descriptionTextView.CursorVisible = true;
   descriptionTextView.AcceptsTab = false;
   descriptionTextView.WrapMode = WrapMode.WordChar;
   ScrolledWindow sw = new ScrolledWindow();
   sw.ShadowType = ShadowType.EtchedIn;
   sw.Add(descriptionTextView);
   optionsTable.Attach(sw, 2,3,1,2,
        AttachOptions.Expand | AttachOptions.Fill, 0,0,0);
   optionsTable.ShowAll();
   return optionsExpander;
  }
  private void OnOptionsExpanded(object o, EventArgs args)
  {
   if (!optionsExpander.Expanded)
    this.Resize(20, 20);
  }
 }
}
