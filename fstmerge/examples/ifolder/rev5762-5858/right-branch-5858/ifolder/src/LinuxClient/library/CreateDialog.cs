

using Gtk;
using System;

namespace Novell.iFolder
{
 public class CreateDialog : FileChooserDialog
 {
  private DomainInformation[] domains;
  private ComboBox domainComboBox;
  private string initialPath;
  TextView descriptionTextView;
  iFolderWebService ifws;
  private uint keyReleasedTimeoutID;

  public string iFolderPath
  {
   get
   {
    return this.Filename;
   }
   set
   {
    this.SetCurrentFolder(value);
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





  public CreateDialog(Gtk.Window parentWindow, DomainInformation[] domainArray, string filteredDomainID, string initialPath, iFolderWebService ifws)
    : base("", Util.GS("Select a folder..."), parentWindow, FileChooserAction.SelectFolder, Stock.Cancel, ResponseType.Cancel,
                Stock.Ok, ResponseType.Ok)
  {
   domains = domainArray;

   this.Icon = new Gdk.Pixbuf(Util.ImagesPath("ifolder24.png"));

   this.initialPath = initialPath;

   this.ifws = ifws;

   keyReleasedTimeoutID = 0;

   if (this.initialPath != null && this.initialPath.Length > 0)
    this.SetCurrentFolder(this.initialPath);


   this.ExtraWidget = CreateMoreOptionsExpander(filteredDomainID);

   this.SetResponseSensitive(ResponseType.Ok, false);
  }

  private Widget CreateMoreOptionsExpander(string filteredDomainID)
  {
   Expander moreOptionsExpander = new Expander(Util.GS("More options"));

   Table optionsTable = new Table(2, 3, false);
   moreOptionsExpander.Add(optionsTable);

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
    if (filteredDomainID != null)
    {
     if (filteredDomainID == domains[x].ID)
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

   return moreOptionsExpander;
  }

  protected override void OnSelectionChanged()
  {
   string currentPath = this.Filename;

   try
   {
    if (ifws.CanBeiFolder(currentPath))
     this.SetResponseSensitive(ResponseType.Ok, true);
    else
     this.SetResponseSensitive(ResponseType.Ok, false);
   }
   catch (Exception e)
   {
    this.SetResponseSensitive(ResponseType.Ok, false);
   }
  }

  protected override bool OnKeyReleaseEvent(Gdk.EventKey evnt)
  {
   if (descriptionTextView.HasFocus)
    return true;

   if (keyReleasedTimeoutID != 0)
   {
    GLib.Source.Remove(keyReleasedTimeoutID);
    keyReleasedTimeoutID = 0;
   }




   keyReleasedTimeoutID =
    GLib.Timeout.Add(100,
         new GLib.TimeoutHandler(CheckEnableOkButton));

   return true;
  }

  private bool CheckEnableOkButton()
  {
   try
   {
    string currentPath = this.Filename;
    if (ifws.CanBeiFolder(currentPath))
    {
     this.SetResponseSensitive(ResponseType.Ok, true);
     return false;
    }
   }
   catch{}
   this.SetResponseSensitive(ResponseType.Ok, false);
   return false;
  }
 }
}
