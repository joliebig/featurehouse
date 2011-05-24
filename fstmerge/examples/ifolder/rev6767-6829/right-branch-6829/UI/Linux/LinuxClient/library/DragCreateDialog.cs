

using Gtk;
using System;

namespace Novell.iFolder
{



 public class DragCreateDialog : Dialog
 {
  private DomainInformation[] domains;
  private string defaultDomainID;
  private ComboBox domainComboBox;


  private RadioButton Regular;
  private RadioButton Encryption;
  private CheckButton SecureSync;
  private string initialPath;
  private string folderName;
  private string folderPath;

  private Expander optionsExpander;
  private iFolderWebService ifws;





  enum SecurityState
  {
   encryption = 1,
   enforceEncryption = 2,
   SSL = 4,
   enforceSSL = 8
  }




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

    return "";
   }




  }




  public bool ssl
  {
   get
   {
    return SecureSync.Active;
   }
  }




  public string EncryptionAlgorithm
  {
   get
   {
    if(Encryption.Active == true)
     return "BlowFish";
    else
     return null;
   }
  }





  public DragCreateDialog(Gtk.Window parentWindow, DomainInformation[] domainArray, string defaultDomainID, string initialPath, iFolderWebService ifws)
    : base(Util.GS("Convert to an iFolder..."), parentWindow,
    DialogFlags.Modal | DialogFlags.DestroyWithParent | DialogFlags.NoSeparator,
    Stock.Help, ResponseType.Help, Stock.Cancel, ResponseType.Cancel, Stock.Ok, ResponseType.Ok)
  {
   domains = domainArray;
   this.defaultDomainID = defaultDomainID;

   this.Icon = new Gdk.Pixbuf(Util.ImagesPath("ifolder16.png"));

   this.initialPath = initialPath;

   this.ifws = ifws;





   Widget widgets = SetupWidgets();
   widgets.ShowAll();

   this.VBox.Add(widgets);
   domainComboBox.Changed += new EventHandler(OnDomainChangedEvent);
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




   Label label = new Label(string.Format(Util.GS("Name:")));
   label.Xalign = 0;
   table.Attach(label, 0, 1, 0, 1,
       AttachOptions.Shrink | AttachOptions.Fill, 0, 0, 0);

   label = new Label(folderName);
   label.Xalign = 0;
   label.UseUnderline = false;
   table.Attach(label, 1, 2, 0, 1,
       AttachOptions.Shrink | AttachOptions.Fill, 0, 0, 0);




   label = new Label(string.Format(Util.GS("Folder:")));
   label.Xalign = 0;
   label.Yalign = 0;
   table.Attach(label, 0, 1, 1, 2,
       AttachOptions.Shrink | AttachOptions.Fill, 0, 0, 0);

   label = new Label(folderPath);
   label.Xalign = 0;
   label.UseUnderline = false;
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
   optionsExpander.Activate();

   Table optionsTable = new Table(2, 3, false);
   optionsExpander.Add(optionsTable);

   optionsTable.ColumnSpacing = 10;
   optionsTable.RowSpacing = 10;
   optionsTable.SetColSpacing(0, 30);


   Label l = new Label(Util.GS("iFolder account"));
   l.Xalign = 0;
   optionsTable.Attach(l, 1,2,0,1,
        AttachOptions.Shrink | AttachOptions.Fill, 0,0,0);


   Encryption = new RadioButton(Util.GS("Passphrase Encryption"));
   optionsTable.Attach(Encryption, 2,3,1,2, AttachOptions.Shrink | AttachOptions.Fill, 0,0,0);


   Regular = new RadioButton(Encryption, Util.GS("Regular"));
   optionsTable.Attach(Regular, 3,4,1,2, AttachOptions.Shrink | AttachOptions.Fill, 0,0,0);

   SecureSync = new CheckButton(Util.GS("Secure Sync"));
                        optionsTable.Attach(SecureSync, 4,5,1,2, AttachOptions.Shrink | AttachOptions.Fill, 0,0,0);

   l = new Label(Util.GS("Security"));
   l.Xalign = 0;
   optionsTable.Attach(l, 1,2,1,2,
        AttachOptions.Shrink | AttachOptions.Fill, 0,0,0);


   domainComboBox = ComboBox.NewText();
   optionsTable.Attach(domainComboBox, 2,3,0,1,
        AttachOptions.Expand | AttachOptions.Fill, 0,0,0);

   int defaultDomain = 0;
   for (int x = 0; x < domains.Length; x++)
   {
    domainComboBox.AppendText(string.Format(domains[x].Name + " - " + domains[x].Host));
    if (defaultDomainID != null)
    {
     if (defaultDomainID == domains[x].ID)
      defaultDomain = x;
    }
    else
     defaultDomain = x;
   }

   domainComboBox.Active = defaultDomain;

   int SecurityPolicy = ifws.GetSecurityPolicy(this.DomainID);
   ChangeStatus(SecurityPolicy);



   optionsTable.ShowAll();

   return optionsExpander;
  }




        private void OnOptionsExpanded(object o, EventArgs args)
  {

   if (!optionsExpander.Expanded)
    this.Resize(20, 20);
  }




        private void OnDomainChangedEvent(System.Object o, EventArgs args)
  {
   int SecurityPolicy = ifws.GetSecurityPolicy(this.DomainID);
   ChangeStatus(SecurityPolicy);
  }





                private void ChangeStatus(int SecurityPolicy)
                {
   Encryption.Active = Encryption.Sensitive = false;
   Regular.Active = Regular.Sensitive = false;

                        if(SecurityPolicy !=0)
                        {
                                if( (SecurityPolicy & (int)SecurityState.encryption) == (int) SecurityState.encryption)
                                {
                                        if( (SecurityPolicy & (int)SecurityState.enforceEncryption) == (int) SecurityState.enforceEncryption)
                                                Encryption.Active = true;
                                        else
                                        {
                                                Encryption.Sensitive = true;
                                                Regular.Sensitive = true;
                                        }
                                }
                                else
                                {
                                        Regular.Active = true;
                                }
                        }
                        else
                        {
                                Regular.Active = true;
                        }

   if( (this.domains[domainComboBox.Active].HostUrl).StartsWith( System.Uri.UriSchemeHttps ) )
                        {
                                SecureSync.Active = true;
                                SecureSync.Sensitive = false;
                        }
                        else
                        {
                                SecureSync.Active = false;
                                SecureSync.Sensitive = true;
                        }

                }

 }
}
