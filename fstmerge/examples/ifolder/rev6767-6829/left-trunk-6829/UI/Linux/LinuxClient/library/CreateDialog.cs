

using Gtk;
using System;
using System.Collections;

namespace Novell.iFolder
{



  public enum SecurityState
  {
   encryption = 1,
   enforceEncryption = 2,
   SSL = 4,
   enforceSSL = 8
  }




 public class CreateDialog : FileChooserDialog
 {
  private DomainInformation[] domains;
  private ComboBox domainComboBox;


  private RadioButton Encryption;
  private RadioButton Regular;
  private CheckButton SecureSync;
  private string initialPath;
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




  public string Description
  {
   get
   {

    return "";
   }





  }





  public CreateDialog(Gtk.Window parentWindow, DomainInformation[] domainArray, string filteredDomainID, string initialPath, iFolderWebService ifws)
    : base("", Util.GS("Select a folder..."), parentWindow, FileChooserAction.SelectFolder, Stock.Cancel, ResponseType.Cancel,
                Stock.Ok, ResponseType.Ok)
  {
   domains = domainArray;

   this.Icon = new Gdk.Pixbuf(Util.ImagesPath("ifolder16.png"));

   this.initialPath = initialPath;

   this.ifws = ifws;

   keyReleasedTimeoutID = 0;

   if (this.initialPath != null && this.initialPath.Length > 0)
    this.SetCurrentFolder(this.initialPath);


   this.ExtraWidget = CreateMoreOptionsExpander(filteredDomainID);
   domainComboBox.Changed += new EventHandler(OnDomainChangedEvent);

   this.SetResponseSensitive(ResponseType.Ok, false);
  }




        private void OnDomainChangedEvent(System.Object o, EventArgs args)
  {
   int SecurityPolicy = ifws.GetSecurityPolicy(this.DomainID);
   ChangeStatus(SecurityPolicy);
  }





  private void ChangeStatus(int SecurityPolicy)
  {
   Encryption.Active = Regular.Active = false;
   Encryption.Sensitive = Regular.Sensitive = false;

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






  private Widget CreateMoreOptionsExpander(string filteredDomainID)
  {
   Expander moreOptionsExpander = new Expander(Util.GS("More options"));

   Table optionsTable = new Table(4, 3, false);
   moreOptionsExpander.Add(optionsTable);

   optionsTable.ColumnSpacing = 10;
   optionsTable.RowSpacing = 10;
   optionsTable.SetColSpacing(0, 30);

   Label l = new Label(Util.GS("iFolder account")+":");
   l.Xalign = 0;

   optionsTable.Attach(l, 1,2,0,1,
        AttachOptions.Shrink | AttachOptions.Fill, 0,0,0);

   Encryption = new RadioButton(Util.GS("Passphrase Encryption"));
   optionsTable.Attach(Encryption, 2,3,1,2, AttachOptions.Shrink | AttachOptions.Fill, 0,0,0);

   Regular = new RadioButton(Encryption, Util.GS("Regular"));
   optionsTable.Attach(Regular, 3,4,1,2, AttachOptions.Shrink | AttachOptions.Fill, 0,0,0);

   SecureSync = new CheckButton(Util.GS("Secure Sync"));
   optionsTable.Attach(SecureSync, 4,5,1,2,
                                                AttachOptions.Fill | AttachOptions.Expand, 0,0,0);


   l = new Label(Util.GS("Type")+":");
   l.Xalign = 0;
   optionsTable.Attach(l, 1,2,1,2,
        AttachOptions.Shrink | AttachOptions.Fill, 0,0,0);


   domainComboBox = ComboBox.NewText();
   optionsTable.Attach(domainComboBox, 2,4,0,1,
        AttachOptions.Expand | AttachOptions.Fill, 0,0,0);

   int defaultDomain = 0;
   for (int x = 0; x < domains.Length; x++)
   {
    domainComboBox.AppendText(String.Format(domains[x].Name + " - " + domains[x].Host));
    if (filteredDomainID != null)
    {
     if (filteredDomainID == domains[x].ID)
      defaultDomain = x;
    }
    else
     defaultDomain = x;
   }

   domainComboBox.Active = defaultDomain;
   int encr_status = ifws.GetSecurityPolicy(domains[defaultDomain].ID);
   ChangeStatus(encr_status);


   moreOptionsExpander.Expanded = true;
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
