using Gtk;
using System;
using Novell.iFolder.Controller;
namespace Novell.iFolder
{
 public class ResetPassPhraseDialog : Dialog
 {
  private ComboBox domainComboBox;
  private Entry oldPassPhrase;
  private Entry newPassPhrase;
  private Entry retypePassPhrase;
  private ComboBox recoveryAgentCombo;
  private string[] RAList;
  private DomainInformation[] domains;
  private CheckButton savePassPhrase;
         private SimiasWebService simws;
  private iFolderWebService ifws;
  private Image iFolderBanner;
  private Image iFolderScaledBanner;
  private Gdk.Pixbuf ScaledPixbuf;
  private bool status;
  public DomainInformation[] Domains
  {
   get
   {
    return this.domains;
   }
   set
   {
    this.domains = value;
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
                                        return null;
                        }
                }
  public string Domain
  {
   get
   {
    if( domains != null)
     return domains[domainComboBox.Active].ID;
    else
     return null;
   }
  }
  public string OldPassphrase
  {
   get
   {
    return oldPassPhrase.Text;
   }
  }
  public string NewPassphrase
  {
   get
   {
    return newPassPhrase.Text;
   }
  }
  public bool SavePassphrase
  {
   get
   {
    return savePassPhrase.Active;
   }
  }
  public string RAName
  {
   get
   {
    if(recoveryAgentCombo.ActiveText == Util.GS("Server_Default") || recoveryAgentCombo.ActiveText == String.Empty)
     return "DEFAULT";
    else
     return recoveryAgentCombo.ActiveText;
   }
  }
  public bool Status
  {
   get
   {
    return status;
   }
  }
  public ResetPassPhraseDialog(SimiasWebService simiasws, iFolderWebService ifws) : base()
  {
   this.simws= simiasws;
   this.ifws = ifws;
   SetupDialog();
  }
  private void SetupDialog()
  {
   this.Title = Util.GS("Change Passphrase");
   this.Icon = new Gdk.Pixbuf(Util.ImagesPath("ifolder16.png"));
   this.HasSeparator = false;
   this.SetDefaultSize (450, 100);
   this.Modal = true;
   this.DefaultResponse = ResponseType.Ok;
   HBox imagebox = new HBox();
   imagebox.Spacing = 0;
   iFolderBanner = new Image(
     new Gdk.Pixbuf(Util.ImagesPath("ifolder-banner.png")));
   imagebox.PackStart(iFolderBanner, false, false, 0);
   ScaledPixbuf =
    new Gdk.Pixbuf(Util.ImagesPath("ifolder-banner-scaler.png"));
   iFolderScaledBanner = new Image(ScaledPixbuf);
   iFolderScaledBanner.ExposeEvent +=
     new ExposeEventHandler(OnBannerExposed);
   imagebox.PackStart(iFolderScaledBanner, true, true, 0);
   this.VBox.PackStart (imagebox, false, true, 0);
   Table table = new Table(6, 2, false);
   this.VBox.PackStart(table, false, false, 0);
   table.ColumnSpacing = 6;
   table.RowSpacing = 6;
   table.BorderWidth = 12;
   Label lbl = new Label(Util.GS("_iFolder account")+":");
   table.Attach(lbl, 0,1, 0,1,
    AttachOptions.Fill | AttachOptions.Expand, 0,0,0);
   lbl.LineWrap = true;
   lbl.Xalign = 0.0F;
   domainComboBox = ComboBox.NewText();
   table.Attach(domainComboBox, 1,2, 0,1,
     AttachOptions.Expand | AttachOptions.Fill, 0,0,0);
   lbl = new Label(Util.GS("_Enter passphrase")+":");
   table.Attach(lbl, 0,1, 1,2,
    AttachOptions.Fill | AttachOptions.Expand, 0,0,0);
   lbl.LineWrap = true;
   lbl.Xalign = 0.0F;
   oldPassPhrase = new Entry();
   oldPassPhrase.Visibility = false;
   table.Attach(oldPassPhrase, 1,2, 1,2,
    AttachOptions.Fill | AttachOptions.Expand, 0,0,0);
   lbl.MnemonicWidget = oldPassPhrase;
   oldPassPhrase.Changed += new EventHandler(UpdateSensitivity);
   lbl = new Label(Util.GS("Enter new _passphrase")+":");
   table.Attach(lbl, 0,1, 2,3,
    AttachOptions.Fill | AttachOptions.Expand, 0,0,0);
   lbl.LineWrap = true;
   lbl.Xalign = 0.0F;
   newPassPhrase = new Entry();
   newPassPhrase.Visibility = false;
   table.Attach(newPassPhrase, 1,2, 2,3,
    AttachOptions.Fill | AttachOptions.Expand, 0,0,0);
   lbl.MnemonicWidget = newPassPhrase;
   newPassPhrase.Changed += new EventHandler(UpdateSensitivity);
   lbl = new Label(Util.GS("_Re-type passphrase:"));
   table.Attach(lbl, 0,1, 3,4,
    AttachOptions.Fill | AttachOptions.Expand, 0,0,0);
   lbl.LineWrap = true;
   lbl.Xalign = 0.0F;
   retypePassPhrase = new Entry();
   retypePassPhrase.Visibility = false;
   table.Attach(retypePassPhrase, 1,2, 3,4,
    AttachOptions.Fill | AttachOptions.Expand, 0,0,0);
   lbl.MnemonicWidget = retypePassPhrase;
   retypePassPhrase.Changed += new EventHandler(UpdateSensitivity);
   lbl = new Label(Util.GS("Recovery _agent")+":");
   table.Attach(lbl, 0,1, 4,5,
    AttachOptions.Fill | AttachOptions.Expand, 0,0,0);
   lbl.LineWrap = true;
   lbl.Xalign = 0.0F;
   recoveryAgentCombo = ComboBox.NewText();
   table.Attach(recoveryAgentCombo, 1,2, 4,5,
     AttachOptions.Expand | AttachOptions.Fill, 0,0,0);
   savePassPhrase = new CheckButton(Util.GS("_Remember passphrase"));
   table.Attach(savePassPhrase, 1,2,5,6, AttachOptions.Expand|AttachOptions.Fill, 0,0,0);
   this.VBox.ShowAll();
            this.AddButton(Stock.Help, ResponseType.Help);
   this.AddButton(Stock.Cancel, ResponseType.Cancel);
   Button but = (Button)this.AddButton(Util.GS("Reset"), ResponseType.Ok);
   but.Clicked += new EventHandler(OnResetClicked);
   but.Image = new Image(Stock.Undo, Gtk.IconSize.Menu);
   this.SetResponseSensitive(ResponseType.Ok, false);
   this.DefaultResponse = ResponseType.Ok;
   this.Realized += new EventHandler(OnResetPassphraseLoad);
   domainComboBox.Changed += new EventHandler(OnDomainChangedEvent);
  }
  private void OnResetClicked( object o, EventArgs args)
  {
   Debug.PrintLine("Reset clicked");
   string publicKey = null;
   bool reset = false;
   try
   {
    if (this.GdkWindow != null)
     this.GdkWindow.Cursor = new Gdk.Cursor(Gdk.CursorType.Watch);
    DomainController domainController = DomainController.GetDomainController();
    Status passphraseStatus = simws.ValidatePassPhrase(this.Domain, this.OldPassphrase);
    if( passphraseStatus != null)
    {
     if( passphraseStatus.statusCode == StatusCodes.PassPhraseInvalid)
     {
      iFolderMsgDialog dialog = new iFolderMsgDialog(
        null,
        iFolderMsgDialog.DialogType.Error,
        iFolderMsgDialog.ButtonSet.None,
        Util.GS("Invalid Passphrase"),
        Util.GS("The Current PassPhrase entered is not valid"),
        Util.GS("Please enter the passphrase again"));
      dialog.Run();
      dialog.Hide();
      dialog.Destroy();
      dialog = null;
      return;
     }
    }
    if( this.RAName != "DEFAULT")
    {
     byte [] RACertificateObj = domainController.GetRACertificate(this.Domain, this.RAName);
     if( RACertificateObj != null && RACertificateObj.Length != 0)
     {
      System.Security.Cryptography.X509Certificates.X509Certificate Cert =
       new System.Security.Cryptography.X509Certificates.X509Certificate(RACertificateObj);
      CertificateDialog dlg = new CertificateDialog(Cert.ToString(true));
      if (!Util.RegisterModalWindow(dlg))
      {
       dlg.Destroy();
       dlg = null;
       return ;
      }
      int res = dlg.Run();
      dlg.Hide();
      dlg.Destroy();
      dlg = null;
      if( res == (int)ResponseType.Ok)
      {
       publicKey = Convert.ToBase64String(Cert.GetPublicKey());
       Debug.PrintLine(String.Format(" The public key is: {0}", publicKey));
       reset = true;
      }
      else
      {
       reset = false;
       Debug.PrintLine("Response type is not ok");
       return;
      }
     }
    }
    else
    {
     DomainInformation domainInfo = (DomainInformation)this.simws.GetDomainInformation(this.Domain);
     string memberUID = domainInfo.MemberUserID;
     publicKey = this.ifws.GetDefaultServerPublicKey(this.Domain, memberUID);
     reset = true;
    }
    if( reset == true)
    {
     try
     {
      status = domainController.ReSetPassphrase(this.Domain,
        this.OldPassphrase,
        this.NewPassphrase,
        this.RAName, publicKey);
      simws.StorePassPhrase(this.Domain, "", CredentialType.None, false);
      simws.StorePassPhrase(this.Domain, this.NewPassphrase, CredentialType.Basic, this.SavePassphrase);
     }
     catch(Exception ex)
     {
      throw ex;
     }
    }
    if( status == false)
    {
     iFolderMsgDialog dialog = new iFolderMsgDialog(
       null,
       iFolderMsgDialog.DialogType.Error,
       iFolderMsgDialog.ButtonSet.None,
       Util.GS("Change Passphrase"),
       Util.GS("Unable to change the Passphrase"),
       Util.GS("Please try again"));
     dialog.Run();
     dialog.Hide();
     dialog.Destroy();
     dialog = null;
    }
   }
   catch(Exception e)
   {
    Debug.PrintLine(String.Format("Exception in reset passphrase : {0}",e.Message));
   }
   if (this.GdkWindow != null)
    this.GdkWindow.Cursor = new Gdk.Cursor(Gdk.CursorType.Watch);
  }
        protected bool OnDeleteEvent(object o, EventArgs args)
  {
   return true;
  }
        private void OnResetPassphraseLoad( object o, EventArgs args)
  {
   DomainController domainController = DomainController.GetDomainController();
   domains = domainController.GetLoggedInDomains();
   if( domains == null)
   {
     this.Respond( ResponseType.DeleteEvent);
     return;
   }
    string defaultDomainID = simws.GetDefaultDomainID();
    int defaultDomain = 0 ;
   for (int x = 0; x < domains.Length; x++)
   {
    domainComboBox.AppendText(domains[x].Name+"-"+domains[x].Host);
    if(defaultDomainID != null && defaultDomainID == domains[x].ID)
                                        defaultDomain = x;
   }
   if( domains.Length > 0)
    domainComboBox.Active = defaultDomain;
   DisplayRAList();
   UpdateUI();
  }
        private void OnDomainChangedEvent( object o, EventArgs args)
  {
   DisplayRAList();
   UpdateUI();
  }
  private void UpdateUI()
  {
                        if( ifws.GetSecurityPolicy(this.DomainID) != 0 && simws.IsPassPhraseSet(this.DomainID))
   {
    oldPassPhrase.Sensitive = newPassPhrase.Sensitive = retypePassPhrase.Sensitive = recoveryAgentCombo.Sensitive = savePassPhrase.Sensitive = true;
   }
   else
   {
    oldPassPhrase.Sensitive = newPassPhrase.Sensitive = retypePassPhrase.Sensitive = recoveryAgentCombo.Sensitive = savePassPhrase.Sensitive = false;
   }
  }
  private void DisplayRAList()
  {
  try
   {string domainID = this.Domain;
   if( RAList != null)
    for( int i=RAList.Length; i>=0; i--)
     recoveryAgentCombo.RemoveText(i);
   DomainController domController = DomainController.GetDomainController();
   Debug.PrintLine(string.Format("domain id is: {0}", domainID));
   RAList = domController.GetRAList(domainID);
   recoveryAgentCombo.AppendText(Util.GS("Server_Default"));
   if( RAList != null)
   {
               foreach (string raagent in RAList )
               {
                recoveryAgentCombo.AppendText(raagent);
               }
   }
   else
   {
    Debug.PrintLine("No recovery agent present");
   }
   recoveryAgentCombo.Active = 0;
   }
   catch(Exception e)
                        {
                                 Debug.PrintLine(String.Format("Server went down : {0},{1}",e.Message,e.StackTrace));
                        iFolderMsgDialog dialog =new iFolderMsgDialog(null,iFolderMsgDialog.DialogType.Error,
                                                                                iFolderMsgDialog.ButtonSet.None,
                                                                                Util.GS("Change Passphrase"),
                                                                                Util.GS("Unable to change the Passphrase"),
                                                                                Util.GS("Please try again"));
                                dialog.Run();
                                dialog.Hide();
                                dialog.Destroy();
                                dialog = null;
                        }
  }
        private void UpdateSensitivity( object o, EventArgs args)
  {
   if( oldPassPhrase != null && newPassPhrase != null && retypePassPhrase != null)
   {
    if( newPassPhrase.Text.Length >0 && newPassPhrase.Text == retypePassPhrase.Text)
    {
     if( oldPassPhrase.Text.Length > 0)
     {
      this.SetResponseSensitive( ResponseType.Ok, true);
      return;
     }
    }
   }
   this.SetResponseSensitive( ResponseType.Ok, false);
  }
        private void OnBannerExposed(object o, ExposeEventArgs args)
  {
   if(args.Event.Count > 0)
    return;
   Gdk.Pixbuf spb =
    ScaledPixbuf.ScaleSimple(iFolderScaledBanner.Allocation.Width,
          iFolderScaledBanner.Allocation.Height,
          Gdk.InterpType.Nearest);
   Gdk.GC gc = new Gdk.GC(iFolderScaledBanner.GdkWindow);
   spb.RenderToDrawable(iFolderScaledBanner.GdkWindow,
           gc,
           0, 0,
           args.Event.Area.X,
           args.Event.Area.Y,
           args.Event.Area.Width,
           args.Event.Area.Height,
           Gdk.RgbDither.Normal,
           0, 0);
  }
        private void OnFieldsChanged(object obj, EventArgs args)
  {
   bool enableOK = false;
   this.SetResponseSensitive(ResponseType.Ok, enableOK);
  }
 }
}
