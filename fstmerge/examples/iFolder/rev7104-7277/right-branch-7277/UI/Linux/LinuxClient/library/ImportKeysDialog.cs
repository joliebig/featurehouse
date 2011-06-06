using Gtk;
using System;
using Novell.iFolder.Controller;
namespace Novell.iFolder
{
 public class ImportKeysDialog : Dialog
 {
  private ComboBox domainCombo;
  private Button BrowseButton;
  private Entry location;
  private Entry oneTimePassphrase;
  private Entry passPhrase;
  private Entry retypePassPhrase;
  private DomainInformation[] domains;
  private iFolderWebService ifws;
  private SimiasWebService simws;
  private Image iFolderBanner;
  private Image iFolderScaledBanner;
  private Gdk.Pixbuf ScaledPixbuf;
  public string FileName
  {
   get
   {
    return this.location.Text;
   }
  }
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
  public string Domain
  {
   get
   {
    if( domains != null)
     return domains[domainCombo.Active].ID;
    else
     return null;
   }
  }
  public string OneTimePP
  {
   get
   {
    if( this.oneTimePassphrase.Text != null)
     return this.oneTimePassphrase.Text;
    else
     return null;
   }
  }
  public string PassPhrase
  {
   get
   {
    return this.passPhrase.Text;
   }
  }
  public ImportKeysDialog(iFolderWebService ifws, SimiasWebService simws) : base()
  {
   this.ifws = ifws;
   this.simws = simws;
   SetupDialog();
  }
  private void SetupDialog()
  {
   this.Title = Util.GS("Import Decrypted Keys");
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
                        Table table = new Table(5, 3, false);
                        table.ColumnSpacing = 6;
                        table.RowSpacing = 6;
                        table.BorderWidth = 12;
   Label l = new Label(Util.GS("iFolder account")+":");
   table.Attach(l, 0,1, 0,1, AttachOptions.Fill | AttachOptions.Expand, 0,0,0);
                        l.LineWrap = true;
   l.Xalign = 0.0F;
   domainCombo = ComboBox.NewText();
   table.Attach(domainCombo, 1,2,0,1, AttachOptions.Fill|AttachOptions.Expand, 0,0,0);
   l.MnemonicWidget = domainCombo;
   domainCombo.Changed += new EventHandler(OnDomainChangedEvent);
                        l = new Label(Util.GS("File path")+":");
                        table.Attach(l, 0,1, 1,2,
                                AttachOptions.Fill | AttachOptions.Expand, 0,0,0);
                        l.LineWrap = true;
                        l.Xalign = 0.0F;
                        location = new Entry();
   this.location.Changed += new EventHandler(OnFieldsChanged);
                        table.Attach(location, 1,2, 1,2,
                                AttachOptions.Expand | AttachOptions.Fill, 0,0,0);
                        l.MnemonicWidget = location;
                        BrowseButton = new Button(Util.GS("_Browse"));
                        table.Attach(BrowseButton, 2,3, 1,2, AttachOptions.Fill, 0,0,0);
                        BrowseButton.Sensitive = true;
   BrowseButton.Clicked += new EventHandler(OnBrowseButtonClicked);
                        l = new Label(Util.GS("One time passphrase")+":");
   l.Xalign = 0.0F;
                        table.Attach(l, 0,1, 2,3,
                                AttachOptions.Fill, 0,0,0);
                        oneTimePassphrase = new Entry();
   oneTimePassphrase.Visibility = false;
   oneTimePassphrase.Changed += new EventHandler(OnFieldsChanged);
                        table.Attach(oneTimePassphrase, 1,2, 2,3,
                                AttachOptions.Expand | AttachOptions.Fill, 0,0,0);
                        l.MnemonicWidget = oneTimePassphrase;
                        l = new Label(Util.GS("New passphrase")+":");
   l.Xalign = 0.0F;
                        table.Attach(l, 0,1, 3,4,
                                AttachOptions.Fill | AttachOptions.Expand, 0,0,0);
                        passPhrase = new Entry();
   passPhrase.Visibility = false;
   passPhrase.Changed += new EventHandler(OnFieldsChanged);
                        table.Attach(passPhrase, 1,2, 3,4,
                                AttachOptions.Expand | AttachOptions.Fill, 0,0,0);
                        l.MnemonicWidget = passPhrase;
                 l = new Label(Util.GS("Re-type passphrase")+":");
   l.Xalign = 0.0F;
                        table.Attach(l, 0,1, 4,5,
                                AttachOptions.Fill | AttachOptions.Expand, 0,0,0);
                        retypePassPhrase = new Entry();
   retypePassPhrase.Visibility = false;
   retypePassPhrase.Changed += new EventHandler(OnFieldsChanged);
                        table.Attach(retypePassPhrase, 1,2, 4,5,
                                AttachOptions.Expand | AttachOptions.Fill, 0,0,0);
                        l.MnemonicWidget = retypePassPhrase;
   this.VBox.PackStart(table, false, false, 0);
   this.VBox.ShowAll();
   this.AddButton(Stock.Help, ResponseType.Help);
   this.AddButton(Stock.Cancel, ResponseType.Cancel);
   this.AddButton(Stock.Ok, ResponseType.Ok);
   this.SetResponseSensitive(ResponseType.Ok, false);
   this.DefaultResponse = ResponseType.Ok;
   this.Realized += new EventHandler(OnImportKeysLoad);
  }
        private void OnDomainChangedEvent( object o, EventArgs args)
                {
                        UpdateUI();
                }
  private void UpdateUI()
  {
   if( ifws.GetSecurityPolicy(Domain) != 0 && simws.IsPassPhraseSet(Domain))
    BrowseButton.Sensitive = location.Sensitive = oneTimePassphrase.Sensitive = retypePassPhrase.Sensitive = passPhrase.Sensitive = true;
   else
    BrowseButton.Sensitive = location.Sensitive = oneTimePassphrase.Sensitive = retypePassPhrase.Sensitive = passPhrase.Sensitive = false;
  }
        private void OnImportKeysLoad(object o, EventArgs args)
  {
   DomainController domainController = DomainController.GetDomainController();
   domains = null;
   domains = domainController.GetLoggedInDomains();
   if( domains == null)
    return;
   for (int x = 0; x < domains.Length; x++)
   {
    domainCombo.AppendText(domains[x].Name);
   }
   if( domains.Length > 0)
    domainCombo.Active = 0;
   UpdateUI();
  }
        private void OnBrowseButtonClicked(object o, EventArgs e)
  {
   FileChooserDialog filedlg = new FileChooserDialog("", Util.GS("Select a folder..."), this, FileChooserAction.Open, Stock.Cancel, ResponseType.Cancel,Stock.Ok, ResponseType.Ok);
   int res = filedlg.Run();
   string str = filedlg.Filename;
   filedlg.Hide();
   filedlg.Destroy();
   if( res == (int)ResponseType.Ok)
   {
    this.location.Text = str;
   }
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
   if( this.passPhrase.Text.Length > 0 && this.passPhrase.Text == this.retypePassPhrase.Text )
    if( this.location.Text.Length > 0)
     enableOK= true;
   this.SetResponseSensitive(ResponseType.Ok, enableOK);
  }
 }
}
