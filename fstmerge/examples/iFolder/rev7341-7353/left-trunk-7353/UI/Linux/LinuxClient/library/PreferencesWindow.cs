using System;
using System.IO;
using System.Collections;
using Gtk;
using Simias.Client;
using Simias.Client.Event;
namespace Novell.iFolder
{
 public class PreferencesWindow : Window
 {
  private iFolderWebService ifws;
  private Gtk.Notebook PrefNoteBook;
  private PrefsGeneralPage generalPage;
  private PrefsAccountsPage accountsPage;
  private PrefsSettingPage settingPage;
  private MigrationPage migrationPage;
  private bool ControlKeyPressed;
  public int CurrentPage
  {
   set
   {
    if(value <= PrefNoteBook.NPages)
     PrefNoteBook.CurrentPage = value;
   }
   get
   {
    return PrefNoteBook.CurrentPage;
   }
  }
  public PreferencesWindow(iFolderWebService webService, Manager simiasManager)
   : base(Util.GS("iFolder Preferences"))
  {
   if(webService == null)
    throw new ApplicationException("iFolderWebServices was null");
   ifws = webService;
   InitializeWidgets(simiasManager);
   ControlKeyPressed = false;
   KeyPressEvent += new KeyPressEventHandler(KeyPressHandler);
   KeyReleaseEvent += new KeyReleaseEventHandler(KeyReleaseHandler);
  }
  private void InitializeWidgets(Manager simiasManager)
  {
   this.SetDefaultSize (480, 550);
   EventBox prefsWindow = new EventBox();
   prefsWindow.ModifyBg(StateType.Normal, this.Style.Background(StateType.Normal));
   VBox winBox = new VBox();
   prefsWindow.Add(winBox);
   this.Add (prefsWindow);
   winBox.BorderWidth = 7;
   winBox.Spacing = 7;
   this.Icon = new Gdk.Pixbuf(Util.ImagesPath("ifolder16.png"));
   this.WindowPosition = Gtk.WindowPosition.Center;
   PrefNoteBook = new Notebook();
   generalPage = new PrefsGeneralPage(this, ifws);
   PrefNoteBook.AppendPage( generalPage,
          new Label(Util.GS("General")));
   accountsPage = new PrefsAccountsPage(this);
   PrefNoteBook.AppendPage( accountsPage,
          new Label(Util.GS("Accounts")));
               new MigrationPage(this, ifws);
   settingPage = new PrefsSettingPage(this);
   PrefNoteBook.AppendPage( settingPage, new Label(Util.GS("Settings")));
   PrefNoteBook.SwitchPage +=
    new SwitchPageHandler(OnSwitchPageEvent);
   winBox.PackStart(PrefNoteBook, true, true, 0);
   HButtonBox buttonBox = new HButtonBox();
   buttonBox.BorderWidth = 10;
   buttonBox.Spacing = 10;
   buttonBox.Layout = ButtonBoxStyle.Edge;
   winBox.PackStart(buttonBox, false, false, 0);
   Button helpButton = new Button(Gtk.Stock.Help);
   buttonBox.PackStart(helpButton);
   helpButton.Clicked += new EventHandler(HelpEventHandler);
   Button closeButton = new Button(Gtk.Stock.Close);
   buttonBox.PackStart(closeButton);
   closeButton.Clicked += new EventHandler(CloseEventHandler);
  }
  void KeyPressHandler(object o, KeyPressEventArgs args)
  {
   args.RetVal = true;
   switch(args.Event.Key)
   {
    case Gdk.Key.Escape:
     CloseWindow();
     break;
    case Gdk.Key.Control_L:
    case Gdk.Key.Control_R:
     ControlKeyPressed = true;
     args.RetVal = false;
     break;
    case Gdk.Key.W:
    case Gdk.Key.w:
     if (ControlKeyPressed)
      CloseWindow();
     else
      args.RetVal = false;
     break;
    default:
     args.RetVal = false;
     break;
   }
  }
        void KeyReleaseHandler(object o, KeyReleaseEventArgs args)
  {
   args.RetVal = false;
   switch(args.Event.Key)
   {
    case Gdk.Key.Control_L:
    case Gdk.Key.Control_R:
     ControlKeyPressed = false;
     break;
    default:
     break;
   }
  }
        private void OnSwitchPageEvent(object o, SwitchPageArgs args)
  {
   if (args.PageNum != 0)
    generalPage.LeavingGeneralPage();
  }
        private void HelpEventHandler(object o, EventArgs args)
  {
   if (CurrentPage == 0)
   {
    Util.ShowHelp("preferences.html", this);
   }
   else if (CurrentPage == 1)
   {
    Util.ShowHelp("accounts.html", this);
   }
   else if (CurrentPage == 2)
   {
    Util.ShowHelp("settings.html", this);
   }
   else
   {
    Util.ShowHelp(Util.HelpMainPage, this);
   }
  }
  private void CloseEventHandler(object o, EventArgs args)
  {
   settingPage.GetSelectedRow();
   CloseWindow();
  }
  private void CloseWindow()
  {
   if (CurrentPage == 0)
    generalPage.LeavingGeneralPage();
   this.Hide();
   this.Destroy();
  }
  public void UpdateDomainStatus(string domainID)
  {
   accountsPage.UpdateDomainStatus(domainID);
  }
  public void ToggelDomain(DomainInformation domainInfo,bool flag)
  {
   accountsPage.ToggelDomainState(domainInfo, flag);
  }
  public void ShowClientUpgradeMessage()
  {
   accountsPage. ShowClientUpgradeMessageBox();
  }
 }
}
