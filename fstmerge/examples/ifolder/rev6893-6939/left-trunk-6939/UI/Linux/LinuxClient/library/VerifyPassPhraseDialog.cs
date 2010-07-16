

using Gtk;
using System;

namespace Novell.iFolder
{



 public class VerifyPassPhraseDialog : Dialog
 {
  private Entry passPhraseEntry;
  private CheckButton savePassPhraseButton;
  private Image iFolderBanner;
  private Image iFolderScaledBanner;
  private Gdk.Pixbuf ScaledPixbuf;




  public string PassPhrase
  {
   get
   {
    return passPhraseEntry.Text;
   }
  }




  public bool ShouldSavePassPhrase
  {
   get
   {
    if (savePassPhraseButton != null)
     return savePassPhraseButton.Active;
    else
     return false;

   }
  }




  public VerifyPassPhraseDialog() : base()
   {

   SetupDialog();
  }




  private void SetupDialog()
  {
   this.Title = Util.GS("iFolder PassPhrase");
   this.Icon = new Gdk.Pixbuf(Util.ImagesPath("ifolder16.png"));
   this.HasSeparator = false;

   this.SetDefaultSize (350, 100);

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

   Table loginTable;

   loginTable = new Table(4,2,false);

   loginTable.BorderWidth = 10;
   loginTable.RowSpacing = 10;
   loginTable.ColumnSpacing = 10;
   loginTable.Homogeneous = false;

   Label passPhraseLabel = new Label(Util.GS("Enter passphrase")+":");
   passPhraseLabel.Xalign = 1;
   loginTable.Attach( passPhraseLabel, 0,1,0,2, AttachOptions.Shrink | AttachOptions.Fill, 0,0,0);
   passPhraseEntry = new Entry();
   passPhraseEntry.ActivatesDefault = true;
   passPhraseEntry.Visibility = false;
   passPhraseEntry.Changed += new EventHandler(OnPassPhraseChanged);
   loginTable.Attach(passPhraseEntry, 1,2,0,2, AttachOptions.Fill | AttachOptions.Expand, 0,0,0);
    savePassPhraseButton =
     new CheckButton(Util.GS(
      "_Remember passphrase"));
    loginTable.Attach(savePassPhraseButton, 1,2,2,4,
      AttachOptions.Fill | AttachOptions.Expand, 0,0,0);

   this.VBox.PackStart(loginTable, false, false, 0);
   this.VBox.ShowAll();

   this.AddButton(Stock.Cancel, ResponseType.Cancel);
   this.AddButton(Stock.Ok, ResponseType.Ok);
   this.SetResponseSensitive(ResponseType.Ok, false);
   this.DefaultResponse = ResponseType.Ok;
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




        private void OnPassPhraseChanged(object obj, EventArgs args)
  {
   bool enableOK = false;

   if(passPhraseEntry.Text.Length > 0)
    enableOK = true;
   this.SetResponseSensitive(ResponseType.Ok, enableOK);
  }


 }
}
