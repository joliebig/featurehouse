

using Gtk;
using System;

namespace Novell.iFolder
{



 public class CertificateDialog : Dialog
 {
  private TextView certificate;
  private Image iFolderBanner;
  private Image iFolderScaledBanner;
  private Gdk.Pixbuf ScaledPixbuf;





  public CertificateDialog(string cert) : base()
   {

   SetupDialog(cert);
  }





  private void SetupDialog(string cert)
  {
   this.Title = Util.GS("Certificate");
   this.Icon = new Gdk.Pixbuf(Util.ImagesPath("ifolder16.png"));
   this.HasSeparator = false;

   this.SetDefaultSize (450, 300);

   this.Modal = true;
   this.DestroyWithParent = true;
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

   certificate = new TextView();
   certificate.Buffer.Text = cert;
   certificate.WrapMode = Gtk.WrapMode.Word;
   certificate.RightMargin = 5;
   certificate.LeftMargin =5;
   certificate.Editable = false;
   ScrolledWindow sw = new ScrolledWindow();
   sw.ShadowType = Gtk.ShadowType.EtchedIn;
   sw.Add(certificate);

   this.VBox.PackStart( sw, true, true, 0);
   Label lbl = new Label( "\n"+Util.GS("Click Ok to accept the certificate and Cancel to return"));
   lbl.Xalign = 0.0f;
   this.VBox.PackStart( lbl, false, true, 0);
   this.VBox.ShowAll();

   this.AddButton(Stock.Cancel, ResponseType.Cancel);
   this.AddButton(Stock.Ok, ResponseType.Ok);
   this.SetResponseSensitive(ResponseType.Ok, true);
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

 }
}
