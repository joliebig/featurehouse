

using Gtk;

namespace Novell.iFolder
{



 public class iFolderCrashDialog : Dialog
 {




  public iFolderCrashDialog(System.Exception e) : base()
  {
   this.SetDefaultSize (600, 400);
   this.Title = "";
   this.HasSeparator = false;

   this.Resizable = true;


   this.Icon = new Gdk.Pixbuf(Util.ImagesPath("ifolder-error16.png"));

   Image crashImage = new Image(Util.ImagesPath("ifolder-error48.png"));

   VBox vbox = new VBox();
   vbox.BorderWidth = 10;
   vbox.Spacing = 10;

   Label l = new Label("<span weight=\"bold\" size=\"larger\">" +
    Util.GS("iFolder crashed because of an unhandled exception") +
    "</span>");
   l.LineWrap = false;
   l.UseMarkup = true;
   l.Selectable = false;
   l.Xalign = 0; l.Yalign = 0;
   vbox.PackStart(l, false, false, 0);

   HBox h = new HBox();
   h.BorderWidth = 10;
   h.Spacing = 12;

   crashImage.SetAlignment(0.5F, 0);
   h.PackStart(crashImage, false, false, 0);

   TextView tv = new TextView();
   tv.WrapMode = Gtk.WrapMode.Word;
   tv.Editable = false;


   tv.Buffer.Text = e.ToString();
   ScrolledWindow sw = new ScrolledWindow();
   sw.ShadowType = Gtk.ShadowType.EtchedIn;
   sw.Add(tv);
   h.PackEnd(sw, true, true, 0);

   vbox.PackEnd(h);
   vbox.ShowAll();
   this.VBox.Add(vbox);

   this.AddButton(Stock.Close, ResponseType.Ok);
  }
 }
}
