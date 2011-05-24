


using System;
using Gtk;
using Simias.Client;

namespace Novell.iFolder
{
 public class RemoveAccountDialog : Dialog
 {
  private CheckButton cbutton;

  public bool RemoveiFoldersFromServer
  {
   get
   {
    return cbutton.Active;
   }
  }

  public RemoveAccountDialog(DomainInformation domainInfo) : base()
  {
   this.Title = "";
   this.Resizable = false;
   this.HasSeparator = false;

   HBox h = new HBox();
   h.BorderWidth = 10;
   h.Spacing = 10;

   Image i = new Image();
   i.SetFromStock(Gtk.Stock.DialogQuestion, IconSize.Dialog);

   i.SetAlignment(0.5F, 0);
   h.PackStart(i, false, false, 0);

   VBox v = new VBox();
   Label l = new Label("<span weight=\"bold\" size=\"larger\">" +
     Util.GS("Remove this iFolder account?") + "</span>");
   l.LineWrap = true;
   l.UseMarkup = true;
   l.Selectable = false;
   l.Xalign = 0; l.Yalign = 0;
   v.PackStart(l, false, false, 10);

   Table table = new Table(3, 2, false);

   table.RowSpacing = 0;
   table.ColumnSpacing = 10;
   table.Homogeneous = false;




   l = new Label(Util.GS("System Name:"));
   l.Xalign = 1;
   table.Attach(l, 0,1, 0,1,
       AttachOptions.Shrink | AttachOptions.Fill, 0,0,0);

   l = new Label(domainInfo.Name);
   l.UseUnderline = false;
   l.Xalign = 0;
   table.Attach(l, 1,2, 0,1,
       AttachOptions.Fill | AttachOptions.Expand, 0,0,0);




   l = new Label(Util.GS("Server:"));
   l.Xalign = 1;
   table.Attach(l, 0,1, 1,2,
       AttachOptions.Shrink | AttachOptions.Fill, 0,0,0);

   l = new Label(domainInfo.Host);
   l.Xalign = 0;
   table.Attach(l, 1,2, 1,2,
       AttachOptions.Fill | AttachOptions.Expand, 0,0,0);




   l = new Label(Util.GS("Username:"));
   l.Xalign = 1;
   table.Attach(l, 0,1, 2,3,
       AttachOptions.Shrink | AttachOptions.Fill, 0,0,0);

   l = new Label(domainInfo.MemberName);
   l.UseUnderline = false;
   l.Xalign = 0;
   table.Attach(l, 1,2, 2,3,
       AttachOptions.Fill | AttachOptions.Expand, 0,0,0);

   v.PackEnd(table, true, true, 0);

   h.PackEnd(v);

   this.VBox.PackStart(h, true, true, 0);

   cbutton = new CheckButton(Util.GS("_Remove my iFolders and files from the server"));
   this.VBox.PackStart(cbutton, false, false, 10);

   cbutton.Toggled +=
    new EventHandler(OnRemoveiFoldersToggled);



   if (!domainInfo.Active || !domainInfo.Authenticated)
   {
    cbutton.Sensitive = false;
   }

   this.VBox.ShowAll();



   Button noButton = new Button(Stock.No);
   noButton.CanFocus = true;
   noButton.CanDefault = true;
   noButton.ShowAll();

   this.AddActionWidget(noButton, ResponseType.No);
   this.AddButton(Stock.Yes, ResponseType.Yes);

   this.DefaultResponse = ResponseType.No;

   this.FocusChild = noButton;
  }

  private void OnRemoveiFoldersToggled(object obj, EventArgs args)
  {



   if (cbutton.Active)
   {
    iFolderMsgDialog dialog = new iFolderMsgDialog(
     this,
     iFolderMsgDialog.DialogType.Warning,
     iFolderMsgDialog.ButtonSet.OkCancel,
     "",
     Util.GS("Removing iFolders from Server"),
     Util.GS("Removing iFolders from the server will delete the files stored on the server.  Your files will remain intact on this computer.\n\nIf you've shared any iFolders with other users, they will no longer be able to synchronize them with the server.  Additionally, you will no longer be able to access these iFolders from any other computer."));
    int rc = dialog.Run();
    dialog.Hide();
    dialog.Destroy();

    if ((ResponseType)rc == ResponseType.Cancel)
    {

     cbutton.Active = false;
    }
   }
  }
 }
}
