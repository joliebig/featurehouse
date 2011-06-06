using Gtk;
using System;
using Novell.iFolder.Controller;
namespace Novell.iFolder
{
 public class ResetPasswordDialog : Dialog
 {
  private ComboBox domainComboBox;
  private Entry oldPassword;
  private Entry newPassword;
  private Entry confirmPassword;
  private DomainInformation[] domains;
  private CheckButton savePassword;
         private SimiasWebService simws;
  private iFolderWebService ifws;
  private Image iFolderBanner;
  private Image iFolderScaledBanner;
  private Gdk.Pixbuf ScaledPixbuf;
  private bool status;
  private int passwordChangeStatus;
  public int PasswordChangeStatus
  {
   get
   {
    return this.passwordChangeStatus;
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
  public string OldPassword
  {
   get
   {
    return oldPassword.Text;
   }
  }
  public string NewPassword
  {
   get
   {
    return newPassword.Text;
   }
  }
  public bool SavePassword
  {
   get
   {
    return savePassword.Active;
   }
  }
  public bool Status
  {
   get
   {
    return status;
   }
  }
  public ResetPasswordDialog(SimiasWebService simiasws, iFolderWebService ifws)
  {
   this.simws= simiasws;
   this.ifws = ifws;
   SetupDialog();
  }
  private void SetupDialog()
  {
   this.Title = Util.GS("Change password");
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
   Label lbl = new Label(Util.GS("iFolder Account")+":");
   table.Attach(lbl, 0,1, 0,1,
    AttachOptions.Fill | AttachOptions.Expand, 0,0,0);
   lbl.LineWrap = true;
   lbl.Xalign = 0.0F;
   domainComboBox = ComboBox.NewText();
    table.Attach(domainComboBox, 1,2, 0,1,
     AttachOptions.Expand | AttachOptions.Fill, 0,0,0);
   lbl = new Label(Util.GS("Current password")+":");
   table.Attach(lbl, 0,1, 1,2,
    AttachOptions.Fill | AttachOptions.Expand, 0,0,0);
   lbl.LineWrap = true;
   lbl.Xalign = 0.0F;
   oldPassword = new Entry();
   oldPassword.Visibility = false;
   table.Attach(oldPassword, 1,2, 1,2,
    AttachOptions.Fill | AttachOptions.Expand, 0,0,0);
   lbl.MnemonicWidget = oldPassword;
   oldPassword.Changed += new EventHandler(UpdateSensitivity);
   lbl = new Label(Util.GS("New password")+":");
   table.Attach(lbl, 0,1, 2,3,
    AttachOptions.Fill | AttachOptions.Expand, 0,0,0);
   lbl.LineWrap = true;
   lbl.Xalign = 0.0F;
   newPassword = new Entry();
   newPassword.Visibility = false;
   table.Attach(newPassword, 1,2, 2,3,
    AttachOptions.Fill | AttachOptions.Expand, 0,0,0);
   lbl.MnemonicWidget = newPassword;
   newPassword.Changed += new EventHandler(UpdateSensitivity);
   lbl = new Label(Util.GS("Confirm new password")+":");
   table.Attach(lbl, 0,1, 3,4,
    AttachOptions.Fill | AttachOptions.Expand, 0,0,0);
   lbl.LineWrap = true;
   lbl.Xalign = 0.0F;
   confirmPassword = new Entry();
   confirmPassword.Visibility = false;
   table.Attach(confirmPassword, 1,2, 3,4,
    AttachOptions.Fill | AttachOptions.Expand, 0,0,0);
   lbl.MnemonicWidget = confirmPassword;
   confirmPassword.Changed += new EventHandler(UpdateSensitivity);
   savePassword = new CheckButton(Util.GS("Remember password"));
   table.Attach(savePassword, 1,2,5,6, AttachOptions.Expand|AttachOptions.Fill, 0,0,0);
   this.VBox.ShowAll();
          Button helpbutton = (Button)this.AddButton(Stock.Help, ResponseType.Help);
   helpbutton.Sensitive = false;
   this.AddButton(Stock.Cancel, ResponseType.Cancel);
   Button but = (Button)this.AddButton(Util.GS("Reset"), ResponseType.Ok);
   but.Clicked += new EventHandler(OnResetClicked);
   but.Image = new Image(Stock.Undo, Gtk.IconSize.Menu);
   this.SetResponseSensitive(ResponseType.Ok, false);
   this.DefaultResponse = ResponseType.Ok;
   this.Realized += new EventHandler(OnResetPasswordLoad);
   domainComboBox.Changed += new EventHandler(OnDomainChangedEvent);
  }
  private void ResetPassword(string domainid, string oldpassword, string newpassword)
  {
   this.passwordChangeStatus = this.ifws.ChangePassword(domainid, oldpassword, newpassword);
   if( this.passwordChangeStatus == 0)
   {
    try
    {
     DomainController domainController = DomainController.GetDomainController();
     domainController.LogoutDomain(domainid);
    }
    catch{ }
    this.status = true;
   }
                                if( this.passwordChangeStatus != 0)
                                {
                                        string Message = Util.GS("Could not change password, ");
                                        switch(this.passwordChangeStatus)
                                        {
                                                case 1:
                                                        Message += Util.GS("Incorrect old password.");
                                                        break;
                                                case 2:
                                                        Message += Util.GS("Failed to reset password.");
                                                        break;
                                                case 3:
                                                        Message += Util.GS("Login disabled.");
                                                        break;
                                                case 4:
                                                        Message += Util.GS("User account expired.");
                                                        break;
                                                case 5:
                                                        Message += Util.GS("User can not change password.");
                                                        break;
                                                case 6:
                                                        Message += Util.GS("User password expired.");
                                                        break;
                                               case 7:
                                                        Message += Util.GS("Minimum password length restriction not met.");
                                                        break;
                                                case 8:
                                                        Message += Util.GS("User not found in simias.");
                                                        break;
                                                default:
                                                        Message = "Error while changing the password.";
                                                        break;
                                        }
                                        iFolderMsgDialog dialog = new iFolderMsgDialog(
                                                                                                                null,
                                                                                                                iFolderMsgDialog.DialogType.Error,
                                                                                                                iFolderMsgDialog.ButtonSet.None,
                                                                                                                Util.GS("Change password"),
                                                                                                                Message, null);
                                        dialog.Run();
                                        dialog.Hide();
                                        dialog.Destroy();
                                        dialog = null;
    }
  }
  private void OnResetClicked( object o, EventArgs args)
  {
   if( newPassword.Text == oldPassword.Text )
   {
    string Message = Util.GS("Old password and new password should not be same.");
                                        iFolderMsgDialog dialog = new iFolderMsgDialog(
                                                                                                                null,
                                                                                                                iFolderMsgDialog.DialogType.Error,
                                                                                                                iFolderMsgDialog.ButtonSet.None,
                                                                                                                Util.GS("Error changing password"),
                                                                                                                Message, null);
                                        dialog.Run();
                                        dialog.Hide();
                                        dialog.Destroy();
                                        dialog = null;
    return;
   }
   else if(newPassword.Text != confirmPassword.Text)
   {
     string Message = Util.GS("New password and confirm password do not match.");
                                        iFolderMsgDialog dialog = new iFolderMsgDialog(
                                                                                                                null,
                                                                                                                iFolderMsgDialog.DialogType.Error,
                                                                                                                iFolderMsgDialog.ButtonSet.None,
                                                                                                                Util.GS("Error changing password"),
                                                                                                                Message, null);
                                        dialog.Run();
                                        dialog.Hide();
                                        dialog.Destroy();
                                        dialog = null;
    return;
   }
   ResetPassword( this.Domain, this.OldPassword, this.NewPassword);
  }
  protected bool OnDeleteEvent(object o, EventArgs args)
  {
   return true;
  }
  private void OnResetPasswordLoad( object o, EventArgs args)
  {
   DomainController domainController = DomainController.GetDomainController();
   domains = domainController.GetLoggedInDomains();
   if( domains == null)
   {
     this.Respond( ResponseType.DeleteEvent);
     return;
   }
   for (int x = 0; x < domains.Length; x++)
   {
    domainComboBox.AppendText(domains[x].Name);
   }
   if( domains.Length > 0)
    domainComboBox.Active = 0;
   oldPassword.Sensitive = newPassword.Sensitive = confirmPassword.Sensitive = savePassword.Sensitive = true;
  }
  private void OnDomainChangedEvent( object o, EventArgs args)
  {
  }
  private void UpdateSensitivity( object o, EventArgs args)
  {
   if( oldPassword != null && newPassword != null && confirmPassword != null)
   {
    if( newPassword.Text.Length > 0 && confirmPassword.Text.Length > 0 && oldPassword.Text.Length > 0)
    {
     if( oldPassword.Text.Length > 0)
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
