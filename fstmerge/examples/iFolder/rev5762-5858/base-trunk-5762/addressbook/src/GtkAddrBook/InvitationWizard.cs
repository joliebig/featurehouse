using System;
using System.IO;
using System.Drawing;
using Simias.Sync;
using Simias.Invite;
using Gtk;
using Gdk;
using Glade;
using GtkSharp;
using GLib;
namespace Novell.iFolder
{
 public class InvitationWizard
 {
  [Glade.Widget] internal HBox WizardBox;
  [Glade.Widget] internal Button BackButton;
  [Glade.Widget] internal Button ForwardButton;
  [Glade.Widget] internal Button CancelButton;
  [Glade.Widget] internal HBox ButtonHBox;
  internal Gtk.Window win;
  internal Gtk.Widget welcomePage;
  internal Gtk.Widget acceptPage;
  internal Gtk.Widget loadPage;
  internal Gtk.Widget finalPage;
  internal Gtk.Button loadBrowseButton;
  internal Gtk.Entry loadPathEntry;
  internal Gtk.Button acceptBrowseButton;
  internal Gtk.Entry acceptPathEntry;
  internal Gtk.Label acceptiFolderName;
  internal Gtk.Label acceptSharerName;
  internal Gtk.Label acceptSharerEmail;
  internal Gtk.Label acceptRights;
  internal Gtk.Label finaliFolderName;
  internal Gtk.Label finalSharerName;
  internal Gtk.Label finalSharerEmail;
  internal Gtk.Label finalRights;
  internal Gtk.Label finalLocation;
  internal int page;
  internal string inviteFile;
  internal bool showLoadPage;
  internal Invitation invitation;
  public const int IW_WELCOME_PAGE = 0;
  public const int IW_LOAD_PAGE = 1;
  public const int IW_ACCEPT_PAGE = 2;
  public const int IW_FINAL_PAGE = 3;
  public event EventHandler WizardClosed;
  public InvitationWizard()
  {
   inviteFile = "";
   showLoadPage = true;
   InitWizardGUI();
  }
  public InvitationWizard(string inviteFile)
  {
   this.inviteFile = inviteFile;
   showLoadPage = false;
   InitWizardGUI();
  }
  private void InitWizardGUI()
  {
   Glade.XML mainXml =
     new Glade.XML (Util.GladePath("ifolder.glade"),
     "InviteWizard",
     null);
   mainXml.Autoconnect (this);
   win = (Gtk.Window) mainXml.GetWidget("InviteWizard");
   Glade.XML welcomeXml =
     new Glade.XML (Util.GladePath("ifolder.glade"),
     "WelcomePage",
     null);
   welcomePage = welcomeXml.GetWidget("WelcomePage");
   WizardBox.PackEnd(welcomePage);
   Glade.XML loadXml =
     new Glade.XML (Util.GladePath("ifolder.glade"),
     "LoadPage",
     null);
   loadPage = loadXml.GetWidget("LoadPage");
   loadBrowseButton = (Gtk.Button)
    loadXml.GetWidget("LoadBrowseButton");
   loadPathEntry = (Gtk.Entry) loadXml.GetWidget("LoadPathEntry");
   loadBrowseButton.Clicked += new
    EventHandler(on_load_browse_clicked);
   loadPathEntry.Changed += new EventHandler(on_load_path_changed);
   Glade.XML acceptXml =
     new Glade.XML (Util.GladePath("ifolder.glade"),
     "AcceptPage",
     null);
   acceptPage = acceptXml.GetWidget("AcceptPage");
   acceptBrowseButton = (Gtk.Button)
    acceptXml.GetWidget("AcceptBrowseButton");
   acceptPathEntry = (Gtk.Entry)
    acceptXml.GetWidget("AcceptPathEntry");
   acceptiFolderName = (Gtk.Label)
    acceptXml.GetWidget("AcceptIFName");
   acceptSharerName = (Gtk.Label)
    acceptXml.GetWidget("AcceptIFSender");
   acceptSharerEmail = (Gtk.Label)
    acceptXml.GetWidget("AcceptIFSenderEmail");
   acceptRights = (Gtk.Label)
    acceptXml.GetWidget("AcceptIFRights");
   acceptBrowseButton.Clicked += new
    EventHandler(on_accept_browse_clicked);
   acceptPathEntry.Changed += new
    EventHandler(on_accept_path_changed);
   Glade.XML finalXml =
     new Glade.XML (Util.GladePath("ifolder.glade"),
     "FinalPage",
     null);
   finalPage = finalXml.GetWidget("FinalPage");
   finaliFolderName = (Gtk.Label)
    finalXml.GetWidget("FinalIFName");
   finalSharerName = (Gtk.Label)
    finalXml.GetWidget("FinalIFSender");
   finalSharerEmail = (Gtk.Label)
    finalXml.GetWidget("FinalIFSenderEmail");
   finalRights = (Gtk.Label)
    finalXml.GetWidget("FinalIFRights");
   finalLocation = (Gtk.Label)
    finalXml.GetWidget("FinalIFLocation");
   page = IW_WELCOME_PAGE;
   BackButton.Sensitive = false;
  }
  public void ShowAll()
  {
   if(win != null)
   {
    win.ShowAll();
   }
  }
  private void on_close(object o, EventArgs args)
  {
   win.Hide();
   win.Destroy();
   win = null;
   if(WizardClosed != null)
   {
    EventArgs e = new EventArgs();
    WizardClosed(this, e);
   }
  }
  private void on_main_delete_event(object o, DeleteEventArgs args)
  {
   args.RetVal = true;
   on_close(o, args);
  }
  private void on_back_clicked(object o, EventArgs args)
  {
   switch(page)
   {
    case IW_LOAD_PAGE:
     {
      WizardBox.Remove(loadPage);
      WizardBox.PackEnd(welcomePage);
      BackButton.Sensitive = false;
      ForwardButton.Sensitive = true;
      page = IW_WELCOME_PAGE;
      break;
     }
    case IW_ACCEPT_PAGE:
     {
      WizardBox.Remove(acceptPage);
      if(showLoadPage)
      {
       MoveToLoadPage();
      }
      else
      {
       WizardBox.PackEnd(welcomePage);
       BackButton.Sensitive = false;
       ForwardButton.Sensitive = true;
       page = IW_WELCOME_PAGE;
      }
      break;
     }
   }
  }
  private void on_forward_clicked(object o, EventArgs args)
  {
   switch(page)
   {
    case IW_WELCOME_PAGE:
     {
      WizardBox.Remove(welcomePage);
      BackButton.Sensitive = true;
      if(showLoadPage)
      {
       MoveToLoadPage();
      }
      else
      {
       MoveToAcceptPage();
      }
      break;
     }
    case IW_LOAD_PAGE:
     {
      WizardBox.Remove(loadPage);
      inviteFile = loadPathEntry.Text;
      MoveToAcceptPage();
      break;
     }
    case IW_ACCEPT_PAGE:
     {
      WizardBox.Remove(acceptPage);
      MoveToFinalPage();
      break;
     }
   }
  }
  private void MoveToFinalPage()
  {
   iFolderManager manager = iFolderManager.Connect();
   if(manager == null)
   {
    Console.WriteLine("Unable to connect to iFolderManager");
    MoveToAcceptPage();
    return;
   }
   Console.WriteLine("Testing path :" + acceptPathEntry.Text);
   if(manager.IsPathIniFolder(acceptPathEntry.Text))
   {
    MessageDialog md = new MessageDialog(win,
       DialogFlags.DestroyWithParent | DialogFlags.Modal,
       MessageType.Error,
       ButtonsType.Close,
       "The location selected for the new iFolder is below an existing iFolder and cannot be used.  Please select a new location.");
    md.Run();
    md.Hide();
    MoveToAcceptPage();
    return;
   }
   Console.WriteLine("Accepting Invitation");
   try
   {
    manager.AcceptInvitation(invitation, acceptPathEntry.Text);
   }
   catch(Exception e)
   {
    Console.WriteLine("Accept failed, path in an existing iFolder");
    MessageDialog md = new MessageDialog(win,
       DialogFlags.DestroyWithParent | DialogFlags.Modal,
       MessageType.Error,
       ButtonsType.Close,
       "Unable to accept the invitation due to an error while processing the invitation.\n" + e);
    md.Run();
    md.Hide();
    MoveToAcceptPage();
    return;
   }
   Console.WriteLine("Displaying Results");
   WizardBox.PackEnd(finalPage);
   BackButton.Sensitive = false;
   ForwardButton.Sensitive = false;
   ButtonHBox.Remove(CancelButton);
   Button OKButton = new Button(Gtk.Stock.Ok);
   OKButton.Clicked += new EventHandler(on_cancel_clicked);
   ButtonHBox.PackEnd(OKButton, true, true, 0);
   ButtonHBox.ShowAll();
   page = IW_FINAL_PAGE;
   finaliFolderName.Text = invitation.CollectionName;
   finalSharerName.Text = invitation.FromName;
   finalSharerEmail.Text = invitation.FromEmail;
   finalRights.Text = invitation.CollectionRights;
   finalLocation.Text = Path.Combine(acceptPathEntry.Text, invitation.CollectionName);
  }
  private void MoveToAcceptPage()
  {
   invitation = new Invitation();
   try
   {
    invitation.Load(inviteFile);
   }
   catch(Exception e)
   {
    Console.WriteLine("Unable to load file: {0}", inviteFile);
    MessageDialog md = new MessageDialog(win,
           DialogFlags.DestroyWithParent | DialogFlags.Modal,
           MessageType.Error,
           ButtonsType.Close,
           "Unable to open file or file is not an iFolder Invitation:\n" + inviteFile);
    md.Run();
    md.Hide();
    MoveToLoadPage();
    return;
   }
   WizardBox.PackEnd(acceptPage);
   BackButton.Sensitive = true;
   if(acceptPathEntry.Text.Length > 0)
    ForwardButton.Sensitive = true;
   else
    ForwardButton.Sensitive = false;
   page = IW_ACCEPT_PAGE;
   acceptiFolderName.Text = invitation.CollectionName;
   acceptSharerName.Text = invitation.FromName;
   acceptSharerEmail.Text = invitation.FromEmail;
   acceptRights.Text = invitation.CollectionRights;
   if(acceptPathEntry.Text.Length < 1)
    acceptPathEntry.Text = Invitation.DefaultRootPath;
  }
  private void MoveToLoadPage()
  {
   showLoadPage = true;
   if(inviteFile.Length > 0)
    loadPathEntry.Text = inviteFile;
   WizardBox.PackEnd(loadPage);
   if(loadPathEntry.Text.Length > 0)
    ForwardButton.Sensitive = true;
   else
    ForwardButton.Sensitive = false;
   page = IW_LOAD_PAGE;
  }
  private void on_cancel_clicked(object o, EventArgs args)
  {
   on_close(o, args);
  }
  private void on_load_browse_clicked(object o, EventArgs args)
  {
   FileSelection fs = new FileSelection ("Choose an invitation file");
   fs.ShowFileops = false;
   if(loadPathEntry.Text.Length > 0)
   {
    fs.Filename = loadPathEntry.Text;
   }
   int rc = fs.Run ();
   if(rc == -5)
    loadPathEntry.Text = fs.Filename;
   fs.Hide ();
  }
  private void on_load_path_changed(object o, EventArgs args)
  {
   if(loadPathEntry.Text.Length > 0)
    ForwardButton.Sensitive = true;
   else
    ForwardButton.Sensitive = false;
  }
  private void on_accept_browse_clicked(object o, EventArgs args)
  {
   FileSelection fs = new FileSelection ("Choose a directory");
   fs.FileList.Parent.Hide();
   fs.SelectionEntry.Hide();
   fs.FileopDelFile.Hide();
   fs.FileopRenFile.Hide();
   if(acceptPathEntry.Text.Length > 0)
   {
    fs.Filename = acceptPathEntry.Text;
    fs.Filename += "/";
   }
   int rc = fs.Run ();
   if(rc == -5)
    acceptPathEntry.Text = fs.Filename;
   fs.Hide ();
  }
  private void on_accept_path_changed(object o, EventArgs args)
  {
   if(acceptPathEntry.Text.Length > 0)
    ForwardButton.Sensitive = true;
   else
    ForwardButton.Sensitive = false;
  }
 }
}
