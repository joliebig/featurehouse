

using System;
using System.Collections;
using Gtk;

using Simias.Client;

namespace Novell.iFolder
{




 public class iFolderPropertiesDialog : Dialog
 {
  private iFolderWebService ifws;
  private SimiasWebService simws;
  private iFolderWeb ifolder;
  private Gtk.Notebook propNoteBook;

  private HBox ConflictBox;
  private HBox ConflictHolder;
  private iFolderConflictDialog ConflictDialog;
  private iFolderPropSharingPage SharingPage;
  private iFolderPropSettingsPage SettingsPage;
  private bool ControlKeyPressed;
  private Manager simiasManager;




  public int CurrentPage
  {
   set
   {
    if(value <= propNoteBook.NPages)
     propNoteBook.CurrentPage = value;
   }
   get
   {
    return propNoteBook.CurrentPage;
   }
  }




  public iFolderWeb iFolder
  {
   get
   {
    return ifolder;
   }
  }






  public iFolderPropertiesDialog( Gtk.Window parent,
          iFolderWeb ifolder,
          iFolderWebService iFolderWS,
          SimiasWebService SimiasWS,
          Manager simiasManager)
   : base()
  {
   if(iFolderWS == null)
    throw new ApplicationException("iFolderWebService was null");
   this.ifws = iFolderWS;
   if(SimiasWS == null)
    throw new ApplicationException("SimiasWebService was null");
   this.simws = SimiasWS;
   this.simiasManager = simiasManager;



   try
   {
    this.ifolder = this.ifws.GetiFolder(ifolder.ID);
   }
   catch(Exception e)
   {
    throw new ApplicationException(
      "Unable to read the iFolder");
   }

   this.Modal = false;
   this.TypeHint = Gdk.WindowTypeHint.Normal;

   this.HasSeparator = false;
   this.Title =
    string.Format("{0} {1}",
         ifolder.Name,
         Util.GS("Properties"));



   InitializeWidgets();

   SetValues();


   ControlKeyPressed = false;
   KeyPressEvent += new KeyPressEventHandler(KeyPressHandler);
   KeyReleaseEvent += new KeyReleaseEventHandler(KeyReleaseHandler);
  }







  public iFolderPropertiesDialog( string ifolderID, Manager manager )
   : base()
  {
   if (manager == null) return;
   this.simiasManager = manager;

   String localServiceUrl = simiasManager.WebServiceUri.ToString();
   if (localServiceUrl == null) return;

   this.ifws = new iFolderWebService();
   if(this.ifws == null)
    throw new ApplicationException(
       "Unable to obtain iFolderWebService");
   this.ifws.Url = localServiceUrl + "/iFolder.asmx";
   LocalService.Start(this.ifws, simiasManager.WebServiceUri, simiasManager.DataPath);

   this.simws = new SimiasWebService();
   if (this.simws == null)
    throw new ApplicationException(
       "Unable to obtain SimiasWebService");
   this.simws.Url = localServiceUrl + "/Simias.asmx";
   LocalService.Start(this.simws, simiasManager.WebServiceUri, simiasManager.DataPath);

   try
   {
    this.ifolder = this.ifws.GetiFolder(ifolderID);
   }
   catch(Exception e)
   {
    throw new ApplicationException(
      "Unable to read the iFolder");
   }

   this.HasSeparator = false;
   this.Modal = true;
   this.Title = Util.GS("iFolder Properties");



   InitializeWidgets();
   SetValues();


   ControlKeyPressed = false;
   KeyPressEvent += new KeyPressEventHandler(KeyPressHandler);
   KeyReleaseEvent += new KeyReleaseEventHandler(KeyReleaseHandler);
  }





  public void UpdateiFolder(iFolderWeb theiFolder)
  {
   SettingsPage.UpdateiFolder(theiFolder);
   SharingPage.UpdateiFolder(theiFolder);
  }







  private void InitializeWidgets()
  {
   VBox dialogBox = new VBox();
   this.VBox.PackStart(dialogBox);
   dialogBox.BorderWidth = 10;
   dialogBox.Spacing = 10;

   this.SetDefaultSize (480, 480);
   this.Icon = new Gdk.Pixbuf(Util.ImagesPath("ifolder16.png"));




   ConflictHolder = new HBox();
   dialogBox.PackStart(ConflictHolder, false, true, 0);




   propNoteBook = new Gtk.Notebook();

   SettingsPage = new iFolderPropSettingsPage(this, ifws);

   propNoteBook.AppendPage(SettingsPage,
        new Label(Util.GS("_General")));

   SharingPage = new iFolderPropSharingPage(this, ifws, simws);

   propNoteBook.AppendPage(SharingPage,
        new Label(Util.GS("_Sharing")));

   dialogBox.PackStart(propNoteBook);

   this.VBox.ShowAll();

   this.AddButton(Stock.Close, ResponseType.Ok);
   this.AddButton(Stock.Help, ResponseType.Help);
  }






  private void SetValues()
  {
   if(!ifolder.HasConflicts)
   {
    if(ConflictBox != null)
     ConflictBox.Visible = false;
   }
   else
   {
    if(ConflictBox == null)
    {
     ConflictBox = new HBox();
     ConflictBox.Spacing = 5;
     ConflictBox.BorderWidth = 10;

     Gdk.Pixbuf conPix = new Gdk.Pixbuf(
        Util.ImagesPath("ifolder-warning22.png"));
     Image conImage = new Image(conPix);

     conImage.SetAlignment(0.5F, 0);
     ConflictBox.PackStart(conImage, false, false, 0);

     Gtk.Label l = new Label("<span weight=\"bold\">" +
        Util.GS("This iFolder contains conflicts.") +
        "</span>");
     l.LineWrap = true;
     l.Xalign = 0;
     l.UseMarkup = true;
     ConflictBox.PackStart(l, true, true, 0);

     Button resButton = new Button(Util.GS("_Resolve conflicts"));
     ConflictBox.PackStart(resButton, false, false, 0);
     resButton.Clicked += new EventHandler(OnResolveConflicts);


     ConflictHolder.PackStart(ConflictBox, false, true, 10);
     ConflictBox.ShowAll();
    }
    else
     ConflictBox.Visible = true;
   }

   UpdateiFolder(ifolder);
  }




        void KeyPressHandler(object o, KeyPressEventArgs args)
  {
   args.RetVal = true;

   switch(args.Event.Key)
   {
    case Gdk.Key.Escape:
     Respond(ResponseType.Cancel);
     break;
    case Gdk.Key.Control_L:
    case Gdk.Key.Control_R:
     ControlKeyPressed = true;
     args.RetVal = false;
     break;
    case Gdk.Key.W:
    case Gdk.Key.w:
     if (ControlKeyPressed)
      Respond(ResponseType.Cancel);
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




  private void OnResolveConflicts(object o, EventArgs args)
  {
   ConflictDialog = new iFolderConflictDialog(
          this,
          ifolder,
          ifws,
          simws);
   ConflictDialog.Response +=
      new ResponseHandler(OnConflictDialogResponse);
   ConflictDialog.ShowAll();
  }





  private void OnConflictDialogResponse(object o, ResponseArgs args)
  {
   if(ConflictDialog != null)
   {
    if (args.ResponseId == ResponseType.Help)
     Util.ShowHelp("conflicts.html", this);
    else
    {
     ConflictDialog.Hide();
     ConflictDialog.Destroy();
     ConflictDialog = null;
    }
   }



  }
 }
}
