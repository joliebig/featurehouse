using System;
using Gtk;
using Simias.Client;
using Novell.iFolder.Controller;
namespace Novell.iFolder
{
 public class iFolderPropSettingsPage : VBox
 {
  private Gtk.Window topLevelWindow;
  private iFolderWebService ifws;
  private DomainInformation domain;
  private iFolderWeb ifolder;
  private DiskSpace ds;
  private Table BasicTable;
  private Label NameLabel;
  private Label OwnerLabel;
  private Label LocationLabel;
  private Label AccountLabel;
  private Label LastSuccessfulSync;
  private Label FFSyncValue;
  private Label SyncIntervalValue;
  private Label UsedValue;
  private Table diskTable;
  private Label LimitLabel;
  private CheckButton LimitCheckButton;
  private CheckButton SecureSync;
  private Label LimitValue;
  private Entry LimitEntry;
  private Label LimitUnit;
  private Label AvailLabel;
  private Label AvailValue;
  private Label AvailUnit;
  private ProgressBar DiskUsageBar;
  private Frame DiskUsageFrame;
  private Label DiskUsageFullLabel;
  private Label DiskUsageEmptyLabel;
  private Button SyncNowButton;
  public iFolderPropSettingsPage( Gtk.Window topWindow,
          iFolderWebService iFolderWS)
   : base()
  {
   this.topLevelWindow = topWindow;
   this.ifws = iFolderWS;
   InitializeWidgets();
  }
  public void UpdateiFolder(iFolderWeb ifolder)
  {
   DomainController domainController = DomainController.GetDomainController();
   this.domain = domainController.GetDomain(ifolder.DomainID);
   this.ifolder = ifolder;
   if (ifolder.LastSyncTime == null || ifolder.LastSyncTime == "")
    LastSuccessfulSync.Text = Util.GS("N/A");
   else
    LastSuccessfulSync.Text = ifolder.LastSyncTime;
   FFSyncValue.Text = Util.GS("0");
   int syncInterval = 0;
   if (ifolder.EffectiveSyncInterval <= 0)
   {
    try
    {
     syncInterval = ifws.GetDefaultSyncInterval();
    }
    catch
    {}
   }
   else
   {
    syncInterval = ifolder.EffectiveSyncInterval;
   }
   if (syncInterval > 0)
   {
    syncInterval = syncInterval / 60;
   }
   SyncIntervalValue.Text = syncInterval + " " + Util.GS("minute(s)");
   NameLabel.Markup = string.Format("<span weight=\"bold\">{0}</span>", GLib.Markup.EscapeText(ifolder.Name));
   OwnerLabel.Markup = string.Format("<span size=\"small\">{0}</span>", GLib.Markup.EscapeText(ifolder.Owner));
   LocationLabel.Markup = string.Format("<span size=\"small\">{0}</span>", GLib.Markup.EscapeText(ifolder.UnManagedPath));
   AccountLabel.Markup = string.Format("<span size=\"small\">{0}</span>", GLib.Markup.EscapeText(domain.Name));
   try
   {
    SyncSize ss = ifws.CalculateSyncSize(ifolder.ID);
    FFSyncValue.Text = string.Format("{0}", ss.SyncNodeCount);
   }
   catch(Exception e)
   {
    FFSyncValue.Text = Util.GS("N/A");
   }
   try
   {
    ds = ifws.GetiFolderDiskSpace(ifolder.ID);
   }
   catch(Exception e)
   {
    ds = null;
   }
    if(LimitLabel == null)
    {
     LimitLabel = new Label(Util.GS("Quota:"));
     LimitLabel.Xalign = 0;
     diskTable.Attach(LimitLabel, 0,1,1,2,
      AttachOptions.Expand | AttachOptions.Fill, 0,0,0);
     LimitValue = new Label(Util.GS("0"));
     LimitValue.Xalign = 1;
     diskTable.Attach(LimitValue, 1,2,1,2,
      AttachOptions.Shrink | AttachOptions.Fill, 0,0,0);
     LimitLabel.ShowAll();
     LimitValue.ShowAll();
    }
    else
    {
     LimitLabel.Visible = true;
     LimitValue.Visible = true;
    }
    if(LimitCheckButton != null)
    {
     LimitCheckButton.Visible = false;
     LimitEntry.Visible = false;
    }
   if(ds != null)
   {
    int tmpValue;
    if(ds.Limit == 0)
    {
     LimitUnit.Sensitive = false;
     AvailLabel.Sensitive = false;
     AvailValue.Sensitive = false;
     AvailUnit.Sensitive = false;
     DiskUsageBar.Sensitive = false;
     DiskUsageFrame.Sensitive = false;
     DiskUsageFullLabel.Sensitive = false;
     DiskUsageEmptyLabel.Sensitive = false;
     if(LimitCheckButton != null)
     {
      LimitCheckButton.Active = false;
      LimitEntry.Sensitive = false;
      LimitEntry.Text = Util.GS("0");
     }
     if(LimitLabel != null)
     {
      LimitLabel.Sensitive = false;
      LimitValue.Sensitive = false;
      LimitValue.Text = Util.GS("0");
     }
     AvailValue.Text = Util.GS("0");
    }
    else
    {
     LimitUnit.Sensitive = true;
     AvailLabel.Sensitive = true;
     AvailValue.Sensitive = true;
     AvailUnit.Sensitive = true;
     DiskUsageBar.Sensitive = true;
     DiskUsageFrame.Sensitive = true;
     DiskUsageFullLabel.Sensitive = true;
     DiskUsageEmptyLabel.Sensitive = true;
     if(LimitCheckButton != null)
     {
      LimitCheckButton.Active = true;
      LimitEntry.Sensitive = true;
      tmpValue = (int)(ds.Limit / (1024 * 1024));
      LimitEntry.Text = string.Format("{0}", tmpValue);
     }
     if(LimitLabel != null)
     {
      LimitLabel.Sensitive = true;
      LimitValue.Sensitive = true;
      tmpValue = (int)(ds.Limit / (1024 * 1024));
      LimitValue.Text = string.Format("{0}", tmpValue);
     }
     tmpValue = (int)(ds.AvailableSpace / (1024 * 1024));
     AvailValue.Text = string.Format("{0}",tmpValue);
    }
    SetGraph(ds.UsedSpace, ds.Limit);
    if(ds.UsedSpace == 0)
    {
     UsedValue.Text = Util.GS("0");
    }
    else
    {
     tmpValue = (int)(ds.UsedSpace / (1024 * 1024)) + 1;
     UsedValue.Text = string.Format("{0}", tmpValue);
    }
   }
   SecureSync.Active = ifolder.ssl;
                        if(ifolder.CurrentUserID != ifolder.OwnerID)
    SecureSync.Sensitive = false;
   if( domain.HostUrl.StartsWith( System.Uri.UriSchemeHttps ) )
   {
    SecureSync.Active = true;
                                SecureSync.Sensitive = false;
   }
  }
  private void InitializeWidgets()
  {
   this.Spacing = 10;
   this.BorderWidth = Util.DefaultBorderWidth;
   HBox basicBox = new HBox();
   basicBox.Spacing = 10;
   this.PackStart(basicBox, false, true, 0);
   Gdk.Pixbuf iFolderPixbuf = new Gdk.Pixbuf(Util.ImagesPath("ifolder48.png"));
   Image iFolderImage = new Image(iFolderPixbuf);
   iFolderImage.SetAlignment(0.5F, 0);
   basicBox.PackStart(iFolderImage, false, false, 0);
   VBox basicLabelsBox = new VBox();
   basicLabelsBox.Spacing = 5;
   basicBox.PackStart(basicLabelsBox, false, true, 0);
   NameLabel = new Label("");
   NameLabel.UseMarkup = true;
   NameLabel.UseUnderline = false;
   NameLabel.Xalign = 0;
   basicLabelsBox.PackStart(NameLabel, false, true, 5);
   BasicTable = new Table(3, 2, false);
   basicLabelsBox.PackStart(BasicTable, true, true, 0);
   BasicTable.ColumnSpacing = 5;
   BasicTable.RowSpacing = 5;
   Label label = new Label(string.Format("<span size=\"small\">{0}</span>", GLib.Markup.EscapeText(Util.GS("Owner:"))));
   label.UseMarkup = true;
   label.Xalign = 0;
   BasicTable.Attach(label, 0, 1, 0, 1,
     AttachOptions.Shrink | AttachOptions.Fill, 0, 0, 0);
   OwnerLabel = new Label("");
   OwnerLabel.UseMarkup = true;
   OwnerLabel.UseUnderline = false;
   OwnerLabel.Xalign = 0;
   BasicTable.Attach(OwnerLabel, 1, 2, 0, 1,
     AttachOptions.Expand | AttachOptions.Fill, 0, 0, 0);
   label = new Label(string.Format("<span size=\"small\">{0}</span>", GLib.Markup.EscapeText(Util.GS("Location:"))));
   label.UseMarkup = true;
   label.Xalign = 0;
   BasicTable.Attach(label, 0, 1, 1, 2,
     AttachOptions.Shrink | AttachOptions.Fill, 0, 0, 0);
   LocationLabel = new Label("");
   LocationLabel.UseMarkup = true;
   LocationLabel.UseUnderline = false;
   LocationLabel.Xalign = 0;
   BasicTable.Attach(LocationLabel, 1, 2, 1, 2,
     AttachOptions.Expand | AttachOptions.Fill, 0, 0, 0);
   label = new Label(string.Format("<span size=\"small\">{0}</span>", GLib.Markup.EscapeText(Util.GS("Account:"))));
   label.UseMarkup = true;
   label.Xalign = 0;
   BasicTable.Attach(label, 0, 1, 2, 3,
     AttachOptions.Shrink | AttachOptions.Fill, 0, 0, 0);
   AccountLabel = new Label("");
   AccountLabel.UseMarkup = true;
   AccountLabel.UseUnderline = false;
   AccountLabel.Xalign = 0;
   BasicTable.Attach(AccountLabel, 1, 2, 2, 3,
     AttachOptions.Expand | AttachOptions.Fill, 0, 0, 0);
   VBox diskSectionBox = new VBox();
   diskSectionBox.Spacing = Util.SectionTitleSpacing;
   this.PackStart(diskSectionBox, false, true, 0);
   Label diskSectionLabel = new Label("<span weight=\"bold\">" +
            Util.GS("Disk Space on Server") +
            "</span>");
   diskSectionLabel.UseMarkup = true;
   diskSectionLabel.Xalign = 0;
   diskSectionBox.PackStart(diskSectionLabel, false, true, 0);
   HBox diskSpacerBox = new HBox();
   diskSpacerBox.Spacing = 10;
   diskSectionBox.PackStart(diskSpacerBox, true, true, 0);
   Label diskSpaceLabel = new Label("");
   diskSpacerBox.PackStart(diskSpaceLabel, false, true, 0);
   diskTable = new Table(3,3,false);
   diskSpacerBox.PackStart(diskTable, true, true, 0);
   diskTable.ColumnSpacing = 20;
   diskTable.RowSpacing = 5;
   Label usedLabel = new Label(Util.GS("Used:"));
   usedLabel.Xalign = 0;
   diskTable.Attach(usedLabel, 0,1,0,1,
     AttachOptions.Expand | AttachOptions.Fill, 0,0,0);
   UsedValue = new Label(Util.GS("0"));
   UsedValue.Xalign = 1;
   diskTable.Attach(UsedValue, 1,2,0,1,
     AttachOptions.Shrink | AttachOptions.Fill, 0,0,0);
   Label usedUnit = new Label(Util.GS("MB"));
   diskTable.Attach(usedUnit, 2,3,0,1,
     AttachOptions.Shrink | AttachOptions.Fill, 0,0,0);
   LimitUnit = new Label(Util.GS("MB"));
   diskTable.Attach(LimitUnit, 2,3,1,2,
     AttachOptions.Shrink | AttachOptions.Fill, 0,0,0);
   AvailLabel = new Label(Util.GS("Available:"));
   AvailLabel.Xalign = 0;
   diskTable.Attach(AvailLabel, 0,1,2,3,
     AttachOptions.Expand | AttachOptions.Fill, 0,0,0);
   AvailValue = new Label(Util.GS("0"));
   AvailValue.Xalign = 1;
   diskTable.Attach(AvailValue, 1,2,2,3,
     AttachOptions.Shrink | AttachOptions.Fill, 0,0,0);
   AvailUnit = new Label(Util.GS("MB"));
   diskTable.Attach(AvailUnit, 2,3,2,3,
     AttachOptions.Shrink | AttachOptions.Fill, 0,0,0);
   DiskUsageFrame = new Frame();
   diskSpacerBox.PackStart(DiskUsageFrame, false, true, 0);
   HBox graphBox = new HBox();
   graphBox.Spacing = 5;
   graphBox.BorderWidth = 5;
   DiskUsageFrame.Add(graphBox);
   DiskUsageBar = new ProgressBar();
   graphBox.PackStart(DiskUsageBar, false, true, 0);
   DiskUsageBar.Orientation = Gtk.ProgressBarOrientation.BottomToTop;
   DiskUsageBar.Fraction = 0;
   VBox graphLabelBox = new VBox();
   graphBox.PackStart(graphLabelBox, false, true, 0);
   DiskUsageFullLabel = new Label(Util.GS("full"));
   DiskUsageFullLabel.Xalign = 0;
   DiskUsageFullLabel.Yalign = 0;
   graphLabelBox.PackStart(DiskUsageFullLabel, true, true, 0);
   DiskUsageEmptyLabel = new Label(Util.GS("empty"));
   DiskUsageEmptyLabel.Xalign = 0;
   DiskUsageEmptyLabel.Yalign = 1;
   graphLabelBox.PackStart(DiskUsageEmptyLabel, true, true, 0);
   VBox syncSectionBox = new VBox();
   syncSectionBox.Spacing = Util.SectionTitleSpacing;
   this.PackStart(syncSectionBox, false, true, 0);
   Label syncSectionLabel = new Label("<span weight=\"bold\">" +
            Util.GS("Synchronization") +
            "</span>");
   syncSectionLabel.UseMarkup = true;
   syncSectionLabel.Xalign = 0;
   syncSectionBox.PackStart(syncSectionLabel, false, true, 0);
   HBox syncSpacerBox = new HBox();
   syncSpacerBox.Spacing = 10;
   syncSectionBox.PackStart(syncSpacerBox, true, true, 0);
   Label srvSpaceLabel = new Label("");
   syncSpacerBox.PackStart(srvSpaceLabel, false, true, 0);
   VBox syncWidgetBox = new VBox();
   syncSpacerBox.PackStart(syncWidgetBox, true, true, 0);
   syncWidgetBox.Spacing = 10;
   Table syncTable = new Table(3,2,false);
   syncWidgetBox.PackStart(syncTable, true, true, 0);
   syncTable.Homogeneous = false;
   syncTable.ColumnSpacing = 20;
   syncTable.RowSpacing = 5;
   Label lastSyncLabel = new Label(Util.GS("Last successful synchronization:"));
   lastSyncLabel.Xalign = 0;
   syncTable.Attach(lastSyncLabel, 0,1,0,1,
     AttachOptions.Shrink | AttachOptions.Fill, 0,0,0);
   LastSuccessfulSync = new Label(Util.GS("N/A"));
   LastSuccessfulSync.Xalign = 0;
   syncTable.Attach(LastSuccessfulSync, 1,2,0,1);
   Label FFSyncLabel =
     new Label(Util.GS("Files/Folders to synchronize:"));
   FFSyncLabel.Xalign = 0;
   syncTable.Attach(FFSyncLabel, 0,1,1,2,
     AttachOptions.Shrink | AttachOptions.Fill, 0,0,0);
   FFSyncValue = new Label(Util.GS("0"));
   FFSyncValue.Xalign = 0;
   syncTable.Attach(FFSyncValue, 1,2,1,2);
   Label SyncIntervalLabel =
    new Label(Util.GS("Automatically synchronizes every:"));
   SyncIntervalLabel.Xalign = 0;
   syncTable.Attach(SyncIntervalLabel, 0,1,2,3,
    AttachOptions.Shrink | AttachOptions.Fill, 0,0,0);
   SyncIntervalValue = new Label("1 minute(s)");
   SyncIntervalValue.Xalign = 0;
   syncTable.Attach(SyncIntervalValue, 1,2,2,3);
   Label SecureSyncLabel =
                                new Label(Util.GS("Secure sync:"));
                        SecureSyncLabel.Xalign = 0;
                        syncTable.Attach(SecureSyncLabel, 0,1,3,4,
                                AttachOptions.Shrink | AttachOptions.Fill, 0,0,0);
                        SecureSync = new CheckButton("");
   SecureSync.Toggled +=
                                                                new EventHandler(OnSecureSync);
                        syncTable.Attach(SecureSync, 1,2,3,4);
   HBox rightBox = new HBox();
   rightBox.Spacing = 10;
   syncWidgetBox.PackEnd(rightBox, false, false, 0);
   SyncNowButton = new Button(Util.GS("Synchronize _Now"));
   rightBox.PackEnd(SyncNowButton, false, false, 0);
   SyncNowButton.Clicked += new EventHandler(OnSyncNowClicked);
  }
  private void OnSecureSync(object o, EventArgs args)
  {
   bool oldvalue = ifolder.ssl;
   if( SecureSync.Active != oldvalue )
   {
    SaveSsl(ifolder.ID , SecureSync.Active);
   }
  }
  private void OnLimitSizeButton(object o, EventArgs args)
  {
   if(LimitCheckButton.Active == true)
   {
    LimitUnit.Sensitive = true;
    AvailLabel.Sensitive = true;
    AvailValue.Sensitive = true;
    AvailUnit.Sensitive = true;
    LimitEntry.Sensitive = true;
    DiskUsageBar.Sensitive = true;
    DiskUsageFrame.Sensitive = true;
    DiskUsageFullLabel.Sensitive = true;
    DiskUsageEmptyLabel.Sensitive = true;
   }
   else
   {
    LimitUnit.Sensitive = false;
    AvailLabel.Sensitive = false;
    AvailValue.Sensitive = false;
    AvailUnit.Sensitive = false;
    DiskUsageBar.Sensitive = false;
    DiskUsageFrame.Sensitive = false;
    DiskUsageFullLabel.Sensitive = false;
    DiskUsageEmptyLabel.Sensitive = false;
    LimitEntry.Sensitive = false;
    LimitEntry.Text = Util.GS("0");
    if(GetCurrentLimit() != ds.Limit)
    {
     SaveLimit();
    }
   }
  }
  private void OnLimitChanged(object o, EventArgs args)
  {
   int tmpValue;
   long sizeLimit = GetCurrentLimit();
   if(sizeLimit == 0)
   {
    AvailValue.Text = Util.GS("0");
   }
   else
   {
    long result = sizeLimit - ds.UsedSpace;
    if(result < 0)
     tmpValue = 0;
    else
    {
     tmpValue = (int)(result / (1024 * 1024));
    }
    AvailValue.Text = string.Format("{0}",tmpValue);
   }
   SetGraph(ds.UsedSpace, sizeLimit);
  }
  private void OnLimitEdited(object o, EventArgs args)
  {
   SaveLimit();
  }
  private void OnLimitFocusLost(object o, FocusOutEventArgs args)
  {
   SaveLimit();
  }
  private void OnSyncNowClicked(object o, EventArgs args)
  {
   try
   {
    ifws.SynciFolderNow(ifolder.ID);
   }
   catch(Exception e)
   {
    iFolderExceptionDialog ied = new iFolderExceptionDialog(
            topLevelWindow, e);
    ied.Run();
    ied.Hide();
    ied.Destroy();
    return;
   }
  }
  private void SaveSsl(string ifolderID, bool ssl)
  {
   ifws.SetiFolderSecureSync(ifolderID, ssl);
  }
  private void SaveLimit()
  {
   long sizeLimit = GetCurrentLimit();
    try
   {
    ifws.SetiFolderDiskSpaceLimit(ifolder.ID, sizeLimit);
   }
   catch(Exception e)
   {
    iFolderExceptionDialog ied = new iFolderExceptionDialog(
            topLevelWindow, e);
    ied.Run();
    ied.Hide();
    ied.Destroy();
    return;
   }
  }
  private long GetCurrentLimit()
  {
   long sizeLimit;
   if(LimitEntry.Text.Length == 0)
    sizeLimit = 0;
   else
   {
    try
    {
     sizeLimit = (long)System.UInt64.Parse(LimitEntry.Text);
    }
    catch(Exception e)
    {
     sizeLimit = 0;
    }
   }
   sizeLimit = sizeLimit * 1024 * 1024;
   return sizeLimit;
  }
  private void SetGraph(long usedSpace, long limit)
  {
   if(limit == 0)
   {
    DiskUsageBar.Fraction = 0;
    return;
   }
   if(limit < usedSpace)
    DiskUsageBar.Fraction = 1;
   else
    DiskUsageBar.Fraction = ((double)usedSpace) /
          ((double)limit);
  }
 }
}
