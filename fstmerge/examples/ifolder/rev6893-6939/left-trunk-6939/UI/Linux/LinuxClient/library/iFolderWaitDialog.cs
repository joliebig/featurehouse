

using System;
using System.Threading;

using Gtk;

namespace Novell.iFolder
{




public class iFolderWaitDialog : Dialog
{
 private ButtonSet buttonSet;
 private ProgressBar progressBar;
 private Timer progressBarTimer;
 private bool bHideCalled;




 public enum ButtonSet : int
 {
  Cancel,
  None
 }
 public iFolderWaitDialog(Window parent,
        Widget waitingWidget,
        ButtonSet buttonSet,
        string title,
        string statement,
        string secondaryStatement)
  : base()
 {
  Init(parent, waitingWidget, buttonSet, title, statement, secondaryStatement);
 }
 internal void Init(Gtk.Window parent,
        Widget waitingWidget,
        ButtonSet buttonSet,
        string title,
        string statement,
        string secondaryStatement)
 {
  this.Title = title;
  this.HasSeparator = false;
  this.Resizable = false;
  this.Modal = true;
  if(parent != null)
   this.TransientFor = parent;
  this.buttonSet = buttonSet;
  VBox contentVBox = new VBox();
  this.VBox.Add(contentVBox);
  HBox h = new HBox();
  contentVBox.PackStart(h, true, true, 0);
  h.BorderWidth = 10;
  h.Spacing = 10;
  if (waitingWidget != null)
  {
   h.PackStart(waitingWidget, false, false, 0);
  }
  VBox v = new VBox();
  v.Spacing = 10;
  Label l = new Label();
  l.LineWrap = true;
  l.UseMarkup = true;
  l.Selectable = false;
  l.CanFocus = false;
  l.Xalign = 0; l.Yalign = 0;
  l.Markup = "<span weight=\"bold\" size=\"larger\">" + GLib.Markup.EscapeText(statement) + "</span>";
  v.PackStart(l);
  l = new Label(secondaryStatement);
  l.LineWrap = true;
  l.Selectable = false;
  l.CanFocus = false;
  l.Xalign = 0; l.Yalign = 0;
  v.PackStart(l, true, true, 8);
  h.PackEnd(v);
  progressBar = new ProgressBar();
  contentVBox.PackStart(progressBar, true, false, 8);
  progressBar.ActivityBlocks = 20;
  progressBar.Orientation = ProgressBarOrientation.LeftToRight;
  progressBar.PulseStep = 0.05;
  contentVBox.ShowAll();
  this.Realized += new EventHandler(OnRealizeWidget);
  switch(buttonSet)
  {
   case ButtonSet.Cancel:
    this.AddButton(Stock.Cancel, ResponseType.Cancel);
    break;
   case ButtonSet.None:
   default:
    break;
  }
  bHideCalled = false;
 }
 private void OnRealizeWidget(object o, EventArgs args)
 {
  progressBarTimer =
   new Timer(new TimerCallback(UpdateProgress),
      null,
      250,
      250);
 }
 private void UpdateProgress(object state)
 {
  if (!bHideCalled)
   GLib.Idle.Add(PulseProgressBar);
 }
 private bool PulseProgressBar()
 {
  if (!bHideCalled)
   progressBar.Pulse();
  return false;
 }
 protected override bool OnDeleteEvent(Gdk.Event evnt)
 {
  return true;
 }
 protected override bool OnDestroyEvent(Gdk.Event evnt)
 {
  if (progressBarTimer != null)
  {
   progressBarTimer.Dispose();
   progressBarTimer = null;
  }
  if (buttonSet == ButtonSet.None)
   return true;
  else
   return false;
 }
 protected override void OnHidden()
 {
  bHideCalled = true;
  if (progressBarTimer != null)
  {
   progressBarTimer.Dispose();
   progressBarTimer = null;
  }
 }
}
}
