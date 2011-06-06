using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.IO;
using Eraser.Util;
using ProgressChangedEventArgs = System.ComponentModel.ProgressChangedEventArgs;
using EraserProgressChangedEventArgs = Eraser.Util.ProgressChangedEventArgs;
namespace Eraser
{
 public partial class BlackBoxUploadForm : Form
 {
  public BlackBoxUploadForm(IList<BlackBoxReport> reports)
  {
   InitializeComponent();
   Theming.ApplyTheme(this);
   UploadWorker.RunWorkerAsync(reports);
  }
  private void BlackBoxUploadForm_FormClosing(object sender, FormClosingEventArgs e)
  {
   if (UploadWorker.IsBusy)
   {
    UploadWorker.CancelAsync();
    e.Cancel = true;
   }
  }
  private void UploadWorker_DoWork(object sender, DoWorkEventArgs e)
  {
   IList<BlackBoxReport> reports = (IList<BlackBoxReport>)e.Argument;
   SteppedProgressManager overallProgress = new SteppedProgressManager();
   for (int i = 0; i < reports.Count; ++i)
   {
    ProgressManager reportProgress = new ProgressManager();
    overallProgress.Steps.Add(new SteppedProgressManagerStep(reportProgress,
     1.0f / reports.Count));
    BlackBoxReportUploader uploader = new BlackBoxReportUploader(reports[i]);
    UploadWorker.ReportProgress((int)(overallProgress.Progress * 100),
     S._("Checking for status of report {0}...", reports[i].Name));
    if (!uploader.IsNew)
     continue;
    if (UploadWorker.CancellationPending)
     throw new OperationCanceledException();
    UploadWorker.ReportProgress((int)(overallProgress.Progress * 100),
     S._("Compressing Report {0}: {1:#0.00%}", reports[i].Name, 0));
    uploader.Submit(delegate(object from, EraserProgressChangedEventArgs e2)
     {
      reportProgress.Completed = (int)(e2.Progress.Progress * reportProgress.Total);
      SteppedProgressManager reportSteps = (SteppedProgressManager)e2.Progress;
      int step = reportSteps.Steps.IndexOf(reportSteps.CurrentStep);
      UploadWorker.ReportProgress((int)overallProgress.Progress,
       step == 0 ?
        S._("Compressing Report {0}: {1:#0.00%}",
         reports[i].Name, reportSteps.Progress) :
        S._("Uploading Report {0}: {1:#0.00%}",
         reports[i].Name, reportSteps.Progress));
      if (UploadWorker.CancellationPending)
       throw new OperationCanceledException();
     });
   }
  }
  private void UploadWorker_ProgressChanged(object sender, ProgressChangedEventArgs e)
  {
   if (e.UserState != null)
    ProgressLbl.Text = (string)e.UserState;
   ProgressPb.Value = e.ProgressPercentage;
  }
  private void UploadWorker_RunWorkerCompleted(object sender, RunWorkerCompletedEventArgs e)
  {
   if (e.Error == null)
   {
    ProgressLbl.Text = S._("Reports submitted successfully.");
    ProgressPb.Value = ProgressPb.Maximum;
    CancelBtn.Text = S._("Close");
   }
   else if (e.Error is OperationCanceledException)
   {
    ProgressLbl.Text = S._("Submission was cancelled.");
    ProgressPb.Value = ProgressPb.Maximum;
    CancelBtn.Text = S._("Close");
   }
   else
   {
    MessageBox.Show(this, e.Error.Message,
     S._("Eraser"), MessageBoxButtons.OK, MessageBoxIcon.Error,
     MessageBoxDefaultButton.Button1, Localisation.IsRightToLeft(this) ?
      MessageBoxOptions.RtlReading | MessageBoxOptions.RightAlign : 0);
    Close();
   }
  }
  private void CancelBtn_Click(object sender, EventArgs e)
  {
   if (UploadWorker.IsBusy)
    UploadWorker.CancelAsync();
   else
    Close();
  }
 }
}
