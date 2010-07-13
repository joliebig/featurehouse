

using System;
using System.Diagnostics;
using System.IO;
using System.Net;
using System.Text;
using System.Windows.Forms;
using ProcessHacker.Common;
using ProcessHacker.Common.Threading;
using ProcessHacker.Components;
using ProcessHacker.Native;
using TaskbarLib;

namespace ProcessHacker
{
    public partial class VirusTotalUploaderWindow : Form
    {
        string fileName;
        string processName;

        long totalFileSize;
        long bytesPerSecond;
        long bytesTransferred;
        Stopwatch uploadStopwatch;

        ThreadTask uploadTask;

        public VirusTotalUploaderWindow(string procName, string procPath)
        {
            this.SetPhParent();
            InitializeComponent();
            this.AddEscapeToClose();
            this.SetTopMost();

            processName = procName;
            fileName = procPath;

            this.Icon = Program.HackerWindow.Icon;
        }

        private void VirusTotalUploaderWindow_Load(object sender, EventArgs e)
        {
            labelFile.Text = string.Format("Uploading: {0}", processName);

            FileInfo finfo = new FileInfo(fileName);
            if (!finfo.Exists)
            {
                if (OSVersion.HasTaskDialogs)
                {
                    TaskDialog td = new TaskDialog();
                    td.PositionRelativeToWindow = true;
                    td.Content = "The selected file doesn't exist or couldnt be found!";
                    td.MainInstruction = "File Location not Available!";
                    td.WindowTitle = "System Error";
                    td.MainIcon = TaskDialogIcon.CircleX;
                    td.CommonButtons = TaskDialogCommonButtons.Ok;
                    td.Show(Program.HackerWindow.Handle);
                }
                else
                {
                    MessageBox.Show(
                       this, "The selected file doesn't exist or couldnt be found!",
                       "System Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation
                       );
                }

                this.Close();
            }
            else if (finfo.Length >= 20971520 )
            {
                if (OSVersion.HasTaskDialogs)
                {
                    TaskDialog td = new TaskDialog();
                    td.PositionRelativeToWindow = true;
                    td.Content = "This file is larger than 20MB, above the VirusTotal limit!";
                    td.MainInstruction = "File is too large";
                    td.WindowTitle = "VirusTotal Error";
                    td.MainIcon = TaskDialogIcon.CircleX;
                    td.CommonButtons = TaskDialogCommonButtons.Ok;
                    td.Show(Program.HackerWindow.Handle);
                }
                else
                {
                     MessageBox.Show(
                        this, "This file is larger than 20MB and is above the VirusTotal size limit!",
                        "VirusTotal Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation
                        );
                }

                this.Close();
            }
            else
            {
                totalFileSize = finfo.Length;
            }

            uploadedLabel.Text = "Uploaded: Initializing";
            speedLabel.Text = "Speed: Initializing";

            ThreadTask getSessionTokenTask = new ThreadTask();

            getSessionTokenTask.RunTask += new ThreadTaskRunTaskDelegate(getSessionTokenTask_RunTask);
            getSessionTokenTask.Completed += new ThreadTaskCompletedDelegate(getSessionTokenTask_Completed);
            getSessionTokenTask.Start();
        }

        private void VirusTotalUploaderWindow_FormClosing(object sender, FormClosingEventArgs e)
        {
            if (uploadTask != null)
                uploadTask.Cancel();

            if (OSVersion.HasExtendedTaskbar)
            {
                Windows7Taskbar.SetTaskbarProgressState(
                    Program.HackerWindowHandle,
                    Windows7Taskbar.ThumbnailProgressState.NoProgress
                    );
            }
        }

        private void getSessionTokenTask_RunTask(object param, ref object result)
        {
            try
            {
                HttpWebRequest sessionRequest = (HttpWebRequest)HttpWebRequest.Create("http://www.virustotal.com/vt/en/identificador");
                sessionRequest.ServicePoint.ConnectionLimit = 20;
                sessionRequest.UserAgent = "Process Hacker " + Application.ProductVersion;
                sessionRequest.Timeout = System.Threading.Timeout.Infinite;
                sessionRequest.KeepAlive = true;

                using (WebResponse Response = sessionRequest.GetResponse())
                using (Stream WebStream = Response.GetResponseStream())
                using (StreamReader Reader = new StreamReader(WebStream))
                {
                    result = Reader.ReadToEnd();
                }
            }
            catch (Exception ex)
            {
                PhUtils.ShowException("Unable to contact VirusTotal", ex);

                if (this.IsHandleCreated)
                    this.BeginInvoke(new MethodInvoker(this.Close));
            }
        }

        private void getSessionTokenTask_Completed(object result)
        {
            if (result != null)
            {
                uploadTask = new ThreadTask();
                uploadTask.RunTask += uploadTask_RunTask;
                uploadTask.Completed += uploadTask_Completed;
                uploadTask.Start(result);
            }
        }

        private void uploadTask_RunTask(object param, ref object result)
        {
            string boundary = "----------" + DateTime.Now.Ticks.ToString("x");

            HttpWebRequest uploadRequest = (HttpWebRequest)WebRequest.Create(
                "http://www.virustotal.com/vt/en/recepcionf?" + (string)param);
            uploadRequest.ServicePoint.ConnectionLimit = 20;
            uploadRequest.UserAgent = "ProcessHacker " + Application.ProductVersion;
            uploadRequest.ContentType = "multipart/form-data; boundary=" + boundary;
            uploadRequest.Timeout = System.Threading.Timeout.Infinite;
            uploadRequest.KeepAlive = true;
            uploadRequest.Method = WebRequestMethods.Http.Post;


            StringBuilder sb = new StringBuilder();
            sb.Append("--");
            sb.Append(boundary);
            sb.Append("\r\n");
            sb.Append(@"Content-Disposition: form-data; name=""archivo""; filename=" + processName + "");
            sb.Append("\r\n");
            sb.Append("Content-Type: application/octet-stream");
            sb.Append("\r\n");
            sb.Append("\r\n");

            string postHeader = sb.ToString();
            byte[] postHeaderBytes = Encoding.UTF8.GetBytes(postHeader);



            byte[] boundaryBytes = Encoding.ASCII.GetBytes("\r\n--" + boundary + "\r\n");

            if (uploadTask.Cancelled)
            {
                uploadRequest.Abort();
                return;
            }

            try
            {
                uploadStopwatch = new Stopwatch();
                uploadStopwatch.Start();

                using (FileStream fileStream = new FileStream(fileName, FileMode.Open, FileAccess.Read))
                {
                    uploadRequest.ContentLength = postHeaderBytes.Length + fileStream.Length + boundaryBytes.Length;

                    using (Stream requestStream = uploadRequest.GetRequestStream())
                    {

                        requestStream.Write(postHeaderBytes, 0, postHeaderBytes.Length);

                        byte[] buffer = new Byte[checked((uint)Math.Min(32, (int)fileStream.Length))];

                        int bytesRead = 0;

                        while ((bytesRead = fileStream.Read(buffer, 0, buffer.Length)) != 0)
                        {
                            if (uploadTask.Cancelled)
                            {
                                uploadRequest.Abort();
                                return;
                            }

                            requestStream.Write(buffer, 0, bytesRead);

                            int progress = (int)(((double)fileStream.Position * 100 / fileStream.Length));

                            if (uploadStopwatch.ElapsedMilliseconds > 0)
                                bytesPerSecond = fileStream.Position * 1000 / uploadStopwatch.ElapsedMilliseconds;

                            bytesTransferred = fileStream.Position;

                            if (this.IsHandleCreated)
                                this.BeginInvoke(new Action<int>(this.ChangeProgress), progress);
                        }

                        if (uploadTask.Cancelled)
                        {
                            uploadRequest.Abort();
                            return;
                        }


                        requestStream.Write(boundaryBytes, 0, boundaryBytes.Length);

                        requestStream.Close();
                    }
                }
            }
            catch (WebException ex)
            {


                if (ex != null)
                {
                    if (ex.Status != WebExceptionStatus.RequestCanceled)
                    {
                        PhUtils.ShowException("Unable to upload the file", ex);
                        Logging.Log(ex);

                        if (this.IsHandleCreated)
                            this.BeginInvoke(new MethodInvoker(this.Close));
                    }
                }
            }

            if (uploadTask.Cancelled)
            {
                uploadRequest.Abort();
                return;
            }

            WebResponse response = uploadRequest.GetResponse();






            result = response.ResponseUri.AbsoluteUri;
        }

        private void ChangeProgress(int progress)
        {
            uploadedLabel.Text = "Uploaded: " + Utils.FormatSize(bytesTransferred) +
                          " (" + ((double)bytesTransferred * 100 / totalFileSize).ToString("F2") + "%)";
            totalSizeLabel.Text = "Total Size: " + Utils.FormatSize(totalFileSize);
            speedLabel.Text = "Speed: " + Utils.FormatSize(bytesPerSecond) + "/s";
            progressUpload.Value = progress;

            if (OSVersion.HasExtendedTaskbar)
                Windows7Taskbar.SetTaskbarProgress(Program.HackerWindow, this.progressUpload);
        }

        private void uploadTask_Completed(object result)
        {
            if (this.InvokeRequired)
            {
                this.BeginInvoke(new ThreadTaskCompletedDelegate(uploadTask_Completed), result);
                return;
            }






            var webException = uploadTask.Exception as WebException;

            if (webException != null && webException.Status != WebExceptionStatus.Success)
            {
                if (webException.Status != WebExceptionStatus.RequestCanceled)
                {
                    PhUtils.ShowException("Unable to upload the file", webException);
                    this.Close();
                }
            }
            else if (result != null && !uploadTask.Cancelled)
            {
                Program.TryStart(result.ToString());
            }

            this.Close();
        }

        private void buttonCancel_Click(object sender, EventArgs e)
        {
            this.Close();
        }
    }
}
