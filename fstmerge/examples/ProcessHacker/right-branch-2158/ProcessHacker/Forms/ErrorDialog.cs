

using System;
using System.Collections.Specialized;
using System.Net;
using System.Text.RegularExpressions;
using System.Windows.Forms;
using ProcessHacker.Common;
using ProcessHacker.Native.Api;

namespace ProcessHacker
{
    public partial class ErrorDialog : Form
    {
        private Exception _exception;
        private string _trackerItem;
        private bool _isTerminating;

        public ErrorDialog(Exception ex, bool terminating)
        {
            InitializeComponent();

            _exception = ex;
            _isTerminating = terminating;

            textException.AppendText(_exception.ToString());

            if (_isTerminating)
                buttonContinue.Enabled = false;

            textException.AppendText("\r\n\r\nDIAGNOSTIC INFORMATION\r\n" + Program.GetDiagnosticInformation());
        }

        private void statusLinkLabel_LinkClicked(object sender, LinkLabelLinkClickedEventArgs e)
        {
            if (!string.IsNullOrEmpty(_trackerItem))
                Program.TryStart(_trackerItem);
            else
                Program.TryStart("http://sourceforge.net/tracker/?atid=1119665&group_id=242527&func=browse");
        }

        private void buttonContinue_Click(object sender, EventArgs e)
        {
            this.Close();
        }

        private void buttonQuit_Click(object sender, EventArgs e)
        {
            try
            {
                Properties.Settings.Default.Save();


                Program.HackerWindow.ExecuteOnIcons((icon) => icon.Visible = false);
                Program.HackerWindow.ExecuteOnIcons((icon) => icon.Dispose());


                if (ProcessHacker.Native.KProcessHacker.Instance != null)
                    ProcessHacker.Native.KProcessHacker.Instance.Close();
            }
            catch (Exception ex)
            {
                Logging.Log(ex);
            }

            Win32.ExitProcess(1);
        }

        private void submitReportButton_Click(object sender, EventArgs e)
        {
            this.buttonContinue.Enabled = false;
            this.buttonQuit.Enabled = false;
            this.buttonSubmitReport.Enabled = false;
            this.statusLinkLabel.Visible = true;

            SFBugReporter wc = new SFBugReporter();
            wc.DownloadProgressChanged += new DownloadProgressChangedEventHandler(wc_DownloadProgressChanged);
            wc.DownloadStringCompleted += new DownloadStringCompletedEventHandler(wc_DownloadStringCompleted);

            NameValueCollection qc = new NameValueCollection();
            qc.Add("group_id", "242527");
            qc.Add("atid", "1119665");
            qc.Add("func", "postadd");
            qc.Add("category_id", "100");
            qc.Add("artifact_group_id", "100");
            qc.Add("assigned_to", "100");
            qc.Add("priority", "5");

            qc.Add("summary", Uri.EscapeDataString(_exception.Message)
                + " - " + DateTime.Now.ToString("F")
                + " - " + DateTime.Now.Ticks.ToString("x"));
            qc.Add("details", Uri.EscapeDataString(textException.Text));


            qc.Add("submit", "Add Artifact");

            wc.QueryString = qc;
            wc.DownloadStringAsync(new Uri("https://sourceforge.net/tracker/index.php"));
        }

        private void wc_DownloadProgressChanged(object sender, DownloadProgressChangedEventArgs e)
        {

        }

        private void wc_DownloadStringCompleted(object sender, DownloadStringCompletedEventArgs e)
        {
            if (!_isTerminating)
                buttonContinue.Enabled = true;
            buttonQuit.Enabled = true;

            if (e.Error != null || this.GetTitle(e.Result).Contains("ERROR"))
            {
                buttonSubmitReport.Enabled = true;

                if (e.Error != null)
                {
                    if (e.Error.InnerException != null)
                        PhUtils.ShowError("Unable to submit the error report: " + e.Error.InnerException.Message);
                    else
                        PhUtils.ShowError("Unable to submit the error report: " + e.Error.Message);
                }
                else
                {
                    PhUtils.ShowError("Unable to submit the error report: " + this.GetTitle(e.Result));
                }
            }
            else
            {
                statusLinkLabel.Enabled = true;
                statusLinkLabel.Text = "View SourceForge error report";

                _trackerItem = GetUrl(Regex.Replace(this.GetResult(e.Result), @"<(.|\n)*?>", string.Empty).Replace("&amp;", "&"));
            }
        }

        private string GetTitle(string data)
        {

            Match m = Regex.Match(data, @"<title>\s*(.+?)\s*</title>", RegexOptions.IgnoreCase);
            if (m.Success)
            {
                return m.Groups[1].Value;
            }
            else
            {
                return "";
            }
        }

        private string GetResult(string data)
        {

            Match m = Regex.Match(data, @"<small>\s*(.+?)\s*</small>", RegexOptions.IgnoreCase);
            if (m.Success)
            {
                return m.Groups[1].Value;
            }
            else
            {
                return "";
            }
        }

        private string GetUrl(string data)
        {

            Match m = Regex.Match(data, @"\b([\d\w\.\/\+\-\?\:]*)((ht|f)tp(s|)\:\/\/|[\d\d\d|\d\d]\.[\d\d\d|\d\d]\.|www\.|\.com|\.net|\.org)([\d\w\.\/\%\+\-\=\&amp;\?\:\\\&quot;\'\,\|\~\;]*)\b", RegexOptions.IgnoreCase);
            if (m.Success)
            {
                return m.Value;
            }
            else
            {
                return "";
            }
        }

        public partial class SFBugReporter : WebClient
        {
            protected override WebRequest GetWebRequest(Uri uri)
            {
                System.Net.HttpWebRequest webRequest = (System.Net.HttpWebRequest)base.GetWebRequest(uri);
                webRequest.UserAgent = "Process Hacker " + Application.ProductVersion;
                webRequest.Timeout = System.Threading.Timeout.Infinite;
                webRequest.ServicePoint.Expect100Continue = true;
                webRequest.KeepAlive = true;
                return webRequest;
            }
        }
    }
}
