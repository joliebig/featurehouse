

using System;
using System.IO;
using System.Reflection;
using System.Windows.Forms;

namespace ProcessHacker
{
    public partial class HelpWindow : Form
    {
        public string[][] contents = {
                                         new string[] {"Introduction", "intro"},
                                         new string[] {"Options", "options"},
                                         new string[] {"Number Input", "numberinput"},
                                         new string[] {"Process Tree", "proctree"},
                                         new string[] {"Process Properties", "procprops"},
                                         new string[] {"Searching Memory", "memsearch"},
                                         new string[] {"Results Window", "results"},
                                         new string[] {"Glossary", "glossary"},
                                         new string[] {"Copyright Information", "copyright"}
                                     };

        public HelpWindow()
        {
            InitializeComponent();
            this.AddEscapeToClose();
            this.SetTopMost();

            Assembly assembly = Assembly.GetExecutingAssembly();
            Stream resource = assembly.GetManifestResourceStream(assembly.GetName().Name + ".Help.htm");

            webBrowser.DocumentStream = resource;

            foreach (string[] s in contents)
            {
                listBoxContents.Items.Add(s[0]);
            }

            webBrowser.Navigating += (sender, e) => e.Cancel = true;
        }

        private void listBoxContents_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (listBoxContents.SelectedItems.Count == 1)
            {
                foreach (string[] s in contents)
                {
                    if (s[0] == listBoxContents.SelectedItem.ToString())
                    {
                        HtmlElement element = webBrowser.Document.GetElementById(s[1]);

                        if (element != null)
                            element.ScrollIntoView(true);

                        break;
                    }
                }
            }
        }

        public void SelectById(string id)
        {
            webBrowser.Document.GetElementById(id).ScrollIntoView(true);
        }

        private void HelpWindow_FormClosing(object sender, FormClosingEventArgs e)
        {
            e.Cancel = true;

            this.Hide();
        }

        private void webBrowser_PreviewKeyDown(object sender, PreviewKeyDownEventArgs e)
        {

            if (e.KeyData != Keys.F5)
            {
                webBrowser.WebBrowserShortcutsEnabled = true;
            }
            else
            {
                webBrowser.WebBrowserShortcutsEnabled = false;
            }
        }
    }
}
