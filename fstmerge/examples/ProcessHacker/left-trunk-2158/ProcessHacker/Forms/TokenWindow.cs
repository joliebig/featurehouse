

using System;
using System.Windows.Forms;
using ProcessHacker.Components;
using ProcessHacker.Native.Objects;

namespace ProcessHacker
{
    public partial class TokenWindow : Form
    {
        TokenProperties _tokenProps;

        public TokenWindow(IWithToken obj)
        {
            InitializeComponent();
            this.AddEscapeToClose();
            this.SetTopMost();

            _tokenProps = new TokenProperties(obj);
            _tokenProps.Dock = DockStyle.Fill;

            panelToken.Controls.Add(_tokenProps);
        }

        public TokenProperties TokenProperties
        {
            get { return _tokenProps; }
        }

        private void TokenWindow_Load(object sender, EventArgs e)
        {
            this.Size = Properties.Settings.Default.TokenWindowSize;
        }

        private void TokenWindow_FormClosing(object sender, FormClosingEventArgs e)
        {
            _tokenProps.SaveSettings();
            Properties.Settings.Default.TokenWindowSize = this.Size;
        }

        private void buttonClose_Click(object sender, EventArgs e)
        {
            this.Close();
        }
    }
}
