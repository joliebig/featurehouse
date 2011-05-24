

using System;
using System.Windows.Forms;
using ProcessHacker.Components;

namespace ProcessHacker
{
    public partial class ServiceWindow : Form
    {
        private ServiceProperties _serviceProps;

        public ServiceWindow(string service)
            : this(new string[] { service })
        { }

        public ServiceWindow(string[] services)
        {
            InitializeComponent();
            this.AddEscapeToClose();
            this.SetTopMost();

            _serviceProps = new ServiceProperties(services);
            _serviceProps.Dock = DockStyle.Fill;
            _serviceProps.NeedsClose += new EventHandler(_serviceProps_NeedsClose);
            this.Controls.Add(_serviceProps);
            this.Text = _serviceProps.Text;
            this.AcceptButton = _serviceProps.ApplyButton;

            if (services.Length == 1)
                _serviceProps.ApplyButtonText = "&OK";
        }

        private void _serviceProps_NeedsClose(object sender, EventArgs e)
        {
            this.Close();
        }

        private void ServiceWindow_FormClosing(object sender, FormClosingEventArgs e)
        {
            _serviceProps.SaveSettings();
            _serviceProps.Dispose();
        }
    }
}
