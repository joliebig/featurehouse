

using System.Windows.Forms;

namespace ProcessHacker
{
    public class ProviderIcon : PlotterIcon
    {
        private bool _enabled = false;

        public ProviderIcon()
        { }

        public override void Dispose()
        {
            this.Enabled = false;
            base.Dispose();
        }

        private void ProcessProvider_Updated()
        {

            this.ProviderUpdated();
        }

        public bool Enabled
        {
            get { return _enabled; }
            set
            {
                if (value != _enabled)
                {
                    if (value)
                        Program.ProcessProvider.Updated += ProcessProvider_Updated;
                    else
                        Program.ProcessProvider.Updated -= ProcessProvider_Updated;
                }

                _enabled = value;
            }
        }

        protected ProcessSystemProvider Provider
        {
            get { return Program.ProcessProvider; }
        }

        protected virtual void ProviderUpdated()
        { }
    }
}
