

using System;
using System.Collections.Generic;
using System.Windows.Forms;
using ProcessHacker.Common;
using ProcessHacker.UI;

namespace ProcessHacker
{
    public partial class ListWindow : Form
    {
        public ListWindow(List<KeyValuePair<string, string> > list)
        {
            InitializeComponent();
            this.AddEscapeToClose();
            this.SetTopMost();

            foreach (KeyValuePair<string, string> kvp in list)
            {
                ListViewItem item = new ListViewItem();

                item.Text = kvp.Key;
                item.SubItems.Add(new ListViewItem.ListViewSubItem(item, kvp.Value));

                listView.Items.Add(item);
            }

            listView.ContextMenu = listView.GetCopyMenu();
        }

        private void ListWindow_Load(object sender, EventArgs e)
        {
            listView.SetTheme("explorer");
        }

        private void buttonClose_Click(object sender, EventArgs e)
        {
            this.Close();
        }
    }
}
