using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;

namespace MeGUI
{
  public partial class frmStreamSelect : Form
  {
    public frmStreamSelect(ChapterExtractor extractor)
    {
        InitializeComponent();

      extractor.StreamDetected += (sender, arg) =>
        {
          listBox1.Items.Add(arg.ProgramChain);
        };
      extractor.ChaptersLoaded += (sender, arg) =>
        {
          for (int i = 0; i < listBox1.Items.Count; i++)
          {
            if (((ChapterInfo)listBox1.Items[i]).SourceName == arg.ProgramChain.SourceName)
            {
              listBox1.Items[i] = arg.ProgramChain;
              break;
            }
          }
        };
      extractor.ExtractionComplete += (sender, arg) =>
        {
          List<ChapterInfo> list = new List<ChapterInfo>(listBox1.Items.Cast<ChapterInfo>());
          list = list.OrderByDescending(p => p.Duration).ToList();
          listBox1.Items.Clear();
          listBox1.Items.AddRange(list.ToArray());
        };

    }

    public ChapterInfo ProgramChain
    {
      get { return listBox1.SelectedItems.Count > 0 ? listBox1.SelectedItem as ChapterInfo : null; }
    }

    private void btnOK_Click(object sender, EventArgs e)
    {
      if (listBox1.SelectedItems.Count == 1)
        DialogResult = DialogResult.OK;
      else
        MessageBox.Show("Please select a stream.");
    }
  }
}
