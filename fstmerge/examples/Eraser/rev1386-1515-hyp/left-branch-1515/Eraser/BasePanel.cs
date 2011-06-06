using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;
namespace Eraser
{
 internal partial class BasePanel : UserControl
 {
  public BasePanel()
  {
   InitializeComponent();
   titleLabel.Font = new Font(SystemFonts.MessageBoxFont.Name, 18f);
  }
 }
}
