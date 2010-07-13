

using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;
using Eraser.Util;

namespace Eraser
{
 public partial class LightGroup : UserControl
 {
  public LightGroup()
  {
   InitializeComponent();
  }

  [Description("The label text for the group")]
  [Category("Appearance")]
  [Localizable(true)]
  public string Label
  {
   get { return label.Text; }
   set { label.Text = value; }
  }

  public override string Text
  {
   get { return Label; }
   set { Label = value; }
  }
 }
}
