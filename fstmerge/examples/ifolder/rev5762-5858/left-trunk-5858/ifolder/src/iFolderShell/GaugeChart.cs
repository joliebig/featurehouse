

using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Windows.Forms;
using System.Runtime.InteropServices;

namespace Novell.iFolderCom
{



 [ComVisible(false)]
 public class GaugeChart : System.Windows.Forms.UserControl
 {

  private System.Windows.Forms.Label gauge;
  private double maxValue;
  private double used;
  private Color barColor = Color.Red;




  private System.ComponentModel.Container components = null;





  public GaugeChart()
  {

   InitializeComponent();
  }




  protected override void Dispose( bool disposing )
  {
   if( disposing )
   {
    if(components != null)
    {
     components.Dispose();
    }
   }
   base.Dispose( disposing );
  }






  private void InitializeComponent()
  {
   System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(GaugeChart));
   this.gauge = new System.Windows.Forms.Label();
   this.SuspendLayout();



   this.gauge.AccessibleDescription = resources.GetString("gauge.AccessibleDescription");
   this.gauge.AccessibleName = resources.GetString("gauge.AccessibleName");
   this.gauge.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("gauge.Anchor")));
   this.gauge.AutoSize = ((bool)(resources.GetObject("gauge.AutoSize")));
   this.gauge.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
   this.gauge.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("gauge.Dock")));
   this.gauge.Enabled = ((bool)(resources.GetObject("gauge.Enabled")));
   this.gauge.Font = ((System.Drawing.Font)(resources.GetObject("gauge.Font")));
   this.gauge.Image = ((System.Drawing.Image)(resources.GetObject("gauge.Image")));
   this.gauge.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("gauge.ImageAlign")));
   this.gauge.ImageIndex = ((int)(resources.GetObject("gauge.ImageIndex")));
   this.gauge.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("gauge.ImeMode")));
   this.gauge.Location = ((System.Drawing.Point)(resources.GetObject("gauge.Location")));
   this.gauge.Name = "gauge";
   this.gauge.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("gauge.RightToLeft")));
   this.gauge.Size = ((System.Drawing.Size)(resources.GetObject("gauge.Size")));
   this.gauge.TabIndex = ((int)(resources.GetObject("gauge.TabIndex")));
   this.gauge.Text = resources.GetString("gauge.Text");
   this.gauge.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("gauge.TextAlign")));
   this.gauge.Visible = ((bool)(resources.GetObject("gauge.Visible")));
   this.gauge.Paint += new System.Windows.Forms.PaintEventHandler(this.gauge_Paint);



   this.AccessibleDescription = resources.GetString("$this.AccessibleDescription");
   this.AccessibleName = resources.GetString("$this.AccessibleName");
   this.AutoScroll = ((bool)(resources.GetObject("$this.AutoScroll")));
   this.AutoScrollMargin = ((System.Drawing.Size)(resources.GetObject("$this.AutoScrollMargin")));
   this.AutoScrollMinSize = ((System.Drawing.Size)(resources.GetObject("$this.AutoScrollMinSize")));
   this.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("$this.BackgroundImage")));
   this.Controls.Add(this.gauge);
   this.Enabled = ((bool)(resources.GetObject("$this.Enabled")));
   this.Font = ((System.Drawing.Font)(resources.GetObject("$this.Font")));
   this.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("$this.ImeMode")));
   this.Location = ((System.Drawing.Point)(resources.GetObject("$this.Location")));
   this.Name = "GaugeChart";
   this.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("$this.RightToLeft")));
   this.Size = ((System.Drawing.Size)(resources.GetObject("$this.Size")));
   this.ResumeLayout(false);

  }



  private void gauge_Paint(object sender, System.Windows.Forms.PaintEventArgs e)
  {
   e.Graphics.Clear(Label.DefaultBackColor);
   Pen pen = new Pen(Color.Black);
   e.Graphics.DrawRectangle(pen, gauge.ClientRectangle);
   if (maxValue != 0)
   {
    int fillLevel = (int)((used/maxValue) * gauge.Size.Height);

    SolidBrush brush = new SolidBrush(barColor);
    Rectangle rect = new Rectangle(0, gauge.ClientSize.Height - fillLevel, gauge.ClientSize.Width, fillLevel);
    rect.Intersect(e.ClipRectangle);
    e.Graphics.FillRectangle(brush, rect);




   }
  }






  public double MaxValue
  {
   set
   {
    maxValue = value;
   }
  }




  public Color BarColor
  {
   set { barColor = value; }
  }




  public double Used
  {
   set
   {
    used = value;
   }
  }

 }
}
