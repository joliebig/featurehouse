

using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;

namespace Novell.FormsTrayApp
{



 public class Credits : System.Windows.Forms.Form
 {
  private System.Windows.Forms.Button close;
  private System.Windows.Forms.ListBox listBox1;



  private System.ComponentModel.Container components = null;

  public Credits()
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
   System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(Credits));
   this.listBox1 = new System.Windows.Forms.ListBox();
   this.close = new System.Windows.Forms.Button();
   this.SuspendLayout();



   this.listBox1.AccessibleDescription = resources.GetString("listBox1.AccessibleDescription");
   this.listBox1.AccessibleName = resources.GetString("listBox1.AccessibleName");
   this.listBox1.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("listBox1.Anchor")));
   this.listBox1.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("listBox1.BackgroundImage")));
   this.listBox1.ColumnWidth = ((int)(resources.GetObject("listBox1.ColumnWidth")));
   this.listBox1.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("listBox1.Dock")));
   this.listBox1.Enabled = ((bool)(resources.GetObject("listBox1.Enabled")));
   this.listBox1.Font = ((System.Drawing.Font)(resources.GetObject("listBox1.Font")));
   this.listBox1.HorizontalExtent = ((int)(resources.GetObject("listBox1.HorizontalExtent")));
   this.listBox1.HorizontalScrollbar = ((bool)(resources.GetObject("listBox1.HorizontalScrollbar")));
   this.listBox1.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("listBox1.ImeMode")));
   this.listBox1.IntegralHeight = ((bool)(resources.GetObject("listBox1.IntegralHeight")));
   this.listBox1.ItemHeight = ((int)(resources.GetObject("listBox1.ItemHeight")));
   this.listBox1.Items.AddRange(new object[] {
                resources.GetString("listBox1.Items"),
                resources.GetString("listBox1.Items1"),
                resources.GetString("listBox1.Items2"),
                resources.GetString("listBox1.Items3"),
                resources.GetString("listBox1.Items4"),
                resources.GetString("listBox1.Items5"),
                resources.GetString("listBox1.Items6"),
                resources.GetString("listBox1.Items7"),
                resources.GetString("listBox1.Items8"),
                resources.GetString("listBox1.Items9"),
                resources.GetString("listBox1.Items10"),
                resources.GetString("listBox1.Items11"),
                resources.GetString("listBox1.Items12"),
                resources.GetString("listBox1.Items13"),
                resources.GetString("listBox1.Items14"),
                resources.GetString("listBox1.Items15"),
                resources.GetString("listBox1.Items16"),
                resources.GetString("listBox1.Items17"),
                resources.GetString("listBox1.Items18"),
                resources.GetString("listBox1.Items19"),
                resources.GetString("listBox1.Items20"),
                resources.GetString("listBox1.Items21"),
                resources.GetString("listBox1.Items22"),
                resources.GetString("listBox1.Items23"),
                resources.GetString("listBox1.Items24"),
                resources.GetString("listBox1.Items25"),
                resources.GetString("listBox1.Items26"),
                resources.GetString("listBox1.Items27"),
                resources.GetString("listBox1.Items28"),
                resources.GetString("listBox1.Items29"),
                resources.GetString("listBox1.Items30"),
                resources.GetString("listBox1.Items31"),
                resources.GetString("listBox1.Items32"),
                resources.GetString("listBox1.Items33"),
                resources.GetString("listBox1.Items34"),
                resources.GetString("listBox1.Items35"),
                resources.GetString("listBox1.Items36"),
                resources.GetString("listBox1.Items37"),
                resources.GetString("listBox1.Items38"),
                resources.GetString("listBox1.Items39"),
                resources.GetString("listBox1.Items40"),
                resources.GetString("listBox1.Items41"),
                resources.GetString("listBox1.Items42"),
                resources.GetString("listBox1.Items43"),
                resources.GetString("listBox1.Items44"),
                resources.GetString("listBox1.Items45"),
                resources.GetString("listBox1.Items46"),
                resources.GetString("listBox1.Items47"),
                resources.GetString("listBox1.Items48")});
   this.listBox1.Location = ((System.Drawing.Point)(resources.GetObject("listBox1.Location")));
   this.listBox1.Name = "listBox1";
   this.listBox1.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("listBox1.RightToLeft")));
   this.listBox1.ScrollAlwaysVisible = ((bool)(resources.GetObject("listBox1.ScrollAlwaysVisible")));
   this.listBox1.Size = ((System.Drawing.Size)(resources.GetObject("listBox1.Size")));
   this.listBox1.TabIndex = ((int)(resources.GetObject("listBox1.TabIndex")));
   this.listBox1.Visible = ((bool)(resources.GetObject("listBox1.Visible")));



   this.close.AccessibleDescription = resources.GetString("close.AccessibleDescription");
   this.close.AccessibleName = resources.GetString("close.AccessibleName");
   this.close.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("close.Anchor")));
   this.close.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("close.BackgroundImage")));
   this.close.DialogResult = System.Windows.Forms.DialogResult.OK;
   this.close.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("close.Dock")));
   this.close.Enabled = ((bool)(resources.GetObject("close.Enabled")));
   this.close.FlatStyle = ((System.Windows.Forms.FlatStyle)(resources.GetObject("close.FlatStyle")));
   this.close.Font = ((System.Drawing.Font)(resources.GetObject("close.Font")));
   this.close.Image = ((System.Drawing.Image)(resources.GetObject("close.Image")));
   this.close.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("close.ImageAlign")));
   this.close.ImageIndex = ((int)(resources.GetObject("close.ImageIndex")));
   this.close.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("close.ImeMode")));
   this.close.Location = ((System.Drawing.Point)(resources.GetObject("close.Location")));
   this.close.Name = "close";
   this.close.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("close.RightToLeft")));
   this.close.Size = ((System.Drawing.Size)(resources.GetObject("close.Size")));
   this.close.TabIndex = ((int)(resources.GetObject("close.TabIndex")));
   this.close.Text = resources.GetString("close.Text");
   this.close.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("close.TextAlign")));
   this.close.Visible = ((bool)(resources.GetObject("close.Visible")));



   this.AcceptButton = this.close;
   this.AccessibleDescription = resources.GetString("$this.AccessibleDescription");
   this.AccessibleName = resources.GetString("$this.AccessibleName");
   this.AutoScaleBaseSize = ((System.Drawing.Size)(resources.GetObject("$this.AutoScaleBaseSize")));
   this.AutoScroll = ((bool)(resources.GetObject("$this.AutoScroll")));
   this.AutoScrollMargin = ((System.Drawing.Size)(resources.GetObject("$this.AutoScrollMargin")));
   this.AutoScrollMinSize = ((System.Drawing.Size)(resources.GetObject("$this.AutoScrollMinSize")));
   this.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("$this.BackgroundImage")));
   this.ClientSize = ((System.Drawing.Size)(resources.GetObject("$this.ClientSize")));
   this.Controls.Add(this.close);
   this.Controls.Add(this.listBox1);
   this.Enabled = ((bool)(resources.GetObject("$this.Enabled")));
   this.Font = ((System.Drawing.Font)(resources.GetObject("$this.Font")));
   this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
   this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
   this.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("$this.ImeMode")));
   this.Location = ((System.Drawing.Point)(resources.GetObject("$this.Location")));
   this.MaximizeBox = false;
   this.MaximumSize = ((System.Drawing.Size)(resources.GetObject("$this.MaximumSize")));
   this.MinimizeBox = false;
   this.MinimumSize = ((System.Drawing.Size)(resources.GetObject("$this.MinimumSize")));
   this.Name = "Credits";
   this.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("$this.RightToLeft")));
   this.ShowInTaskbar = false;
   this.StartPosition = ((System.Windows.Forms.FormStartPosition)(resources.GetObject("$this.StartPosition")));
   this.Text = resources.GetString("$this.Text");
   this.ResumeLayout(false);

  }

 }
}
