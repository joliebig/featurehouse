using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Runtime.InteropServices;
using System.Collections.ObjectModel;
using Eraser.Util;
namespace Eraser
{
 public partial class ToolBar : System.Windows.Forms.MenuStrip
 {
  public ToolBar()
  {
   InitializeComponent();
   Renderer = new EraserToolStripRenderer();
  }
  private class EraserToolStripRenderer : ToolStripRenderer
  {
   protected override void OnRenderItemText(ToolStripItemTextRenderEventArgs e)
   {
    Graphics g = e.Graphics;
    Rectangle tempRect = e.TextRectangle;
    tempRect.Inflate(3, 3);
    tempRect.Offset(3, 3);
    e.TextRectangle = tempRect;
    using (SolidBrush textBrush = new SolidBrush(TextColour))
     g.DrawString(e.Text, e.TextFont, textBrush, e.TextRectangle);
    if (e.Item.Selected)
    {
     SizeF textSize = g.MeasureString(e.Text, e.TextFont);
     using (Pen underlinePen = new Pen(TextColour))
     {
      Point underlineStart = e.TextRectangle.Location;
      underlineStart.Offset(0, Point.Truncate(textSize.ToPointF()).Y);
      Point underlineEnd = underlineStart;
      underlineEnd.Offset(e.TextRectangle.Width, 0);
      g.DrawLine(underlinePen, underlineStart, underlineEnd);
     }
    }
   }
   private const int ArrowMargin = 0;
   private readonly Color TextColour = Color.White;
  }
 }
}
