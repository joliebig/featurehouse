

using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Windows.Forms;

namespace Novell.FormsTrayApp
{



 [Serializable]
 public class TileListViewItem : System.Windows.Forms.UserControl
 {


  private ToolTip toolTip;
  private bool selected = false;
  private Image icon;
  private Rectangle iconRect;

  private SolidBrush nameBrush;
  private Font nameFont;
  private string completeName = string.Empty;
  private Rectangle nameRect;
  private SolidBrush descriptionBrush;
  private Font descriptionFont;
  private string completeLocation = string.Empty;
  private Rectangle locationRect;
  private string completeStatus = string.Empty;
  private Rectangle statusRect;

  private Color selectionColor = Color.FromKnownColor(KnownColor.Highlight);
  private Color normalColor = Color.White;
  private Color activeTextColor = Color.FromKnownColor( KnownColor.ControlText );
        private Color inactiveTextColor = Color.DarkGray;
  private TileListView owner;
  private int imageIndex;
  private System.Windows.Forms.Timer timer1;
  private System.ComponentModel.IContainer components;
        private System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(TileListViewItem));
  public TileListViewItem( iFolderObject ifolderObject ) :
   this()
  {
   Tag = ifolderObject;
   ItemName = ifolderObject.iFolderWeb.Name;
   if ( ifolderObject.iFolderWeb.IsSubscription )
   {
                ItemLocation = string.Format("{0} {1}", resources.GetString("Owner"), ifolderObject.iFolderWeb.Owner);
   }
   else
   {
    ItemLocation = ifolderObject.iFolderWeb.UnManagedPath;
            }
  }
  public TileListViewItem( string[] items, int imageIndex ) :
   this()
  {
   ImageIndex = imageIndex;
   ItemName = items.Length > 0 ? items[0] : string.Empty;
   ItemLocation = items.Length > 1 ? items[1] : string.Empty;
   Status = items.Length > 2 ? items[2] : string.Empty;
  }
  private TileListViewItem()
  {
   InitializeComponent();
   iconRect = new Rectangle( 8, 12, 48, 48 );
   nameRect = new Rectangle( 64, 12, 206, 20 );
   locationRect = new Rectangle( 64, 32, 206, 16 );
   statusRect = new Rectangle( 64, 48, 206, 16 );
   nameBrush = new SolidBrush( activeTextColor );
   nameFont = new Font( "Microsoft Sans Serif", 11, FontStyle.Bold );
   descriptionBrush = new SolidBrush( inactiveTextColor );
   descriptionFont = new Font( "Microsoft Sans Serif", 8, FontStyle.Regular );
   SetStyle( ControlStyles.AllPaintingInWmPaint | ControlStyles.DoubleBuffer | ControlStyles.UserPaint, true );
   toolTip = new ToolTip();
   toolTip.Active = true;
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
  protected override bool IsInputKey(Keys keyData)
  {
   if ( (keyData & Keys.Up) == Keys.Up ||
    (keyData & Keys.Down) == Keys.Down ||
    (keyData & Keys.Left) == Keys.Left ||
    (keyData & Keys.Right) == Keys.Right ||
    (keyData & Keys.PageDown) == Keys.PageDown ||
    (keyData & Keys.PageUp) == Keys.PageUp )
   {
    return true;
   }
   else
   {
    return base.IsInputKey (keyData);
   }
  }
  private void InitializeComponent()
  {
   this.components = new System.ComponentModel.Container();
   this.timer1 = new System.Windows.Forms.Timer(this.components);
   this.timer1.Interval = 60000;
   this.timer1.Tick += new System.EventHandler(this.timer1_Tick);
   this.AccessibleDescription = resources.GetString("$this.AccessibleDescription");
   this.AccessibleName = resources.GetString("$this.AccessibleName");
   this.AutoScroll = ((bool)(resources.GetObject("$this.AutoScroll")));
   this.AutoScrollMargin = ((System.Drawing.Size)(resources.GetObject("$this.AutoScrollMargin")));
   this.AutoScrollMinSize = ((System.Drawing.Size)(resources.GetObject("$this.AutoScrollMinSize")));
   this.BackColor = System.Drawing.SystemColors.Window;
   this.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("$this.BackgroundImage")));
   this.Enabled = ((bool)(resources.GetObject("$this.Enabled")));
   this.Font = ((System.Drawing.Font)(resources.GetObject("$this.Font")));
   this.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("$this.ImeMode")));
   this.Location = ((System.Drawing.Point)(resources.GetObject("$this.Location")));
   this.Name = "TileListViewItem";
   this.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("$this.RightToLeft")));
   this.Size = ((System.Drawing.Size)(resources.GetObject("$this.Size")));
   this.Paint += new System.Windows.Forms.PaintEventHandler(this.TileListViewItem_Paint);
   this.MouseEnter += new System.EventHandler(this.TileListViewItem_MouseEnter);
   this.KeyDown += new System.Windows.Forms.KeyEventHandler(this.TileListViewItem_KeyDown);
   this.MouseLeave += new System.EventHandler(this.TileListViewItem_MouseLeave);
   this.MouseDown += new System.Windows.Forms.MouseEventHandler(this.TileListViewItem_MouseDown);
  }
  public event EventHandler ItemSelected;
  private void TileListViewItem_MouseEnter(object sender, System.EventArgs e)
  {
   if ( !selected )
   {
    descriptionBrush = new SolidBrush( activeTextColor );
    Invalidate();
   }
  }
  private void TileListViewItem_MouseLeave(object sender, System.EventArgs e)
  {
   if ( !selected )
   {
    descriptionBrush = new SolidBrush( inactiveTextColor );
                toolTip.Hide(this);
    Invalidate();
   }
  }
  private void TileListViewItem_MouseHover(object sender, System.EventArgs e)
  {
  }
  private void TileListViewItem_MouseDown(object sender, System.Windows.Forms.MouseEventArgs e)
  {
   if ( !Selected )
   {
    Selected = true;
   }
  }
  private void TileListViewItem_Paint(object sender, System.Windows.Forms.PaintEventArgs e)
  {
   e.Graphics.DrawImage( icon, iconRect );
   drawFormattedString( e.Graphics, completeName, nameFont, nameBrush, nameRect );
   drawFormattedString( e.Graphics, completeLocation, descriptionFont, descriptionBrush, locationRect );
   drawFormattedString( e.Graphics, completeStatus, descriptionFont, descriptionBrush, statusRect );
  }
  private void timer1_Tick(object sender, System.EventArgs e)
  {
  }
  public TileListView iFoldersListView
  {
   get { return owner; }
  }
  public int ImageIndex
  {
   get { return imageIndex; }
   set
   {
    imageIndex = value;
    if ( owner != null )
    {
     icon = ImageList.Images[ imageIndex ];
     Invalidate();
    }
   }
  }
  public ImageList ImageList
  {
   get { return owner.LargeImageList; }
  }
  public string ItemLocation
  {
   get { return completeLocation; }
   set
   {
    completeLocation = value;
    setToolTipText();
    Invalidate();
   }
  }
  public string ItemName
  {
   get { return completeName; }
   set
   {
    completeName = value;
    setToolTipText();
    Invalidate();
   }
  }
  public string Status
  {
   get { return completeStatus; }
   set
   {
    completeStatus = value;
    setToolTipText();
    Invalidate();
   }
  }
  internal TileListView Owner
  {
   set
   {
    owner = value;
    ImageIndex = imageIndex;
   }
  }
  public bool Selected
  {
   get
   {
    return selected;
   }
   set
   {
    selected = value;
    if ( selected )
    {
     BackColor = selectionColor;
     if (!(BackColor.R == 178 && BackColor.G == 180 && BackColor.B == 191))
                    {
                        nameBrush = descriptionBrush = new SolidBrush(System.Drawing.SystemColors.ButtonHighlight);
                    }
                        Invalidate();
     if (ItemSelected != null)
     {
      ItemSelected(this, new EventArgs());
     }
    }
    else
    {
     BackColor = normalColor;
     nameBrush = new SolidBrush( activeTextColor );
     descriptionBrush = new SolidBrush( inactiveTextColor );
     Invalidate();
    }
   }
  }
        private void drawFormattedString( Graphics graphics, string s, Font font, SolidBrush brush, Rectangle rectangle )
  {
   string formattedString = s;
   SizeF stringSize = graphics.MeasureString(s, font);
   if (stringSize.Width > rectangle.Width)
   {
    int length = (int)(rectangle.Width * s.Length / stringSize.Width);
    string tmp = String.Empty;
    while (stringSize.Width > rectangle.Width)
    {
     tmp = s.Substring(0, length) + "...";
     stringSize = graphics.MeasureString(tmp, font);
     length -= 1;
    }
    formattedString = tmp;
   }
   graphics.DrawString( formattedString, font, brush, rectangle );
  }
  private void setToolTipText()
  {
            string text = resources.GetString("Name") + " " + completeName;
   iFolderObject ifolderObject = (iFolderObject)Tag;
   if ( ifolderObject.iFolderWeb.IsSubscription )
   {
    text += "\n" + completeLocation;
   }
   else
   {
                text += "\n" + resources.GetString("Location") + " " +completeLocation;
    if ( ifolderObject.iFolderState.Equals( iFolderState.Normal ) )
    {
     text += "\n" + completeStatus;
    }
    else
    {
                    text += "\n" + resources.GetString("Status") + " "+ completeStatus;
    }
   }
   toolTip.SetToolTip( this, text );
  }
  public void Remove()
  {
   owner.Items.Remove( this );
  }
        private void TileListViewItem_KeyDown(object sender, System.Windows.Forms.KeyEventArgs e)
  {
   switch ( e.KeyCode )
   {
    case Keys.Up:
     owner.MoveUp( this );
     break;
    case Keys.Down:
     owner.MoveDown( this );
     break;
    case Keys.Right:
     owner.MoveRight( this );
     break;
    case Keys.Left:
     owner.MoveLeft( this );
     break;
    case Keys.PageDown:
     break;
    case Keys.PageUp:
     break;
   }
  }
 }
}
