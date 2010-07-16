

using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Windows.Forms;

using Simias.Client;

namespace Novell.FormsTrayApp
{



 public class iFoldersListView : System.Windows.Forms.UserControl
 {


  private DomainInformation domainInfo;
  private System.Windows.Forms.RichTextBox richTextBox1;
        private System.Windows.Forms.RichTextBox richTextBox2;
  private Novell.FormsTrayApp.TileListView tileListView1;
  private static System.Resources.ResourceManager Resource = new System.Resources.ResourceManager( typeof(FormsTrayApp));



  private System.ComponentModel.Container components = null;
  public iFoldersListView( DomainInformation domainInfo, ImageList largeImageList )
  {
   this.domainInfo = domainInfo;
   InitializeComponent();
   tileListView1.LargeImageList = largeImageList;
   this.richTextBox1.Text = string.Format( richTextBox1.Text, domainInfo.Name + " - " + domainInfo.Host );
            this.richTextBox2.Text = string.Format(" ");
   Graphics g = richTextBox1.CreateGraphics();
   try
   {
    SizeF textSize = g.MeasureString(richTextBox1.Text, richTextBox1.Font);
    this.Width = (int)(textSize.Width * 1.1);
   }
   finally
   {
    g.Dispose();
   }
   base.DoubleClick += new EventHandler(iFoldersListView_DoubleClick);
  }
        public void updateqouta(string Qouta)
        {
            this.richTextBox2.Text = string.Format(Qouta);
        }
  public new event EventHandler DoubleClick;
  public delegate bool NavigateItemDelegate( object sender, NavigateItemEventArgs e );
  public event NavigateItemDelegate NavigateItem;
  public delegate void LastItemRemovedDelegate( object sender, EventArgs e);
  public event LastItemRemovedDelegate LastItemRemoved;
  public delegate void SelectedIndexChangedDelegate( object sender, EventArgs e );
  public event SelectedIndexChangedDelegate SelectedIndexChanged;
        private void iFoldersListView_DoubleClick(object sender, EventArgs e)
  {
   if ( DoubleClick != null )
   {
    DoubleClick( sender, e );
   }
  }
        private void tileListView1_DoubleClick(object sender, System.EventArgs e)
  {
   if ( DoubleClick != null )
   {
    DoubleClick( sender, e );
   }
  }
        private void tileListView1_SizeChanged(object sender, System.EventArgs e)
  {
            Height = richTextBox1.Height + tileListView1.Height + richTextBox2.Height;
  }
        private void tileListView1_SelectedIndexChanged(object sender, System.EventArgs e)
  {
   if ( SelectedIndexChanged != null )
   {
    SelectedIndexChanged( sender, e );
   }
  }
        private void tileListView1_LastItemRemoved(object sender, System.EventArgs e)
  {
   if ( LastItemRemoved != null )
   {
    LastItemRemoved( sender, e );
   }
  }
        private bool tileListView1_NavigateItem(object sender, Novell.FormsTrayApp.NavigateItemEventArgs e)
  {
   if ( NavigateItem != null )
   {
    return NavigateItem( this, e );
   }
   return false;
  }
  private void listView1_MouseDown(object sender, System.Windows.Forms.MouseEventArgs e)
  {
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
   this.richTextBox1 = new System.Windows.Forms.RichTextBox();
            this.richTextBox2 = new System.Windows.Forms.RichTextBox();
   this.tileListView1 = new Novell.FormsTrayApp.TileListView();
   this.SuspendLayout();
   this.richTextBox1.BorderStyle = System.Windows.Forms.BorderStyle.None;
   this.richTextBox1.Cursor = System.Windows.Forms.Cursors.Arrow;
   this.richTextBox1.Dock = System.Windows.Forms.DockStyle.Top;
            this.richTextBox1.Font = new System.Drawing.Font("Microsoft Sans Serif", 15.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
   this.richTextBox1.ForeColor = System.Drawing.SystemColors.Desktop;
   this.richTextBox1.BackColor = System.Drawing.SystemColors.Window;
   this.richTextBox1.Location = new System.Drawing.Point(0, 0);
   this.richTextBox1.Name = "richTextBox1";
   this.richTextBox1.Size = new System.Drawing.Size(288, 25);
   this.richTextBox1.TabIndex = 0;
   this.richTextBox1.ReadOnly = true;
   this.richTextBox1.TabStop = false;
            this.richTextBox1.Enabled = false;
   this.richTextBox1.Text = Resource.GetString("iFoldersOnServerHeading");
            this.richTextBox2.BorderStyle = System.Windows.Forms.BorderStyle.None;
            this.richTextBox2.Cursor = System.Windows.Forms.Cursors.Arrow;
            this.richTextBox2.Dock = System.Windows.Forms.DockStyle.Top;
            this.richTextBox2.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
            this.richTextBox2.ForeColor = System.Drawing.SystemColors.Desktop;
            this.richTextBox2.BackColor = System.Drawing.SystemColors.Window;
            this.richTextBox2.Location = new System.Drawing.Point(0, 20);
            this.richTextBox2.Name = "richTextBox1";
            this.richTextBox2.Size = new System.Drawing.Size(288, 15);
            this.richTextBox2.TabIndex = 1;
            this.richTextBox2.ReadOnly = true;
            this.richTextBox2.TabStop = false;
            this.richTextBox2.Enabled = false;
            this.richTextBox2.Text = Resource.GetString("iFoldersOnServerHeading");
   this.tileListView1.BackColor = System.Drawing.SystemColors.Window;
   this.tileListView1.Dock = System.Windows.Forms.DockStyle.Fill;
   this.tileListView1.HorizontalSpacing = 5;
   this.tileListView1.ItemHeight = 72;
   this.tileListView1.ItemWidth = 280;
   this.tileListView1.LargeImageList = null;
   this.tileListView1.Location = new System.Drawing.Point(0, 60);
   this.tileListView1.Name = "tileListView1";
   this.tileListView1.SelectedItem = null;
   this.tileListView1.Size = new System.Drawing.Size(288, 32);
   this.tileListView1.TabIndex = 1;
   this.tileListView1.VerticleSpacing = 5;
   this.tileListView1.LastItemRemoved += new Novell.FormsTrayApp.TileListView.LastItemRemovedDelegate(this.tileListView1_LastItemRemoved);
   this.tileListView1.NavigateItem += new Novell.FormsTrayApp.TileListView.NavigateItemDelegate(this.tileListView1_NavigateItem);
   this.tileListView1.SizeChanged += new System.EventHandler(this.tileListView1_SizeChanged);
   this.tileListView1.DoubleClick += new System.EventHandler(this.tileListView1_DoubleClick);
   this.tileListView1.SelectedIndexChanged += new Novell.FormsTrayApp.TileListView.SelectedIndexChangedDelegate(this.tileListView1_SelectedIndexChanged);
   this.Controls.Add(this.tileListView1);
            this.Controls.Add(this.richTextBox2);
   this.Controls.Add(this.richTextBox1);
   this.Name = "iFoldersListView";
   this.Size = new System.Drawing.Size(288, 72);
   this.ResumeLayout(false);
  }
  public DomainInformation DomainInfo
  {
   get
   {
    return domainInfo;
   }
  }
  public TileListViewItemCollection Items
  {
   get { return tileListView1.Items; }
  }
  public TileListViewItem SelectedItem
  {
   get { return tileListView1.SelectedItem; }
   set { tileListView1.SelectedItem = value; }
  }
  public bool setVisible
  {
   get
   {
    return this.richTextBox1.Visible;
   }
   set
   {
    this.richTextBox1.Visible = value;
                this.richTextBox2.Visible = value;
   }
  }
  public TileListViewItem AddiFolderToListView( iFolderObject ifolderObject )
  {
   TileListViewItem tlvi = null;
   tlvi = new TileListViewItem( ifolderObject );
   tlvi = tileListView1.Items.Add( tlvi );
   tileListView1.Items.Sort();
   return tlvi;
  }
  public void FinalizeUpdate()
  {
   this.ResumeLayout();
  }
  public void InitializeUpdate()
  {
   tileListView1.Items.Clear();
   this.SuspendLayout();
  }
  public bool MoveToItem( int row, int column )
  {
   return tileListView1.MoveToItem( row, column );
  }
  public void UpdateiFolderStatus( string iFolderID, iFolderState state )
  {
  }
 }
}
