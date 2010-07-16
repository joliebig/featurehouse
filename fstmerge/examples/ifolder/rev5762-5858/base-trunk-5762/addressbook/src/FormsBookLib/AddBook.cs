

using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;

namespace Novell.iFolder.FormsBookLib
{



 public class AddBook : System.Windows.Forms.Form
 {

  private System.Windows.Forms.TextBox bookName;
  private System.Windows.Forms.Label label1;
  private System.Windows.Forms.Button ok;
  private System.Windows.Forms.Button cancel;



  private System.ComponentModel.Container components = null;





  public AddBook()
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
   this.bookName = new System.Windows.Forms.TextBox();
   this.label1 = new System.Windows.Forms.Label();
   this.ok = new System.Windows.Forms.Button();
   this.cancel = new System.Windows.Forms.Button();
   this.SuspendLayout();



   this.bookName.Location = new System.Drawing.Point(72, 24);
   this.bookName.Name = "bookName";
   this.bookName.Size = new System.Drawing.Size(200, 20);
   this.bookName.TabIndex = 0;
   this.bookName.Text = "";



   this.label1.Location = new System.Drawing.Point(32, 26);
   this.label1.Name = "label1";
   this.label1.Size = new System.Drawing.Size(40, 16);
   this.label1.TabIndex = 1;
   this.label1.Text = "Name:";



   this.ok.DialogResult = System.Windows.Forms.DialogResult.OK;
   this.ok.FlatStyle = System.Windows.Forms.FlatStyle.System;
   this.ok.Location = new System.Drawing.Point(116, 64);
   this.ok.Name = "ok";
   this.ok.TabIndex = 2;
   this.ok.Text = "OK";



   this.cancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
   this.cancel.FlatStyle = System.Windows.Forms.FlatStyle.System;
   this.cancel.Location = new System.Drawing.Point(200, 64);
   this.cancel.Name = "cancel";
   this.cancel.TabIndex = 3;
   this.cancel.Text = "Cancel";



   this.AcceptButton = this.ok;
   this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
   this.CancelButton = this.cancel;
   this.ClientSize = new System.Drawing.Size(292, 104);
   this.Controls.Add(this.cancel);
   this.Controls.Add(this.ok);
   this.Controls.Add(this.label1);
   this.Controls.Add(this.bookName);
   this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
   this.MaximizeBox = false;
   this.MinimizeBox = false;
   this.Name = "AddBook";
   this.ShowInTaskbar = false;
   this.Text = "Create Address Book";
   this.ResumeLayout(false);
  }






  public new string Name
  {
   get
   {
    return bookName.Text;
   }

   set
   {
    bookName.Text = value;
   }
  }

 }
}
