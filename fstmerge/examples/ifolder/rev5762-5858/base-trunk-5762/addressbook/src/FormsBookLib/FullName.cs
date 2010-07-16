

using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;

namespace Novell.iFolder.FormsBookLib
{



 public class FullName : System.Windows.Forms.Form
 {
  private System.Windows.Forms.Button ok;
  private System.Windows.Forms.Button cancel;
  private System.Windows.Forms.TextBox firstName;
  private System.Windows.Forms.TextBox middleName;
  private System.Windows.Forms.TextBox lastName;
  private System.Windows.Forms.Label label1;
  private System.Windows.Forms.Label label2;
  private System.Windows.Forms.Label label3;
  private System.Windows.Forms.Label label4;
  private System.Windows.Forms.Label label5;
  private System.Windows.Forms.ComboBox title;
  private System.Windows.Forms.ComboBox suffix;
  private System.Windows.Forms.GroupBox groupBox1;



  private System.ComponentModel.Container components = null;




  public FullName()
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
   this.ok = new System.Windows.Forms.Button();
   this.cancel = new System.Windows.Forms.Button();
   this.firstName = new System.Windows.Forms.TextBox();
   this.middleName = new System.Windows.Forms.TextBox();
   this.lastName = new System.Windows.Forms.TextBox();
   this.label1 = new System.Windows.Forms.Label();
   this.label2 = new System.Windows.Forms.Label();
   this.label3 = new System.Windows.Forms.Label();
   this.label4 = new System.Windows.Forms.Label();
   this.label5 = new System.Windows.Forms.Label();
   this.title = new System.Windows.Forms.ComboBox();
   this.suffix = new System.Windows.Forms.ComboBox();
   this.groupBox1 = new System.Windows.Forms.GroupBox();
   this.SuspendLayout();



   this.ok.DialogResult = System.Windows.Forms.DialogResult.OK;
   this.ok.FlatStyle = System.Windows.Forms.FlatStyle.System;
   this.ok.Location = new System.Drawing.Point(72, 192);
   this.ok.Name = "ok";
   this.ok.TabIndex = 6;
   this.ok.Text = "OK";



   this.cancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
   this.cancel.FlatStyle = System.Windows.Forms.FlatStyle.System;
   this.cancel.Location = new System.Drawing.Point(160, 192);
   this.cancel.Name = "cancel";
   this.cancel.TabIndex = 7;
   this.cancel.Text = "Cancel";



   this.firstName.Location = new System.Drawing.Point(72, 48);
   this.firstName.Name = "firstName";
   this.firstName.Size = new System.Drawing.Size(160, 20);
   this.firstName.TabIndex = 1;
   this.firstName.Text = "";



   this.middleName.Location = new System.Drawing.Point(72, 80);
   this.middleName.Name = "middleName";
   this.middleName.Size = new System.Drawing.Size(160, 20);
   this.middleName.TabIndex = 2;
   this.middleName.Text = "";



   this.lastName.Location = new System.Drawing.Point(72, 112);
   this.lastName.Name = "lastName";
   this.lastName.Size = new System.Drawing.Size(160, 20);
   this.lastName.TabIndex = 3;
   this.lastName.Text = "";



   this.label1.Location = new System.Drawing.Point(16, 16);
   this.label1.Name = "label1";
   this.label1.Size = new System.Drawing.Size(40, 16);
   this.label1.TabIndex = 8;
   this.label1.Text = "Title:";



   this.label2.Location = new System.Drawing.Point(16, 48);
   this.label2.Name = "label2";
   this.label2.Size = new System.Drawing.Size(40, 16);
   this.label2.TabIndex = 9;
   this.label2.Text = "First:";



   this.label3.Location = new System.Drawing.Point(16, 80);
   this.label3.Name = "label3";
   this.label3.Size = new System.Drawing.Size(56, 16);
   this.label3.TabIndex = 10;
   this.label3.Text = "Middle:";



   this.label4.Location = new System.Drawing.Point(16, 112);
   this.label4.Name = "label4";
   this.label4.Size = new System.Drawing.Size(40, 16);
   this.label4.TabIndex = 11;
   this.label4.Text = "Last:";



   this.label5.Location = new System.Drawing.Point(16, 144);
   this.label5.Name = "label5";
   this.label5.Size = new System.Drawing.Size(48, 16);
   this.label5.TabIndex = 12;
   this.label5.Text = "Suffix:";



   this.title.Items.AddRange(new object[] {
                "Dr.",
                "Miss",
                "Mr.",
                "Mrs.",
                "Ms.",
                "Prof."});
   this.title.Location = new System.Drawing.Point(72, 16);
   this.title.Name = "title";
   this.title.Size = new System.Drawing.Size(88, 21);
   this.title.TabIndex = 0;



   this.suffix.Items.AddRange(new object[] {
              "I",
              "II",
              "III",
              "Jr.",
              "Sr."});
   this.suffix.Location = new System.Drawing.Point(72, 144);
   this.suffix.Name = "suffix";
   this.suffix.Size = new System.Drawing.Size(88, 21);
   this.suffix.TabIndex = 4;



   this.groupBox1.Location = new System.Drawing.Point(4, 176);
   this.groupBox1.Name = "groupBox1";
   this.groupBox1.Size = new System.Drawing.Size(236, 4);
   this.groupBox1.TabIndex = 15;
   this.groupBox1.TabStop = false;



   this.AcceptButton = this.ok;
   this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
   this.CancelButton = this.cancel;
   this.ClientSize = new System.Drawing.Size(242, 224);
   this.Controls.Add(this.lastName);
   this.Controls.Add(this.middleName);
   this.Controls.Add(this.firstName);
   this.Controls.Add(this.suffix);
   this.Controls.Add(this.label5);
   this.Controls.Add(this.label4);
   this.Controls.Add(this.label3);
   this.Controls.Add(this.label2);
   this.Controls.Add(this.title);
   this.Controls.Add(this.label1);
   this.Controls.Add(this.groupBox1);
   this.Controls.Add(this.cancel);
   this.Controls.Add(this.ok);
   this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
   this.MaximizeBox = false;
   this.MinimizeBox = false;
   this.Name = "FullName";
   this.ShowInTaskbar = false;
   this.Text = "Full Name";
   this.ResumeLayout(false);

  }






  public string Title
  {
   get
   {
    return title.Text.Trim();
   }
   set
   {
    title.Text = value;
   }
  }




  public string FirstName
  {
   get
   {
    return firstName.Text.Trim();
   }
   set
   {
    firstName.Text = value;
   }
  }




  public string MiddleName
  {
   get
   {
    return middleName.Text.Trim();
   }
   set
   {
    middleName.Text = value;
   }
  }




  public string LastName
  {
   get
   {
    return lastName.Text.Trim();
   }
   set
   {
    lastName.Text = value;
   }
  }




  public string Suffix
  {
   get
   {
    return suffix.Text.Trim();
   }
   set
   {
    suffix.Text = value;
   }
  }

 }
}
