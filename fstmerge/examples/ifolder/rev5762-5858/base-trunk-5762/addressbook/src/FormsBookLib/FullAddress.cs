

using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;

namespace Novell.iFolder.FormsBookLib
{



 public class FullAddress : System.Windows.Forms.Form
 {
  private System.Windows.Forms.Label label1;
  private System.Windows.Forms.Label label2;
  private System.Windows.Forms.Label label3;
  private System.Windows.Forms.Label label4;
  private System.Windows.Forms.Label label5;
  private System.Windows.Forms.Label label6;
  private System.Windows.Forms.Label label7;
  private System.Windows.Forms.GroupBox groupBox1;
  private System.Windows.Forms.Button ok;
  private System.Windows.Forms.Button cancel;
  private System.Windows.Forms.TextBox street;
  private System.Windows.Forms.TextBox extendedAddress;
  private System.Windows.Forms.TextBox locality;
  private System.Windows.Forms.TextBox region;
  private System.Windows.Forms.TextBox postalBox;
  private System.Windows.Forms.TextBox postalCode;
  private System.Windows.Forms.ComboBox country;






  private System.ComponentModel.Container components = null;




  public FullAddress()
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
   this.street = new System.Windows.Forms.TextBox();
   this.extendedAddress = new System.Windows.Forms.TextBox();
   this.label1 = new System.Windows.Forms.Label();
   this.label2 = new System.Windows.Forms.Label();
   this.label3 = new System.Windows.Forms.Label();
   this.label4 = new System.Windows.Forms.Label();
   this.label5 = new System.Windows.Forms.Label();
   this.label6 = new System.Windows.Forms.Label();
   this.label7 = new System.Windows.Forms.Label();
   this.locality = new System.Windows.Forms.TextBox();
   this.region = new System.Windows.Forms.TextBox();
   this.postalBox = new System.Windows.Forms.TextBox();
   this.postalCode = new System.Windows.Forms.TextBox();
   this.groupBox1 = new System.Windows.Forms.GroupBox();
   this.ok = new System.Windows.Forms.Button();
   this.cancel = new System.Windows.Forms.Button();
   this.country = new System.Windows.Forms.ComboBox();
   this.SuspendLayout();



   this.street.Location = new System.Drawing.Point(88, 8);
   this.street.Name = "street";
   this.street.Size = new System.Drawing.Size(464, 20);
   this.street.TabIndex = 0;
   this.street.Text = "";



   this.extendedAddress.Location = new System.Drawing.Point(88, 40);
   this.extendedAddress.Name = "extendedAddress";
   this.extendedAddress.Size = new System.Drawing.Size(200, 20);
   this.extendedAddress.TabIndex = 1;
   this.extendedAddress.Text = "";



   this.label1.Location = new System.Drawing.Point(40, 10);
   this.label1.Name = "label1";
   this.label1.Size = new System.Drawing.Size(56, 16);
   this.label1.TabIndex = 9;
   this.label1.Text = "&Address:";



   this.label2.Location = new System.Drawing.Point(31, 42);
   this.label2.Name = "label2";
   this.label2.Size = new System.Drawing.Size(64, 16);
   this.label2.TabIndex = 10;
   this.label2.Text = "Address &2:";



   this.label3.Location = new System.Drawing.Point(59, 74);
   this.label3.Name = "label3";
   this.label3.Size = new System.Drawing.Size(40, 16);
   this.label3.TabIndex = 11;
   this.label3.Text = "Ci&ty:";



   this.label4.Location = new System.Drawing.Point(6, 106);
   this.label4.Name = "label4";
   this.label4.Size = new System.Drawing.Size(88, 16);
   this.label4.TabIndex = 12;
   this.label4.Text = "&State/Province:";



   this.label5.Location = new System.Drawing.Point(304, 42);
   this.label5.Name = "label5";
   this.label5.Size = new System.Drawing.Size(56, 16);
   this.label5.TabIndex = 13;
   this.label5.Text = "&PO Box:";



   this.label6.Location = new System.Drawing.Point(296, 74);
   this.label6.Name = "label6";
   this.label6.Size = new System.Drawing.Size(64, 16);
   this.label6.TabIndex = 14;
   this.label6.Text = "&ZIP Code:";



   this.label7.Location = new System.Drawing.Point(304, 106);
   this.label7.Name = "label7";
   this.label7.Size = new System.Drawing.Size(48, 16);
   this.label7.TabIndex = 15;
   this.label7.Text = "Countr&y:";



   this.locality.Location = new System.Drawing.Point(88, 72);
   this.locality.Name = "locality";
   this.locality.Size = new System.Drawing.Size(200, 20);
   this.locality.TabIndex = 3;
   this.locality.Text = "";



   this.region.Location = new System.Drawing.Point(88, 104);
   this.region.Name = "region";
   this.region.Size = new System.Drawing.Size(200, 20);
   this.region.TabIndex = 4;
   this.region.Text = "";



   this.postalBox.Location = new System.Drawing.Point(352, 40);
   this.postalBox.Name = "postalBox";
   this.postalBox.Size = new System.Drawing.Size(200, 20);
   this.postalBox.TabIndex = 2;
   this.postalBox.Text = "";



   this.postalCode.Location = new System.Drawing.Point(352, 72);
   this.postalCode.Name = "postalCode";
   this.postalCode.Size = new System.Drawing.Size(200, 20);
   this.postalCode.TabIndex = 5;
   this.postalCode.Text = "";



   this.groupBox1.Location = new System.Drawing.Point(4, 136);
   this.groupBox1.Name = "groupBox1";
   this.groupBox1.Size = new System.Drawing.Size(552, 4);
   this.groupBox1.TabIndex = 16;
   this.groupBox1.TabStop = false;



   this.ok.DialogResult = System.Windows.Forms.DialogResult.OK;
   this.ok.FlatStyle = System.Windows.Forms.FlatStyle.System;
   this.ok.Location = new System.Drawing.Point(400, 152);
   this.ok.Name = "ok";
   this.ok.TabIndex = 7;
   this.ok.Text = "OK";



   this.cancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
   this.cancel.FlatStyle = System.Windows.Forms.FlatStyle.System;
   this.cancel.Location = new System.Drawing.Point(480, 152);
   this.cancel.Name = "cancel";
   this.cancel.TabIndex = 8;
   this.cancel.Text = "Cancel";



   this.country.Location = new System.Drawing.Point(352, 104);
   this.country.Name = "country";
   this.country.Size = new System.Drawing.Size(200, 21);
   this.country.TabIndex = 6;



   this.AcceptButton = this.ok;
   this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
   this.CancelButton = this.cancel;
   this.ClientSize = new System.Drawing.Size(562, 184);
   this.Controls.Add(this.country);
   this.Controls.Add(this.cancel);
   this.Controls.Add(this.ok);
   this.Controls.Add(this.groupBox1);
   this.Controls.Add(this.postalCode);
   this.Controls.Add(this.postalBox);
   this.Controls.Add(this.extendedAddress);
   this.Controls.Add(this.street);
   this.Controls.Add(this.region);
   this.Controls.Add(this.locality);
   this.Controls.Add(this.label7);
   this.Controls.Add(this.label6);
   this.Controls.Add(this.label5);
   this.Controls.Add(this.label4);
   this.Controls.Add(this.label3);
   this.Controls.Add(this.label2);
   this.Controls.Add(this.label1);
   this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
   this.MaximizeBox = false;
   this.MinimizeBox = false;
   this.Name = "FullAddress";
   this.ShowInTaskbar = false;
   this.Text = "FullAddress";
   this.ResumeLayout(false);

  }






  public string Street
  {
   get
   {
    return street.Text.Trim();
   }
   set
   {
    street.Text = value;
   }
  }




  public string ExtendedAddress
  {
   get
   {
    return extendedAddress.Text.Trim();
   }
   set
   {
    extendedAddress.Text = value;
   }
  }




  public string Locality
  {
   get
   {
    return locality.Text.Trim();
   }
   set
   {
    locality.Text = value;
   }
  }




  public string RegionAddr
  {
   get
   {
    return region.Text.Trim();
   }
   set
   {
    region.Text = value;
   }
  }




  public string PostalBox
  {
   get
   {
    return postalBox.Text.Trim();
   }
   set
   {
    postalBox.Text = value;
   }
  }




  public string PostalCode
  {
   get
   {
    return postalCode.Text.Trim();
   }
   set
   {
    postalCode.Text = value;
   }
  }




  public string Country
  {
   get
   {
    return country.Text.Trim();
   }
   set
   {
    country.Text = value;
   }
  }

 }
}
