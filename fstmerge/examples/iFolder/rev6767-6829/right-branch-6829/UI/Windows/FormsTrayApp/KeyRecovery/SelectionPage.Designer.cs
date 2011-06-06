using System;
using System.Windows.Forms;
using System.Drawing;
using System.ComponentModel;
using System.Collections;
namespace Novell.Wizard
{
 public partial class SelectionPage:Novell.Wizard.InteriorPageTemplate
 {
  private System.Windows.Forms.Label label2;
private System.Windows.Forms.RadioButton singleWizRadio;
private System.Windows.Forms.RadioButton exportRadio;
private RadioButton importRadio;
  private void InitializeComponent()
        {
            this.label2 = new System.Windows.Forms.Label();
            this.singleWizRadio = new System.Windows.Forms.RadioButton();
            this.exportRadio = new System.Windows.Forms.RadioButton();
            this.importRadio = new System.Windows.Forms.RadioButton();
            this.SuspendLayout();
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(29, 72);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(215, 13);
            this.label2.TabIndex = 1;
            this.label2.Text = "Select an option to recover your passphrase";
            this.singleWizRadio.AutoSize = true;
            this.singleWizRadio.Location = new System.Drawing.Point(105, 98);
            this.singleWizRadio.Name = "singleWizRadio";
            this.singleWizRadio.Size = new System.Drawing.Size(251, 17);
            this.singleWizRadio.TabIndex = 3;
            this.singleWizRadio.Text = global::TrayApp.Properties.Resources.selectionPageSecond;
            this.singleWizRadio.UseVisualStyleBackColor = true;
            this.singleWizRadio.CheckedChanged += new System.EventHandler(this.exportRadio_CheckedChanged);
            this.exportRadio.AutoSize = true;
            this.exportRadio.Location = new System.Drawing.Point(105, 148);
            this.exportRadio.Name = "exportRadio";
            this.exportRadio.Size = new System.Drawing.Size(206, 17);
            this.exportRadio.TabIndex = 4;
            this.exportRadio.TabStop = true;
            this.exportRadio.Text = global::TrayApp.Properties.Resources.selectionPageFour;
            this.exportRadio.UseVisualStyleBackColor = true;
            this.exportRadio.CheckedChanged += new System.EventHandler(this.importradio_CheckedChanged);
            this.importRadio.AutoSize = true;
            this.importRadio.Location = new System.Drawing.Point(105, 124);
            this.importRadio.Name = "importRadio";
            this.importRadio.Size = new System.Drawing.Size(159, 17);
            this.importRadio.TabIndex = 5;
            this.importRadio.Text = global::TrayApp.Properties.Resources.selectionPageThird;
            this.importRadio.UseVisualStyleBackColor = true;
            this.importRadio.CheckedChanged += new System.EventHandler(this.recoverButton_CheckedChanged);
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.importRadio);
            this.Controls.Add(this.exportRadio);
            this.Controls.Add(this.singleWizRadio);
            this.Controls.Add(this.label2);
            this.Name = "SelectionPage";
            this.Size = new System.Drawing.Size(579, 413);
            this.Controls.SetChildIndex(this.label2, 0);
            this.Controls.SetChildIndex(this.singleWizRadio, 0);
            this.Controls.SetChildIndex(this.exportRadio, 0);
            this.Controls.SetChildIndex(this.importRadio, 0);
            this.ResumeLayout(false);
            this.PerformLayout();
        }
  protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }
 }
}
