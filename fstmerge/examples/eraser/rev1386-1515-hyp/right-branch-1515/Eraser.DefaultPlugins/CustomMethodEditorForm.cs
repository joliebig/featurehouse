

using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Globalization;
using Eraser.Manager;
using Eraser.Util;

namespace Eraser.DefaultPlugins
{
 public partial class CustomMethodEditorForm : Form
 {
  public CustomMethodEditorForm()
  {
   InitializeComponent();
   UXThemeApi.UpdateControlTheme(this);
  }





  public CustomErasureMethod Method
  {
   get
   {
    if (method == null)
    {
     method = new CustomErasureMethod();
     method.Guid = Guid.NewGuid();
    }


    method.Name = nameTxt.Text;


    method.RandomizePasses = randomizeChk.Checked;


    ErasureMethodPass[] passes = new ErasureMethodPass[passesLv.Items.Count];
    for (int i = 0; i < passesLv.Items.Count; ++i)
     passes[i] = (ErasureMethodPass)passesLv.Items[i].Tag;
    method.Passes = passes;

    return method;
   }
   set
   {
    method = value;


    nameTxt.Text = method.Name;


    randomizeChk.Checked = method.RandomizePasses;


    foreach (ErasureMethodPass pass in method.Passes)
     AddPass(pass);
   }
  }






  private ListViewItem AddPass(ErasureMethodPass pass)
  {
   ListViewItem item = new ListViewItem((passesLv.Items.Count + 1).ToString(
    CultureInfo.CurrentCulture));
   item.Tag = pass;
   if (pass.Function == ErasureMethod.WriteRandom)
    item.SubItems.Add(S._("Random Data"));
   else
    item.SubItems.Add(S._("Constant ({0} bytes)", ((byte[])pass.OpaqueValue).Length));

   passesLv.Items.Add(item);
   return item;
  }




  private void SavePass(ListViewItem item)
  {
   ErasureMethodPass pass = (ErasureMethodPass)item.Tag;
   if (passEditor.PassType == CustomMethodPassEditorPassType.Random)
   {
    pass.Function = ErasureMethod.WriteRandom;
    pass.OpaqueValue = null;
    item.SubItems[1].Text = S._("Random Data");
   }
   else
   {
    pass.Function = ErasureMethod.WriteConstant;
    pass.OpaqueValue = passEditor.PassData;
    item.SubItems[1].Text = S._("Constant ({0} bytes)", passEditor.PassData.Length);
   }
  }





  private void DisplayPass(ListViewItem item)
  {
   currentPass = item;
   ErasureMethodPass pass = (ErasureMethodPass)item.Tag;
   passEditor.PassData = (byte[])pass.OpaqueValue;
   passEditor.PassType = pass.Function == ErasureMethod.WriteRandom ?
    CustomMethodPassEditorPassType.Random :
    CustomMethodPassEditorPassType.Text;
  }




  private void RenumberPasses()
  {
   foreach (ListViewItem item in passesLv.Items)
    item.Text = (item.Index + 1).ToString(CultureInfo.CurrentCulture);
  }




  private void EnableButtons()
  {
   passesRemoveBtn.Enabled = passesDuplicateBtn.Enabled = passesMoveUpBtn.Enabled =
    passesMoveDownBtn.Enabled = passesLv.SelectedItems.Count >= 1;
   passGrp.Enabled = passEditor.Enabled = passesLv.SelectedItems.Count == 1;

   ListView.SelectedListViewItemCollection items = passesLv.SelectedItems;
   if (items.Count > 0)
   {
    foreach (ListViewItem item in items)
    {
     int index = item.Index;
     passesMoveUpBtn.Enabled = passesMoveUpBtn.Enabled && index > 0;
     passesMoveDownBtn.Enabled = passesMoveDownBtn.Enabled && index < passesLv.Items.Count - 1;
    }
   }
  }

  private void passesAddBtn_Click(object sender, EventArgs e)
  {
   ErasureMethodPass pass = new ErasureMethodPass(ErasureMethod.WriteRandom, null);
   ListViewItem item = AddPass(pass);

   if (passesLv.SelectedIndices.Count > 0)
   {
    item.Remove();
    passesLv.Items.Insert(passesLv.SelectedIndices[passesLv.SelectedIndices.Count - 1],
     item);
    RenumberPasses();
   }
  }

  private void passesRemoveBtn_Click(object sender, EventArgs e)
  {
   foreach (ListViewItem item in passesLv.SelectedItems)
    passesLv.Items.Remove(item);

   RenumberPasses();
  }

  private void passesDuplicateBtn_Click(object sender, EventArgs e)
  {
   foreach (ListViewItem item in passesLv.SelectedItems)
   {
    ErasureMethodPass oldPass = (ErasureMethodPass)item.Tag;
    ErasureMethodPass pass = new ErasureMethodPass(
     oldPass.Function, oldPass.OpaqueValue);
    AddPass(pass);
   }
  }

  private void passesMoveUpBtn_Click(object sender, EventArgs e)
  {

   SavePass(currentPass);

   foreach (ListViewItem item in passesLv.SelectedItems)
   {


    int index = item.Index;
    if (index >= 1)
    {
     passesLv.Items.RemoveAt(index);
     passesLv.Items.Insert(index - 1, item);
    }
   }

   RenumberPasses();
   EnableButtons();
  }

  private void passesMoveDownBtn_Click(object sender, EventArgs e)
  {

   SavePass(currentPass);

   ListView.SelectedListViewItemCollection items = passesLv.SelectedItems;
   for (int i = items.Count; i-- != 0; )
   {


    ListViewItem item = items[i];
    int index = item.Index;
    if (index < passesLv.Items.Count - 1)
    {
     passesLv.Items.RemoveAt(index);
     passesLv.Items.Insert(index + 1, item);
    }
   }

   RenumberPasses();
   EnableButtons();
  }

  private void passesLv_ItemSelectionChanged(object sender, ListViewItemSelectionChangedEventArgs e)
  {
   EnableButtons();


   if (!e.Item.Selected)
   {
    if (e.Item == currentPass)
     SavePass(e.Item);
   }
   else if (passesLv.SelectedIndices.Count == 1)
   {
    DisplayPass(passesLv.SelectedItems[0]);
   }
  }

  private void okBtn_Click(object sender, EventArgs e)
  {

   errorProvider.Clear();
   bool hasError = false;


   if (passesLv.SelectedItems.Count == 1)
    SavePass(passesLv.SelectedItems[0]);


   if (nameTxt.Text.Length == 0)
   {
    errorProvider.SetError(nameTxt, S._("The name of the custom method cannot be empty."));
    errorProvider.SetIconPadding(nameTxt, -16);
    hasError = true;
   }


   if (passesLv.Items.Count == 0)
   {
    errorProvider.SetError(passesLv, S._("The method needs to have at least one pass " +
     "defined."));
    errorProvider.SetIconPadding(passesLv, -16);
    hasError = true;
   }


   if (!hasError)
   {
    DialogResult = DialogResult.OK;
    Close();
   }
  }





  private CustomErasureMethod method;




  private ListViewItem currentPass;
 }
}
