using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Globalization;
using Eraser.Util;
namespace Eraser.DefaultPlugins
{
 public partial class CustomMethodPassEditor : UserControl
 {
  public CustomMethodPassEditor()
  {
   InitializeComponent();
   Theming.ApplyTheme(this);
  }
  [Description("The type of pass being edited")]
  [Category("Behavior")]
  public CustomMethodPassEditorPassType PassType
  {
   get
   {
    if (passTypeText.Checked)
     return CustomMethodPassEditorPassType.Text;
    else if (passTypeHex.Checked)
     return CustomMethodPassEditorPassType.Hex;
    return CustomMethodPassEditorPassType.Random;
   }
   set
   {
    switch (value)
    {
     case CustomMethodPassEditorPassType.Text:
     case CustomMethodPassEditorPassType.Hex:
      UpdateEditorSuitably();
      break;
     default:
      passTypeRandom.Checked = true;
      break;
    }
   }
  }
  [Description("The pass constant being edited.")]
  [Category("Behavior")]
  public byte[] PassData
  {
   get
   {
    switch (PassType)
    {
     case CustomMethodPassEditorPassType.Random:
      return null;
     default:
      return passData;
    }
   }
   set
   {
    passData = value;
    UpdateEditorSuitably();
   }
  }
  private static byte[] GetConstantArray(string text, bool parseHex)
  {
   if (parseHex)
   {
    string str = text.Replace(" ", "").ToUpper(CultureInfo.CurrentCulture);
    List<byte> passConstantList = new List<byte>();
    if (!string.IsNullOrEmpty(str))
    {
     for (int i = 0, j = str.Length - 2; i < j; i += 2)
      passConstantList.Add(Convert.ToByte(str.Substring(i, 2), 16));
     passConstantList.Add(Convert.ToByte(str.Substring(Math.Max(0, str.Length - 2)), 16));
    }
    byte[] result = new byte[passConstantList.Count];
    passConstantList.CopyTo(result);
    return result;
   }
   return Encoding.UTF8.GetBytes(text);
  }
  private static string GetConstantStr(byte[] array, bool asHex)
  {
   if (array == null || array.Length == 0)
    return string.Empty;
   if (asHex)
   {
    StringBuilder displayText = new StringBuilder();
    foreach (byte b in array)
     displayText.Append(string.Format(CultureInfo.CurrentCulture,
      "{0:X2} ", b, 16));
    return displayText.ToString();
   }
   else
   {
    foreach (byte b in array)
     if (b == 0)
      throw new DecoderFallbackException("The custom pass constant contains " +
       "embedded NULL bytes which cannot be represented as text.");
    UTF8Encoding encoding = new UTF8Encoding(false, true);
    return encoding.GetString(array);
   }
  }
  private void UpdateEditorSuitably()
  {
   if (passData != null && passData.Length > 0)
    try
    {
     passTxt.Text = GetConstantStr(passData, false);
     passTypeText.Checked = true;
    }
    catch (DecoderFallbackException)
    {
     passTxt.Text = GetConstantStr(passData, true);
     passTypeHex.Checked = true;
    }
  }
  private void UpdateEditor()
  {
   try
   {
    if (!passTypeRandom.Checked)
     passTxt.Text = GetConstantStr(passData, passTypeHex.Checked);
    else
     passTxt.Text = string.Empty;
    passTxt.Enabled = PassType != CustomMethodPassEditorPassType.Random;
   }
   catch (DecoderFallbackException)
   {
    passTypeHex.CheckedChanged -= passType_CheckedChanged;
    passTypeHex.Checked = true;
    passTypeHex.CheckedChanged += passType_CheckedChanged;
    MessageBox.Show(this, S._("The pass constant cannot be displayed as " +
     "text because it contains invalid characters."), S._("Eraser"),
      MessageBoxButtons.OK, MessageBoxIcon.Information,
      MessageBoxDefaultButton.Button1,
      Localisation.IsRightToLeft(this) ?
      MessageBoxOptions.RtlReading | MessageBoxOptions.RightAlign : 0);
   }
  }
  private void CustomMethodPassEditor_Validating(object sender, CancelEventArgs e)
  {
   if ((passTypeText.Checked || passTypeHex.Checked) &&
    (passData == null || passData.Length == 0))
   {
    errorProvider.SetError(passTxt, S._("The pass constant must not be empty."));
    errorProvider.SetIconPadding(passTxt, -errorProvider.Icon.Width);
    e.Cancel = true;
   }
  }
  private void passText_Validating(object sender, CancelEventArgs e)
  {
   try
   {
    passData = GetConstantArray(passTxt.Text, passTypeHex.Checked);
   }
   catch (FormatException)
   {
    e.Cancel = true;
    errorProvider.SetError(passTxt, S._("The input text is invalid " +
     "for the current data type. Valid hexadecimal characters are the " +
     "digits 0-9 and letters A-F"));
   }
  }
  private void passTxt_Validated(object sender, EventArgs e)
  {
   errorProvider.Clear();
  }
  private void passType_CheckedChanged(object sender, EventArgs e)
  {
   if (!((RadioButton)sender).Checked)
    return;
   UpdateEditor();
  }
  private byte[] passData;
 }
 public enum CustomMethodPassEditorPassType
 {
  Text,
  Hex,
  Random
 }
}
