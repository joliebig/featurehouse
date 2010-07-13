using System;
using System.ComponentModel;
using System.Windows.Forms;
using System.Globalization;


namespace Aga.Controls
{






 public class NumericTextBox : TextBox
 {
  private const int WM_PASTE = 0x302;
  private NumberStyles numberStyle = NumberStyles.AllowDecimalPoint | NumberStyles.AllowLeadingSign;







  protected override void OnKeyPress(KeyPressEventArgs e)
  {
   base.OnKeyPress(e);

   e.Handled = invalidNumeric(e.KeyChar);
  }
  private bool invalidNumeric(char key)
  {
   bool handled = false;
   NumberFormatInfo numberFormatInfo = CultureInfo.CurrentCulture.NumberFormat;
   string decimalSeparator = numberFormatInfo.NumberDecimalSeparator;
   string negativeSign = numberFormatInfo.NegativeSign;
   string keyString = key.ToString();
   if (Char.IsDigit(key))
   {
   }
   else if (AllowDecimalSeperator && keyString.Equals(decimalSeparator))
   {
    if (Text.IndexOf(decimalSeparator) >= 0)
    {
     handled = true;
    }
   }
   else if (AllowNegativeSign && keyString.Equals(negativeSign))
   {
    if (Text.IndexOf(negativeSign) >= 0)
    {
     handled = true;
    }
   }
   else if (key == '\b')
   {
   }
   else if ((ModifierKeys & (Keys.Control)) != 0)
   {
   }
   else
   {
    handled = true;
   }
   return handled;
  }
  protected override void WndProc(ref Message m)
  {
   switch (m.Msg)
   {
    case WM_PASTE:
     {
      IDataObject clipboardData = Clipboard.GetDataObject();
      string pasteText = (string)clipboardData.GetData(
        DataFormats.UnicodeText);
      int selectionLength = SelectionLength;
      if (pasteText.Length == 0)
      {
       break;
      }
      else if (selectionLength != 0)
      {
       base.Text = base.Text.Remove(SelectionStart, selectionLength);
      }
      bool containsInvalidChars = false;
      foreach (char c in pasteText)
      {
       if (containsInvalidChars)
       {
        break;
       }
       else if (invalidNumeric(c))
       {
        containsInvalidChars = true;
       }
      }
      if (!containsInvalidChars)
      {
       base.Text = base.Text.Insert(SelectionStart, pasteText);
      }
      return;
     }
   }
   base.WndProc(ref m);
  }
  public int IntValue
  {
   get
   {
    int intValue;
    Int32.TryParse(this.Text, numberStyle, CultureInfo.CurrentCulture.NumberFormat, out intValue);
    return intValue;
   }
  }
  public decimal DecimalValue
  {
   get
   {
    decimal decimalValue;
    Decimal.TryParse(this.Text, numberStyle, CultureInfo.CurrentCulture.NumberFormat, out decimalValue);
    return decimalValue;
   }
  }
  private bool allowNegativeSign;
  [DefaultValue(true)]
  public bool AllowNegativeSign
  {
   get { return allowNegativeSign; }
   set { allowNegativeSign = value; }
  }
  private bool allowDecimalSeperator;
  [DefaultValue(true)]
  public bool AllowDecimalSeperator
  {
   get { return allowDecimalSeperator; }
   set { allowDecimalSeperator = value; }
  }
 }
}
