using System;
using System.Text;
using System.Text.RegularExpressions;
using Novell.AddressBook;
namespace Novell.iFolder.FormsBookLib
{
 public class FullNameParser
 {
  public FullNameParser()
  {
  }
  public static bool Parse(string fullName, ref Name name)
  {
   bool validName = true;
   if (fullName.Length > 0)
   {
    int index;
    if (name.Prefix != String.Empty &&
     (index = fullName.IndexOf(name.Prefix + " ")) != -1)
    {
     fullName = fullName.Remove(index, name.Prefix.Length + 1);
    }
    else
    {
     name.Prefix = String.Empty;
     Regex prefix = new Regex(@"mr |mr\. |mrs |mrs\. |miss |ms |ms\. |dr |dr\. ", RegexOptions.IgnoreCase);
     Match matchPrefix = prefix.Match(fullName);
     if (matchPrefix.Success)
     {
      if (matchPrefix.Index != 0)
      {
       validName = false;
      }
      name.Prefix = fullName.Substring(0, matchPrefix.Index + matchPrefix.Length - 1);
      fullName = fullName.Substring(matchPrefix.Index + matchPrefix.Length).Trim();
     }
    }
    if (name.Suffix != String.Empty &&
     (index = fullName.IndexOf(" " + name.Suffix)) != -1)
    {
     fullName = fullName.Remove(index, name.Suffix.Length + 1);
    }
    else
    {
     name.Suffix = String.Empty;
     Regex suffix = new Regex(@" i| ii| iii| jr| jr\.| sr| sr\.| esq| esq\.| md| m\.d\.", RegexOptions.IgnoreCase);
     Match matchSuffix = suffix.Match(fullName);
     if (matchSuffix.Success)
     {
      if (fullName.IndexOf(" ", matchSuffix.Index + 1) != -1)
      {
       validName = false;
      }
      name.Suffix = fullName.Substring(matchSuffix.Index + 1);
      fullName = fullName.Substring(0, matchSuffix.Index).Trim();
     }
    }
    if (fullName.Length > 0)
    {
     Regex ex = new Regex("[ ]+");
     string[] s = ex.Split(fullName);
     int length = s.Length;
     name.Given = s[0];
     if (length != 2)
     {
      if (length == 1)
      {
       validName = false;
      }
      else
      {
       StringBuilder middleName = new StringBuilder();
       for (int n = 1; n < length - 1; n++)
       {
        middleName.Append(s[n] + " ");
       }
       name.Other = middleName.ToString().Trim();
       name.Family = s[length - 1];
      }
      if (length > 3)
      {
       validName = false;
      }
     }
     else
     {
      name.Family = s[length - 1];
      name.Other = String.Empty;
     }
    }
   }
   else
   {
    name.Given = "";
    name.Other = "";
    name.Family = "";
    name.Prefix = "";
    name.Suffix = "";
   }
   return validName;
  }
 }
}
