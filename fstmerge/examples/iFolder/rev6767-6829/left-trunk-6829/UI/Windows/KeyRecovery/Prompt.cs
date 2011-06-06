using System;
using System.IO;
using System.Text.RegularExpressions;
namespace Novell.iFolder.Utility
{
 public class Prompt
 {
  static bool canPrompt = false;
  public static bool CanPrompt
  {
   get { return canPrompt; }
   set { canPrompt = value; }
  }
  private Prompt()
  {
  }
  public static string ForString(string prompt, string defaultValue)
  {
   Console.Write("{0}? [{1}]: ", prompt, defaultValue);
   string response = Console.ReadLine();
   if ((response == null) || (response.Length == 0))
   {
    response = defaultValue;
   }
   return response;
  }
  public static void ForOption(Option option)
  {
   if (!option.Assigned && option.Prompt && CanPrompt)
   {
    if (option.Description != null)
    {
     Console.WriteLine();
     Console.WriteLine("----- {0} -----", option.Title.ToUpper());
     Regex lineSplitter = new Regex(@".{0,50}[^\s]*");
     MatchCollection matches = lineSplitter.Matches(option.Description);
     foreach (Match line in matches)
     {
      Console.WriteLine(line.Value.Trim());
     }
     Console.WriteLine();
    }
    if (option.GetType() == typeof(BoolOption))
     option.Value = ForYesNo(option.Title, Boolean.Parse(option.DefaultValue)).ToString();
    else
     option.Value = ForString(option.Title, option.DefaultValue);
   }
  }
  public static bool ForYesNo(string prompt, bool defaultValue)
  {
   bool result = defaultValue;
   Console.Write("{0}? [{1}]: ", prompt, result ? "Y" : "N");
   string response = Console.ReadLine();
   if ((response != null) && (response.Length != 0))
   {
    result = response.ToLower().StartsWith("y");
   }
   return result;
  }
 }
}
