

using System;
using System.Collections.Generic;
using System.Text;
using System.Globalization;
using System.Reflection;
using System.IO;

namespace Eraser.Manager
{



 public class Language
 {





  public Language(CultureInfo info)
  {
   culture = info;
  }

  public override string ToString()
  {
   return culture.DisplayName;
  }
  public string Name
  {
   get { return culture.Name; }
  }
  public string DisplayName
  {
   get { return culture.DisplayName; }
  }
  public string EnglishName
  {
   get { return culture.EnglishName; }
  }
  public static explicit operator CultureInfo(Language lang)
  {
   return lang.culture;
  }
  public override int GetHashCode()
  {
   return culture.GetHashCode();
  }
  CultureInfo culture;
 }
 public static class LanguageManager
 {
  public static IList<Language> Items
  {
   get
   {
    List<Language> result = new List<Language>();
    foreach (CultureInfo info in CultureInfo.GetCultures(CultureTypes.AllCultures))
    {
     if (string.IsNullOrEmpty(info.Name))
      continue;
     else if (new DirectoryInfo(Path.GetDirectoryName(Assembly.GetEntryAssembly().Location) +
      Path.DirectorySeparatorChar + info.Name).Exists)
      result.Add(new Language(info));
    }
    if (result.Count == 0)
     result.Add(new Language(CultureInfo.GetCultureInfo("EN")));
    return result;
   }
  }
 }
}
