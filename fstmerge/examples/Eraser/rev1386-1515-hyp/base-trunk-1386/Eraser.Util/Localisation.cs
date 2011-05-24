

using System;
using System.Collections.Generic;
using System.Text;

using System.IO;
using System.Reflection;
using System.Globalization;
using System.Windows.Forms;
using System.Diagnostics;
using System.Resources;
using System.Threading;

namespace Eraser.Util
{




 public static class S
 {





  public static string _(string str)
  {
   return TranslateText(str, Assembly.GetCallingAssembly());
  }
  public static string _(string str, params object[] args)
  {
   string localStr = TranslateText(str, Assembly.GetCallingAssembly());
   return string.Format(CultureInfo.CurrentCulture, localStr, args);
  }
  public static bool IsRightToLeft(Control control)
  {
   if (control == null)
    return CultureInfo.CurrentCulture.TextInfo.IsRightToLeft;
   switch (control.RightToLeft)
   {
    case RightToLeft.No:
     return false;
    case RightToLeft.Yes:
     return true;
    default:
     return IsRightToLeft(control.Parent);
   }
  }
  public static string TranslateText(string str, Assembly assembly)
  {
   if (str.Length == 0)
    return str;
   if (!managers.ContainsKey(Thread.CurrentThread.CurrentUICulture))
    managers[Thread.CurrentThread.CurrentUICulture] =
     new Dictionary<Assembly, ResourceManager>();
   Dictionary<Assembly, ResourceManager> assemblies = managers[
    Thread.CurrentThread.CurrentUICulture];
   ResourceManager res = null;
   if (!assemblies.ContainsKey(assembly))
   {
    string languageID = string.Empty;
    Assembly languageAssembly = LoadLanguage(Path.GetDirectoryName(
     Assembly.GetEntryAssembly().Location), Thread.CurrentThread.CurrentUICulture,
     assembly, out languageID);
    string resourceName = Path.GetFileNameWithoutExtension(assembly.Location) +
     ".Strings" + (languageID.Length != 0 ? ("." + languageID) : "");
    res = new ResourceManager(resourceName,
     languageAssembly != null ? languageAssembly : assembly);
    assemblies[assembly] = res;
   }
   else
    res = assemblies[assembly];
   string result = res.GetString(Escape(str), Thread.CurrentThread.CurrentUICulture);
   return string.IsNullOrEmpty(result) || result == "(Untranslated)" ? str : Unescape(result);
  }
  private static string Escape(string str)
  {
   return str.Replace("\n", "\\n").Replace("\r", "\\r");
  }
  private static string Unescape(string str)
  {
   return str.Replace("\\n", "\n").Replace("\\r", "\r");
  }
  private static Assembly LoadLanguage(string directory, CultureInfo culture, Assembly assembly,
   out string languageID)
  {
   languageID = string.Empty;
   string path = string.Empty;
   while (culture != CultureInfo.InvariantCulture)
   {
    path = Path.Combine(directory, culture.Name);
    if (System.IO.Directory.Exists(path))
    {
     string assemblyPath = Path.Combine(path,
      Path.GetFileNameWithoutExtension(assembly.Location) + ".resources.dll");
     if (System.IO.File.Exists(assemblyPath))
     {
      languageID = culture.Name;
      return Assembly.LoadFile(assemblyPath);
     }
    }
    culture = culture.Parent;
   }
   return null;
  }
  private static Dictionary<CultureInfo, Dictionary<Assembly, ResourceManager> > managers =
   new Dictionary<CultureInfo, Dictionary<Assembly, ResourceManager> >();
 }
}
