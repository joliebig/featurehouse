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
 public static class Localisation
 {
  public static bool IsRightToLeft(Control control)
  {
   while (control != null)
   {
    switch (control.RightToLeft)
    {
     case RightToLeft.No:
      return false;
     case RightToLeft.Yes:
      return true;
     default:
      control = control.Parent;
      break;
    }
   }
   if (Application.OpenForms.Count > 0)
   {
    return IsRightToLeft(Application.OpenForms[0]);
   }
   else
   {
    using (Form form = new Form())
     return IsRightToLeft(form);
   }
  }
  public static string TranslateText(string text, Assembly assembly)
  {
   if (text.Length == 0)
    return text;
   if (!managers.ContainsKey(Thread.CurrentThread.CurrentUICulture))
    managers[Thread.CurrentThread.CurrentUICulture] =
     new Dictionary<Assembly, ResourceManager>();
   Dictionary<Assembly, ResourceManager> assemblies = managers[
    Thread.CurrentThread.CurrentUICulture];
   ResourceManager res = null;
   if (!assemblies.ContainsKey(assembly))
   {
    string languageID = string.Empty;
    Assembly languageAssembly = LoadLanguage(Thread.CurrentThread.CurrentUICulture,
     assembly, out languageID);
    string resourceName = Path.GetFileNameWithoutExtension(assembly.Location) +
     ".Strings" + (languageID.Length != 0 ? ("." + languageID) : "");
    res = new ResourceManager(resourceName,
     languageAssembly != null ? languageAssembly : assembly);
    assemblies[assembly] = res;
   }
   else
    res = assemblies[assembly];
   string result = res.GetString(Escape(text), Thread.CurrentThread.CurrentUICulture);
   return string.IsNullOrEmpty(result) || result == "(Untranslated)" ? text : Unescape(result);
  }
  public static bool LocalisationExists(CultureInfo culture, Assembly assembly)
  {
   return File.Exists(Path.Combine(
    Path.Combine(Path.GetDirectoryName(assembly.Location), culture.Name),
    Path.GetFileNameWithoutExtension(assembly.Location) + ".resources.dll"));
  }
  public static IList<CultureInfo> Localisations
  {
   get
   {
    List<CultureInfo> result = new List<CultureInfo>();
    Assembly assembly = Assembly.GetEntryAssembly();
    foreach (CultureInfo info in CultureInfo.GetCultures(CultureTypes.AllCultures))
    {
     if (string.IsNullOrEmpty(info.Name))
      continue;
     else if (LocalisationExists(info, assembly))
      result.Add(info);
    }
    if (result.Count == 0)
     result.Add(CultureInfo.GetCultureInfo("EN"));
    return result.AsReadOnly();
   }
  }
  private static string Escape(string str)
  {
   return str.Replace("\n", "\\n").Replace("\r", "\\r");
  }
  private static string Unescape(string str)
  {
   return str.Replace("\\n", "\n").Replace("\\r", "\r");
  }
  private static Assembly LoadLanguage(CultureInfo culture, Assembly assembly,
   out string languageID)
  {
   languageID = string.Empty;
   string path = string.Empty;
   while (culture != CultureInfo.InvariantCulture)
   {
    path = Path.Combine(Path.GetDirectoryName(assembly.Location), culture.Name);
    if (Directory.Exists(path))
    {
     string assemblyPath = Path.Combine(path,
      Path.GetFileNameWithoutExtension(assembly.Location) + ".resources.dll");
     if (File.Exists(assemblyPath))
     {
      languageID = culture.Name;
      return Assembly.LoadFrom(assemblyPath);
     }
    }
    culture = culture.Parent;
   }
   return null;
  }
  private static Dictionary<CultureInfo, Dictionary<Assembly, ResourceManager> > managers =
   new Dictionary<CultureInfo, Dictionary<Assembly, ResourceManager> >();
 }
 [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "S")]
 public static class S
 {
  [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA1707:IdentifiersShouldNotContainUnderscores")]
  [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "_")]
  public static string _(string text)
  {
   return Localisation.TranslateText(text, Assembly.GetCallingAssembly());
  }
  [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA1707:IdentifiersShouldNotContainUnderscores")]
  [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "_")]
  public static string _(string text, params object[] args)
  {
   string localStr = Localisation.TranslateText(text, Assembly.GetCallingAssembly());
   return string.Format(CultureInfo.CurrentCulture, localStr, args);
  }
 }
}
