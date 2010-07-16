

using System;
using System.Collections;
using System.Collections.Specialized;
using System.IO;
using System.Text;
using System.Xml;
using System.Threading;

namespace Novell.iFolder
{



 public class ClientConfig
 {

  private static string RootElementTag = "configuration";
  private static string SectionTag = "section";
  private static string SettingTag = "setting";
  private static string NameAttr = "name";
  private static string ValueAttr = "value";
  private static string DefaultSection = "iFolderClientConfig";
  private static string DefaultFileName = "ifolder3.config";

  public static string KEY_SHOW_CREATION = "ShowCreationDialog";
  public static string KEY_NOTIFY_IFOLDERS = "NotifyiFolders";
  public static string KEY_NOTIFY_COLLISIONS = "NotifyCollisions";
  public static string KEY_NOTIFY_USERS = "NotifyUsers";

  public static string KEY_SYNC_UNIT = "SyncUnit";

  public static string KEY_IFOLDER_WINDOW_X_POS = "iFolderWindowXPos";
  public static string KEY_IFOLDER_WINDOW_Y_POS = "iFolderWindowYPos";
  public static string KEY_IFOLDER_WINDOW_WIDTH = "iFolderWindowWidth";
  public static string KEY_IFOLDER_WINDOW_HEIGHT = "iFolderWindowHeight";
  public static string KEY_IFOLDER_WINDOW_VISIBLE = "iFolderWindowVisible";

  public static string KEY_SHOW_SERVER_IFOLDERS = "ShowServeriFolders";


  private static XmlDocument configDoc;






  private static string DefaultFilePath
  {
   get { return Path.Combine(DefaultPath, DefaultFileName); }
  }

  private static string DefaultPath
  {
   get
   {
    string path = Environment.GetFolderPath(
       Environment.SpecialFolder.LocalApplicationData);

    if ((path == null) || (path.Length == 0))
    {
     path = Environment.GetFolderPath(
        Environment.SpecialFolder.ApplicationData);
    }

    return fixupPath(path);
   }
  }







  static ClientConfig()
  {

   if (!File.Exists(DefaultFilePath))
   {
    XmlDocument document = new XmlDocument();
    document.AppendChild(document.CreateElement(RootElementTag));
    document.Save(DefaultFilePath);
   }


   configDoc = new XmlDocument();
   configDoc.Load(DefaultFilePath);
  }



  private static XmlElement GetSection(string section, ref bool changed)
  {
   string str = string.Format("//section[@name='{0}']", section);
   XmlElement sectionElement =
    (XmlElement)configDoc.DocumentElement.SelectSingleNode(str);
   if(sectionElement == null)
   {

    sectionElement = configDoc.CreateElement(SectionTag);
    sectionElement.SetAttribute(NameAttr, section);
    configDoc.DocumentElement.AppendChild(sectionElement);
    changed = true;
   }

   return sectionElement;
  }

  private static XmlElement GetKey(string section, string key,
     ref bool changed)
  {

   XmlElement sectionElement = GetSection(section, ref changed);

   string str = string.Format("//{0}[@{1}='{2}']/{3}[@{1}='{4}']",
      SectionTag, NameAttr, section, SettingTag, key);
   XmlElement keyElement =
    (XmlElement)sectionElement.SelectSingleNode(str);
   if (keyElement == null)
   {

    keyElement = configDoc.CreateElement(SettingTag);
    keyElement.SetAttribute(NameAttr, key);
    sectionElement.AppendChild(keyElement);
    changed = true;
   }

   return keyElement;
  }

  private static void UpdateConfigFile()
  {
   XmlTextWriter xtw = new XmlTextWriter(DefaultFilePath,
      Encoding.ASCII);
   try
   {
    xtw.Formatting = Formatting.Indented;
    configDoc.WriteTo(xtw);
   }
   finally
   {
    xtw.Close();
   }
  }

  private static string fixupPath(string path)
  {
   if ((path.EndsWith("ifolder") == false) &&
    (path.EndsWith("ifolder/") == false) &&
    (path.EndsWith(@"ifolder\") == false))
   {
    path = Path.Combine(path, "ifolder");
   }

   if (!Directory.Exists(path))
   {
    Directory.CreateDirectory(path);
   }
   return path;
  }


  private static bool KeyExists(string section, string key)
  {
   bool foundKey = false;

   string str = string.Format("",
        SectionTag, NameAttr, section);
   XmlElement sectionElement =
    (XmlElement)configDoc.DocumentElement.SelectSingleNode(str);
   if(sectionElement != null)
   {
    str = string.Format("//{0}[@{1}='{2}']/{3}[@{1}='{4}']",
      SectionTag, NameAttr, section, SettingTag, key);
    if(sectionElement.SelectSingleNode(str) != null)
    {
     foundKey = true;
    }
   }

   return foundKey;
  }
  public static XmlElement GetElement(string key)
  {
   return GetElement(DefaultSection, key);
  }
  public static XmlElement GetElement(string section, string key)
  {
   lock(typeof(ClientConfig))
   {
    bool changed = false;
    XmlElement element = GetKey(section, key, ref changed);
    if (changed)
    {
     UpdateConfigFile();
    }
    return element.Clone() as XmlElement;
   }
  }
  public static void SetElement(string section, string key,
    XmlElement newElement)
  {
   lock(typeof(ClientConfig))
   {
    bool changed = false;
    XmlElement keyElement = GetKey(section, key, ref changed);
    keyElement.InnerXml = newElement.InnerXml;
    UpdateConfigFile();
   }
  }
  public static string Get(string key, string defaultValue)
  {
   return Get(DefaultSection, key, defaultValue);
  }
  public static string Get(string section, string key,
    string defaultValue)
  {
   lock(typeof(ClientConfig))
   {
    bool changed = false;
    XmlElement keyElement = GetKey(section, key, ref changed);
    string keyValue = keyElement.GetAttribute(ValueAttr);
    if (keyValue == string.Empty)
    {
     if (defaultValue != null )
     {
      keyElement.SetAttribute(ValueAttr, defaultValue);
      keyValue = defaultValue;
      changed = true;
     }
     else
     {
      keyValue = null;
     }
    }
    if (changed)
    {
     UpdateConfigFile();
    }
    return keyValue;
   }
  }
  public static void Set(string key, string keyValue)
  {
   Set(DefaultSection, key, keyValue);
  }
  public static void Set(string section, string key, string keyValue)
  {
   lock(typeof(ClientConfig))
   {
    bool changed = false;
    XmlElement keyElement = GetKey(section, key, ref changed);
    keyElement.SetAttribute(ValueAttr, keyValue);
    UpdateConfigFile();
   }
  }
  public static bool Exists(string key)
  {
   return Exists(DefaultSection, key);
  }
  public static bool Exists(string section, string key)
  {
   lock(typeof(ClientConfig))
   {
    return KeyExists(section, key);
   }
  }
  public static void DeleteKey(string key)
  {
   DeleteKey(DefaultSection, key);
  }
  public static void DeleteKey(string section, string key)
  {
   lock(typeof(ClientConfig))
   {
    if (KeyExists(section, key))
    {
     bool changed = false;
     XmlElement sectionElement =
        GetSection(section, ref changed);
     XmlElement keyElement = GetKey(section, key, ref changed);
     sectionElement.RemoveChild(keyElement);
     UpdateConfigFile();
    }
   }
  }
 }
}
