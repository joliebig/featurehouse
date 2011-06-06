using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;
using System.Runtime.InteropServices;
using Eraser.Util;
using System.Globalization;
namespace Eraser.Manager
{
 public abstract class SettingsManager
 {
  public abstract void Save();
  public Settings ModuleSettings
  {
   get
   {
    return GetSettings(new Guid(((GuidAttribute)Assembly.GetCallingAssembly().
     GetCustomAttributes(typeof(GuidAttribute), false)[0]).Value));
   }
  }
  protected abstract Settings GetSettings(Guid value);
 }
 public abstract class Settings
 {
  public abstract object this[string setting]
  {
   get;
   set;
  }
 }
 [AttributeUsage(AttributeTargets.All, Inherited = false, AllowMultiple = true)]
 public abstract class DefaultAttribute : Attribute
 {
  protected DefaultAttribute(int priority)
  {
   Priority = priority;
  }
  public int Priority
  {
   get
   {
    return priority;
   }
   private set
   {
    priority = value;
   }
  }
  private int priority;
 }
 [AttributeUsage(AttributeTargets.All, Inherited = false, AllowMultiple = false)]
 public sealed class DefaultFileErasureAttribute : DefaultAttribute
 {
  public DefaultFileErasureAttribute(int priority)
   : base(priority)
  {
  }
 }
 [AttributeUsage(AttributeTargets.All, Inherited = false, AllowMultiple = false)]
 public sealed class DefaultUnusedSpaceErasureAttribute : DefaultAttribute
 {
  public DefaultUnusedSpaceErasureAttribute(int priority)
   : base(priority)
  {
  }
 }
 [AttributeUsage(AttributeTargets.All, Inherited = false, AllowMultiple = false)]
 public sealed class DefaultPrngAttribute : DefaultAttribute
 {
  public DefaultPrngAttribute(int priority)
   : base(priority)
  {
  }
 }
 public class ManagerSettings
 {
  public ManagerSettings()
  {
   settings = ManagerLibrary.Instance.SettingsManager.ModuleSettings;
  }
  public Guid DefaultFileErasureMethod
  {
   get
   {
    if (settings["DefaultFileErasureMethod"] == null)
    {
     Guid result = FindHighestPriorityDefault(typeof(ErasureMethod),
      typeof(DefaultFileErasureAttribute));
     return result == Guid.Empty ? new Guid("{1407FC4E-FEFF-4375-B4FB-D7EFBB7E9922}") :
      result;
    }
    else
     return (Guid)settings["DefaultFileErasureMethod"];
   }
   set
   {
    settings["DefaultFileErasureMethod"] = value;
   }
  }
  public Guid DefaultUnusedSpaceErasureMethod
  {
   get
   {
    if (settings["DefaultUnusedSpaceErasureMethod"] == null)
    {
     Guid result = FindHighestPriorityDefault(typeof(UnusedSpaceErasureMethod),
      typeof(DefaultUnusedSpaceErasureAttribute));
     return result == Guid.Empty ? new Guid("{BF8BA267-231A-4085-9BF9-204DE65A6641}") :
      result;
    }
    else
     return (Guid)settings["DefaultUnusedSpaceErasureMethod"];
   }
   set
   {
    settings["DefaultUnusedSpaceErasureMethod"] = value;
   }
  }
  public Guid ActivePrng
  {
   get
   {
    if (settings["ActivePRNG"] == null)
    {
     Guid result = FindHighestPriorityDefault(typeof(Prng),
      typeof(DefaultPrngAttribute));
     return result == Guid.Empty ? new Guid("{6BF35B8E-F37F-476e-B6B2-9994A92C3B0C}") :
      result;
    }
    else
     return (Guid)settings["ActivePRNG"];
   }
   set
   {
    settings["ActivePRNG"] = value;
   }
  }
  public bool ForceUnlockLockedFiles
  {
   get
   {
    return settings["ForceUnlockLockedFiles"] == null ? true :
     Convert.ToBoolean(settings["ForceUnlockLockedFiles"],
      CultureInfo.InvariantCulture);
   }
   set
   {
    settings["ForceUnlockLockedFiles"] = value;
   }
  }
  public bool ExecuteMissedTasksImmediately
  {
   get
   {
    return settings["ExecuteMissedTasksImmediately"] == null ?
     true : Convert.ToBoolean(settings["ExecuteMissedTasksImmediately"],
      CultureInfo.InvariantCulture);
   }
   set
   {
    settings["ExecuteMissedTasksImmediately"] = value;
   }
  }
  public bool PlausibleDeniability
  {
   get
   {
    return settings["PlausibleDeniability"] == null ? false :
     Convert.ToBoolean(settings["PlausibleDeniability"],
      CultureInfo.InvariantCulture);
   }
   set
   {
    settings["PlausibleDeniability"] = value;
   }
  }
  public IList<string> PlausibleDeniabilityFiles
  {
   get
   {
    return new SettingsList<string>(settings, "PlausibleDeniabilityFiles");
   }
  }
  private static Guid FindHighestPriorityDefault(Type superClass,
   Type attributeType)
  {
   if (DefaultForAttributes.ContainsKey(attributeType) &&
    DefaultForAttributes[attributeType].ContainsKey(superClass))
   {
    return DefaultForAttributes[attributeType][superClass];
   }
   Plugin.Host pluginHost = ManagerLibrary.Instance.Host;
   ICollection<Plugin.PluginInstance> plugins = pluginHost.Plugins;
   SortedList<int, Guid> priorities = new SortedList<int, Guid>();
   foreach (Plugin.PluginInstance plugin in plugins)
   {
    byte[] pluginKey = plugin.Assembly.GetName().GetPublicKey();
    byte[] ourKey = Assembly.GetExecutingAssembly().GetName().GetPublicKey();
    if (pluginKey.Length != ourKey.Length ||
     !MsCorEEApi.VerifyStrongName(plugin.Assembly.Location))
     continue;
    bool officialPlugin = true;
    for (int i = 0, j = ourKey.Length; i != j; ++i)
     if (pluginKey[i] != ourKey[i])
      officialPlugin = false;
    if (!officialPlugin)
     continue;
    Type[] types = FindTypeAttributeInAssembly(plugin.Assembly,
     superClass, attributeType);
    if (types != null)
     foreach (Type type in types)
     {
      object[] guids =
       type.GetCustomAttributes(typeof(GuidAttribute), false);
      DefaultAttribute defaultAttr = (DefaultAttribute)
       type.GetCustomAttributes(attributeType, false)[0];
      if (guids.Length == 1)
       priorities.Add(defaultAttr.Priority,
        new Guid(((GuidAttribute)guids[0]).Value));
     }
   }
   if (priorities.Count > 0)
   {
    Guid result = priorities[priorities.Keys[priorities.Count - 1]];
    if (!DefaultForAttributes.ContainsKey(attributeType))
     DefaultForAttributes.Add(attributeType, new Dictionary<Type, Guid>());
    DefaultForAttributes[attributeType].Add(superClass, result);
    return result;
   }
   return Guid.Empty;
  }
  private static Type[] FindTypeAttributeInAssembly(Assembly assembly, Type superClass,
   Type attributeType)
  {
   Type[] types = assembly.GetExportedTypes();
   List<Type> result = new List<Type>();
   foreach (Type type in types)
   {
    if (!type.IsPublic || type.IsAbstract)
     continue;
    if (!type.IsSubclassOf(superClass))
     continue;
    object[] attributes = type.GetCustomAttributes(attributeType, false);
    if (attributes.Length > 0)
     result.Add(type);
   }
   return result.ToArray();
  }
  private static Dictionary<Type, Dictionary<Type, Guid> > DefaultForAttributes =
   new Dictionary<Type, Dictionary<Type, Guid> >();
  public IDictionary<Guid, bool> PluginApprovals
  {
   get
   {
    return new SettingsDictionary<Guid, bool>(settings, "ApprovedPlugins");
   }
  }
  private Settings settings;
  private class SettingsList<T> : IList<T>
  {
   public SettingsList(Settings settings, string settingName)
   {
    Settings = settings;
    SettingName = settingName;
    List = (List<T>)settings[settingName];
    if (List == null)
     List = new List<T>();
   }
   ~SettingsList()
   {
    Save();
   }
   public int IndexOf(T item)
   {
    return List.IndexOf(item);
   }
   public void Insert(int index, T item)
   {
    List.Insert(index, item);
    Save();
   }
   public void RemoveAt(int index)
   {
    List.RemoveAt(index);
    Save();
   }
   public T this[int index]
   {
    get
    {
     return List[index];
    }
    set
    {
     List[index] = value;
     Save();
    }
   }
   public void Add(T item)
   {
    List.Add(item);
    Save();
   }
   public void Clear()
   {
    List.Clear();
    Save();
   }
   public bool Contains(T item)
   {
    return List.Contains(item);
   }
   public void CopyTo(T[] array, int arrayIndex)
   {
    List.CopyTo(array, arrayIndex);
   }
   public int Count
   {
    get { return List.Count; }
   }
   public bool IsReadOnly
   {
    get { return false; }
   }
   public bool Remove(T item)
   {
    bool result = List.Remove(item);
    Save();
    return result;
   }
   public IEnumerator<T> GetEnumerator()
   {
    return List.GetEnumerator();
   }
   System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
   {
    return List.GetEnumerator();
   }
   private void Save()
   {
    Settings[SettingName] = List;
   }
   private Settings Settings;
   private string SettingName;
   private List<T> List;
  }
  private class SettingsDictionary<TKey, TValue> : IDictionary<TKey, TValue>
  {
   public SettingsDictionary(Settings settings, string settingName)
   {
    Settings = settings;
    SettingName = settingName;
    Dictionary = (Dictionary<TKey, TValue>)settings[settingName];
    if (Dictionary == null)
     Dictionary = new Dictionary<TKey, TValue>();
   }
   ~SettingsDictionary()
   {
    Save();
   }
   public void Add(TKey key, TValue value)
   {
    Dictionary.Add(key, value);
    Save();
   }
   public bool ContainsKey(TKey key)
   {
    return Dictionary.ContainsKey(key);
   }
   public ICollection<TKey> Keys
   {
    get { return Dictionary.Keys; }
   }
   public bool Remove(TKey key)
   {
    bool result = Dictionary.Remove(key);
    Save();
    return result;
   }
   public bool TryGetValue(TKey key, out TValue value)
   {
    return Dictionary.TryGetValue(key, out value);
   }
   public ICollection<TValue> Values
   {
    get { return Dictionary.Values; }
   }
   public TValue this[TKey key]
   {
    get
    {
     return Dictionary[key];
    }
    set
    {
     Dictionary[key] = value;
     Save();
    }
   }
   public void Add(KeyValuePair<TKey, TValue> item)
   {
    Dictionary.Add(item.Key, item.Value);
    Save();
   }
   public void Clear()
   {
    Dictionary.Clear();
    Save();
   }
   public bool Contains(KeyValuePair<TKey, TValue> item)
   {
    return Dictionary.ContainsKey(item.Key) && Dictionary[item.Key].Equals(item.Value);
   }
   public void CopyTo(KeyValuePair<TKey, TValue>[] array, int arrayIndex)
   {
    throw new NotImplementedException();
   }
   public int Count
   {
    get { return Dictionary.Count; }
   }
   public bool IsReadOnly
   {
    get { return false; }
   }
   public bool Remove(KeyValuePair<TKey, TValue> item)
   {
    if (Dictionary.ContainsKey(item.Key) && Dictionary[item.Key].Equals(item.Value))
    {
     bool result = Dictionary.Remove(item.Key);
     Save();
     return result;
    }
    return false;
   }
   public IEnumerator<KeyValuePair<TKey, TValue> > GetEnumerator()
   {
    return Dictionary.GetEnumerator();
   }
   System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
   {
    return Dictionary.GetEnumerator();
   }
   private void Save()
   {
    Settings[SettingName] = Dictionary;
   }
   private Settings Settings;
   private string SettingName;
   private Dictionary<TKey, TValue> Dictionary;
  }
 }
}
