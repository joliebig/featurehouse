

using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Reflection;
using System.Windows.Forms;
using System.Runtime.InteropServices;
using System.Security.Cryptography;
using System.Security.Cryptography.X509Certificates;
using Eraser.Util;

namespace Eraser.Manager.Plugin
{






 public abstract class Host : IDisposable
 {

  protected virtual void Dispose(bool disposing)
  {
  }




  public void Dispose()
  {
   Dispose(true);
   GC.SuppressFinalize(this);
  }





  public static Host Instance
  {
   get { return ManagerLibrary.Instance.Host; }
  }





  public abstract ICollection<PluginInstance> Plugins
  {
   get;
  }




  public abstract void Load();




  public EventHandler<PluginLoadedEventArgs> PluginLoaded { get; set; }




  internal void OnPluginLoaded(object sender, PluginLoadedEventArgs e)
  {
   if (PluginLoaded != null)
    PluginLoaded(sender, e);
  }






  public abstract void LoadPlugin(string filePath);
 }




 public class PluginLoadedEventArgs : EventArgs
 {




  public PluginLoadedEventArgs(PluginInstance instance)
  {
   Instance = instance;
  }




  public PluginInstance Instance { get; private set; }
 }




 internal class DefaultHost : Host
 {



  public DefaultHost()
  {
  }

  public override void Load()
  {
   AppDomain.CurrentDomain.AssemblyResolve += AssemblyResolve;
   AppDomain.CurrentDomain.ReflectionOnlyAssemblyResolve += ResolveReflectionDependency;
   string pluginsFolder = Path.Combine(
    Path.GetDirectoryName(Assembly.GetEntryAssembly().Location),
    PLUGINSFOLDER
   );

   foreach (string fileName in Directory.GetFiles(pluginsFolder))
   {
    FileInfo file = new FileInfo(fileName);
    if (file.Extension.Equals(".dll"))
     try
     {
      LoadPlugin(file.FullName);
     }
     catch (BadImageFormatException)
     {
     }
     catch (FileLoadException)
     {
     }
   }
  }

  protected override void Dispose(bool disposing)
  {
   if (disposing)
   {


    foreach (PluginInstance plugin in plugins)
     if (plugin.Plugin != null)
      plugin.Plugin.Dispose();
   }
  }




  public const string PLUGINSFOLDER = "Plugins";

  public override ICollection<PluginInstance> Plugins
  {
   get { return plugins.AsReadOnly(); }
  }

  public override void LoadPlugin(string filePath)
  {

   Assembly reflectAssembly = Assembly.ReflectionOnlyLoadFrom(filePath);
   PluginInstance instance = new PluginInstance(reflectAssembly, null);
   Type typePlugin = null;


   foreach (Type type in instance.Assembly.GetExportedTypes())
   {

    Type typeInterface = type.GetInterface("Eraser.Manager.Plugin.IPlugin", true);
    if (typeInterface != null)
    {
     typePlugin = type;
     break;
    }
   }



   if (typePlugin == null)
    return;


   lock (plugins)
    plugins.Add(instance);



   IDictionary<Guid, bool> approvals = ManagerLibrary.Settings.PluginApprovals;
   if ((reflectAssembly.GetName().GetPublicKey().Length == 0 ||
    !MsCorEEApi.VerifyStrongName(filePath) ||
    instance.AssemblyAuthenticode == null) &&
    !approvals.ContainsKey(instance.AssemblyInfo.Guid))
   {
    return;
   }


   instance.Assembly = Assembly.LoadFrom(filePath);



   if (reflectAssembly.GetName().GetPublicKey().Length ==
    Assembly.GetExecutingAssembly().GetName().GetPublicKey().Length)
   {
    bool sameKey = true;
    byte[] reflectAssemblyKey = reflectAssembly.GetName().GetPublicKey();
    byte[] thisAssemblyKey = Assembly.GetExecutingAssembly().GetName().GetPublicKey();
    for (int i = 0, j = reflectAssemblyKey.Length; i != j; ++i)
     if (reflectAssemblyKey[i] != thisAssemblyKey[i])
     {
      sameKey = false;
      break;
     }


    if (sameKey)
    {
     object[] attr = instance.Assembly.GetCustomAttributes(typeof(CoreAttribute), true);
     if (attr.Length != 0)
      instance.IsCore = true;
    }
   }


   if (approvals.ContainsKey(instance.AssemblyInfo.Guid) &&
    !approvals[instance.AssemblyInfo.Guid] && !instance.IsCore)
   {
    return;
   }


   IPlugin pluginInterface = (IPlugin)Activator.CreateInstance(
    instance.Assembly.GetType(typePlugin.ToString()));
   pluginInterface.Initialize(this);
   instance.Plugin = pluginInterface;


   OnPluginLoaded(this, new PluginLoadedEventArgs(instance));
  }

  private Assembly AssemblyResolve(object sender, ResolveEventArgs args)
  {
   lock (plugins)
    foreach (PluginInstance instance in plugins)
     if (instance.Assembly.FullName == args.Name)
      return instance.Assembly;
   return null;
  }

  private Assembly ResolveReflectionDependency(object sender, ResolveEventArgs args)
  {
   return Assembly.ReflectionOnlyLoad(args.Name);
  }

  private List<PluginInstance> plugins = new List<PluginInstance>();
 }




 public class PluginInstance
 {






  internal PluginInstance(Assembly assembly, IPlugin plugin)
  {
   Assembly = assembly;
   Plugin = plugin;
   IsCore = false;


   if (WintrustApi.VerifyAuthenticode(assembly.Location))
   {
    X509Certificate2 cert = new X509Certificate2(
     X509Certificate.CreateFromSignedFile(assembly.Location));
    AssemblyAuthenticode = cert;
   }
  }




  public Assembly Assembly
  {
   get
   {
    return assembly;
   }
   internal set
   {
    assembly = value;

    AssemblyInfo info = new AssemblyInfo();
    info.Version = assembly.GetName().Version;
    IList<CustomAttributeData> attributes = CustomAttributeData.GetCustomAttributes(assembly);
    foreach (CustomAttributeData attr in attributes)
     if (attr.Constructor.DeclaringType == typeof(GuidAttribute))
      info.Guid = new Guid((string)attr.ConstructorArguments[0].Value);
     else if (attr.Constructor.DeclaringType == typeof(AssemblyCompanyAttribute))
      info.Author = (string)attr.ConstructorArguments[0].Value;

    this.AssemblyInfo = info;
   }
  }




  public AssemblyInfo AssemblyInfo { get; private set; }




  public X509Certificate2 AssemblyAuthenticode { get; private set; }





  public bool IsCore { get; internal set; }




  public IPlugin Plugin { get; internal set; }

  private Assembly assembly;
 }




 public struct AssemblyInfo
 {



  public Guid Guid { get; set; }




  public string Author { get; set; }




  public Version Version { get; set; }

  public override bool Equals(object obj)
  {
   if (!(obj is AssemblyInfo))
    return false;
   return Equals((AssemblyInfo)obj);
  }

  public bool Equals(AssemblyInfo other)
  {
   return Guid == other.Guid;
  }

  public static bool operator ==(AssemblyInfo assembly1, AssemblyInfo assembly2)
  {
   return assembly1.Equals(assembly2);
  }

  public static bool operator !=(AssemblyInfo assembly1, AssemblyInfo assembly2)
  {
   return !assembly1.Equals(assembly2);
  }

  public override int GetHashCode()
  {
   return Guid.GetHashCode();
  }
 }





 public interface IPlugin : IDisposable
 {





  void Initialize(Host host);




  string Name
  {
   get;
  }







  string Author
  {
   get;
  }




  bool Configurable
  {
   get;
  }






  void DisplaySettings(Control parent);
 }






 [AttributeUsage(AttributeTargets.All, Inherited = false, AllowMultiple = true)]
 public sealed class CoreAttribute : Attribute
 {
 }
}
