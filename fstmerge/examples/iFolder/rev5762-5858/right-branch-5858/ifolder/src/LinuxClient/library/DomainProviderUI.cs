using System;
using System.Collections;
using System.IO;
using System.Reflection;
using Gtk;
using Simias.Client;
using Novell.iFolder;
namespace Novell.iFolder.DomainProvider
{
 public class DomainProviderUI
 {
  static private DomainProviderUI instance = null;
  private Hashtable registeredProviders = new Hashtable();
  private const string DllFiles = "*.dll";
  public int Count
  {
   get
   {
    return registeredProviders.Count;
   }
  }
  public IDomainProviderUI[] Providers
  {
   get
   {
    lock(typeof(DomainProviderUI))
    {
     IDomainProviderUI[] providers =
      new IDomainProviderUI[registeredProviders.Count];
     ICollection icol = registeredProviders.Values;
     icol.CopyTo(providers, 0);
     return providers;
    }
   }
  }
  static public DomainProviderUI GetDomainProviderUI()
  {
   lock(typeof(DomainProviderUI))
   {
    if (instance == null)
     instance = new DomainProviderUI();
    return instance;
   }
  }
  public IDomainProviderUI GetProviderForID(string domainID)
  {
   lock(typeof(DomainProviderUI))
   {
    IDomainProviderUI provider = null;
    if (registeredProviders.Contains(domainID))
     provider = (IDomainProviderUI)registeredProviders[domainID];
    return provider;
   }
  }
  private DomainProviderUI()
  {
   lock(this)
   {
    registeredProviders = new Hashtable();
    LoadProviders();
   }
  }
  private void LoadProviders()
  {
   string[] libFileList = null;
   if (Directory.Exists(Util.PluginsPath))
    libFileList = Directory.GetFiles(Util.PluginsPath, DllFiles);
   if (libFileList == null || libFileList.Length == 0) return;
   foreach(string libFile in libFileList)
   {
    ArrayList providers = LoadProviders(libFile);
    if (providers != null)
    {
     foreach(IDomainProviderUI provider in providers)
     {
      registeredProviders[provider.ID] = provider;
Console.WriteLine("DomainProviderUI: Added {0}", provider.Name);
     }
    }
   }
  }
  private ArrayList LoadProviders(string libFile)
  {
   lock(typeof(DomainProviderUI))
   {
    ArrayList providers = new ArrayList();
    try
    {
     Assembly assembly = Assembly.LoadFrom(libFile);
     if (assembly != null)
     {
      foreach(Type type in assembly.GetTypes())
      {
       IDomainProviderUI potentialProvider = null;
       try
       {
        potentialProvider =
         (IDomainProviderUI)assembly.CreateInstance(type.FullName);
       }
       catch{}
       if (potentialProvider != null &&
        IsProviderValid(potentialProvider))
        providers.Add((IDomainProviderUI)potentialProvider);
      }
     }
    }
    catch(Exception e)
    {
     Console.WriteLine(e.Message);
     Console.WriteLine(e.StackTrace);
    }
    return providers;
   }
  }
  private bool IsProviderValid(IDomainProviderUI potentialProvider)
  {
   return true;
  }
 }
}
