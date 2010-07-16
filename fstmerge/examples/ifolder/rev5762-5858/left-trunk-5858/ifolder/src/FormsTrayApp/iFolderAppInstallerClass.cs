

using System;
using System.Diagnostics;
using System.Collections;
using System.ComponentModel;
using System.Configuration.Install;
using System.Xml;
using System.IO;
using System.Text;
using Simias.Client;

namespace Novell.FormsTrayApp
{




 [RunInstaller(true)]
 public class iFolderAppInstallerClass: Installer
 {
  private XmlDocument configDoc;




  public iFolderAppInstallerClass() :base()
  {

   this.Committed += new InstallEventHandler(iFolderAppInstaller_Committed);

   this.Committing += new InstallEventHandler(iFolderAppInstaller_Committing);

  }





  private void iFolderAppInstaller_Committing(object sender, InstallEventArgs e)
  {



  }





  private void iFolderAppInstaller_Committed(object sender, InstallEventArgs e)
  {



  }




  public override void Install(IDictionary savedState)
  {
   base.Install(savedState);
   Console.WriteLine("iFolderApp Install");


   string installDir = new Uri(Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().CodeBase)).LocalPath;


   string windowsDir = Environment.GetEnvironmentVariable("windir");

   fixAppConfigFile(Path.Combine(windowsDir, "explorer.exe.config"), installDir);
   fixWebConfigFile(Path.Combine(installDir, @"web\web.config"));
  }




  public override void Commit(IDictionary savedState)
  {
            Console.WriteLine("iFolderApp Commit");
   base.Commit(savedState);
  }




  public override void Rollback(IDictionary savedState)
  {
            Console.WriteLine("iFolderApp Rollback");
   base.Rollback(savedState);
  }




        public override void Uninstall(IDictionary savedState)
  {
   if (savedState == null)
   {
    throw new InstallException("iFolderApp Uninstall: savedState should not be null");
   }
   else
   {
    base.Uninstall(savedState);
    Console.WriteLine( "iFolderApp Uninstall" );


                Process[] ifolderAppProcesses = Process.GetProcessesByName("iFolderApp");
    foreach (Process process in ifolderAppProcesses)
    {
     try
     {
      process.Kill();
     }
     catch {}
     process.Close();
    }


    string path = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.CommonApplicationData), "iFolder");

    try
    {

     Directory.Delete(path, true);
    }
    catch {}
   }
  }

  private bool fixAppConfigFile(string configFilePath, string installDir)
  {
   try
   {

    configDoc = new XmlDocument();
    configDoc.Load(configFilePath);

    bool found = false;


    XmlNodeList nodeList = configDoc.GetElementsByTagName("dependentAssembly");
    foreach (XmlNode n in nodeList)
    {

     XmlNodeList nList2 = ((XmlElement)n).GetElementsByTagName("assemblyIdentity");
     foreach (XmlNode n2 in nList2)
     {
      string name = ((XmlElement)n2).GetAttribute("name");
      if (name.Equals("simiasclient"))
      {

       replaceCodeBase(n, installDir);
       found = true;
       break;
      }
     }

     if (found)
      break;
    }

    if (!found)
    {

     XmlNode assemblyBindingNode = null;
     nodeList = configDoc.GetElementsByTagName("assemblyBinding");
     foreach (XmlNode n in nodeList)
     {

      assemblyBindingNode = n;
      break;
     }

     if (assemblyBindingNode == null)
     {

      XmlNode runtime = configDoc.SelectSingleNode("/configuration/runtime");
      if (runtime == null)
      {

       runtime = configDoc.CreateNode(XmlNodeType.Element, "runtime", null);
       XmlNode configuration = configDoc.SelectSingleNode("/configuration");
       configuration.AppendChild(runtime);
      }

      assemblyBindingNode = configDoc.CreateElement(string.Empty, "assemblyBinding", "urn:schemas-microsoft-com:asm.v1");
      runtime.AppendChild(assemblyBindingNode);
     }

     if (assemblyBindingNode != null)
     {

      Uri fileURI = new Uri(Path.Combine(installDir, Path.Combine(@"web\bin", "simiasclient.dll")));


      string ns = assemblyBindingNode.GetNamespaceOfPrefix(String.Empty);


      XmlElement dependentAssembly = configDoc.CreateElement(String.Empty, "dependentAssembly", ns);
      XmlElement assemblyIdentity = configDoc.CreateElement(String.Empty, "assemblyIdentity", ns);
      assemblyIdentity.SetAttribute("name", "simiasclient");
      XmlElement codeBase = configDoc.CreateElement(String.Empty, "codeBase", ns);
      codeBase.SetAttribute("href", fileURI.AbsoluteUri);
      dependentAssembly.AppendChild(assemblyIdentity);
      dependentAssembly.AppendChild(codeBase);


      assemblyBindingNode.AppendChild(dependentAssembly);
     }
    }

    saveXmlFile(configDoc, configFilePath);
   }
   catch
   {
    return false;
   }

   return true;
  }

  private void fixWebConfigFile(string webConfigFile)
  {
   configDoc = new XmlDocument();
   configDoc.Load(webConfigFile);


   string path = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.CommonApplicationData), "iFolder");

   try
   {

    Directory.Delete(path, true);
   }
   catch {}


   if (!Directory.Exists(path))
   {
    Directory.CreateDirectory(path);
   }


   new Security().SetAccess(
    path,
    Security.CONTAINER_INHERIT_ACE | Security.OBJECT_INHERIT_ACE,
    Security.GENERIC_EXECUTE | Security.GENERIC_READ | Security.GENERIC_WRITE);

   XmlNode compilation = configDoc.DocumentElement.SelectSingleNode("/configuration/system.web/compilation");
   if (compilation != null)
   {

    ((XmlElement)compilation).SetAttribute("tempDirectory", path);
   }
   else
   {

    XmlNode webNode = configDoc.DocumentElement.SelectSingleNode("/configuration/system.web");
    if (webNode != null)
    {

     XmlElement element = configDoc.CreateElement("compilation");
     element.SetAttribute("tempDirectory", path);
     webNode.AppendChild(element);
    }
   }

   saveXmlFile(configDoc, webConfigFile);
  }

  private void replaceCodeBase(XmlNode node, string path)
  {
   XmlNode identity = node.FirstChild;
   XmlNode codebase = identity.NextSibling;


   Uri fileURI = new Uri(Path.Combine(path, Path.Combine(@"web\bin", ((XmlElement)identity).GetAttribute("name") + ".dll")));


   string ns = codebase.GetNamespaceOfPrefix(String.Empty);


   XmlElement element = configDoc.CreateElement(String.Empty, "codeBase", ns);
   element.SetAttribute("href", fileURI.AbsoluteUri);


   node.ReplaceChild(element, codebase);
  }

  private void saveXmlFile(XmlDocument doc, string path)
  {

   XmlTextWriter xtw = new XmlTextWriter(path, Encoding.UTF8);
   try
   {
    xtw.Formatting = Formatting.Indented;

    doc.WriteTo(xtw);
   }
   finally
   {
    xtw.Close();
   }
  }

  private void StopShell()
  {
  }

  private void StartShell()
  {
  }
 }
}
