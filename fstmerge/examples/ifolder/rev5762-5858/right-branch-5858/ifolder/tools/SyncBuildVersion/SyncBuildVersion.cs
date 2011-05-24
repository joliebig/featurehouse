using System;
using System.Diagnostics;
using System.IO;
using System.Text;
using System.Xml;

namespace Novell.iFolder.Build
{



 class SyncBuildVersion
 {



  [STAThread]
  static void Main(string[] args)
  {
   if ( args.Length != 2 )
   {
    Console.WriteLine( "Useage: iFolderVersionUpdate <iFolder Windows Package Directory> <Final bin directory>" );
    Environment.Exit( -2 );
   }


   string iFolderAppFileName = Path.Combine( args[ 1 ], "iFolderApp.exe" );
   if ( !File.Exists( iFolderAppFileName ) )
   {
    Console.WriteLine( "Error: Cannot file file: {0}", iFolderAppFileName );
    Environment.Exit( -1 );
   }


   FileVersionInfo versionInfo = FileVersionInfo.GetVersionInfo( iFolderAppFileName );
   if ( versionInfo.ProductVersion == null )
   {
    Console.WriteLine( "Cannot get version from file: {0}", iFolderAppFileName );
   }


   string msiFileName = Path.Combine( args[ 0 ], "ifolder-msi.ism" );
   if ( !File.Exists( msiFileName ) )
   {
    Console.WriteLine( "Error: Cannot file file: {0}", msiFileName );
    Environment.Exit( -1 );
   }


   Console.WriteLine( "Updating version: {0} from {1} to {2}", versionInfo.ProductVersion, iFolderAppFileName, msiFileName );

   bool updatedVersion = false;
   try
   {

    XmlDocument document = new XmlDocument();
    document.Load( msiFileName );


    XmlNamespaceManager nsmgr = new XmlNamespaceManager( document.NameTable );
    nsmgr.AddNamespace( "dt", document.DocumentElement.NamespaceURI );


    XmlNode parentNode = document.DocumentElement.SelectSingleNode( "table[@name='Property']", nsmgr );
    if ( parentNode != null )
    {

     XmlNode childNode = parentNode.SelectSingleNode( "row/td[.='ProductVersion']", nsmgr );
     if ( childNode != null )
     {

      XmlNode versionNode = childNode.NextSibling;
      if ( versionNode != null )
      {

       versionNode.InnerText = versionInfo.ProductVersion;


       XmlTextWriter xtw = new XmlTextWriter( msiFileName, Encoding.ASCII );
       try
       {
        xtw.Formatting = Formatting.Indented;
        document.WriteTo( xtw );
        updatedVersion = true;
       }
       finally
       {
        xtw.Close();
       }
      }
     }
    }
   }
   catch
   {}


   Environment.Exit( updatedVersion ? 0 : -1 );
  }
 }
}
