


using System;
using System.IO;
using System.Text;

using Novell.iFolder;

namespace Novell.iFolder.Tests
{
 public class Tests
 {
  static void Main()
  {
   iFolderWebService ifws;

   try
   {
    ifws = new iFolderWebService();

    iFolderSettings ifSettings = ifws.GetSettings();

   }
   catch(Exception e)
   {
    Console.WriteLine(e);
   }
  }
 }
}
