

using System;
using System.Xml;
using System.Text;
using System.Reflection;
using System.Web.Services.Description;




class GenerateWsdl
{



 [STAThread]
 static int Main(string[] args)
 {
  int result = 0;

  if (args.Length == 0)
  {
   Console.WriteLine("USAGE: GenerateWsdl.exe [Assembly] [Type] [URL] [File]");
   result = -1;;
  }
  else
  {
   try
   {
    Assembly assembly = Assembly.LoadFrom(args[0]);
    Type type = assembly.GetType(args[1]);

    ServiceDescriptionReflector reflector = new ServiceDescriptionReflector();
    reflector.Reflect(type, args[2]);

    XmlTextWriter writer = new XmlTextWriter(args[3], Encoding.ASCII);
    writer.Formatting = Formatting.Indented;
    reflector.ServiceDescriptions[0].Write(writer);
    writer.Close();
   }
   catch(Exception ex)
   {
    Console.Error.WriteLine(ex);
    result = -1;
   }
  }

  return result;
 }
}
