

using System;
using System.IO;
using System.Reflection;
using System.Xml;

using log4net;
using log4net.Config;
using log4net.Appender;
using log4net.Repository;
using log4net.Layout;

using Simias.Client;

namespace Novell.iFolder
{



    public class iFolderLogManager
    {
        private static readonly string DefaultConfigFile = "UI.log4net";
        private static string logConfDirPath = null;

        private static bool configured = false;
        private static string configFile = null;




        private iFolderLogManager()
        {
        }




  public static string LogConfDirPath
  {
   get
   {
    return logConfDirPath;
   }
   set
   {
    logConfDirPath = value;
   }
  }




  public static string LogConfFileName
  {
   get
   {
    return DefaultConfigFile;
   }
  }




  public static string LogConfFilePath
  {
   get
   {
    return Path.Combine(LogConfDirPath, DefaultConfigFile);
   }
  }







        public static IiFolderLog GetLogger(Type type)
        {
            return new iFolderLog(LogManager.GetLogger(type));
        }




        public static void ResetConfiguration()
        {
            LogManager.ResetConfiguration();

            log4net.Config.XmlConfigurator.ConfigureAndWatch(new FileInfo(configFile));
        }





        public static void Configure(String storePath)
        {
            lock(typeof(iFolderLogManager))
            {

                if (!configured)
                {

                    configFile = Path.Combine(storePath, DefaultConfigFile);


                    if (!File.Exists(configFile))
                    {

                        File.Copy(Path.Combine(SimiasSetup.sysconfdir, DefaultConfigFile), configFile);


                        XmlDocument doc = new XmlDocument();
                        doc.Load(configFile);

                        XmlNodeList list = doc.GetElementsByTagName("file");

                        for (int i=0; i < list.Count; i++)
                        {
                            XmlNode attr = list[i].Attributes.GetNamedItem("value");

                            string logDir = Directory.GetParent(attr.Value).FullName;
                            if (!Directory.Exists(logDir))
                            {
                                Directory.CreateDirectory(logDir);
                            }

                            attr.Value = attr.Value.Replace("\\", "/");
                        }

                        list = doc.GetElementsByTagName("header");
                        for (int i=0; i < list.Count; i++)
                        {
                            XmlNode attr = list[i].Attributes.GetNamedItem("value");
                            attr.Value = attr.Value.Replace("%n", Environment.NewLine);
                        }

                        XmlTextWriter writer = new XmlTextWriter(configFile, null);
                        writer.Formatting = Formatting.Indented;
                        doc.Save(writer);
                        writer.Close();
                    }

                    ResetConfiguration();

                    configured = true;
                }
            }
        }
    }
}
