

using System;
using System.Collections;
using System.Security.Cryptography;
using System.Text.RegularExpressions;
using System.Xml;



namespace Simias.Gaim
{





 public class GaimBuddy
 {
  internal class SimiasInfo
  {
   public string userID;
   public string simiasURL;
   public RSACryptoServiceProvider credential = null;

   public SimiasInfo(string UserID, string SimiasURL, RSACryptoServiceProvider Credential)
   {
    userID = UserID;
    simiasURL = SimiasURL;
    credential = Credential;
   }
  }


  private string accountName = null;
  private string accountProtocolID = null;
  private string name = null;
  private string alias = null;
  private Hashtable simiasInfos;
  private XmlNode xmlBuddyNode = null;







  public string AccountName
  {
   get
   {
    if (accountName != null) return accountName;


    XmlAttribute attr = xmlBuddyNode.Attributes["account"];
    if (attr != null)
    {
     accountName = attr.Value;
    }

    return accountName;
   }
  }




  public string AccountProtocolID
  {
   get
   {
    if (accountProtocolID != null) return accountProtocolID;


    XmlAttribute attr = xmlBuddyNode.Attributes["proto"];
    if (attr != null)
    {
     accountProtocolID = attr.Value;
    }

    return accountProtocolID;
   }
  }




  public string Name
  {
   get
   {
    if (name != null) return name;


    XmlNode node =
     xmlBuddyNode.SelectSingleNode("name/text()");
    if (node != null)
    {
     name = node.Value;
    }

    return name;
   }
  }




  public string Alias
  {
   get
   {
    if (alias != null) return alias;


    XmlNode node =
     xmlBuddyNode.SelectSingleNode("alias/text()");
    if (node != null)
    {
     alias = node.Value;
    }

    return alias;
   }
  }

  public string MungedID
  {
   get
   {
    return AccountName + ":" + AccountProtocolID + ":" + Name;
   }
  }

  public string[] MachineNames
  {
   get
   {
    ArrayList machineNames = new ArrayList(simiasInfos.Keys);
    return (string[])machineNames.ToArray(typeof(string));
   }
  }





  public GaimBuddy(XmlNode buddyNode)
  {

   xmlBuddyNode = buddyNode;


   string protoID = this.AccountProtocolID;
   if (protoID == null)
   {
    throw new Exception("Gaim Buddy has no protocol id");
   }
   if (protoID != "prpl-oscar")
   {
    throw new GaimProtocolNotSupportedException(protoID);
   }

   simiasInfos = new Hashtable();

   ParseSimiasInfo();
  }
  internal void ParseSimiasInfo()
  {
   XmlNodeList userIDSettings = xmlBuddyNode.SelectNodes("setting[starts-with(@name, 'simias-user-id:')]");
   foreach(XmlNode userIDSettingNode in userIDSettings)
   {
    string userID = userIDSettingNode.InnerText;
    string simiasURL = null;
    string machineName = ParseMachineName(userIDSettingNode);
    if (machineName != null)
    {
     XmlNode simiasURLSetting =
      xmlBuddyNode.SelectSingleNode(string.Format("setting[@name='simias-url:{0}']/text()", machineName));
     if (simiasURLSetting != null)
     {
      simiasURL = simiasURLSetting.Value;
      XmlNode base64KeySetting =
       xmlBuddyNode.SelectSingleNode(string.Format("setting[@name='simias-public-key:{0}']/text()", machineName));
      if (base64KeySetting != null)
      {
       string base64Key = base64KeySetting.Value;
       try
       {
        string credentialXml = Base64Decode(base64Key);
        RSACryptoServiceProvider credential = new RSACryptoServiceProvider();
        credential.FromXmlString(credentialXml);
        SimiasInfo simiasInfo = new SimiasInfo(userID, simiasURL, credential);
        simiasInfos.Add(machineName, simiasInfo);
       }
       catch{}
      }
     }
    }
   }
  }
  internal string ParseMachineName(XmlNode userIDSettingNode)
  {
   string machineName = null;
   XmlAttributeCollection attribs = userIDSettingNode.Attributes;
   if (attribs != null)
   {
    for(int i = 0; i < attribs.Count; i++)
    {
     XmlNode attrib = attribs.Item(i);
     string attribValue = attrib.Value;
     if (attribValue.StartsWith("simias-user-id:"))
     {
      int colonPos = attribValue.IndexOf(':');
      if (colonPos > 0)
      {
       machineName =
        attribValue.Substring(colonPos + 1,
               attribValue.Length - colonPos - 1);
      }
     }
    }
   }
   return machineName;
  }
  public static string ParseMachineName(string simiasMemberName)
  {
   int length = simiasMemberName.Length;
   if (simiasMemberName[length - 1] != ')')
    throw new ArgumentException("The specified simiasMemberName doesn't end with a closing parenthesis");
   int openingParen = simiasMemberName.LastIndexOf('(');
   if (openingParen <= 0)
    throw new ArgumentException("The specified simiasMemberName doesn't contain an opening parenthesis or it starts the string");
   return simiasMemberName.Substring(openingParen + 1, length - openingParen - 2);
  }
  public string GetSimiasMemberName(string machineName)
  {
   return string.Format("{0} ({1})", Name, machineName);
  }
  public string GetSimiasUserID(string machineName)
  {
   if (simiasInfos.Contains(machineName))
   {
    SimiasInfo simiasInfo = (SimiasInfo)simiasInfos[machineName];
    return simiasInfo.userID;
   }
   return null;
  }
  public string GetSimiasURL(string machineName)
  {
   if (simiasInfos.Contains(machineName))
   {
    SimiasInfo simiasInfo = (SimiasInfo)simiasInfos[machineName];
    return simiasInfo.simiasURL;
   }
   return null;
  }
  public RSACryptoServiceProvider GetCredentialByUserID(string userID)
  {
   RSACryptoServiceProvider credential = null;
   foreach (SimiasInfo simiasInfo in simiasInfos.Values)
   {
    if (simiasInfo.userID.Equals(userID))
    {
     credential = simiasInfo.credential;
     break;
    }
   }
   return credential;
  }
  public string GetSimiasURLByUserID(string userID)
  {
   string simiasURL = null;
   foreach (SimiasInfo simiasInfo in simiasInfos.Values)
   {
    if (simiasInfo.userID.Equals(userID))
    {
     simiasURL = simiasInfo.simiasURL;
     break;
    }
   }
   return simiasURL;
  }
  public static bool ParseMungedID(string mungedID, out string accountName, out string accountProtocolID, out string buddyName)
  {
   accountName = null;
   accountProtocolID = null;
   buddyName = null;
   if (mungedID == null || mungedID.Length == 0)
    return false;
   int firstColon = mungedID.IndexOf(':');
   int lastColon = mungedID.LastIndexOf(':');
   if (firstColon < 0 || lastColon < 0 || firstColon == lastColon)
    return false;
   accountName = mungedID.Substring(0, firstColon);
   accountProtocolID = mungedID.Substring(firstColon + 1, lastColon - firstColon - 1);
   buddyName = mungedID.Substring(lastColon + 1);
   if (accountName.Length == 0
    || accountProtocolID.Length == 0
    || buddyName.Length == 0)
    return false;
   return true;
  }
  public static string Base64Decode(string encodedString)
  {
   try
   {
    System.Text.UTF8Encoding encoder = new System.Text.UTF8Encoding();
    System.Text.Decoder utf8Decode = encoder.GetDecoder();
    byte[] todecode_byte = Convert.FromBase64String(encodedString);
    int charCount = utf8Decode.GetCharCount(todecode_byte, 0, todecode_byte.Length);
    char[] decoded_char = new char[charCount];
    utf8Decode.GetChars(todecode_byte, 0, todecode_byte.Length, decoded_char, 0);
    string result = new String(decoded_char);
    return result;
   }
   catch (Exception e)
   {
    throw new Exception("Error decoding Base64 string: " + e.Message);
   }
  }
 }
 public class GaimAccount
 {
  private string name = null;
  private string protoID = null;
  private string alias = null;
  private XmlNode xmlNode = null;
  public string Name
  {
   get
   {
    if (name != null) return name;
    XmlNode node =
     xmlNode.SelectSingleNode("name/text()");
    if (node != null)
    {
     name = node.Value;
    }
    return name;
   }
  }
  public string ProtocolID
  {
   get
   {
    if (protoID != null) return protoID;
    XmlNode node =
     xmlNode.SelectSingleNode("protocol/text()");
    if (node != null)
    {
     protoID = node.Value;
    }
    return protoID;
   }
  }
  public string Alias
  {
   get
   {
    if (alias != null) return alias;
    XmlNode node =
     xmlNode.SelectSingleNode("alias/text()");
    if (node != null)
    {
     alias = node.Value;
    }
    return alias;
   }
  }
  public GaimAccount(XmlNode accountNode)
  {
   xmlNode = accountNode;
  }
 }
 public class GaimProtocolNotSupportedException : Exception
 {
  public GaimProtocolNotSupportedException(string protoID):
   base(string.Format("This protocol is not supported in the Gaim iFolder Plugin: {0}", protoID))
  {
  }
 }
}
