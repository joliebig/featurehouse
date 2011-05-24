

using System;
using System.Collections;
using System.IO;
using System.Security.Cryptography;
using System.Text;
using System.Threading;
using System.Web;
using System.Web.Services;
using System.Web.Services.Protocols;

using Simias;
using Simias.Client;
using Simias.Domain;
using Simias.Storage;
using Simias.Sync;
using Simias.POBox;

namespace Simias.Gaim.DomainService
{



 [WebService(
  Namespace="http://novell.com/simias/domain",
  Name="Gaim Domain Service",
  Description="Web Service providing access to Gaim Domain Server functionality.")]
 public class GaimDomainService : System.Web.Services.WebService
 {



  private static readonly ISimiasLog log =
   SimiasLogManager.GetLogger(System.Reflection.MethodBase.GetCurrentMethod().DeclaringType);




  public GaimDomainService()
  {
  }







  [WebMethod(EnableSession=true)]
  [SoapDocumentMethod]
  public GaimDomainInfo GetGaimDomainInfo()
  {

   Simias.Storage.Domain domain = GaimDomain.GetDomain();
   if ( domain == null )
   {
    throw new SimiasException( "Gaim domain does not exist" );
   }

   GaimDomainInfo info = new GaimDomainInfo();
   info.ID = domain.ID;
   info.Name = domain.Name;
   info.Description = domain.Description;

   return info;
  }




  [WebMethod(Description="SynchronizeMemberList", EnableSession=true)]
  [SoapDocumentMethod]
  public void SynchronizeMemberList()
  {
   log.Debug("GaimDomainService.SynchronizeMemberList() entered");
   Simias.Gaim.Sync.SyncNow(null);
  }
  [WebMethod(Description="UpdateMember", EnableSession=true)]
  [SoapDocumentMethod]
  public void UpdateMember(string AccountName, string AccountProtocolID, string BuddyName, string MachineName)
  {
   log.Debug("GaimDomainService.UpdateMember() entered");
   GaimDomain.UpdateMember(AccountName, AccountProtocolID, BuddyName, MachineName);
  }
  [WebMethod(Description="GetUserInfo", EnableSession=true)]
  [SoapDocumentMethod]
  public bool GetUserInfo(out string MachineName, out string UserID, out string SimiasURL)
  {
   MachineName = null;
   UserID = null;
   SimiasURL = null;
   MachineName = GetMachineName();
   Simias.Storage.Domain domain = GaimDomain.GetDomain();
   if (domain != null)
   {
    UserID = Store.GetStore().GetUserIDFromDomainID(domain.ID);
   }
   SimiasURL = Manager.LocalServiceUrl.ToString();
   if (MachineName != null && MachineName.Length > 0
    && UserID != null && UserID.Length > 0
    && SimiasURL != null && SimiasURL.Length > 0)
    return true;
   return false;
  }
  [WebMethod(Description="GetMachineName returns the machine name of the computer we're running on.", EnableSession=true)]
  [SoapDocumentMethod]
  public string GetMachineName()
  {
   string machineName = Environment.MachineName.ToLower();
   int firstDot = machineName.IndexOf('.');
   if (firstDot > 0)
    machineName = machineName.Substring(0, firstDot);
   return machineName;
  }
  [WebMethod(Description="GetRSACredential", EnableSession=true)]
  [SoapDocumentMethod]
  public bool GetRSACredential(out string PublicCredential, out string PrivateCredential)
  {
   PublicCredential = null;
   PrivateCredential = null;
   Identity ident = Store.GetStore().CurrentUser;
   RSACryptoServiceProvider rsaProvider = ident.Credential;
   PublicCredential = rsaProvider.ToXmlString(false);
   PrivateCredential = rsaProvider.ToXmlString(true);
   if (PublicCredential != null && PublicCredential.Length > 0
    && PrivateCredential != null && PrivateCredential.Length > 0)
    return true;
   return false;
  }
  [WebMethod(Description="RSAEncryptString() uses RsaCryptoXml (A .NET RSACryptoServiceProvider in XML format representing a public key) to encrypt UnencryptedString.", EnableSession=true)]
  [SoapDocumentMethod]
  public string RSAEncryptString(string RsaCryptoXml, string UnencryptedString)
  {
   RSACryptoServiceProvider rsaProvider = new RSACryptoServiceProvider();
   rsaProvider.FromXmlString(RsaCryptoXml);
   byte[] stringBytes = new UTF8Encoding().GetBytes(UnencryptedString);
   byte[] encryptedText = rsaProvider.Encrypt(stringBytes, false);
   return Convert.ToBase64String(encryptedText);
  }
  [WebMethod(Description="RSADecryptString() uses RsaCryptoXml (A .NET RSACryptoServiceProvider in XML format representing a private key) to decrypt EncryptedString.", EnableSession=true)]
  [SoapDocumentMethod]
  public string RSADecryptString(string RsaCryptoXml, string EncryptedString)
  {
   RSACryptoServiceProvider rsaProvider = new RSACryptoServiceProvider();
   rsaProvider.FromXmlString(RsaCryptoXml);
   byte[] stringBytes = Convert.FromBase64String(EncryptedString);
   byte[] decryptedText = rsaProvider.Decrypt(stringBytes, false);
   UTF8Encoding utf8 = new UTF8Encoding();
   return utf8.GetString( decryptedText );
  }
  [WebMethod(Description="Generates a Base64 Encoded DES Symmetric Key", EnableSession=true)]
  [SoapDocumentMethod]
  public string GenerateDESKey()
  {
   DESCryptoServiceProvider des = (DESCryptoServiceProvider) DESCryptoServiceProvider.Create();
   return Convert.ToBase64String(des.Key);
  }
  [WebMethod(Description="DESEncryptString() uses DESKey (A Base64 encoded DES key) to encrypt UnencryptedString.", EnableSession=true)]
  [SoapDocumentMethod]
  public string DESEncryptString(string DESKey, string UnencryptedString)
  {
   return Crypto.EncryptData(DESKey, UnencryptedString);
  }
  [WebMethod(Description="DESDecryptString() uses DESKey (A Base64 encoded DES key) to decrypt EncryptedString.", EnableSession=true)]
  [SoapDocumentMethod]
  public string DESDecryptString(string DESKey, string EncryptedString)
  {
   return Crypto.DecryptData(DESKey, EncryptedString);
  }
  internal class Crypto
  {
   public static string EncryptData(string base64Key, string
    strData)
   {
    string strResult;
    DESCryptoServiceProvider descsp = new DESCryptoServiceProvider();
    descsp.Key = Convert.FromBase64String(base64Key);
    descsp.IV = Convert.FromBase64String(base64Key);
    ICryptoTransform desEncrypt = descsp.CreateEncryptor();
    MemoryStream mOut = new MemoryStream();
    CryptoStream encryptStream = new CryptoStream(mOut,
     desEncrypt, CryptoStreamMode.Write);
    byte[] rbData = UnicodeEncoding.Unicode.GetBytes(strData);
    try
    {
     encryptStream.Write(rbData, 0,
      rbData.Length);
    }
    catch
    {
    }
    encryptStream.FlushFinalBlock();
    if (mOut.Length == 0)
     strResult = "";
    else
    {
     byte []buff = mOut.ToArray();
     strResult = Convert.ToBase64String(buff, 0,
      buff.Length);
    }
    try
    {
     encryptStream.Close();
    }
    catch
    {
    }
    return strResult;
   }
   public static string DecryptData(string base64Key, string
    strData)
   {
    string strResult;
    DESCryptoServiceProvider descsp = new
     DESCryptoServiceProvider();
    descsp.Key = Convert.FromBase64String(base64Key);
    descsp.IV = Convert.FromBase64String(base64Key);
    ICryptoTransform desDecrypt = descsp.CreateDecryptor();
    MemoryStream mOut = new MemoryStream();
    CryptoStream decryptStream = new CryptoStream(mOut,
     desDecrypt, CryptoStreamMode.Write);
    char [] carray = strData.ToCharArray();
    byte[] rbData = Convert.FromBase64CharArray(carray,
     0, carray.Length);
    try
    {
     decryptStream.Write(rbData, 0,
      rbData.Length);
    }
    catch
    {
    }
    decryptStream.FlushFinalBlock();
    UnicodeEncoding aEnc = new UnicodeEncoding();
    strResult = aEnc.GetString(mOut.ToArray());
    try
    {
     decryptStream.Close();
    }
    catch
    {
    }
    return strResult;
   }
  }
 }
}
