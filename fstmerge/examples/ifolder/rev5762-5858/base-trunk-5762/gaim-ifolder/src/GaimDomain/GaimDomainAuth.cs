
using System;
using System.Collections;
using System.Diagnostics;
using System.IO;
using System.Net;
using System.Security.Cryptography;
using System.Text;
using System.Web;
using System.Web.Services;
using System.Web.Services.Protocols;
using System.Xml;
using System.Xml.Serialization;

using Simias;
using Simias.Storage;
using SCodes = Simias.Authentication.StatusCodes;


namespace Simias.Gaim
{



 public class ClientAuthentication
 {



  private static readonly ISimiasLog log =
   SimiasLogManager.GetLogger(System.Reflection.MethodBase.GetCurrentMethod().DeclaringType);

  public ClientAuthentication()
  {
  }
  public
  Simias.Authentication.Status
  Authenticate( string collectionID )
  {
   HttpWebResponse response = null;
   Simias.Authentication.Status status =
    new Simias.Authentication.Status( SCodes.Unknown );
   Store store = Store.GetStore();
   Simias.Storage.Domain domain = store.GetDomain( GaimDomain.ID );
   Simias.Storage.Member member = domain.GetCurrentMember();
   GaimDomainProvider gaimDomainProvider = new GaimDomainProvider();
   Uri remoteUri = gaimDomainProvider.ResolveLocation( GaimDomain.ID, collectionID );
   if ( remoteUri == null )
   {
    status.statusCode = SCodes.UnknownDomain;
    return status;
   }
   Uri loginUri = new Uri( remoteUri.ToString() + "/Login.ashx" );
   HttpWebRequest request = WebRequest.Create( loginUri ) as HttpWebRequest;
   WebState webState = new WebState( GaimDomain.ID );
   webState.InitializeWebRequest( request, GaimDomain.ID );
   request.Headers.Add(
    Simias.Security.Web.AuthenticationService.Login.DomainIDHeader,
    GaimDomain.ID );
   request.Headers.Add( "gaim-member", member.UserID );
   request.Method = "POST";
   request.ContentLength = 0;
   try
   {
    request.GetRequestStream().Close();
    response = request.GetResponse() as HttpWebResponse;
    if ( response != null )
    {
     status.statusCode = SCodes.Success;
    }
   }
   catch(WebException webEx)
   {
    response = webEx.Response as HttpWebResponse;
    if (response != null)
    {
     log.Debug( response.StatusCode.ToString() );
     if ( response.StatusCode == System.Net.HttpStatusCode.Unauthorized )
     {
      string oneTimeChallenge = response.Headers[ "gaim-secret" ];
      if ( oneTimeChallenge != null && oneTimeChallenge != "" )
      {
       HttpWebRequest request2 = WebRequest.Create( loginUri ) as HttpWebRequest;
       WebState webState2 = new WebState( GaimDomain.ID );
       webState2.InitializeWebRequest( request2, GaimDomain.ID );
       request2.CookieContainer.Add(response.Cookies);
       request2.Headers.Add(
        Simias.Security.Web.AuthenticationService.Login.DomainIDHeader,
        GaimDomain.ID );
       request2.Headers.Add( "gaim-member", member.UserID );
       try
       {
        RSACryptoServiceProvider credential = GaimDomain.GetCredential();
        if (credential != null)
        {
         byte[] oneTime = Convert.FromBase64String( oneTimeChallenge );
         byte[] decryptedText = credential.Decrypt( oneTime, false );
         request2.Headers.Add( "gaim-secret", Convert.ToBase64String( decryptedText ) );
        }
        else
        {
         log.Debug( "Couldn't get our own RSACryptoServiceProvider (our private key)" );
         status.statusCode = SCodes.Unknown;
         return status;
        }
       }
       catch( Exception enc )
       {
        log.Debug( "Error decrypting one time secret." );
        log.Debug( enc.Message );
        log.Debug( enc.StackTrace );
       }
       request2.Method = "POST";
       request2.ContentLength = 0;
       try
       {
        request2.GetRequestStream().Close();
        response = request2.GetResponse() as HttpWebResponse;
        if ( response != null )
        {
         status.statusCode = SCodes.Success;
        }
       }
       catch( WebException webEx2 )
       {
        log.Debug( "WebException: " + webEx2.Status.ToString() );
        log.Debug( webEx2.Message );
        log.Debug( webEx2.StackTrace );
       }
       catch( Exception e2 )
       {
        log.Debug( "Other exception" );
        log.Debug( e2.Message );
        log.Debug( e2.StackTrace );
       }
      }
      else
      {
       log.Debug( "One Time Challenge NOT present");
      }
     }
    }
    else
    {
     log.Debug( "Couldn't get the HttpWebResponse" );
     log.Debug(webEx.Message);
     log.Debug(webEx.StackTrace);
    }
   }
   catch(Exception ex)
   {
    log.Debug( "Catch-all Exception" );
    log.Debug(ex.Message);
    log.Debug(ex.StackTrace);
   }
   return status;
  }
 }
}
