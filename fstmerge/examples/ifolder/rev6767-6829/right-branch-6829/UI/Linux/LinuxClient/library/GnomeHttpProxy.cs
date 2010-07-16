

using System;
using GConf;

namespace Novell.iFolder
{



 public class GnomeHttpProxy
 {
  private GConf.Client client;
  private string domainUrl;
  private bool bypass = false;


  static string GCONF_PROXY_PATH = "/system/http_proxy";
  static string GCONF_SPROXY_PATH = "/system/proxy";
  static string GCONF_IGNORE_HOSTS = GCONF_PROXY_PATH + "/ignore_hosts";
  static string GCONF_USE_AUTHENTICATION = GCONF_PROXY_PATH + "/use_authentication";
  static string GCONF_HOST = GCONF_PROXY_PATH + "/host";
  static string GCONF_PORT = GCONF_PROXY_PATH + "/port";
  static string GCONF_USE_PROXY = GCONF_PROXY_PATH + "/use_http_proxy";
  static string GCONF_USER = GCONF_PROXY_PATH + "/authentication_user";
  static string GCONF_PASSWORD = GCONF_PROXY_PATH + "/authentication_password";
  static string GCONF_SECURE_HOST = GCONF_SPROXY_PATH + "/secure_host";
  static string GCONF_SECURE_PORT = GCONF_SPROXY_PATH + "/secure_port";





  public bool IsProxySet
  {
   get
   {
    try
    {
     if ( bypass == false )
     {
      return (bool) client.Get( GCONF_USE_PROXY );
     }
    }
    catch{}
    return false;
   }
  }




  public bool IsSecureProxySet
  {
   get
   {
    try
    {
     if ( bypass == false )
     {
      if ( (bool) client.Get( GCONF_USE_PROXY ) == true )
      {
       string shost = (string) client.Get( GCONF_SECURE_HOST );
       if ( shost != null && shost != "" )
       {
        return true;
       }
      }
     }
    }
    catch{}
    return false;
   }
  }





  public bool CredentialsSet
  {
   get
   {
    try
    {
     if ( bypass == false )
     {
      if ( (bool) client.Get( GCONF_USE_PROXY ) == true )
      {
       return (bool) client.Get( GCONF_USE_AUTHENTICATION );
      }
     }
    }
    catch{}
    return false;
   }
  }





  public string Host
  {
   get
   {
    try
    {
     if ( bypass == false &&
      (bool) client.Get( GCONF_USE_PROXY ) == true )
     {
      string host = (string) client.Get( GCONF_HOST );
      if ( host != null && host != "" )
      {
       int port = (int) client.Get( GCONF_PORT );
       if ( port != 0 )
       {
        return host + ":" + port.ToString();
       }

       return host;
      }
     }
    }
    catch{}
    return null;
   }
  }






  public string Username
  {
   get
   {
    try
    {
     if ( bypass == false &&
      (bool) client.Get( GCONF_USE_PROXY ) == true &&
      (bool) client.Get( GCONF_USE_AUTHENTICATION ) == true )
     {
      return (string) client.Get( GCONF_USER );
     }
    }
    catch{}
    return null;
   }
  }






  public string Password
  {
   get
   {
    try
    {
     if ( bypass == false &&
      (bool) client.Get( GCONF_USE_PROXY ) == true &&
      (bool) client.Get( GCONF_USE_AUTHENTICATION ) == true )
     {
      return (string) client.Get( GCONF_PASSWORD );
     }
    }
    catch{}
    return null;
   }
  }





  public string SecureHost
  {
   get
   {
    try
    {
     if ( bypass == false )
     {
      if ( (bool) client.Get( GCONF_USE_PROXY ) == true )
      {
       string shost = (string) client.Get( GCONF_SECURE_HOST );
       if ( shost != null && shost != "" )
       {
        int sport = (int) client.Get( GCONF_SECURE_PORT );
        if ( sport != 0 )
        {
         return shost + ":" + sport.ToString();
        }

        return shost;
       }
      }
     }
    }
    catch{}
    return null;
   }
  }
  public GnomeHttpProxy( string domainUrl )
  {
   client = new GConf.Client();
   this.domainUrl = domainUrl.ToLower();
   try
   {
    string[] bypassHosts = (string[]) client.Get( GCONF_IGNORE_HOSTS );
    foreach( string host in bypassHosts )
    {
     string normalizedHost = host.Replace( '/', ':' ).ToLower();
     if ( normalizedHost == this.domainUrl )
     {
      this.bypass = true;
      break;
     }
    }
   }
   catch( Exception e )
   {
   }
  }
 }
}
