

using System;
using System.Collections;

using Novell.iFolder.Events;

using Simias.Client;

namespace Novell.iFolder.Controller
{
 public class iFolderController
 {



  private static iFolderController instance = null;




  private iFolderWebService ifws = null;





  private SimiasWebService simws = null;




  private Hashtable keyediFolders = null;




  private Hashtable keyedSubscriptions = null;




  private DomainController domainController = null;




  public event iFolderAddedEventHandler iFolderAdded;
  public event iFolderDeletedEventHandler iFolderDeleted;
  public event iFolderChangedEventHandler iFolderChanged;
  public event iFolderInvitationReceivedEventHandler iFolderInvitationReceived;

  private iFolderController()
  {
   string localServiceUrl = Simias.Client.Manager.LocalServiceUrl.ToString();
   try
   {
    ifws = new iFolderWebService();
    ifws.Url = localServiceUrl + "/iFolder.asmx";
    LocalService.Start(ifws);
   }
   catch(Exception e)
   {
    ifws = null;
    throw new Exception("Unable to create ifolder web service in iFolderController");
   }
   try
   {
    simws = new SimiasWebService();
    simws.Url = localServiceUrl + "/Simias.asmx";
    LocalService.Start(simws);
   }
   catch(Exception e)
   {
    simws = null;
    throw new Exception("Unable to create simias web service in iFolderController");
   }

   keyediFolders = new Hashtable();
   keyedSubscriptions = new Hashtable();

   domainController = DomainController.GetDomainController();
   if (domainController != null)
   {
    domainController.DomainLoggedIn +=
     new DomainLoggedInEventHandler(OnDomainLoggedInEvent);
    domainController.DomainLoggedOut +=
     new DomainLoggedOutEventHandler(OnDomainLoggedOutEvent);
   }
  }

  public static iFolderController GetiFolderController()
  {
   lock (typeof(iFolderController))
   {
    if (instance == null)
    {
     instance = new iFolderController();
    }

    return instance;
   }
  }

  private void OnDomainLoggedInEvent(object sender, DomainEventArgs args)
  {
Console.WriteLine("iFolderController.OnDomainLoggedInEvent() entered");
  }

  private void OnDomainLoggedOutEvent(object sender, DomainEventArgs args)
  {
Console.WriteLine("iFolderController.OnDomainLoggedOutEvent() entered");
  }
 }
}
