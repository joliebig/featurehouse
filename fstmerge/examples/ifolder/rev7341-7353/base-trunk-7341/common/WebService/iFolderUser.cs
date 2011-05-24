

using System;

using Simias;
using Simias.Storage;

namespace Novell.iFolder.Web
{




 [Serializable]
 public class iFolderUser
 {


  public string Name;


  public string UserID;


  public string Rights;


  public string ID;


  public string State;


  public string iFolderID;


  public bool IsOwner;


  public string FirstName;


  public string Surname;


  public string FN;



  public iFolderUser()
  {
  }






  public iFolderUser( Simias.Storage.Domain domain, Simias.Storage.Member member )
  {
   this.Name = member.Name;
   this.UserID = member.UserID;
   this.ID = member.ID;
   this.State = "Member";
   this.IsOwner = member.IsOwner;
   this.Rights = member.Rights.ToString();

   if ( member.Given != null )
   {
    this.Surname = member.Family;
    this.FirstName = member.Given;
    this.FN = member.FN;
   }
   else
   {
    if ( domain != null )
    {
     Simias.Storage.Member dMember = domain.GetMemberByID( member.UserID );
     if ( dMember != null )
     {
      this.Surname = dMember.Family;
      this.FirstName = dMember.Given;
      this.FN = dMember.FN;
     }
    }
   }
  }




  public iFolderUser( Simias.POBox.Subscription sub )
  {
   this.Name = sub.ToName;
   this.UserID = sub.ToIdentity;


   Simias.Storage.Domain domain = Store.GetStore().GetDomain( sub.DomainID );
   if ( domain != null )
   {
    Simias.Storage.Member simMem = domain.GetMemberByID( sub.ToIdentity );
    if ( simMem != null )
    {
     this.Surname = simMem.Family;
     this.FirstName = simMem.Given;
     this.FN = simMem.FN;
    }
   }

   this.Rights = sub.SubscriptionRights.ToString();
   this.ID = sub.ID;
   this.iFolderID = sub.SubscriptionCollectionID;
   this.State = "Invited";
   this.IsOwner = false;

   if(sub.SubscriptionState ==
    Simias.POBox.SubscriptionStates.Invited)
   {
    this.State = "WaitSync";
   }
   else if(sub.SubscriptionState ==
    Simias.POBox.SubscriptionStates.Posted)
   {
    this.State = "Invited";
   }
   else if(sub.SubscriptionState ==
    Simias.POBox.SubscriptionStates.Pending)
   {
    this.State = "AccessRequest";
   }
   else if(sub.SubscriptionState ==
    Simias.POBox.SubscriptionStates.Responded)
   {
    if(sub.SubscriptionDisposition ==
     Simias.POBox.SubscriptionDispositions.Declined)
     this.State = "Declined";
    else
     this.State = "Unknown";
   }
   else
   {
    this.State = "Unknown";
   }
  }

 }
}
