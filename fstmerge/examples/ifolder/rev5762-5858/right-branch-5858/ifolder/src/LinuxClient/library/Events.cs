

using System;
using System.Collections;
using System.Diagnostics;
using System.Threading;
using Gtk;

using Simias.Client.Event;
using Simias.Client;



namespace Novell.iFolder.Events
{



 public delegate void iFolderAddedEventHandler(object sender, iFolderAddedEventArgs args);
 public delegate void iFolderDeletedEventHandler(object sender, iFolderDeletedEventArgs args);
 public delegate void iFolderChangedEventHandler(object sender, iFolderChangedEventArgs args);
 public delegate void iFolderInvitationReceivedEventHandler(object sender, iFolderInvitationEventArgs args);




 public delegate void iFolderUserAddedEventHandler(object sender, iFolderUserAddedEventArgs args);
 public delegate void iFolderUserChangedEventHandler(object sender, iFolderUserChangedEventArgs args);
 public delegate void iFolderUserDeletedEventHandler(object sender, iFolderUserDeletedEventArgs args);




 public delegate void CollectionSyncEventHandler(object sender, CollectionSyncEventArgs args);
 public delegate void FileSyncEventHandler(object sender, FileSyncEventArgs args);




 public delegate void DomainAddedEventHandler(object sender, DomainEventArgs args);
 public delegate void DomainDeletedEventHandler(object sender, DomainEventArgs args);
 public delegate void DomainHostModifiedEventHandler(object sender, DomainEventArgs args);
 public delegate void DomainLoggedInEventHandler(object sender, DomainEventArgs args);
 public delegate void DomainLoggedOutEventHandler(object sender, DomainEventArgs args);
 public delegate void DomainUpEventHandler(object sender, DomainEventArgs args);
 public delegate void DomainNeedsCredentialsEventHandler(object sender, DomainEventArgs args);
 public delegate void DomainActivatedEventHandler(object sender, DomainEventArgs args);
 public delegate void DomainInactivatedEventHandler(object sender, DomainEventArgs args);
 public delegate void DomainNewDefaultEventHandler(object sender, NewDefaultDomainEventArgs args);
 public delegate void DomainInGraceLoginPeriodEventHandler(object sender, DomainInGraceLoginPeriodEventArgs args);







 public class iFolderAddedEventArgs : EventArgs
 {
  private string ifolderID;

  public iFolderAddedEventArgs(string ifolderID)
  {
   this.ifolderID = ifolderID;
  }

  public string iFolderID
  {
   get{ return this.ifolderID; }
  }
 }

 public class iFolderDeletedEventArgs : EventArgs
 {
  private string ifolderID;

  public iFolderDeletedEventArgs(string ifolderID)
  {
   this.ifolderID = ifolderID;
  }

  public string iFolderID
  {
   get{ return this.ifolderID; }
  }
 }

 public class iFolderChangedEventArgs : EventArgs
 {
  private string ifolderID;

  public iFolderChangedEventArgs(string ifolderID)
  {
   this.ifolderID = ifolderID;
  }

  public string iFolderID
  {
   get{ return this.ifolderID; }
  }
 }

 public class iFolderInvitationEventArgs : EventArgs
 {
  private string domainID;
  private string subscriptionID;
  private string ifolderID;

  public iFolderInvitationEventArgs(string domainID, string subscriptionID, string ifolderID)
  {
   this.subscriptionID = subscriptionID;
  }

  public string DomainID
  {
   get{ return this.domainID; }
  }

  public string SubscriptionID
  {
   get{ return this.subscriptionID; }
  }

  public string iFolderID
  {
   get{ return this.ifolderID; }
  }
 }

 public class iFolderUserAddedEventArgs : EventArgs
 {
  private iFolderUser user;
  private string ifolderID;

  public iFolderUserAddedEventArgs(iFolderUser ifUser, string iFolderID)
  {
   this.user = ifUser;
   this.ifolderID = iFolderID;
  }

  public iFolderUser iFolderUser
  {
   get{ return this.user; }
  }

  public string iFolderID
  {
   get{ return this.ifolderID; }
  }
 }

 public class iFolderUserChangedEventArgs : EventArgs
 {
  private iFolderUser user;
  private string ifolderID;

  public iFolderUserChangedEventArgs(iFolderUser ifUser, string iFolderID)
  {
   this.user = ifUser;
   this.ifolderID = iFolderID;
  }

  public iFolderUser iFolderUser
  {
   get{ return this.user; }
  }

  public string iFolderID
  {
   get{ return this.ifolderID; }
  }
 }

 public class iFolderUserDeletedEventArgs : EventArgs
 {
  private string userID;
  private string ifolderID;

  public iFolderUserDeletedEventArgs(string ifUserID, string iFolderID)
  {
   this.userID = ifUserID;
   this.ifolderID = iFolderID;
  }

  public string UserID
  {
   get{ return this.userID; }
  }

  public string iFolderID
  {
   get{ return this.ifolderID; }
  }
 }






 public class DomainEventArgs : EventArgs
 {
  private string domainID;

  public DomainEventArgs(string domainID)
  {
   this.domainID = domainID;
  }

  public string DomainID
  {
   get{ return this.domainID; }
  }
 }

 public class NewDefaultDomainEventArgs : EventArgs
 {
  private string oldDomainID;
  private string newDomainID;

  public NewDefaultDomainEventArgs(string oldDomainID, string newDomainID)
  {
   this.oldDomainID = oldDomainID;
   this.newDomainID = newDomainID;
  }

  public string OldDomainID
  {
   get{ return this.oldDomainID; }
  }

  public string NewDomainID
  {
   get{ return this.newDomainID; }
  }
 }

 public class DomainInGraceLoginPeriodEventArgs : EventArgs
 {
  private string domainID;
  private int remainingGraceLogins;

  public DomainInGraceLoginPeriodEventArgs(
   string domainID, int remainingGraceLogins)
  {
   this.domainID = domainID;
   this.remainingGraceLogins = remainingGraceLogins;
  }

  public string DomainID
  {
   get{ return this.domainID; }
  }

  public int RemainingGraceLogins
  {
   get{ return this.remainingGraceLogins; }
  }
 }
}
