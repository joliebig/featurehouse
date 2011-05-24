

using System;

using Gtk;

using Simias.Client;

using Novell.iFolder;

namespace Novell.iFolder.DomainProvider
{
 public class BonjourDomainProviderUI : IDomainProviderUI
 {


  private const string id = "74d3a71f-daae-4a36-b9f3-6466081f6401";
  private const string name = "iFolder Peer to Peer (P2P) DomainProviderUI";
  private const string description = "This module provides the UI for the iFolder Peer to Peer (P2P) domain";





  public string ID
  {
   get
   {
    return id;
   }
  }

  public string Name
  {
   get
   {
    return name;
   }
  }

  public string Description
  {
   get
   {
    return description;
   }
  }

  public bool CanDelete
  {
   get
   {
    return false;
   }
  }

  public bool HasDetails
  {
   get
   {
    return true;
   }
  }





  public AccountDialog CreateAccountDialog(Window parent, DomainInformation domain)
  {
   BonjourAccountDialog dialog =
    new BonjourAccountDialog(parent, domain);

   return dialog;
  }





 }
}
