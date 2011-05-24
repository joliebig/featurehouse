

using System;

namespace Novell.FormsTrayApp
{



 public class DomainConnectEventArgs : EventArgs
 {
  private DomainInformation domainInfo;




  public DomainConnectEventArgs(DomainInformation domainInfo)
  {
   this.domainInfo = domainInfo;
  }




  public DomainInformation DomainInfo
  {
   get { return domainInfo; }
  }
 }
}
