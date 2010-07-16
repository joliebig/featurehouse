

using System;

namespace Novell.FormsTrayApp
{



 public class DomainRemoveEventArgs : EventArgs
 {
  private DomainInformation domainInfo;
  private string defaultDomainID;




  public DomainRemoveEventArgs(DomainInformation domainInfo, string defaultDomainID)
  {
   this.domainInfo = domainInfo;
   this.defaultDomainID = defaultDomainID;
  }




  public DomainInformation DomainInfo
  {
   get { return domainInfo; }
  }




  public string DefaultDomainID
  {
   get { return defaultDomainID; }
  }
 }
}
