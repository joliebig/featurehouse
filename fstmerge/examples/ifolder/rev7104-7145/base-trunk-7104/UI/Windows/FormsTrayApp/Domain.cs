

using System;
using System.Text;

namespace Novell.FormsTrayApp
{



 public class Domain
 {
  private DomainInformation domainInfo;
  private string name;
  private bool showAll = false;





  public Domain(DomainInformation domainInfo)
  {
   this.domainInfo = domainInfo;
  }





  public Domain(string name)
  {
   this.name = name;
   this.showAll = true;
  }





  public DomainInformation DomainInfo
  {
   get { return domainInfo; }
   set { domainInfo = value; }
  }




  public string Name
  {
   get
   {
    return showAll ? name : domainInfo.Name;
   }
   set { name = value; }
  }




  public string ID
  {
   get
   {
    if (showAll)
     return name;
    else
     return domainInfo.ID;
   }
  }




  public bool ShowAll
  {
   get { return showAll; }
  }






  public override string ToString()
  {
   if (showAll)
    return name;
   else
    return domainInfo.Name;
  }
 }
}
