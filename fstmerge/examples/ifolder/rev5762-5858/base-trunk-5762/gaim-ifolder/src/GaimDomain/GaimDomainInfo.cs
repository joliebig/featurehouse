

using System;
using System.Text;

namespace Simias.Domain
{



 [Serializable]
 public class GaimDomainInfo
 {



  public string Name;




  public string Description;




  public string ID;




  public GaimDomainInfo()
  {
  }





  public override string ToString()
  {
   StringBuilder builder = new StringBuilder();

   string newLine = Environment.NewLine;

   builder.AppendFormat("Domain Information{0}", newLine);
   builder.AppendFormat("  ID               : {0}{1}", this.ID, newLine);
   builder.AppendFormat("  Name             : {0}{1}", this.Name, newLine);
   builder.AppendFormat("  Description      : {0}{1}", this.Description, newLine);

   return builder.ToString();
  }
 }
}
