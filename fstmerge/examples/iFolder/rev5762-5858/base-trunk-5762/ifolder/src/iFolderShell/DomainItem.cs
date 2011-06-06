using System;
using System.Text;
using System.Runtime.InteropServices;
using Simias.Client;
namespace Novell.iFolderCom
{
 [ComVisible(false)]
 public class DomainItem
 {
  private string name;
  private string id;
  public DomainItem(string name, string ID)
  {
   this.name = name;
   this.id = ID;
  }
  public string Name
  {
   get { return name; }
   set { name = value; }
  }
  public string ID
  {
   get { return id; }
  }
  public override string ToString()
  {
   return name;
  }
 }
}
