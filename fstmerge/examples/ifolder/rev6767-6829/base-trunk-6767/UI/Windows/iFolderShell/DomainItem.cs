
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
        private string host;
        private string url;






  public DomainItem(string name, string ID)
  {
   this.name = name;
   this.id = ID;
            this.host = null;
  }







        public DomainItem(string name, string ID, string host)
        {
            this.name = name;
            this.id = ID;
            this.host = host;
        }
        public DomainItem(string name, string ID, string host, string url)
        {
            this.name = name;
            this.id = ID;
            this.host = host;
            this.url = url;
        }
  public string Name
  {
   get { return name; }
   set { name = value; }
  }
        public string Url
        {
            get { return url; }
            set { url = value; }
        }
        public string Host
        {
            get { return host; }
            set { host = value; }
        }
  public string ID
  {
   get { return id; }
  }
  public override string ToString()
  {
            if (this.host != null)
                return String.Format(name + " - " + host);
            else
            return name;
  }
 }
}
