

using Gtk;
using System;

namespace Novell.iFolder
{



 public abstract class AccountDialog : Dialog
 {
  protected Window parentWindow;
  protected DomainInformation domain;




  public string DomainID
  {
   get { return domain.ID; }
  }







  public AccountDialog(Window parent, DomainInformation curDomain)
   : base()
  {
   parentWindow = parent;
   domain = curDomain;
  }




  ~AccountDialog()
  {
  }
 }
}
