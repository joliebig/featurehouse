using System;
using Gtk;
using Simias.Client;
using Novell.iFolder;
namespace Novell.iFolder.DomainProvider
{
 public interface IDomainProviderUI
 {
  string ID { get; }
  string Name { get; }
  string Description { get; }
  bool CanDelete { get; }
  bool HasDetails { get; }
  AccountDialog CreateAccountDialog(Window parent, DomainInformation domain);
 }
}
