using System;
using Simias;
using Simias.Sync;
namespace Novell.iFolder.Web
{
 [Serializable]
 public class SyncSize
 {
  public uint SyncNodeCount;
  public ulong SyncByteCount;
  public SyncSize()
  {
  }
  public SyncSize(uint SyncNodeCount, ulong SyncByteCount)
  {
   this.SyncNodeCount = SyncNodeCount;
   this.SyncByteCount = SyncByteCount;
  }
 }
}
