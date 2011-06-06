using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using Eraser.Manager;
using Eraser.Util;
namespace Eraser.DefaultPlugins
{
 [Guid("F335CC40-5DE5-4733-90B1-6957B4A45688")]
 sealed class RCMP_TSSIT_OPS_II : PassBasedErasureMethod
 {
  public override string Name
  {
   get { return S._("RCMP TSSIT OPS-II"); }
  }
  public override Guid Guid
  {
   get { return GetType().GUID; }
  }
  protected override bool RandomizePasses
  {
   get { return false; }
  }
  protected override ErasureMethodPass[] PassesSet
  {
   get
   {
    Prng prng = ManagerLibrary.Instance.PrngRegistrar[ManagerLibrary.Settings.ActivePrng];
    int rand = prng.Next();
    return new ErasureMethodPass[]
    {
     new ErasureMethodPass(WriteConstant, new byte[] { (byte)0}),
     new ErasureMethodPass(WriteConstant, new byte[] { (byte)0x01 }),
     new ErasureMethodPass(WriteConstant, new byte[] { (byte)0 }),
     new ErasureMethodPass(WriteConstant, new byte[] { (byte)0x01 }),
     new ErasureMethodPass(WriteConstant, new byte[] { (byte)0 }),
     new ErasureMethodPass(WriteConstant, new byte[] { (byte)0x01 }),
     new ErasureMethodPass(WriteConstant, new byte[] { (byte)(rand & 0xFF) }),
    };
   }
  }
 }
}
