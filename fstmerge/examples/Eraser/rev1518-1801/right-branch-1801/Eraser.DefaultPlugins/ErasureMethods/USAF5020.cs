using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using Eraser.Manager;
using Eraser.Util;
namespace Eraser.DefaultPlugins
{
 [Guid("7BF5B185-8EA5-4e12-83F1-F6C2EFB3D2C2")]
 sealed class USAF5020 : PassBasedErasureMethod
 {
  public override string Name
  {
   get { return S._("US Air Force 5020"); }
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
     new ErasureMethodPass(WriteConstant, new byte[] { (byte)(rand & 0xFF) }),
     new ErasureMethodPass(WriteConstant, new byte[] { (byte)((rand >> 8) & 0xFF) }),
     new ErasureMethodPass(WriteConstant, new byte[] { (byte)((rand >> 16) & 0xFF) })
    };
   }
  }
 }
}
