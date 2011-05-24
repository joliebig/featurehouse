

using System;
using System.Collections.Generic;
using System.Text;
using Eraser.Manager;
using Eraser.Util;

namespace Eraser.DefaultPlugins
{
 sealed class USAF5020 : PassBasedErasureMethod
 {
  public override string Name
  {
   get { return S._("US Air Force 5020"); }
  }

  public override Guid Guid
  {
   get { return new Guid("{7BF5B185-8EA5-4e12-83F1-F6C2EFB3D2C2}"); }
  }

  protected override bool RandomizePasses
  {
   get { return false; }
  }

  protected override ErasureMethodPass[] PassesSet
  {
   get
   {
    Prng prng = PrngManager.GetInstance(ManagerLibrary.Settings.ActivePrng);
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
