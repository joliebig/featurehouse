using System;
using System.Collections.Generic;
using System.Text;
using Eraser.Manager;
using Eraser.Util;
namespace Eraser.DefaultPlugins
{
 class VSITR : PassBasedErasureMethod
 {
  public override string Name
  {
   get { return S._("German VSITR"); }
  }
  public override Guid Guid
  {
   get { return new Guid("{607632B2-651B-4935-883A-BDAA74FEBB54}"); }
  }
  protected override bool RandomizePasses
  {
   get { return false; }
  }
  protected override ErasureMethodPass[] PassesSet
  {
   get
   {
    return new ErasureMethodPass[]
    {
     new ErasureMethodPass(WriteConstant, new byte[] { (byte)0}),
     new ErasureMethodPass(WriteConstant, new byte[] { (byte)0x01 }),
     new ErasureMethodPass(WriteConstant, new byte[] { (byte)0 }),
     new ErasureMethodPass(WriteConstant, new byte[] { (byte)0x01 }),
     new ErasureMethodPass(WriteConstant, new byte[] { (byte)0 }),
     new ErasureMethodPass(WriteConstant, new byte[] { (byte)0x01 }),
     new ErasureMethodPass(WriteRandom, null),
    };
   }
  }
 }
}
