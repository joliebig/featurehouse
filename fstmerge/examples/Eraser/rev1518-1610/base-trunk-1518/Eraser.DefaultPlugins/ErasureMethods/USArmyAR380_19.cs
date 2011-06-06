using System;
using System.Collections.Generic;
using System.Text;
using Eraser.Manager;
using Eraser.Util;
namespace Eraser.DefaultPlugins
{
 sealed class USArmyAR380_19 : PassBasedErasureMethod
 {
  public override string Name
  {
   get { return S._("US Army AR380-19"); }
  }
  public override Guid Guid
  {
   get { return new Guid("{0FE620EA-8055-4861-B5BB-BD8BDC3FD4AC}"); }
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
     new ErasureMethodPass(WriteRandom, null),
     new ErasureMethodPass(WriteConstant, new byte[] { (byte)(rand & 0xFF) }),
     new ErasureMethodPass(WriteConstant, new byte[] { (byte)~(rand & 0xFF) })
    };
   }
  }
 }
}
