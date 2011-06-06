using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using Eraser.Manager;
using Eraser.Util;
namespace Eraser.DefaultPlugins
{
 [Guid("0FE620EA-8055-4861-B5BB-BD8BDC3FD4AC")]
 sealed class USArmyAR380_19 : PassBasedErasureMethod
 {
  public override string Name
  {
   get { return S._("US Army AR380-19"); }
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
     new ErasureMethodPass(WriteRandom, null),
     new ErasureMethodPass(WriteConstant, new byte[] { (byte)(rand & 0xFF) }),
     new ErasureMethodPass(WriteConstant, new byte[] { (byte)~(rand & 0xFF) })
    };
   }
  }
 }
}
