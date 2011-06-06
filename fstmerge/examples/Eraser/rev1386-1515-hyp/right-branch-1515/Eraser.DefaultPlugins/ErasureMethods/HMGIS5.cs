using System;
using System.Collections.Generic;
using System.Text;
using Eraser.Manager;
using Eraser.Util;
namespace Eraser.DefaultPlugins
{
 sealed class HMGIS5Baseline : PassBasedErasureMethod
 {
  public override string Name
  {
   get { return S._("British HMG IS5 (Baseline)"); }
  }
  public override Guid Guid
  {
   get { return new Guid("{9ACDBD78-0406-4116-87E5-263E5E3B2E0D}"); }
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
     new ErasureMethodPass(WriteConstant, new byte[] { (byte)0 })
    };
   }
  }
 }
 sealed class HMGIS5Enhanced : PassBasedErasureMethod
 {
  public override string Name
  {
   get { return S._("British HMG IS5 (Enhanced)"); }
  }
  public override Guid Guid
  {
   get { return new Guid("{45671DA4-9401-46e4-9C0D-89B94E89C8B5}"); }
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
     new ErasureMethodPass(WriteConstant, new byte[] { (byte)0 }),
     new ErasureMethodPass(WriteConstant, new byte[] { (byte)0x01 }),
     new ErasureMethodPass(WriteRandom, null),
    };
   }
  }
 }
}
