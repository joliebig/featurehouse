using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using Eraser.Manager;
using Eraser.Util;
namespace Eraser.DefaultPlugins
{
 [DefaultFileErasure(1)]
 [Guid("1407FC4E-FEFF-4375-B4FB-D7EFBB7E9922")]
 sealed class Gutmann : PassBasedErasureMethod
 {
  public override string Name
  {
   get { return S._("Gutmann"); }
  }
  public override Guid Guid
  {
   get { return new Guid("{1407FC4E-FEFF-4375-B4FB-D7EFBB7E9922}"); }
  }
  protected override bool RandomizePasses
  {
   get { return true; }
  }
  protected override ErasureMethodPass[] PassesSet
  {
   get
   {
    return new ErasureMethodPass[]
    {
     new ErasureMethodPass(WriteRandom, null),
     new ErasureMethodPass(WriteRandom, null),
     new ErasureMethodPass(WriteRandom, null),
     new ErasureMethodPass(WriteRandom, null),
     new ErasureMethodPass(WriteConstant, new byte[] {0x55}),
     new ErasureMethodPass(WriteConstant, new byte[] {0xAA}),
     new ErasureMethodPass(WriteConstant, new byte[] {0x92, 0x49, 0x24}),
     new ErasureMethodPass(WriteConstant, new byte[] {0x49, 0x24, 0x92}),
     new ErasureMethodPass(WriteConstant, new byte[] {0x24, 0x92, 0x49}),
     new ErasureMethodPass(WriteConstant, new byte[] {0x00}),
     new ErasureMethodPass(WriteConstant, new byte[] {0x11}),
     new ErasureMethodPass(WriteConstant, new byte[] {0x22}),
     new ErasureMethodPass(WriteConstant, new byte[] {0x33}),
     new ErasureMethodPass(WriteConstant, new byte[] {0x44}),
     new ErasureMethodPass(WriteConstant, new byte[] {0x55}),
     new ErasureMethodPass(WriteConstant, new byte[] {0x66}),
     new ErasureMethodPass(WriteConstant, new byte[] {0x77}),
     new ErasureMethodPass(WriteConstant, new byte[] {0x88}),
     new ErasureMethodPass(WriteConstant, new byte[] {0x99}),
     new ErasureMethodPass(WriteConstant, new byte[] {0xAA}),
     new ErasureMethodPass(WriteConstant, new byte[] {0xBB}),
     new ErasureMethodPass(WriteConstant, new byte[] {0xCC}),
     new ErasureMethodPass(WriteConstant, new byte[] {0xDD}),
     new ErasureMethodPass(WriteConstant, new byte[] {0xEE}),
     new ErasureMethodPass(WriteConstant, new byte[] {0xFF}),
     new ErasureMethodPass(WriteConstant, new byte[] {0x92, 0x49, 0x24}),
     new ErasureMethodPass(WriteConstant, new byte[] {0x49, 0x24, 0x92}),
     new ErasureMethodPass(WriteConstant, new byte[] {0x24, 0x92, 0x49}),
     new ErasureMethodPass(WriteConstant, new byte[] {0x6D, 0xB6, 0xDB}),
     new ErasureMethodPass(WriteConstant, new byte[] {0xB6, 0xDB, 0x6D}),
     new ErasureMethodPass(WriteConstant, new byte[] {0xDB, 0x6D, 0xB6}),
     new ErasureMethodPass(WriteRandom, null),
     new ErasureMethodPass(WriteRandom, null),
     new ErasureMethodPass(WriteRandom, null),
     new ErasureMethodPass(WriteRandom, null)
    };
   }
  }
 }
 sealed class GutmannLite : PassBasedErasureMethod
 {
  public override string Name
  {
   get { return S._("Gutmann Lite"); }
  }
  public override Guid Guid
  {
   get { return new Guid("{AE5EB764-41B0-4601-BDF2-326B5838D44A}"); }
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
     new ErasureMethodPass(WriteRandom, null),
     new ErasureMethodPass(WriteConstant, new byte[] {0x55}),
     new ErasureMethodPass(WriteConstant, new byte[] {0xAA}),
     new ErasureMethodPass(WriteConstant, new byte[] {0x92, 0x49, 0x24}),
     new ErasureMethodPass(WriteConstant, new byte[] {0x49, 0x24, 0x92}),
     new ErasureMethodPass(WriteConstant, new byte[] {0x24, 0x92, 0x49}),
     new ErasureMethodPass(WriteConstant, new byte[] {0x4B}),
     new ErasureMethodPass(WriteConstant, new byte[] {0xB4}),
     new ErasureMethodPass(WriteConstant, new byte[] {0x00}),
     new ErasureMethodPass(WriteConstant, new byte[] {0x11}),
    };
   }
  }
 }
}
