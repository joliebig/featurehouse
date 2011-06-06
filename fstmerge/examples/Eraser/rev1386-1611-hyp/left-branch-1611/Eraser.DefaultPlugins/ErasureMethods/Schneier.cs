using System;
using System.Collections.Generic;
using System.Text;
using Eraser.Manager;
using Eraser.Util;
namespace Eraser.DefaultPlugins
{
 sealed class Schneier : PassBasedErasureMethod
 {
  public override string Name
  {
   get { return S._("Schneier 7 pass"); }
  }
  public override Guid Guid
  {
   get { return new Guid("{B1BFAB4A-31D3-43a5-914C-E9892C78AFD8}"); }
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
     new ErasureMethodPass(WriteConstant, new byte[] { 1 }),
     new ErasureMethodPass(WriteConstant, new byte[] { 0 }),
     new ErasureMethodPass(WriteRandom, null),
     new ErasureMethodPass(WriteRandom, null),
     new ErasureMethodPass(WriteRandom, null),
     new ErasureMethodPass(WriteRandom, null),
     new ErasureMethodPass(WriteRandom, null)
    };
   }
  }
 }
}
