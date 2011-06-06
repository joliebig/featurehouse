using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using Eraser.Manager;
using Eraser.Util;
namespace Eraser.DefaultPlugins
{
 [DefaultUnusedSpaceErasure(1)]
 [Guid("BF8BA267-231A-4085-9BF9-204DE65A6641")]
 sealed class Pseudorandom : PassBasedErasureMethod
 {
  public override string Name
  {
   get { return S._("Pseudorandom Data"); }
  }
  public override Guid Guid
  {
   get { return new Guid("{BF8BA267-231A-4085-9BF9-204DE65A6641}"); }
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
     new ErasureMethodPass(WriteRandom, null)
    };
   }
  }
 }
}
