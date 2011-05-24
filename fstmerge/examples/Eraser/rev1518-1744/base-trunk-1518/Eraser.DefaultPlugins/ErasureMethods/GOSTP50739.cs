

using System;
using System.Collections.Generic;
using System.Text;
using Eraser.Manager;
using Eraser.Util;

namespace Eraser.DefaultPlugins
{
 sealed class GOSTP50739 : PassBasedErasureMethod
 {
  public override string Name
  {
   get { return S._("Russian GOST P50739-95"); }
  }

  public override Guid Guid
  {
   get { return new Guid("{92681583-F484-415f-A66C-CC210222EDC5}"); }
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
     new ErasureMethodPass(WriteRandom, null)
    };
   }
  }
 }
}
