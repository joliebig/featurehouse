

using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;

using Eraser.Manager;
using Eraser.Util;

namespace Eraser.DefaultPlugins
{
 [Guid("D1583631-702E-4dbf-A0E9-C35DBA481702")]
 sealed class DoD_EcE : PassBasedErasureMethod
 {
  public override string Name
  {
   get { return S._("US DoD 5220.22-M (8-306./E, C & E)"); }
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

    ErasureMethodPass[] result = new ErasureMethodPass[]
    {
     new ErasureMethodPass(WriteConstant, new byte[] { (byte)(rand & 0xFF) }),
     new ErasureMethodPass(WriteConstant, new byte[] { 0 }),
     new ErasureMethodPass(WriteRandom, null),
     new ErasureMethodPass(WriteConstant, new byte[] { (byte)((rand >> 8) & 0xFF) }),
     new ErasureMethodPass(WriteConstant, new byte[] { (byte)((rand >> 16) & 0xFF) }),
     new ErasureMethodPass(WriteConstant, new byte[] { 0 }),
     new ErasureMethodPass(WriteRandom, null)
    };


    result[1] = new ErasureMethodPass(WriteConstant, new byte[] {
     (byte)(~((byte[])result[0].OpaqueValue)[0]) });
    result[5] = new ErasureMethodPass(WriteConstant, new byte[] {
     (byte)(~((byte[])result[4].OpaqueValue)[0]) });
    return result;
   }
  }
 }

 [Guid("ECBF4998-0B4F-445c-9A06-23627659E419")]
 sealed class DoD_E : PassBasedErasureMethod
 {
  public override string Name
  {
   get { return S._("US DoD 5220.22-M (8-306./E)"); }
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
    return new ErasureMethodPass[]
    {
     new ErasureMethodPass(WriteConstant, new byte[] { 0 }),
     new ErasureMethodPass(WriteConstant, new byte[] { 0xFF }),
     new ErasureMethodPass(WriteRandom, null)
    };
   }
  }
 }
}
