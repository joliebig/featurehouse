using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using Eraser.Manager;
using System.Security.Cryptography;
using Eraser.Util;
namespace Eraser.DefaultPlugins
{
 [DefaultPrng(1)]
 [Guid("6BF35B8E-F37F-476e-B6B2-9994A92C3B0C")]
 public class RngCrypto : Prng
 {
  public override string Name
  {
   get { return S._("RNGCryptoServiceProvider"); }
  }
  public override Guid Guid
  {
   get { return new Guid("{6BF35B8E-F37F-476e-B6B2-9994A92C3B0C}"); }
  }
  public override void NextBytes(byte[] buffer)
  {
   rand.GetBytes(buffer);
  }
  protected override void Reseed(byte[] seed)
  {
  }
  RNGCryptoServiceProvider rand = new RNGCryptoServiceProvider();
 }
}
