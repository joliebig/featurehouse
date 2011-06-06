using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using System.Security.Cryptography;
using System.Runtime.InteropServices;
using System.Diagnostics;
using System.Reflection;
using System.IO;
using Microsoft.Win32.SafeHandles;
using Eraser.Util;
namespace Eraser.Manager
{
 public abstract class Prng : IRegisterable
 {
  public override string ToString()
  {
   return Name;
  }
  public abstract string Name
  {
   get;
  }
  public abstract Guid Guid
  {
   get;
  }
  protected internal abstract void Reseed(byte[] seed);
  public int Next(int maxValue)
  {
   if (maxValue == 0)
    return 0;
   return Next() % maxValue;
  }
  public int Next(int minValue, int maxValue)
  {
   if (minValue > maxValue)
    throw new ArgumentOutOfRangeException("minValue", minValue,
     "minValue is greater than maxValue");
   else if (minValue == maxValue)
    return minValue;
   return (Next() % (maxValue - minValue)) + minValue;
  }
  public int Next()
  {
   byte[] rand = new byte[sizeof(int)];
   NextBytes(rand);
   return Math.Abs(BitConverter.ToInt32(rand, 0));
  }
  public abstract void NextBytes(byte[] buffer);
 }
 public class PrngRegistrar : Registrar<Prng>
 {
  internal PrngRegistrar()
  {
  }
  internal void AddEntropy(byte[] entropy)
  {
   lock (ManagerLibrary.Instance.PrngRegistrar)
    foreach (Prng prng in ManagerLibrary.Instance.PrngRegistrar)
     prng.Reseed(entropy);
  }
  internal static byte[] GetEntropy()
  {
   return ManagerLibrary.Instance.EntropySourceRegistrar.Poller.GetPool();
  }
 }
}
