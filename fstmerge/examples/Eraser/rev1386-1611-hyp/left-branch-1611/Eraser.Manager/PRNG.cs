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
 public abstract class Prng
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
     S._("minValue is greater than maxValue"));
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
 public class PrngManager
 {
  public PrngManager()
  {
  }
  public static Dictionary<Guid, Prng> Items
  {
   get
   {
    lock (ManagerLibrary.Instance.PRNGManager.prngs)
     return ManagerLibrary.Instance.PRNGManager.prngs;
   }
  }
  public static Prng GetInstance(Guid value)
  {
   lock (ManagerLibrary.Instance.PRNGManager.prngs)
   {
    if (!ManagerLibrary.Instance.PRNGManager.prngs.ContainsKey(value))
     throw new PrngNotFoundException(value);
    return ManagerLibrary.Instance.PRNGManager.prngs[value];
   }
  }
  public static void Register(Prng prng)
  {
   lock (ManagerLibrary.Instance.PRNGManager.prngs)
    ManagerLibrary.Instance.PRNGManager.prngs.Add(prng.Guid, prng);
  }
  internal void AddEntropy(byte[] entropy)
  {
   lock (ManagerLibrary.Instance.PRNGManager.prngs)
    foreach (Prng prng in prngs.Values)
     prng.Reseed(entropy);
  }
  public static byte[] GetEntropy()
  {
   return ManagerLibrary.Instance.EntropySourceManager.Poller.GetPool();
  }
  private Dictionary<Guid, Prng> prngs = new Dictionary<Guid, Prng>();
 }
}
