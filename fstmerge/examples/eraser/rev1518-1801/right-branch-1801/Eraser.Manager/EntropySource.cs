

using System;
using System.Collections.Generic;
using System.Text;

using System.Globalization;
using System.Threading;
using System.Security.Cryptography;
using System.Runtime.InteropServices;
using System.Diagnostics;
using System.Reflection;
using System.IO;

using System.Windows.Forms;
using Microsoft.VisualBasic.Devices;
using Microsoft.Win32.SafeHandles;
using Eraser.Util;

namespace Eraser.Manager
{




 public abstract class EntropySource : IRegisterable
 {



  protected EntropySource()
  {
  }




  public abstract string Name
  {
   get;
  }




  public abstract Guid Guid
  {
   get;
  }






  public abstract byte[] GetPrimer();






  public abstract byte[] GetSlowEntropy();






  public abstract byte[] GetFastEntropy();





  public abstract byte[] GetEntropy();
  protected static byte[] StructToBuffer<T>(T entropy) where T : struct
  {
   int sizeofObject = Marshal.SizeOf(entropy);
   IntPtr memory = Marshal.AllocHGlobal(sizeofObject);
   try
   {
    Marshal.StructureToPtr(entropy, memory, false);
    byte[] dest = new byte[sizeofObject];
    Marshal.Copy(memory, dest, 0, sizeofObject);
    return dest;
   }
   finally
   {
    Marshal.FreeHGlobal(memory);
   }
  }
 }
 public class EntropySourceRegistrar : Registrar<EntropySource>
 {
  internal EntropySourceRegistrar()
  {
   Poller = new EntropyPoller();
   Poller.AddEntropySource(new KernelEntropySource());
  }
  public EntropyPoller Poller { get; private set; }
  private Dictionary<Guid, EntropySource> sources = new Dictionary<Guid, EntropySource>();
 };
 public class KernelEntropySource : EntropySource
 {
  public override byte[] GetPrimer()
  {
   List<byte> result = new List<byte>();
   result.AddRange(StructToBuffer(Process.GetCurrentProcess().Id));
   result.AddRange(StructToBuffer(Process.GetCurrentProcess().StartTime.Ticks));
   result.AddRange(GetFastEntropy());
   result.AddRange(GetSlowEntropy());
   return result.ToArray();
  }
  public override Guid Guid
  {
   get
   {
    return new Guid("{11EDCECF-AD81-4e50-A73D-B9CF1F813093}");
   }
  }
  public override string Name
  {
   get
   {
    return "Kernel Entropy Source";
   }
  }
  public override byte[] GetEntropy()
  {
   List<byte> result = new List<byte>();
   result.AddRange(GetFastEntropy());
   result.AddRange(GetSlowEntropy());
   return result.ToArray();
  }
  public override byte[] GetFastEntropy()
  {
   List<byte> result = new List<byte>();
   result.AddRange(StructToBuffer(new DriveInfo(new DirectoryInfo(Environment.SystemDirectory).
    Root.FullName).TotalFreeSpace));
   result.AddRange(StructToBuffer(UserApi.MessagePos));
   result.AddRange(StructToBuffer(UserApi.MessageTime));
   result.AddRange(StructToBuffer(UserApi.CaretPos));
   result.AddRange(StructToBuffer(Cursor.Position));
   Process currProcess = Process.GetCurrentProcess();
   foreach (ProcessThread thread in currProcess.Threads)
    result.AddRange(StructToBuffer(thread.Id));
   result.AddRange(StructToBuffer(currProcess.VirtualMemorySize64));
   result.AddRange(StructToBuffer(currProcess.MaxWorkingSet));
   result.AddRange(StructToBuffer(currProcess.MinWorkingSet));
   result.AddRange(StructToBuffer(currProcess.NonpagedSystemMemorySize64));
   result.AddRange(StructToBuffer(currProcess.PagedMemorySize64));
   result.AddRange(StructToBuffer(currProcess.PagedSystemMemorySize64));
   result.AddRange(StructToBuffer(currProcess.PeakPagedMemorySize64));
   result.AddRange(StructToBuffer(currProcess.PeakVirtualMemorySize64));
   result.AddRange(StructToBuffer(currProcess.PeakWorkingSet64));
   result.AddRange(StructToBuffer(currProcess.PrivateMemorySize64));
   result.AddRange(StructToBuffer(currProcess.WorkingSet64));
   result.AddRange(StructToBuffer(currProcess.HandleCount));
   ComputerInfo computerInfo = new ComputerInfo();
   result.AddRange(StructToBuffer(computerInfo.AvailablePhysicalMemory));
   result.AddRange(StructToBuffer(computerInfo.AvailableVirtualMemory));
   result.AddRange(StructToBuffer(currProcess.TotalProcessorTime));
   result.AddRange(StructToBuffer(currProcess.UserProcessorTime));
   result.AddRange(StructToBuffer(currProcess.PrivilegedProcessorTime));
   foreach (ProcessThread thread in currProcess.Threads)
   {
    try
    {
     result.AddRange(StructToBuffer(thread.TotalProcessorTime));
     result.AddRange(StructToBuffer(thread.UserProcessorTime));
     result.AddRange(StructToBuffer(thread.PrivilegedProcessorTime));
    }
    catch (InvalidOperationException)
    {
    }
    catch (System.ComponentModel.Win32Exception e)
    {
     if (e.NativeErrorCode != Win32ErrorCode.AccessDenied)
      throw;
    }
   }
   result.AddRange(StructToBuffer(DateTime.Now.Ticks));
   result.AddRange(StructToBuffer(SystemInfo.PerformanceCounter));
   result.AddRange(StructToBuffer(Environment.TickCount));
   byte[] cryptGenRandom = new byte[160];
   if (Security.Randomise(cryptGenRandom))
    result.AddRange(cryptGenRandom);
   return result.ToArray();
  }
  public override byte[] GetSlowEntropy()
  {
   List<byte> result = new List<byte>();
   byte[] netApiStats = NetApi.NetStatisticsGet(null, NetApiService.Workstation, 0, 0);
   if (netApiStats != null)
    result.AddRange(netApiStats);
   byte[] cryptGenRandom = new byte[1536];
   if (Security.Randomise(cryptGenRandom))
    result.AddRange(cryptGenRandom);
   return result.ToArray();
  }
 }
 public class EntropyPoller
 {
  private enum PRFAlgorithms
  {
   Md5,
   Sha1,
   Ripemd160,
   Sha256,
   Sha384,
   Sha512,
  };
  public EntropyPoller()
  {
   pool = new byte[sizeof(uint) << 7];
   Thread = new Thread(Main);
   Thread.Start();
  }
  private void Main()
  {
   DateTime lastAddedEntropy = DateTime.Now;
   TimeSpan managerEntropySpan = new TimeSpan(0, 10, 0);
   Stopwatch st = new Stopwatch();
   while (Thread.ThreadState != System.Threading.ThreadState.AbortRequested)
   {
    st.Start();
    lock (EntropySources)
     foreach (EntropySource src in EntropySources)
     {
      byte[] entropy = src.GetEntropy();
      AddEntropy(entropy);
     }
    st.Stop();
    Thread.Sleep(2000 + (int)(st.ElapsedTicks % 2049L));
    st.Reset();
    if (DateTime.Now - lastAddedEntropy > managerEntropySpan)
     ManagerLibrary.Instance.PrngRegistrar.AddEntropy(GetPool());
   }
  }
  public void Abort()
  {
   Thread.Abort();
  }
  public void AddEntropySource(EntropySource source)
  {
   lock (EntropySources)
    EntropySources.Add(source);
   AddEntropy(source.GetPrimer());
   MixPool();
   PRFAlgorithm = PRFAlgorithms.Ripemd160;
   MixPool();
   PRFAlgorithm = PRFAlgorithms.Sha512;
  }
  public byte[] GetPool()
  {
   MixPool();
   InvertPool();
   lock (poolLock)
   {
    byte[] result = new byte[pool.Length];
    pool.CopyTo(result, 0);
    return result;
   }
  }
  private void InvertPool()
  {
   lock (poolLock)
    unsafe
    {
     fixed (byte* fPool = pool)
     {
      uint* pPool = (uint*)fPool;
      uint poolLength = (uint)(pool.Length / sizeof(uint));
      while (poolLength-- != 0)
       *pPool = (uint)(*pPool++ ^ uint.MaxValue);
     }
    }
  }
  private void MixPool()
  {
   lock (poolLock)
   {
    const int mixBlockSize = 128;
    int hashSize = PRF.HashSize / 8;
    PRF.ComputeHash(pool, pool.Length - mixBlockSize, mixBlockSize).CopyTo(pool, 0);
    int i = 0;
    for (; i < pool.Length - hashSize; i += hashSize)
     Buffer.BlockCopy(PRF.ComputeHash(pool, i,
      i + mixBlockSize >= pool.Length ? pool.Length - i : mixBlockSize),
      0, pool, i, i + hashSize >= pool.Length ? pool.Length - i : hashSize);
    byte[] combinedBuffer = new byte[mixBlockSize];
    for (; i < pool.Length; i += hashSize)
    {
     Buffer.BlockCopy(pool, i, combinedBuffer, 0, pool.Length - i);
     Buffer.BlockCopy(pool, 0, combinedBuffer, pool.Length - i,
        mixBlockSize - (pool.Length - i));
     Buffer.BlockCopy(PRF.ComputeHash(combinedBuffer, 0, mixBlockSize), 0,
      pool, i, pool.Length - i > hashSize ? hashSize : pool.Length - i);
    }
   }
  }
  public unsafe void AddEntropy(byte[] entropy)
  {
   lock (poolLock)
    fixed (byte* pEntropy = entropy)
    fixed (byte* pPool = pool)
    {
     int size = entropy.Length;
     byte* mpEntropy = pEntropy;
     while (size > 0)
     {
      if (poolPosition >= pool.Length)
       poolPosition = 0;
      int amountToMix = Math.Min(size, pool.Length - poolPosition);
      MemoryXor(pPool + poolPosition, mpEntropy, amountToMix);
      mpEntropy = mpEntropy + amountToMix;
      size -= amountToMix;
     }
    }
  }
  private static unsafe void MemoryXor(byte* destination, byte* source, int size)
  {
   int wsize = size / sizeof(uint);
   size -= wsize * sizeof(uint);
   uint* d = (uint*)destination;
   uint* s = (uint*)source;
   while (wsize-- > 0)
    *d++ ^= *s++;
   if (size > 0)
   {
    byte* db = (byte*)d,
          ds = (byte*)s;
    while (size-- > 0)
     *db++ ^= *ds++;
   }
  }
  private HashAlgorithm PRF
  {
   get
   {
    Type type = null;
    switch (PRFAlgorithm)
    {
     case PRFAlgorithms.Md5:
      type = typeof(MD5CryptoServiceProvider);
      break;
     case PRFAlgorithms.Sha1:
      type = typeof(SHA1Managed);
      break;
     case PRFAlgorithms.Ripemd160:
      type = typeof(RIPEMD160Managed);
      break;
     case PRFAlgorithms.Sha256:
      type = typeof(SHA256Managed);
      break;
     case PRFAlgorithms.Sha384:
      type = typeof(SHA384Managed);
      break;
     default:
      type = typeof(SHA512Managed);
      break;
    }
    if (type.IsInstanceOfType(prfCache))
     return prfCache;
    ConstructorInfo hashConstructor = type.GetConstructor(Type.EmptyTypes);
    return prfCache = (HashAlgorithm)hashConstructor.Invoke(null);
   }
  }
  private HashAlgorithm prfCache;
  private PRFAlgorithms PRFAlgorithm = PRFAlgorithms.Sha512;
  private byte[] pool;
  private int poolPosition;
  private object poolLock = new object();
  private Thread Thread;
  private List<EntropySource> EntropySources = new List<EntropySource>();
 }
}
