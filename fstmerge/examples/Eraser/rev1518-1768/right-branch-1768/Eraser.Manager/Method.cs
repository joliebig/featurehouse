using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Reflection;
using Eraser.Util;
namespace Eraser.Manager
{
 public abstract class ErasureMethod
 {
  public override string ToString()
  {
   if (Passes == 0)
    return Name;
   return Passes == 1 ? S._("{0} (1 pass)", Name) :
    S._("{0} ({1} passes)", Name, Passes);
  }
  public abstract string Name
  {
   get;
  }
  public abstract int Passes
  {
   get;
  }
  public abstract Guid Guid
  {
   get;
  }
  public abstract long CalculateEraseDataSize(ICollection<string> paths, long targetSize);
  public abstract void Erase(Stream stream, long erasureLength, Prng prng,
   ErasureMethodProgressFunction callback);
  public const int DiskOperationUnit = 1536 * 4096;
  public const int FreeSpaceFileUnit = DiskOperationUnit * 36;
  protected static ErasureMethodPass[] ShufflePasses(ErasureMethodPass[] passes)
  {
   ErasureMethodPass[] result = new ErasureMethodPass[passes.Length];
   passes.CopyTo(result, 0);
   Prng rand = PrngManager.GetInstance(ManagerLibrary.Settings.ActivePrng);
   for (int i = 0; i < result.Length; ++i)
   {
    int val = rand.Next(result.Length - 1);
    ErasureMethodPass tmpPass = result[val];
    result[val] = result[i];
    result[i] = tmpPass;
   }
   return result;
  }
  public static void WriteRandom(byte[] buffer, object value)
  {
   ((Prng)value).NextBytes(buffer);
  }
  public static void WriteConstant(byte[] buffer, object value)
  {
   byte[] constant = (byte[])value;
   for (int i = 0; i < buffer.Length; ++i)
    buffer[i] = constant[i % constant.Length];
  }
 }
 public delegate void ErasureMethodProgressFunction(long lastWritten, long totalData,
  int currentPass);
 public class ErasureMethodPass
 {
  public override string ToString()
  {
   return OpaqueValue == null ? S._("Random") : OpaqueValue.ToString();
  }
  public ErasureMethodPass(ErasureMethodPassFunction function, object opaqueValue)
  {
   Function = function;
   OpaqueValue = opaqueValue;
  }
  public void Execute(byte[] buffer, Prng prng)
  {
   Function(buffer, OpaqueValue == null ? prng : OpaqueValue);
  }
  public ErasureMethodPassFunction Function { get; set; }
  public object OpaqueValue { get; set; }
 }
 public delegate void ErasureMethodPassFunction(byte[] buffer, object opaque);
 public abstract class UnusedSpaceErasureMethod : ErasureMethod
 {
  public virtual void EraseUnusedSpace(Stream stream, Prng prng, ErasureMethodProgressFunction callback)
  {
   Erase(stream, long.MaxValue, prng, callback);
  }
 }
 public abstract class PassBasedErasureMethod : UnusedSpaceErasureMethod
 {
  public override int Passes
  {
   get { return PassesSet.Length; }
  }
  protected abstract bool RandomizePasses
  {
   get;
  }
  protected abstract ErasureMethodPass[] PassesSet
  {
   get;
  }
  public override long CalculateEraseDataSize(ICollection<string> paths, long targetSize)
  {
   return targetSize * Passes;
  }
  public override void Erase(Stream stream, long erasureLength, Prng prng,
   ErasureMethodProgressFunction callback)
  {
   ErasureMethodPass[] randomizedPasses = PassesSet;
   if (RandomizePasses)
    randomizedPasses = ShufflePasses(randomizedPasses);
   long strmStart = stream.Position;
   long strmLength = Math.Min(stream.Length - strmStart, erasureLength);
   long totalData = CalculateEraseDataSize(null, strmLength);
   byte[] buffer = new byte[Math.Min(DiskOperationUnit, strmLength)];
   for (int pass = 0; pass < Passes; ++pass)
   {
    if (callback != null)
     callback(0, totalData, pass + 1);
    stream.Seek(strmStart, SeekOrigin.Begin);
    long toWrite = strmLength;
    int dataStopped = buffer.Length;
    while (toWrite > 0)
    {
     int amount = (int)Math.Min(toWrite, buffer.Length - dataStopped);
     if (amount == 0)
     {
      randomizedPasses[pass].Execute(buffer, prng);
      dataStopped = 0;
      continue;
     }
     stream.Write(buffer, dataStopped, amount);
     stream.Flush();
     toWrite -= amount;
     if (callback != null)
      callback(amount, totalData, pass + 1);
    }
   }
  }
 }
 public class ErasureMethodManager
 {
  private class DefaultMethod : ErasureMethod
  {
   public DefaultMethod()
   {
   }
   public override string Name
   {
    get { return S._("(default)"); }
   }
   public override int Passes
   {
    get { return 0; }
   }
   public override Guid Guid
   {
    get { return Guid.Empty; }
   }
   public override long CalculateEraseDataSize(ICollection<string> paths, long targetSize)
   {
    throw new InvalidOperationException("The DefaultMethod class should never " +
     "be used and should instead be replaced before execution!");
   }
   public override void Erase(Stream strm, long erasureLength, Prng prng,
    ErasureMethodProgressFunction callback)
   {
    throw new InvalidOperationException("The DefaultMethod class should never " +
     "be used and should instead be replaced before execution!");
   }
  }
  [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
  public static readonly ErasureMethod Default = new DefaultMethod();
  public static Dictionary<Guid, ErasureMethod> Items
  {
   get
   {
    Dictionary<Guid, ErasureMethod> result = new Dictionary<Guid, ErasureMethod>();
    lock (ManagerLibrary.Instance.ErasureMethodManager.methods)
    {
     Dictionary<Guid, MethodConstructorInfo>.Enumerator iter =
      ManagerLibrary.Instance.ErasureMethodManager.methods.GetEnumerator();
     while (iter.MoveNext())
     {
      MethodConstructorInfo info = iter.Current.Value;
      result.Add(iter.Current.Key,
       (ErasureMethod)info.Constructor.Invoke(info.Parameters));
     }
    }
    return result;
   }
  }
  public static ErasureMethod GetInstance(Guid value)
  {
   lock (ManagerLibrary.Instance.ErasureMethodManager.methods)
   {
    if (!ManagerLibrary.Instance.ErasureMethodManager.methods.ContainsKey(value))
     throw new ErasureMethodNotFoundException(value);
    MethodConstructorInfo info = ManagerLibrary.Instance.ErasureMethodManager.methods[value];
    return (ErasureMethod)info.Constructor.Invoke(info.Parameters);
   }
  }
  public static void Register(ErasureMethod method)
  {
   Register(method, new object[0]);
  }
  public static void Register(ErasureMethod method, object[] parameters)
  {
   ConstructorInfo ctor = null;
   if (parameters == null || parameters.Length == 0)
    ctor = method.GetType().GetConstructor(Type.EmptyTypes);
   else
   {
    Type[] parameterTypes = new Type[parameters.Length];
    for (int i = 0; i < parameters.Length; ++i)
     parameterTypes[i] = parameters[i].GetType();
    ctor = method.GetType().GetConstructor(parameterTypes);
   }
   if (ctor == null)
    throw new ArgumentException("Registered erasure methods must contain " +
     "a parameterless constructor that is called whenever clients request " +
     "for an instance of the method. If a constructor requires parameters, " +
     "specify it in the parameters parameter.");
   lock (ManagerLibrary.Instance.ErasureMethodManager.methods)
   {
    MethodConstructorInfo info = new MethodConstructorInfo();
    info.Constructor = ctor;
    info.Parameters = parameters == null || parameters.Length == 0 ? null : parameters;
    ManagerLibrary.Instance.ErasureMethodManager.methods.Add(method.Guid, info);
   }
   OnMethodRegistered(new ErasureMethodRegistrationEventArgs(method.Guid));
  }
  public static void Unregister(Guid value)
  {
   if (!ManagerLibrary.Instance.ErasureMethodManager.methods.ContainsKey(value))
    throw new ArgumentException("The GUID of the erasure method to remove " +
     "refers to an invalid erasure method.");
   ManagerLibrary.Instance.ErasureMethodManager.methods.Remove(value);
   OnMethodUnregistered(new ErasureMethodRegistrationEventArgs(value));
  }
  private struct MethodConstructorInfo
  {
   public ConstructorInfo Constructor;
   public object[] Parameters;
  }
  private Dictionary<Guid, MethodConstructorInfo> methods =
   new Dictionary<Guid, MethodConstructorInfo>();
  public static EventHandler<ErasureMethodRegistrationEventArgs>
   MethodRegistered { get; set; }
  public static EventHandler<ErasureMethodRegistrationEventArgs>
   MethodUnregistered { get; set; }
  private static void OnMethodRegistered(ErasureMethodRegistrationEventArgs e)
  {
   if (MethodRegistered != null)
    MethodRegistered(ManagerLibrary.Instance.ErasureMethodManager, e);
  }
  private static void OnMethodUnregistered(ErasureMethodRegistrationEventArgs e)
  {
   if (MethodUnregistered != null)
    MethodUnregistered(ManagerLibrary.Instance.ErasureMethodManager, e);
  }
 }
 public class ErasureMethodRegistrationEventArgs : EventArgs
 {
  public ErasureMethodRegistrationEventArgs(Guid value)
  {
   Guid = value;
  }
  public Guid Guid { get; private set; }
 }
}
