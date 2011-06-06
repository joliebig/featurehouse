using System;
using System.Linq;
using System.Collections.Generic;
using System.Text;
using System.Collections.ObjectModel;
using System.Threading;
using System.Runtime.Serialization;
using System.Security.Permissions;
namespace Eraser.Util
{
 public enum LogLevel
 {
  Information,
  Notice,
  Warning,
  Error,
  Fatal
 }
 [Serializable]
 public struct LogEntry : ISerializable
 {
  private LogEntry(SerializationInfo info, StreamingContext context)
   : this()
  {
   Level = (LogLevel)info.GetValue("Level", typeof(LogLevel));
   Timestamp = (DateTime)info.GetValue("Timestamp", typeof(DateTime));
   Message = (string)info.GetValue("Message", typeof(string));
  }
  [SecurityPermission(SecurityAction.Demand, SerializationFormatter = true)]
  public void GetObjectData(SerializationInfo info, StreamingContext context)
  {
   info.AddValue("Level", Level);
   info.AddValue("Timestamp", Timestamp);
   info.AddValue("Message", Message);
  }
  public LogEntry(string message, LogLevel level)
   : this()
  {
   Message = message;
   Level = level;
   Timestamp = DateTime.Now;
  }
  public LogLevel Level { get; private set; }
  public DateTime Timestamp { get; private set; }
  public string Message { get; private set; }
 }
 public class LogEventArgs : EventArgs
 {
  public LogEventArgs(LogEntry entry)
  {
   LogEntry = entry;
  }
  public LogEntry LogEntry { get; private set; }
 }
 public static class Logger
 {
  static Logger()
  {
   Listeners = new LogThreadDictionary();
  }
  public static void Log(string message)
  {
   Log(new LogEntry(message, LogLevel.Information));
  }
  public static void Log(string message, LogLevel level)
  {
   Log(new LogEntry(message, level));
  }
  public static void Log(LogEntry entry)
  {
   Thread currentThread = Thread.CurrentThread;
   List<ILogTarget> targets = new List<ILogTarget>();
   if (Listeners.ContainsKey(currentThread))
   {
    LogThreadTargets threadTargets = Listeners[currentThread];
    if (threadTargets != null)
     targets.AddRange(threadTargets);
   }
   targets.ForEach(
    target => target.OnEventLogged(currentThread, new LogEventArgs(entry)));
  }
  public static LogThreadDictionary Listeners { get; private set; }
 }
 public class LogThreadDictionary : IDictionary<Thread, LogThreadTargets>
 {
  public void Add(Thread key, LogThreadTargets value)
  {
   lock (Dictionary)
    Dictionary.Add(key, value);
  }
  public bool ContainsKey(Thread key)
  {
   return Dictionary.ContainsKey(key);
  }
  public ICollection<Thread> Keys
  {
   get
   {
    lock (Dictionary)
    {
     Thread[] result = new Thread[Dictionary.Keys.Count];
     Dictionary.Keys.CopyTo(result, 0);
     return new ReadOnlyCollection<Thread>(result);
    }
   }
  }
  public bool Remove(Thread key)
  {
   lock (Dictionary)
    return Dictionary.Remove(key);
  }
  public bool TryGetValue(Thread key, out LogThreadTargets value)
  {
   lock (Dictionary)
    return Dictionary.TryGetValue(key, out value);
  }
  public ICollection<LogThreadTargets> Values
  {
   get
   {
    lock (Dictionary)
    {
     LogThreadTargets[] result =
      new LogThreadTargets[Dictionary.Values.Count];
     Dictionary.Values.CopyTo(result, 0);
     return new ReadOnlyCollection<LogThreadTargets>(result);
    }
   }
  }
  public LogThreadTargets this[Thread key]
  {
   get
   {
    lock (Dictionary)
     return Dictionary[key];
   }
   set
   {
    lock (Dictionary)
     Dictionary[key] = value;
   }
  }
  public void Add(KeyValuePair<Thread, LogThreadTargets> item)
  {
   lock (Dictionary)
    Dictionary.Add(item.Key, item.Value);
  }
  public void Clear()
  {
   lock (Dictionary)
    Dictionary.Clear();
  }
  public bool Contains(KeyValuePair<Thread, LogThreadTargets> item)
  {
   lock (Dictionary)
    return Dictionary.ContainsKey(item.Key) && Dictionary[item.Key] == item.Value;
  }
  public void CopyTo(KeyValuePair<Thread, LogThreadTargets>[] array, int arrayIndex)
  {
   throw new NotImplementedException();
  }
  public int Count
  {
   get
   {
    lock (Dictionary)
     return Dictionary.Count;
   }
  }
  public bool IsReadOnly
  {
   get { return false; }
  }
  public bool Remove(KeyValuePair<Thread, LogThreadTargets> item)
  {
   lock (Dictionary)
    return Dictionary.Remove(item.Key);
  }
  public IEnumerator<KeyValuePair<Thread, LogThreadTargets> > GetEnumerator()
  {
   return Dictionary.GetEnumerator();
  }
  System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
  {
   return Dictionary.GetEnumerator();
  }
  private Dictionary<Thread, LogThreadTargets> Dictionary =
   new Dictionary<Thread, LogThreadTargets>();
 }
 public class LogThreadTargets : IList<ILogTarget>
 {
  public int IndexOf(ILogTarget item)
  {
   lock (List)
    return List.IndexOf(item);
  }
  public void Insert(int index, ILogTarget item)
  {
   lock (List)
    List.Insert(index, item);
  }
  public void RemoveAt(int index)
  {
   lock (List)
    List.RemoveAt(index);
  }
  public ILogTarget this[int index]
  {
   get
   {
    lock (List)
     return List[index];
   }
   set
   {
    lock (List)
     List[index] = value;
   }
  }
  public void Add(ILogTarget item)
  {
   lock (List)
    List.Add(item);
  }
  public void Clear()
  {
   lock (List)
    List.Clear();
  }
  public bool Contains(ILogTarget item)
  {
   lock (List)
    return List.Contains(item);
  }
  public void CopyTo(ILogTarget[] array, int arrayIndex)
  {
   lock (List)
    List.CopyTo(array, arrayIndex);
  }
  public int Count
  {
   get
   {
    lock (List)
     return List.Count;
   }
  }
  public bool IsReadOnly
  {
   get { return false; }
  }
  public bool Remove(ILogTarget item)
  {
   lock (List)
    return List.Remove(item);
  }
  public IEnumerator<ILogTarget> GetEnumerator()
  {
   return List.GetEnumerator();
  }
  System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
  {
   return List.GetEnumerator();
  }
  private List<ILogTarget> List = new List<ILogTarget>();
 }
 public interface ILogTarget
 {
  void OnEventLogged(object sender, LogEventArgs e);
  void Chain(ILogTarget target);
  void Unchain(ILogTarget target);
 }
 public sealed class LogSession : IDisposable
 {
  public LogSession(ILogTarget target, params Thread[] threads)
  {
   Target = target;
   Threads = threads.Distinct().ToArray();
   foreach (Thread thread in Threads)
   {
    if (!Logger.Listeners.ContainsKey(thread))
     Logger.Listeners.Add(thread, new LogThreadTargets());
    Logger.Listeners[thread].Add(target);
   }
   Target.OnEventLogged(this, new LogEventArgs(
    new LogEntry(("Session started"), LogLevel.Information)));
  }
  public LogSession(ILogTarget target)
   : this(target, Thread.CurrentThread)
  {
  }
  ~LogSession()
  {
   Dispose(false);
  }
  private void Dispose(bool disposing)
  {
   if (Threads == null || Target == null)
    return;
   if (disposing)
   {
    foreach (Thread thread in Threads)
     Logger.Listeners[thread].Remove(Target);
   }
   Target.OnEventLogged(this, new LogEventArgs(
    new LogEntry(("Session ended"), LogLevel.Information)));
   Threads = null;
   Target = null;
  }
  public void Dispose()
  {
   Dispose(true);
   GC.SuppressFinalize(this);
  }
  private ILogTarget Target;
  private Thread[] Threads;
 }
 [Serializable]
 public class LogSink : ISerializable, ILogTarget, IList<LogEntry>
 {
  public LogSink()
  {
  }
  public LogSink(SerializationInfo info, StreamingContext context)
  {
   List = (List<LogEntry>)info.GetValue("List", typeof(List<LogEntry>));
  }
  [SecurityPermission(SecurityAction.Demand, SerializationFormatter = true)]
  public void GetObjectData(SerializationInfo info, StreamingContext context)
  {
   info.AddValue("List", List);
  }
  public void OnEventLogged(object sender, LogEventArgs e)
  {
   lock (List)
    List.Add(e.LogEntry);
   lock (ChainedTargets)
    ChainedTargets.ForEach(target => target.OnEventLogged(sender, e));
  }
  public void Chain(ILogTarget target)
  {
   lock (ChainedTargets)
    ChainedTargets.Add(target);
  }
  public void Unchain(ILogTarget target)
  {
   lock (ChainedTargets)
    ChainedTargets.Remove(target);
  }
  private List<ILogTarget> ChainedTargets = new List<ILogTarget>();
  public int IndexOf(LogEntry item)
  {
   lock (List)
    return IndexOf(item);
  }
  public void Insert(int index, LogEntry item)
  {
   lock (List)
    List.Insert(index, item);
  }
  public void RemoveAt(int index)
  {
   lock (List)
    List.RemoveAt(index);
  }
  public LogEntry this[int index]
  {
   get
   {
    lock (List)
     return List[index];
   }
   set
   {
    lock (List)
     List[index] = value;
   }
  }
  public void Add(LogEntry item)
  {
   lock (List)
    List.Add(item);
  }
  public void Clear()
  {
   lock (List)
    List.Clear();
  }
  public bool Contains(LogEntry item)
  {
   lock (List)
    return List.Contains(item);
  }
  public void CopyTo(LogEntry[] array, int arrayIndex)
  {
   lock (List)
    List.CopyTo(array, arrayIndex);
  }
  public int Count
  {
   get
   {
    lock(List)
     return Count;
   }
  }
  public bool IsReadOnly
  {
   get { return true; }
  }
  public bool Remove(LogEntry item)
  {
   lock (List)
    return List.Remove(item);
  }
  public IEnumerator<LogEntry> GetEnumerator()
  {
   return List.GetEnumerator();
  }
  System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
  {
   return List.GetEnumerator();
  }
  public LogLevel Highest
  {
   get
   {
    lock (List)
     return List.Max(delegate(LogEntry e) { return e.Level; });
   }
  }
  public DateTime StartTime
  {
   get
   {
    lock (List)
     return List.First().Timestamp;
   }
  }
  public DateTime EndTime
  {
   get
   {
    lock (List)
     return List.Last().Timestamp;
   }
  }
  private List<LogEntry> List = new List<LogEntry>();
 }
}
