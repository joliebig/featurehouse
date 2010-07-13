

using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.Serialization;
using System.Security.Permissions;

namespace Eraser.Manager
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
 public class Logger : ISerializable
 {
  protected Logger(SerializationInfo info, StreamingContext context)
  {
   Entries = (LogSessionDictionary)info.GetValue("Entries", typeof(LogSessionDictionary));
   Entries.Owner = this;
   foreach (DateTime key in Entries.Keys)
    LastSession = key;
  }
  [SecurityPermission(SecurityAction.Demand, SerializationFormatter=true)]
  public virtual void GetObjectData(SerializationInfo info, StreamingContext context)
  {
   info.AddValue("Entries", Entries);
  }
  public Logger()
  {
   Entries = new LogSessionDictionary(this);
  }
  public EventHandler<LogEventArgs> Logged { get; set; }
  internal void OnLogged(object sender, LogEventArgs e)
  {
   if (Logged != null)
    Logged(sender, e);
  }
  public EventHandler<EventArgs> NewSession { get; set; }
  internal void OnNewSession(object sender, EventArgs e)
  {
   if (NewSession != null)
    NewSession(sender, e);
  }
  public LogSessionDictionary Entries { get; private set; }
  public LogEntryCollection LastSessionEntries
  {
   get
   {
    return Entries[LastSession];
   }
  }
  public void Clear()
  {
   LogEntryCollection lastSessionEntries = null;
   if (Entries.ContainsKey(LastSession))
    lastSessionEntries = Entries[LastSession];
   Entries.Clear();
   if (lastSessionEntries != null)
    Entries.Add(LastSession, lastSessionEntries);
  }
  public DateTime LastSession
  {
   get { return lastSession; }
   internal set { lastSession = value; OnNewSession(null, EventArgs.Empty); }
  }
  private DateTime lastSession;
 }
 public class LogEventArgs : EventArgs
 {
  public LogEventArgs(LogEntry entry)
  {
   LogEntry = entry;
  }
  public LogEntry LogEntry { get; private set; }
 }
 [Serializable]
 public class LogSessionDictionary : IDictionary<DateTime, LogEntryCollection>,
  ISerializable
 {
  public LogSessionDictionary(Logger logger)
  {
   Owner = logger;
  }
  public void NewSession()
  {
   DateTime sessionTime = DateTime.Now;
   Add(sessionTime, new LogEntryCollection(Owner));
   Owner.LastSession = sessionTime;
  }
  protected LogSessionDictionary(SerializationInfo info, StreamingContext context)
  {
   dictionary = (Dictionary<DateTime, LogEntryCollection>)info.GetValue("Dictionary",
    dictionary.GetType());
  }
  [SecurityPermission(SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
  public virtual void GetObjectData(SerializationInfo info, StreamingContext context)
  {
   lock (dictionary)
    info.AddValue("Dictionary", dictionary);
  }
  public void Add(DateTime key, LogEntryCollection value)
  {
   lock (dictionary)
    dictionary.Add(key, value);
  }
  public bool ContainsKey(DateTime key)
  {
   lock (dictionary)
    return dictionary.ContainsKey(key);
  }
  public ICollection<DateTime> Keys
  {
   get
   {
    lock (dictionary)
    {
     DateTime[] result = new DateTime[dictionary.Keys.Count];
     dictionary.Keys.CopyTo(result, 0);
     return result;
    }
   }
  }
  public bool Remove(DateTime key)
  {
   lock (dictionary)
    return dictionary.Remove(key);
  }
  public bool TryGetValue(DateTime key, out LogEntryCollection value)
  {
   lock (dictionary)
    return dictionary.TryGetValue(key, out value);
  }
  public ICollection<LogEntryCollection> Values
  {
   get
   {
    lock (dictionary)
    {
     LogEntryCollection[] result = new LogEntryCollection[dictionary.Values.Count];
     dictionary.Values.CopyTo(result, 0);
     return result;
    }
   }
  }
  public LogEntryCollection this[DateTime key]
  {
   get
   {
    lock (dictionary)
     return dictionary[key];
   }
   set
   {
    lock (dictionary)
     dictionary[key] = value;
   }
  }
  public void Add(KeyValuePair<DateTime, LogEntryCollection> item)
  {
   Add(item.Key, item.Value);
  }
  public void Clear()
  {
   lock (dictionary)
    dictionary.Clear();
  }
  public bool Contains(KeyValuePair<DateTime, LogEntryCollection> item)
  {
   lock (dictionary)
    return dictionary.ContainsKey(item.Key) && dictionary[item.Key] == item.Value;
  }
  public void CopyTo(KeyValuePair<DateTime, LogEntryCollection>[] array, int arrayIndex)
  {
   throw new NotImplementedException();
  }
  public int Count
  {
   get
   {
    lock (dictionary)
     return dictionary.Count;
   }
  }
  public bool IsReadOnly
  {
   get { return false; }
  }
  public bool Remove(KeyValuePair<DateTime, LogEntryCollection> item)
  {
   lock (dictionary)
    return dictionary.Remove(item.Key);
  }
  public IEnumerator<KeyValuePair<DateTime, LogEntryCollection> > GetEnumerator()
  {
   return dictionary.GetEnumerator();
  }
  System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
  {
   return GetEnumerator();
  }
  internal Logger Owner
  {
   get
   {
    return owner;
   }
   set
   {
    lock (dictionary)
     foreach (LogEntryCollection entries in dictionary.Values)
      entries.owner = value;
    owner = value;
   }
  }
  private Logger owner;
  private Dictionary<DateTime, LogEntryCollection> dictionary =
   new Dictionary<DateTime, LogEntryCollection>();
 }
 [Serializable]
 public class LogEntryCollection : IList<LogEntry>
 {
  internal LogEntryCollection(Logger logger)
  {
   owner = logger;
  }
  public int IndexOf(LogEntry item)
  {
   lock (list)
    return list.IndexOf(item);
  }
  public void Insert(int index, LogEntry item)
  {
   lock (list)
    list.Insert(index, item);
   owner.OnLogged(owner, new LogEventArgs(item));
  }
  public void RemoveAt(int index)
  {
   throw new InvalidOperationException();
  }
  public LogEntry this[int index]
  {
   get
   {
    lock (list)
     return list[index];
   }
   set
   {
    throw new InvalidOperationException();
   }
  }
  public void Add(LogEntry item)
  {
   Insert(Count, item);
  }
  public void Clear()
  {
   throw new InvalidOperationException();
  }
  public bool Contains(LogEntry item)
  {
   lock (list)
    return list.Contains(item);
  }
  public void CopyTo(LogEntry[] array, int arrayIndex)
  {
   lock (list)
    list.CopyTo(array, arrayIndex);
  }
  public int Count
  {
   get
   {
    lock (list)
     return list.Count;
   }
  }
  public bool IsReadOnly
  {
   get { return false; }
  }
  public bool Remove(LogEntry item)
  {
   lock (list)
    return list.Remove(item);
  }
  public IEnumerator<LogEntry> GetEnumerator()
  {
   return list.GetEnumerator();
  }
  System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
  {
   return GetEnumerator();
  }
  internal Logger owner;
  private List<LogEntry> list = new List<LogEntry>();
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
}
