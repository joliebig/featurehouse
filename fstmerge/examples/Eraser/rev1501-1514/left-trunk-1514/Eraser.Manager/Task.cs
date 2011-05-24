

using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Text.RegularExpressions;
using System.Runtime.Serialization;
using System.ComponentModel;
using Eraser.Util;
using System.Security.Permissions;
using System.Threading;

namespace Eraser.Manager
{



 [Serializable]
 public class Task : ISerializable
 {

  protected Task(SerializationInfo info, StreamingContext context)
  {
   Name = (string)info.GetValue("Name", typeof(string));
   Executor = context.Context as Executor;
   Targets = (ErasureTargetsCollection)info.GetValue("Targets", typeof(ErasureTargetsCollection));
   Targets.Owner = this;
   Log = (Logger)info.GetValue("Log", typeof(Logger));
   Canceled = false;

   Schedule schedule = (Schedule)info.GetValue("Schedule", typeof(Schedule));
   if (schedule.GetType() == Schedule.RunManually.GetType())
    Schedule = Schedule.RunManually;
   else if (schedule.GetType() == Schedule.RunNow.GetType())
    Schedule = Schedule.RunNow;
   else if (schedule.GetType() == Schedule.RunOnRestart.GetType())
    Schedule = Schedule.RunOnRestart;
   else if (schedule is RecurringSchedule)
    Schedule = schedule;
   else
    throw new InvalidDataException(S._("An invalid type was found when loading " +
     "the task schedule"));
  }

  [SecurityPermission(SecurityAction.Demand, SerializationFormatter = true)]
  public virtual void GetObjectData(SerializationInfo info, StreamingContext context)
  {
   info.AddValue("Name", Name);
   info.AddValue("Schedule", Schedule);
   info.AddValue("Targets", Targets);
   info.AddValue("Log", Log);
  }





  public Task()
  {
   Name = string.Empty;
   Targets = new ErasureTargetsCollection(this);
   Schedule = Schedule.RunNow;
   Canceled = false;
   Log = new Logger();
  }





  public void Cancel()
  {
   Executor.UnqueueTask(this);
   Canceled = true;
  }




  public Executor Executor { get; internal set; }





  public string Name { get; set; }




  public string UIText
  {
   get
   {

    if (Name.Length != 0)
     return Name;

    string result = string.Empty;
    if (Targets.Count < 5)
    {

     foreach (ErasureTarget tgt in Targets)
      result += tgt.UIText + ", ";

     return result.Remove(result.Length - 2);
    }
    else
    {

     result = Targets[0].UIText + ", ";
     result += Targets[Targets.Count / 2].UIText + ", ";
     result += Targets[Targets.Count - 1].UIText;

     return S._("{0} and {1} other targets", result, Targets.Count - 3);
    }
   }
  }




  public bool Executing { get; private set; }






  public bool Queued
  {
   get
   {
    return Executor.IsTaskQueued(this);
   }
  }




  public bool Canceled
  {
   get
   {
    return canceled;
   }

   internal set
   {
    canceled = value;
   }
  }




  public ErasureTargetsCollection Targets { get; private set; }




  public Schedule Schedule
  {
   get
   {
    return schedule;
   }
   set
   {
    if (value.Owner != null)
     throw new ArgumentException(S._("The schedule provided can only " +
      "belong to one task at a time"));

    if (schedule is RecurringSchedule)
     ((RecurringSchedule)schedule).Owner = null;
    schedule = value;
    if (schedule is RecurringSchedule)
     ((RecurringSchedule)schedule).Owner = this;
    OnTaskEdited();
   }
  }




  public Logger Log { get; private set; }

  private Schedule schedule;


  private volatile bool canceled;





  public EventHandler<TaskEventArgs> TaskEdited { get; set; }




  public EventHandler<TaskEventArgs> TaskStarted { get; set; }




  public EventHandler<TaskProgressEventArgs> ProgressChanged { get; set; }




  public EventHandler<TaskEventArgs> TaskFinished { get; set; }




  internal void OnTaskEdited()
  {
   if (TaskEdited != null)
    TaskEdited(this, new TaskEventArgs(this));
  }





  internal void OnTaskStarted(TaskEventArgs e)
  {
   if (TaskStarted != null)
    TaskStarted(this, e);
   Executing = true;
  }





  internal void OnProgressChanged(TaskProgressEventArgs e)
  {
   if (ProgressChanged != null)
    ProgressChanged(this, e);
  }





  internal void OnTaskFinished(TaskEventArgs e)
  {
   if (TaskFinished != null)
    TaskFinished(this, e);
   Executing = false;
  }

 }




 [Serializable]
 public abstract class ErasureTarget : ISerializable
 {

  protected ErasureTarget(SerializationInfo info, StreamingContext context)
  {
   Guid methodGuid = (Guid)info.GetValue("Method", typeof(Guid));
   if (methodGuid == Guid.Empty)
    method = ErasureMethodManager.Default;
   else
    method = ErasureMethodManager.GetInstance(methodGuid);
  }

  [SecurityPermission(SecurityAction.Demand, SerializationFormatter = true)]
  public virtual void GetObjectData(SerializationInfo info, StreamingContext context)
  {
   info.AddValue("Method", method.Guid);
  }





  protected ErasureTarget()
  {
  }
  public virtual ErasureMethod Method
  {
   get
   {
    return method;
   }
   set
   {
    method = value;
    MethodDefined = method != ErasureMethodManager.Default;
   }
  }
  public bool MethodDefined { get; private set; }
  public Task Task { get; internal set; }
  public abstract string UIText
  {
   get;
  }
  public abstract long TotalData
  {
   get;
  }
  private ErasureMethod method;
 }
 [Serializable]
 public abstract class FileSystemObjectTarget : ErasureTarget
 {
  protected FileSystemObjectTarget(SerializationInfo info, StreamingContext context)
   : base(info, context)
  {
   Path = (string)info.GetValue("Path", typeof(string));
  }
  [SecurityPermission(SecurityAction.Demand, SerializationFormatter = true)]
  public override void GetObjectData(SerializationInfo info, StreamingContext context)
  {
   base.GetObjectData(info, context);
   info.AddValue("Path", Path);
  }
  protected FileSystemObjectTarget()
   : base()
  {
   Method = ErasureMethodManager.Default;
  }
  internal abstract List<string> GetPaths(out long totalSize);
  protected void GetPathADSes(ICollection<string> list, out long totalSize, string file)
  {
   totalSize = 0;
   try
   {
    ICollection<string> adses = Util.File.GetADSes(new FileInfo(file));
    foreach (string adsName in adses)
    {
     string adsPath = file + ':' + adsName;
     list.Add(adsPath);
     Util.StreamInfo info = new Util.StreamInfo(adsPath);
     totalSize += info.Length;
    }
   }
   catch (FileLoadException)
   {
    if (!ManagerLibrary.Settings.ForceUnlockLockedFiles)
     throw;
    foreach (OpenHandle handle in OpenHandle.Items)
     if (handle.Path == file && handle.Close())
     {
      GetPathADSes(list, out totalSize, file);
      return;
     }
   }
   catch (UnauthorizedAccessException e)
   {
    Task.Log.LastSessionEntries.Add(new LogEntry(e.Message, LogLevel.Error));
   }
  }
  public string Path { get; set; }
  public sealed override ErasureMethod Method
  {
   get
   {
    if (base.MethodDefined)
     return base.Method;
    return ErasureMethodManager.GetInstance(
     ManagerLibrary.Settings.DefaultFileErasureMethod);
   }
   set
   {
    base.Method = value;
   }
  }
  public override string UIText
  {
   get { return System.IO.Path.GetFileName(Path); }
  }
  public override long TotalData
  {
   get
   {
    long totalSize = 0;
    List<string> paths = GetPaths(out totalSize);
    return Method.CalculateEraseDataSize(paths, totalSize);
   }
  }
 }
 [Serializable]
 public class UnusedSpaceTarget : ErasureTarget
 {
  protected UnusedSpaceTarget(SerializationInfo info, StreamingContext context)
   : base(info, context)
  {
   Drive = (string)info.GetValue("Drive", typeof(string));
   EraseClusterTips = (bool)info.GetValue("EraseClusterTips", typeof(bool));
  }
  [SecurityPermission(SecurityAction.Demand, SerializationFormatter = true)]
  public override void GetObjectData(SerializationInfo info, StreamingContext context)
  {
   base.GetObjectData(info, context);
   info.AddValue("Drive", Drive);
   info.AddValue("EraseClusterTips", EraseClusterTips);
  }
  public UnusedSpaceTarget()
   : base()
  {
   Method = ErasureMethodManager.Default;
  }
  public override sealed ErasureMethod Method
  {
   get
   {
    if (base.MethodDefined)
     return base.Method;
    return ErasureMethodManager.GetInstance(
     ManagerLibrary.Settings.DefaultUnusedSpaceErasureMethod);
   }
   set
   {
    base.Method = value;
   }
  }
  public override string UIText
  {
   get { return S._("Unused disk space ({0})", Drive); }
  }
  public override long TotalData
  {
   get
   {
    VolumeInfo info = VolumeInfo.FromMountpoint(Drive);
    return Method.CalculateEraseDataSize(null, info.AvailableFreeSpace);
   }
  }
  public string Drive { get; set; }
  public bool EraseClusterTips { get; set; }
 }
 [Serializable]
 public class FileTarget : FileSystemObjectTarget
 {
  protected FileTarget(SerializationInfo info, StreamingContext context)
   : base(info, context)
  {
  }
  public FileTarget()
  {
  }
  internal override List<string> GetPaths(out long totalSize)
  {
   totalSize = 0;
   List<string> result = new List<string>();
   FileInfo fileInfo = new FileInfo(Path);
   if (fileInfo.Exists)
   {
    GetPathADSes(result, out totalSize, Path);
    totalSize += fileInfo.Length;
   }
   result.Add(Path);
   return result;
  }
 }
 [Serializable]
 public class FolderTarget : FileSystemObjectTarget
 {
  protected FolderTarget(SerializationInfo info, StreamingContext context)
   : base(info, context)
  {
   IncludeMask = (string)info.GetValue("IncludeMask", typeof(string));
   ExcludeMask = (string)info.GetValue("ExcludeMask", typeof(string));
   DeleteIfEmpty = (bool)info.GetValue("DeleteIfEmpty", typeof(bool));
  }
  [SecurityPermission(SecurityAction.Demand, SerializationFormatter = true)]
  public override void GetObjectData(SerializationInfo info, StreamingContext context)
  {
   base.GetObjectData(info, context);
   info.AddValue("IncludeMask", IncludeMask);
   info.AddValue("ExcludeMask", ExcludeMask);
   info.AddValue("DeleteIfEmpty", DeleteIfEmpty);
  }
  public FolderTarget()
  {
   IncludeMask = string.Empty;
   ExcludeMask = string.Empty;
   DeleteIfEmpty = true;
  }
  internal override List<string> GetPaths(out long totalSize)
  {
   List<string> result = new List<string>();
   DirectoryInfo dir = new DirectoryInfo(Path);
   FileInfo[] files = GetFiles(dir);
   totalSize = 0;
   if (ExcludeMask.Length != 0)
   {
    string regex = Regex.Escape(ExcludeMask).Replace("\\*", ".*").
     Replace("\\?", ".");
    Regex excludePattern = new Regex(regex, RegexOptions.IgnoreCase);
    foreach (FileInfo file in files)
     if ((file.Attributes & FileAttributes.ReparsePoint) == 0 &&
      excludePattern.Matches(file.FullName).Count == 0)
     {
      totalSize += file.Length;
      GetPathADSes(result, out totalSize, file.FullName);
      result.Add(file.FullName);
     }
   }
   else
    foreach (FileInfo file in files)
    {
     if ((file.Attributes & FileAttributes.ReparsePoint) != 0)
      continue;
     totalSize += file.Length;
     long adsesSize = 0;
     GetPathADSes(result, out adsesSize, file.FullName);
     totalSize += adsesSize;
     result.Add(file.FullName);
    }
   return result;
  }
  private FileInfo[] GetFiles(DirectoryInfo info)
  {
   List<FileInfo> result = new List<FileInfo>();
   if (info.Exists)
   {
    foreach (DirectoryInfo dir in info.GetDirectories())
     try
     {
      result.AddRange(GetFiles(dir));
     }
     catch (DirectoryNotFoundException e)
     {
      Task.Log.LastSessionEntries.Add(new LogEntry(S._("Could not erase {0} because {1}",
       dir.FullName, e.Message), LogLevel.Error));
     }
    if (IncludeMask.Length == 0)
     result.AddRange(info.GetFiles());
    else
     result.AddRange(info.GetFiles(IncludeMask, SearchOption.TopDirectoryOnly));
   }
   return result.ToArray();
  }
  public string IncludeMask { get; set; }
  public string ExcludeMask { get; set; }
  public bool DeleteIfEmpty { get; set; }
 }
 [Serializable]
 public class RecycleBinTarget : FileSystemObjectTarget
 {
  protected RecycleBinTarget(SerializationInfo info, StreamingContext context)
   : base(info, context)
  {
  }
  public RecycleBinTarget()
  {
  }
  internal override List<string> GetPaths(out long totalSize)
  {
   totalSize = 0;
   List<string> result = new List<string>();
   string[] rootDirectory = new string[] {
     "$RECYCLE.BIN",
     "RECYCLER"
    };
   foreach (DriveInfo drive in DriveInfo.GetDrives())
   {
    foreach (string rootDir in rootDirectory)
    {
     DirectoryInfo dir = new DirectoryInfo(
      System.IO.Path.Combine(
       System.IO.Path.Combine(drive.Name, rootDir),
       System.Security.Principal.WindowsIdentity.GetCurrent().
        User.ToString()));
     if (!dir.Exists)
      continue;
     GetRecyclerFiles(dir, ref result, ref totalSize);
    }
   }
   return result;
  }
  private void GetRecyclerFiles(DirectoryInfo info, ref List<string> paths,
   ref long totalSize)
  {
   try
   {
    foreach (FileSystemInfo fsInfo in info.GetFileSystemInfos())
    {
     FileInfo fileInfo = fsInfo as FileInfo;
     if (fileInfo != null)
     {
      totalSize += fileInfo.Length;
      GetPathADSes(paths, out totalSize, fileInfo.FullName);
      paths.Add(fileInfo.FullName);
     }
     else
      GetRecyclerFiles((DirectoryInfo)fsInfo, ref paths, ref totalSize);
    }
   }
   catch (UnauthorizedAccessException e)
   {
    Task.Log.LastSessionEntries.Add(new LogEntry(e.Message, LogLevel.Error));
   }
  }
  public override string UIText
  {
   get
   {
    return S._("Recycle Bin");
   }
  }
 }
 [Serializable]
 public class ErasureTargetsCollection : IList<ErasureTarget>, ICollection<ErasureTarget>,
  IEnumerable<ErasureTarget>, ISerializable
 {
  internal ErasureTargetsCollection(Task owner)
  {
   this.list = new List<ErasureTarget>();
   this.owner = owner;
  }
  internal ErasureTargetsCollection(Task owner, int capacity)
   : this(owner)
  {
   list.Capacity = capacity;
  }
  internal ErasureTargetsCollection(Task owner, IEnumerable<ErasureTarget> targets)
   : this(owner)
  {
   list.AddRange(targets);
  }
  protected ErasureTargetsCollection(SerializationInfo info, StreamingContext context)
  {
   list = (List<ErasureTarget>)info.GetValue("list", typeof(List<ErasureTarget>));
  }
  [SecurityPermission(SecurityAction.Demand, SerializationFormatter = true)]
  public virtual void GetObjectData(SerializationInfo info, StreamingContext context)
  {
   info.AddValue("list", list);
  }
  public IEnumerator<ErasureTarget> GetEnumerator()
  {
   return list.GetEnumerator();
  }
  System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
  {
   return GetEnumerator();
  }
  public void Add(ErasureTarget item)
  {
   item.Task = owner;
   list.Add(item);
  }
  public void Clear()
  {
   foreach (ErasureTarget item in list)
    Remove(item);
  }
  public bool Contains(ErasureTarget item)
  {
   return list.Contains(item);
  }
  public void CopyTo(ErasureTarget[] array, int arrayIndex)
  {
   list.CopyTo(array, arrayIndex);
  }
  public int Count
  {
   get
   {
    return list.Count;
   }
  }
  public bool IsReadOnly
  {
   get
   {
    return false;
   }
  }
  public bool Remove(ErasureTarget item)
  {
   int index = list.IndexOf(item);
   if (index < 0)
    return false;
   RemoveAt(index);
   return true;
  }
  public int IndexOf(ErasureTarget item)
  {
   return list.IndexOf(item);
  }
  public void Insert(int index, ErasureTarget item)
  {
   item.Task = owner;
   list.Insert(index, item);
  }
  public void RemoveAt(int index)
  {
   list.RemoveAt(index);
  }
  public ErasureTarget this[int index]
  {
   get
   {
    return list[index];
   }
   set
   {
    list[index] = value;
   }
  }
  public Task Owner
  {
   get
   {
    return owner;
   }
   internal set
   {
    owner = value;
    foreach (ErasureTarget target in list)
     target.Task = owner;
   }
  }
  private Task owner;
  private List<ErasureTarget> list;
 }
 public class TaskEventArgs : EventArgs
 {
  public TaskEventArgs(Task task)
  {
   Task = task;
  }
  public Task Task { get; private set; }
 }
 public class TaskProgressEventArgs : TaskEventArgs
 {
  public TaskProgressEventArgs(Task task)
   : base(task)
  {
   CurrentItemPass = 1;
  }
  public float OverallProgress
  {
   get { return overallProgress; }
  }
  public TimeSpan TimeLeft { get; internal set; }
  public ErasureTarget CurrentTarget { get; internal set; }
  public int CurrentTargetIndex { get; internal set; }
  public int CurrentTargetTotalPasses { get; internal set; }
  public string CurrentTargetStatus { get; internal set; }
  public float CurrentItemProgress { get; internal set; }
  public string CurrentItemName { get; internal set; }
  public int CurrentItemPass { get; internal set; }
  internal float CurrentTargetProgress
  {
   set
   {
    overallProgress = Math.Min(
     (value + (float)(CurrentTargetIndex - 1)) / Task.Targets.Count,
     1.0f);
   }
  }
  private float overallProgress;
 }
}
