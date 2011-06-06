using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Text;
using System.Threading;
using System.IO;
using Eraser.Util;
using System.Security.Principal;
using System.Runtime.Serialization;
using System.Runtime.Serialization.Formatters.Binary;
using System.Security.Permissions;
namespace Eraser.Manager
{
 public class DirectExecutor : Executor
 {
  public DirectExecutor()
  {
   TaskAdded += OnTaskAdded;
   TaskDeleted += OnTaskDeleted;
   tasks = new DirectExecutorTasksCollection(this);
   thread = new Thread(Main);
  }
  protected override void Dispose(bool disposing)
  {
   if (disposing)
   {
    thread.Abort();
    schedulerInterrupt.Set();
    schedulerInterrupt.Close();
   }
   base.Dispose(disposing);
  }
  public override void Run()
  {
   thread.CurrentUICulture = Thread.CurrentThread.CurrentUICulture;
   thread.Start();
  }
  public override void QueueTask(Task task)
  {
   lock (tasksLock)
   {
    DateTime executionTime = DateTime.Now;
    if (!scheduledTasks.ContainsKey(executionTime))
     scheduledTasks.Add(executionTime, new List<Task>());
    scheduledTasks[executionTime].Add(task);
    schedulerInterrupt.Set();
   }
  }
  public override void ScheduleTask(Task task)
  {
   RecurringSchedule schedule = task.Schedule as RecurringSchedule;
   if (schedule == null)
    return;
   DateTime executionTime = (schedule.MissedPreviousSchedule &&
    ManagerLibrary.Settings.ExecuteMissedTasksImmediately) ?
     DateTime.Now : schedule.NextRun;
   lock (tasksLock)
   {
    if (!scheduledTasks.ContainsKey(executionTime))
     scheduledTasks.Add(executionTime, new List<Task>());
    scheduledTasks[executionTime].Add(task);
   }
  }
  public override void QueueRestartTasks()
  {
   lock (tasksLock)
   {
    foreach (Task task in Tasks)
     if (task.Schedule == Schedule.RunOnRestart)
      QueueTask(task);
   }
  }
  public override void UnqueueTask(Task task)
  {
   lock (tasksLock)
    for (int i = 0; i != scheduledTasks.Count; ++i)
     for (int j = 0; j < scheduledTasks.Values[i].Count; )
     {
      Task currentTask = scheduledTasks.Values[i][j];
      if (currentTask == task &&
       (!(currentTask.Schedule is RecurringSchedule) ||
        ((RecurringSchedule)currentTask.Schedule).NextRun != scheduledTasks.Keys[i]))
      {
       scheduledTasks.Values[i].RemoveAt(j);
      }
      else
      {
       ++j;
      }
     }
  }
  internal override bool IsTaskQueued(Task task)
  {
   lock (tasksLock)
    foreach (KeyValuePair<DateTime, List<Task> > tasks in scheduledTasks)
     foreach (Task i in tasks.Value)
      if (task == i)
       if (task.Schedule is RecurringSchedule)
       {
        if (((RecurringSchedule)task.Schedule).NextRun != tasks.Key)
         return true;
       }
       else
        return true;
   return false;
  }
  private void OnTaskAdded(object sender, TaskEventArgs e)
  {
   e.Task.TaskEdited += OnTaskEdited;
  }
  private void OnTaskEdited(object sender, TaskEventArgs e)
  {
   lock (tasksLock)
    for (int i = 0; i != scheduledTasks.Count; ++i)
     for (int j = 0; j < scheduledTasks.Values[i].Count; )
     {
      Task currentTask = scheduledTasks.Values[i][j];
      if (currentTask == e.Task)
       scheduledTasks.Values[i].RemoveAt(j);
      else
       j++;
     }
   if (e.Task.Schedule is RecurringSchedule)
    ScheduleTask(e.Task);
  }
  private void OnTaskDeleted(object sender, TaskEventArgs e)
  {
   e.Task.TaskEdited -= OnTaskEdited;
  }
  public override ExecutorTasksCollection Tasks
  {
   get
   {
    return tasks;
   }
  }
  private void Main()
  {
   while (thread.ThreadState != ThreadState.AbortRequested)
   {
    Task task = null;
    lock (tasksLock)
    {
     while (scheduledTasks.Count != 0)
      if (scheduledTasks.Values[0].Count == 0)
      {
       scheduledTasks.RemoveAt(0);
      }
      else
      {
       if (scheduledTasks.Keys[0] <= DateTime.Now)
       {
        List<Task> tasks = scheduledTasks.Values[0];
        task = tasks[0];
        tasks.RemoveAt(0);
       }
       if (task == null)
       {
        for (int i = 0; i < scheduledTasks.Count; )
         if (scheduledTasks.Values[i].Count == 0)
          scheduledTasks.RemoveAt(i);
         else
          ++i;
       }
       break;
      }
    }
    if (task != null)
    {
     currentTask = task;
     try
     {
      Power.ExecutionState = ExecutionState.Continuous |
       ExecutionState.SystemRequired;
      task.Canceled = false;
      task.OnTaskStarted(new TaskEventArgs(task));
      OnTaskProcessing(new TaskEventArgs(task));
      task.Log.Entries.NewSession();
      foreach (ErasureTarget target in task.Targets)
       try
       {
        UnusedSpaceTarget unusedSpaceTarget =
         target as UnusedSpaceTarget;
        FileSystemObjectTarget fileSystemObjectTarget =
         target as FileSystemObjectTarget;
        if (unusedSpaceTarget != null)
         EraseUnusedSpace(task, unusedSpaceTarget);
        else if (fileSystemObjectTarget != null)
         EraseFilesystemObject(task, fileSystemObjectTarget);
        else
         throw new ArgumentException(S._("Unknown erasure target."));
       }
       catch (FatalException)
       {
        throw;
       }
       catch (OperationCanceledException)
       {
        throw;
       }
       catch (Exception e)
       {
        task.Log.LastSessionEntries.Add(new LogEntry(e.Message, LogLevel.Error));
        BlackBox.Get().CreateReport(e);
       }
     }
     catch (FatalException e)
     {
      task.Log.LastSessionEntries.Add(new LogEntry(e.Message, LogLevel.Fatal));
     }
     catch (OperationCanceledException e)
     {
      task.Log.LastSessionEntries.Add(new LogEntry(e.Message, LogLevel.Fatal));
     }
     catch (Exception e)
     {
      task.Log.LastSessionEntries.Add(new LogEntry(e.Message, LogLevel.Error));
      BlackBox.Get().CreateReport(e);
     }
     finally
     {
      Power.ExecutionState = ExecutionState.Continuous;
      if (task.Schedule is RecurringSchedule)
       ((RecurringSchedule)task.Schedule).Reschedule(DateTime.Now);
      if (task.Schedule == Schedule.RunOnRestart)
       task.Schedule = Schedule.RunNow;
      task.OnTaskFinished(new TaskEventArgs(task));
      OnTaskProcessed(new TaskEventArgs(task));
      currentTask = null;
     }
    }
    schedulerInterrupt.WaitOne(30000, false);
   }
  }
  private void EraseUnusedSpace(Task task, UnusedSpaceTarget target)
  {
   if (!Security.IsAdministrator())
   {
    if (Environment.OSVersion.Platform == PlatformID.Win32NT &&
     Environment.OSVersion.Version >= new Version(6, 0))
    {
     throw new UnauthorizedAccessException(S._("The program does not have the " +
      "required permissions to erase the unused space on disk. Run the program " +
      "as an administrator and retry the operation."));
    }
    else
     throw new UnauthorizedAccessException(S._("The program does not have the " +
      "required permissions to erase the unused space on disk."));
   }
   if (SystemRestore.GetInstances().Count != 0)
   {
    task.Log.LastSessionEntries.Add(new LogEntry(S._("The drive {0} has System " +
     "Restore or Volume Shadow Copies enabled. This may allow copies of files " +
     "stored on the disk to be recovered and pose a security concern.",
     target.Drive), LogLevel.Warning));
   }
   if (VolumeInfo.FromMountPoint(target.Drive).HasQuota)
    task.Log.LastSessionEntries.Add(new LogEntry(S._("The drive {0} has disk quotas " +
     "active. This will prevent the complete erasure of unused space and may pose " +
     "a security concern.", target.Drive), LogLevel.Warning));
   ErasureMethod method = target.Method;
   DirectoryInfo info = new DirectoryInfo(target.Drive);
   VolumeInfo volInfo = VolumeInfo.FromMountPoint(target.Drive);
   FileSystem fsManager = FileSystemManager.Get(volInfo);
   SteppedProgressManager progress = new SteppedProgressManager();
   target.Progress = progress;
   task.Progress.Steps.Add(new SteppedProgressManagerStep(
    progress, 1.0f / task.Targets.Count));
   if (target.EraseClusterTips)
   {
    ProgressManager tipSearch = new ProgressManager();
    progress.Steps.Add(new SteppedProgressManagerStep(tipSearch,
     0.0f, S._("Searching for files' cluster tips...")));
    tipSearch.Total = 1;
    ClusterTipsSearchProgress searchProgress = delegate(string path)
     {
      if (currentTask.Canceled)
       throw new OperationCanceledException(S._("The task was cancelled."));
      task.OnProgressChanged(target,
       new ProgressChangedEventArgs(tipSearch,
        new TaskProgressChangedEventArgs(path, 0, 0)));
     };
    ProgressManager tipProgress = new ProgressManager();
    progress.Steps.Add(new SteppedProgressManagerStep(tipProgress, 0.1f,
     S._("Erasing cluster tips...")));
    ClusterTipsEraseProgress eraseProgress =
     delegate(int currentFile, int totalFiles, string currentFilePath)
     {
      tipSearch.Completed = tipSearch.Total;
      tipProgress.Total = totalFiles;
      tipProgress.Completed = currentFile;
      task.OnProgressChanged(target,
       new ProgressChangedEventArgs(tipProgress,
        new TaskProgressChangedEventArgs(currentFilePath, 0, 0)));
      if (currentTask.Canceled)
       throw new OperationCanceledException(S._("The task was cancelled."));
     };
    fsManager.EraseClusterTips(VolumeInfo.FromMountPoint(target.Drive),
     method, task.Log, searchProgress, eraseProgress);
   }
   info = info.CreateSubdirectory(Path.GetFileName(
    FileSystem.GenerateRandomFileName(info, 18)));
   try
   {
    if (Eraser.Util.File.IsCompressed(info.FullName))
     Eraser.Util.File.SetCompression(info.FullName, false);
    ProgressManager mainProgress = new ProgressManager();
    progress.Steps.Add(new SteppedProgressManagerStep(mainProgress,
     target.EraseClusterTips ? 0.8f : 0.9f, S._("Erasing unused space...")));
    while (volInfo.AvailableFreeSpace > 0)
    {
     string currFile = FileSystem.GenerateRandomFileName(info, 18);
     using (FileStream stream = new FileStream(currFile, FileMode.CreateNew,
      FileAccess.Write, FileShare.None, 8, FileOptions.WriteThrough))
     {
      mainProgress.Total = mainProgress.Completed + volInfo.AvailableFreeSpace;
      long streamLength = Math.Min(ErasureMethod.FreeSpaceFileUnit,
       mainProgress.Total);
      while (true)
       try
       {
        stream.SetLength(streamLength);
        break;
       }
       catch (IOException)
       {
        if (streamLength > volInfo.ClusterSize)
         streamLength -= volInfo.ClusterSize;
        else
         throw;
       }
      method.Erase(stream, long.MaxValue,
       PrngManager.GetInstance(ManagerLibrary.Settings.ActivePrng),
       delegate(long lastWritten, long totalData, int currentPass)
       {
        mainProgress.Completed += lastWritten;
        task.OnProgressChanged(target,
         new ProgressChangedEventArgs(mainProgress,
          new TaskProgressChangedEventArgs(target.Drive, currentPass, method.Passes)));
        if (currentTask.Canceled)
         throw new OperationCanceledException(S._("The task was cancelled."));
       }
      );
     }
    }
    mainProgress.Completed = mainProgress.Total;
    ProgressManager residentProgress = new ProgressManager();
    progress.Steps.Add(new SteppedProgressManagerStep(residentProgress,
     0.05f, S._("Old resident file system table files")));
    fsManager.EraseOldFileSystemResidentFiles(volInfo, info, method,
     delegate(int currentFile, int totalFiles)
     {
      residentProgress.Completed = currentFile;
      residentProgress.Total = totalFiles;
      task.OnProgressChanged(target,
       new ProgressChangedEventArgs(residentProgress,
        new TaskProgressChangedEventArgs(string.Empty, 0, 0)));
      if (currentTask.Canceled)
       throw new OperationCanceledException(S._("The task was cancelled."));
     }
    );
    residentProgress.Completed = residentProgress.Total = 1;
   }
   finally
   {
    ProgressManager tempFiles = new ProgressManager();
    progress.Steps.Add(new SteppedProgressManagerStep(tempFiles,
     0.0f, S._("Removing temporary files...")));
    task.OnProgressChanged(target, new ProgressChangedEventArgs(tempFiles,
     new TaskProgressChangedEventArgs(string.Empty, 0, 0)));
    fsManager.DeleteFolder(info);
    tempFiles.Completed = tempFiles.Total = 1;
   }
   ProgressManager structureProgress = new ProgressManager();
   progress.Steps.Add(new SteppedProgressManagerStep(structureProgress,
    0.05f, S._("Erasing unused directory structures...")));
   fsManager.EraseDirectoryStructures(volInfo,
    delegate(int currentFile, int totalFiles)
    {
     if (currentTask.Canceled)
      throw new OperationCanceledException(S._("The task was cancelled."));
     structureProgress.Total = totalFiles;
     structureProgress.Completed = currentFile;
     task.OnProgressChanged(target,
      new ProgressChangedEventArgs(structureProgress,
       new TaskProgressChangedEventArgs(string.Empty, 0, 0)));
    }
   );
   structureProgress.Completed = structureProgress.Total;
   target.Progress = null;
  }
  private void EraseFilesystemObject(Task task, FileSystemObjectTarget target)
  {
   long dataTotal = 0;
   List<string> paths = target.GetPaths(out dataTotal);
   ErasureMethod method = target.Method;
   SteppedProgressManager progress = new SteppedProgressManager();
   target.Progress = progress;
   task.Progress.Steps.Add(new SteppedProgressManagerStep(progress, 1.0f / task.Targets.Count));
   for (int i = 0; i < paths.Count; ++i)
   {
    ProgressManager step = new ProgressManager();
    progress.Steps.Add(new SteppedProgressManagerStep(step,
     1.0f / paths.Count, S._("Erasing files...")));
    task.OnProgressChanged(target,
     new ProgressChangedEventArgs(step,
      new TaskProgressChangedEventArgs(paths[i], 0, method.Passes)));
    StreamInfo info = new StreamInfo(paths[i]);
    FileSystem fsManager = FileSystemManager.Get(
     VolumeInfo.FromMountPoint(info.DirectoryName));
    if (!info.Exists)
    {
     task.Log.LastSessionEntries.Add(new LogEntry(S._("The file {0} was not erased " +
      "as the file does not exist.", paths[i]), LogLevel.Notice));
     continue;
    }
    bool isReadOnly = false;
    try
    {
     if (isReadOnly = info.IsReadOnly)
      info.IsReadOnly = false;
     if ((info.Attributes & FileAttributes.Compressed) != 0 ||
      (info.Attributes & FileAttributes.Encrypted) != 0 ||
      (info.Attributes & FileAttributes.SparseFile) != 0)
     {
      task.Log.LastSessionEntries.Add(new LogEntry(S._("The file {0} could " +
       "not be erased because the file was either compressed, encrypted or " +
       "a sparse file.", info.FullName), LogLevel.Error));
     }
     fsManager.EraseFileSystemObject(info, method,
      delegate(long lastWritten, long totalData, int currentPass)
      {
       if (currentTask.Canceled)
        throw new OperationCanceledException(S._("The task was cancelled."));
       step.Completed += lastWritten;
       step.Total = totalData;
       task.OnProgressChanged(target,
        new ProgressChangedEventArgs(step,
         new TaskProgressChangedEventArgs(info.FullName, currentPass, method.Passes)));
      });
     FileInfo fileInfo = info.File;
     if (fileInfo != null)
      fsManager.DeleteFile(fileInfo);
     step.Completed = step.Total = 1;
    }
    catch (UnauthorizedAccessException)
    {
     task.Log.LastSessionEntries.Add(new LogEntry(S._("The file {0} could not " +
      "be erased because the file's permissions prevent access to the file.",
      info.FullName), LogLevel.Error));
    }
    catch (IOException)
    {
     if (System.Runtime.InteropServices.Marshal.GetLastWin32Error() ==
      Win32ErrorCode.SharingViolation)
     {
      if (!ManagerLibrary.Settings.ForceUnlockLockedFiles)
       throw;
      List<System.Diagnostics.Process> processes =
       new List<System.Diagnostics.Process>();
      foreach (OpenHandle handle in OpenHandle.Items)
       if (handle.Path == paths[i])
        processes.Add(System.Diagnostics.Process.GetProcessById(handle.ProcessId));
      StringBuilder processStr = new StringBuilder();
      foreach (System.Diagnostics.Process process in processes)
       processStr.AppendFormat(System.Globalization.CultureInfo.InvariantCulture,
        "{0}, ", process.MainModule.FileName);
      task.Log.LastSessionEntries.Add(new LogEntry(S._(
        "Could not force closure of file \"{0}\" (locked by {1})",
        paths[i], processStr.ToString().Remove(processStr.Length - 2)),
       LogLevel.Error));
     }
     else
      throw;
    }
    finally
    {
     if (isReadOnly && info.Exists && !info.IsReadOnly)
      info.IsReadOnly = isReadOnly;
    }
   }
   if (target is FolderTarget)
   {
    ProgressManager step = new ProgressManager();
    progress.Steps.Add(new SteppedProgressManagerStep(step,
     0.0f, S._("Removing folders...")));
    FolderTarget fldr = (FolderTarget)target;
    FileSystem fsManager = FileSystemManager.Get(VolumeInfo.FromMountPoint(fldr.Path));
    Action<DirectoryInfo> eraseEmptySubFolders = null;
    eraseEmptySubFolders = delegate(DirectoryInfo info)
    {
      foreach (DirectoryInfo subDir in info.GetDirectories())
       eraseEmptySubFolders(subDir);
      task.OnProgressChanged(target,
       new ProgressChangedEventArgs(step,
        new TaskProgressChangedEventArgs(info.FullName, 0, 0)));
      FileSystemInfo[] files = info.GetFileSystemInfos();
      if (files.Length == 0)
       fsManager.DeleteFolder(info);
    };
    eraseEmptySubFolders(new DirectoryInfo(fldr.Path));
    if (fldr.DeleteIfEmpty)
    {
     DirectoryInfo info = new DirectoryInfo(fldr.Path);
     task.OnProgressChanged(target,
      new ProgressChangedEventArgs(step,
       new TaskProgressChangedEventArgs(info.FullName, 0, 0)));
     bool isVolumeRoot = info.Parent == null;
     foreach (VolumeInfo volume in VolumeInfo.Volumes)
      foreach (string mountPoint in volume.MountPoints)
       if (info.FullName == mountPoint)
        isVolumeRoot = true;
     if (!isVolumeRoot && info.Exists && info.GetFiles("*", SearchOption.AllDirectories).Length == 0)
      fsManager.DeleteFolder(info);
    }
   }
   if (target is RecycleBinTarget)
   {
    ProgressManager step = new ProgressManager();
    progress.Steps.Add(new SteppedProgressManagerStep(step,
     0.0f, S._("Emptying recycle bin...")));
    task.OnProgressChanged(target,
     new ProgressChangedEventArgs(step,
      new TaskProgressChangedEventArgs(string.Empty, 0, 0)));
    RecycleBin.Empty(EmptyRecycleBinOptions.NoConfirmation |
     EmptyRecycleBinOptions.NoProgressUI | EmptyRecycleBinOptions.NoSound);
   }
   target.Progress = null;
  }
  private Thread thread;
  private object tasksLock = new object();
  private SortedList<DateTime, List<Task> > scheduledTasks =
   new SortedList<DateTime, List<Task> >();
  private DirectExecutorTasksCollection tasks;
  Task currentTask;
  AutoResetEvent schedulerInterrupt = new AutoResetEvent(true);
  private class DirectExecutorTasksCollection : ExecutorTasksCollection
  {
   public DirectExecutorTasksCollection(DirectExecutor executor)
    : base(executor)
   {
   }
   public override int IndexOf(Task item)
   {
    return list.IndexOf(item);
   }
   public override void Insert(int index, Task item)
   {
    item.Executor = Owner;
    lock (list)
     list.Insert(index, item);
    Owner.OnTaskAdded(new TaskEventArgs(item));
    if (item.Schedule == Schedule.RunNow)
    {
     Owner.QueueTask(item);
    }
    else if (item.Schedule != Schedule.RunOnRestart)
    {
     Owner.ScheduleTask(item);
    }
   }
   public override void RemoveAt(int index)
   {
    lock (list)
    {
     Task task = list[index];
     task.Cancel();
     task.Executor = null;
     list.RemoveAt(index);
     Owner.OnTaskDeleted(new TaskEventArgs(task));
    }
   }
   public override Task this[int index]
   {
    get
    {
     lock (list)
      return list[index];
    }
    set
    {
     lock (list)
      list[index] = value;
    }
   }
   public override void Add(Task item)
   {
    Insert(Count, item);
   }
   public override void Clear()
   {
    foreach (Task task in list)
     Remove(task);
   }
   public override bool Contains(Task item)
   {
    lock (list)
     return list.Contains(item);
   }
   public override void CopyTo(Task[] array, int arrayIndex)
   {
    lock (list)
     list.CopyTo(array, arrayIndex);
   }
   public override int Count
   {
    get
    {
     lock (list)
      return list.Count;
    }
   }
   public override bool Remove(Task item)
   {
    lock (list)
    {
     int index = list.IndexOf(item);
     if (index < 0)
      return false;
     RemoveAt(index);
    }
    return true;
   }
   public override IEnumerator<Task> GetEnumerator()
   {
    return list.GetEnumerator();
   }
   public override void SaveToStream(Stream stream)
   {
    lock (list)
     new BinaryFormatter().Serialize(stream, list);
   }
   public override void LoadFromStream(Stream stream)
   {
    StreamingContext context = new StreamingContext(
     StreamingContextStates.All, Owner);
    BinaryFormatter formatter = new BinaryFormatter(null, context);
    List<Task> deserialised = (List<Task>)formatter.Deserialize(stream);
    list.AddRange(deserialised);
    foreach (Task task in deserialised)
    {
     Owner.OnTaskAdded(new TaskEventArgs(task));
     if (task.Schedule is RecurringSchedule)
      Owner.ScheduleTask(task);
    }
   }
   private List<Task> list = new List<Task>();
  }
 }
}
