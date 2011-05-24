

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

      KernelApi.SetThreadExecutionState(ThreadExecutionState.Continuous |
       ThreadExecutionState.SystemRequired);


      task.Canceled = false;
      task.OnTaskStarted(new TaskEventArgs(task));
      OnTaskProcessing(new TaskEventArgs(task));



      task.Log.Entries.NewSession();


      TaskProgressManager progress = new TaskProgressManager(task);
      foreach (ErasureTarget target in task.Targets)
       try
       {
        progress.Event.CurrentTarget = target;
        ++progress.Event.CurrentTargetIndex;

        UnusedSpaceTarget unusedSpaceTarget =
         target as UnusedSpaceTarget;
        FileSystemObjectTarget fileSystemObjectTarget =
         target as FileSystemObjectTarget;

        if (unusedSpaceTarget != null)
         EraseUnusedSpace(task, unusedSpaceTarget, progress);
        else if (fileSystemObjectTarget != null)
         EraseFilesystemObject(task, fileSystemObjectTarget, progress);
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
     }
     finally
     {

      KernelApi.SetThreadExecutionState(ThreadExecutionState.Continuous);


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




  private class ProgressManager
  {



   public void Start()
   {
    startTime = DateTime.Now;
   }




   public long Completed
   {
    get
    {
     return completed;
    }
    set
    {
     lastCompleted += value - completed;
     completed = value;
    }
   }




   public long Total
   {
    get
    {
     return total;
    }
    set
    {
     total = value;
    }
   }




   public float Progress
   {
    get
    {
     return (float)((double)Completed / Total);
    }
   }





   public int Speed
   {
    get
    {
     if (DateTime.Now == startTime)
      return 0;

     if ((DateTime.Now - lastSpeedCalc).Seconds < 5 && lastSpeed != 0)
      return lastSpeed;


     double timeElapsed = (DateTime.Now - lastSpeedCalc).TotalSeconds;
     if (timeElapsed == 0.0)
      return 0;


     lastSpeed = (int)(lastCompleted / timeElapsed);
     lastSpeedCalc = DateTime.Now;
     lastCompleted = 0;
     return lastSpeed;
    }
   }





   public TimeSpan TimeLeft
   {
    get
    {
     if (Speed == 0)
      return new TimeSpan(0, 0, -1);
     return new TimeSpan(0, 0, (int)((Total - Completed) / Speed));
    }
   }




   private DateTime startTime;





   private DateTime lastSpeedCalc;




   private int lastSpeed;




   private long lastCompleted;




   private long completed;




   private long total;
  }




  private class TaskProgressManager : ProgressManager
  {



   public TaskProgressManager(Task task)
   {
    foreach (ErasureTarget target in task.Targets)
     Total += target.TotalData;

    Event = new TaskProgressEventArgs(task);
    Start();
   }





   public TaskProgressEventArgs Event
   {
    get
    {
     return evt;
    }
    set
    {
     evt = value;
    }
   }

   private TaskProgressEventArgs evt;
  }







  private void EraseUnusedSpace(Task task, UnusedSpaceTarget target, TaskProgressManager progress)
  {

   if (!AdvApi.IsAdministrator())
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
      "required permissions to erase the unused space on disk"));
   }


   if (VolumeInfo.FromMountpoint(target.Drive).HasQuota)
    task.Log.LastSessionEntries.Add(new LogEntry(S._("The drive which is having its " +
     "unused space erased has disk quotas active. This will prevent the complete " +
     "erasure of unused space and will pose a security concern"), LogLevel.Warning));


   ErasureMethod method = target.Method;


   DirectoryInfo info = new DirectoryInfo(target.Drive);
   VolumeInfo volInfo = VolumeInfo.FromMountpoint(target.Drive);
   FileSystem fsManager = FileSystemManager.Get(volInfo);


   if (target.EraseClusterTips)
   {
    progress.Event.CurrentTargetStatus = S._("Searching for files' cluster tips...");
    progress.Event.CurrentTargetTotalPasses = method.Passes;
    progress.Event.CurrentItemProgress = -1.0f;
    progress.Event.TimeLeft = new TimeSpan(0, 0, -1);


    ProgressManager tipProgress = new ProgressManager();
    tipProgress.Start();


    ClusterTipsSearchProgress searchProgress = delegate(string path)
     {
      progress.Event.CurrentItemName = path;
      task.OnProgressChanged(progress.Event);

      if (currentTask.Canceled)
       throw new OperationCanceledException(S._("The task was cancelled."));
     };

    ClusterTipsEraseProgress eraseProgress =
     delegate(int currentFile, int totalFiles, string currentFilePath)
     {
      tipProgress.Total = totalFiles;
      tipProgress.Completed = currentFile;

      progress.Event.CurrentTargetStatus = S._("Erasing cluster tips...");
      progress.Event.CurrentItemName = currentFilePath;
      progress.Event.CurrentItemProgress = tipProgress.Progress;
      progress.Event.CurrentTargetProgress = progress.Event.CurrentItemProgress / 10;
      progress.Event.TimeLeft = tipProgress.TimeLeft;
      task.OnProgressChanged(progress.Event);

      if (currentTask.Canceled)
       throw new OperationCanceledException(S._("The task was cancelled."));
     };

    fsManager.EraseClusterTips(VolumeInfo.FromMountpoint(target.Drive),
     method, task.Log, searchProgress, eraseProgress);
   }

   info = info.CreateSubdirectory(Path.GetFileName(
    FileSystem.GenerateRandomFileName(info, 18)));
   try
   {


    if (Eraser.Util.File.IsCompressed(info.FullName))
     Eraser.Util.File.SetCompression(info.FullName, false);


    progress.Event.CurrentTargetStatus = S._("Erasing unused space...");
    progress.Event.CurrentItemName = target.Drive;
    task.OnProgressChanged(progress.Event);
    while (volInfo.AvailableFreeSpace > 0)
    {

     string currFile = FileSystem.GenerateRandomFileName(info, 18);


     using (FileStream stream = new FileStream(currFile, FileMode.CreateNew,
      FileAccess.Write, FileShare.None, 8, FileOptions.WriteThrough))
     {


      long streamLength = Math.Min(ErasureMethod.FreeSpaceFileUnit,
       volInfo.AvailableFreeSpace);



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
        progress.Completed = Math.Min(progress.Total,
         progress.Completed + lastWritten);
        progress.Event.CurrentItemPass = currentPass;
        progress.Event.CurrentItemProgress = progress.Progress;
        if (target.EraseClusterTips)
         progress.Event.CurrentTargetProgress = (float)
          (0.1f + progress.Event.CurrentItemProgress * 0.8f);
        else
         progress.Event.CurrentTargetProgress = (float)
          (progress.Event.CurrentItemProgress * 0.9f);
        progress.Event.TimeLeft = progress.TimeLeft;
        task.OnProgressChanged(progress.Event);

        if (currentTask.Canceled)
         throw new OperationCanceledException(S._("The task was cancelled."));
       }
      );
     }
    }


    progress.Event.CurrentItemName = S._("Old resident file system table files");
    task.OnProgressChanged(progress.Event);
    fsManager.EraseOldFileSystemResidentFiles(volInfo, info, method, null);
   }
   finally
   {

    progress.Event.CurrentTargetStatus = S._("Removing temporary files...");
    task.OnProgressChanged(progress.Event);
    fsManager.DeleteFolder(info);
   }


   progress.Event.CurrentTargetStatus = S._("Erasing unused directory structures...");
   ProgressManager fsEntriesProgress = new ProgressManager();
   fsEntriesProgress.Start();
   fsManager.EraseDirectoryStructures(volInfo,
    delegate(int currentFile, int totalFiles)
    {
     if (currentTask.Canceled)
      throw new OperationCanceledException(S._("The task was cancelled."));


     fsEntriesProgress.Total = totalFiles;
     fsEntriesProgress.Completed = currentFile;


     progress.Event.TimeLeft = fsEntriesProgress.TimeLeft;
     progress.Event.CurrentItemProgress = fsEntriesProgress.Progress;
     progress.Event.CurrentTargetProgress = (float)(
      0.9 + progress.Event.CurrentItemProgress / 10);
     task.OnProgressChanged(progress.Event);
    }
   );
  }






  private delegate void FolderEraseDelegate(DirectoryInfo info);







  private void EraseFilesystemObject(Task task, FileSystemObjectTarget target,
   TaskProgressManager progress)
  {

   long dataTotal = 0;
   List<string> paths = target.GetPaths(out dataTotal);


   ErasureMethod method = target.Method;


   dataTotal = method.CalculateEraseDataSize(paths, dataTotal);


   progress.Event.CurrentTargetStatus = S._("Erasing files...");


   for (int i = 0; i < paths.Count; ++i)
   {

    progress.Event.CurrentTargetProgress = i / (float)paths.Count;
    progress.Event.CurrentTarget = target;
    progress.Event.CurrentItemName = paths[i];
    progress.Event.CurrentItemProgress = 0;
    progress.Event.CurrentTargetTotalPasses = method.Passes;
    task.OnProgressChanged(progress.Event);


    StreamInfo info = new StreamInfo(paths[i]);
    FileSystem fsManager = FileSystemManager.Get(
     VolumeInfo.FromMountpoint(info.DirectoryName));


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

     long itemWritten = 0;
     fsManager.EraseFileSystemObject(info, method,
      delegate(long lastWritten, long totalData, int currentPass)
      {
       dataTotal -= lastWritten;
       progress.Completed += lastWritten;
       progress.Event.CurrentItemPass = currentPass;
       progress.Event.CurrentItemProgress = (float)
        ((itemWritten += lastWritten) / (float)totalData);
       progress.Event.CurrentTargetProgress =
        (i + progress.Event.CurrentItemProgress) /
        (float)paths.Count;
       progress.Event.TimeLeft = progress.TimeLeft;
       task.OnProgressChanged(progress.Event);

       if (currentTask.Canceled)
        throw new OperationCanceledException(S._("The task was cancelled."));
      });


     FileInfo fileInfo = info.File;
     if (fileInfo != null)
      fsManager.DeleteFile(fileInfo);
    }
    catch (UnauthorizedAccessException)
    {
     task.Log.LastSessionEntries.Add(new LogEntry(S._("The file {0} could not " +
      "be erased because the file's permissions prevent access to the file.",
      info.FullName), LogLevel.Error));
    }
    catch (FileLoadException)
    {
     if (!ManagerLibrary.Settings.ForceUnlockLockedFiles)
      throw;

     List<System.Diagnostics.Process> processes = new List<System.Diagnostics.Process>();
     foreach (OpenHandle handle in OpenHandle.Items)
      if (handle.Path == paths[i])
       processes.Add(System.Diagnostics.Process.GetProcessById(handle.ProcessId));

     StringBuilder processStr = new StringBuilder();
     foreach (System.Diagnostics.Process process in processes)
      processStr.AppendFormat(System.Globalization.CultureInfo.InvariantCulture,
       "{0}, ", process.MainModule.FileName);

     task.Log.LastSessionEntries.Add(new LogEntry(S._(
      "Could not force closure of file \"{0}\" (locked by {1})",
      paths[i], processStr.ToString().Remove(processStr.Length - 2)), LogLevel.Error));
    }
    finally
    {

     if (isReadOnly && info.Exists && !info.IsReadOnly)
      info.IsReadOnly = isReadOnly;
    }
   }


   if (target is FolderTarget)
   {
    progress.Event.CurrentTargetStatus = S._("Removing folders...");


    FolderTarget fldr = (FolderTarget)target;
    FileSystem fsManager = FileSystemManager.Get(VolumeInfo.FromMountpoint(fldr.Path));
    FolderEraseDelegate eraseEmptySubFolders = null;
    eraseEmptySubFolders = delegate(DirectoryInfo info)
    {
     foreach (DirectoryInfo subDir in info.GetDirectories())
      eraseEmptySubFolders(subDir);

     progress.Event.CurrentItemName = info.FullName;
     task.OnProgressChanged(progress.Event);

     FileSystemInfo[] files = info.GetFileSystemInfos();
     if (files.Length == 0)
      fsManager.DeleteFolder(info);
    };
    eraseEmptySubFolders(new DirectoryInfo(fldr.Path));

    if (fldr.DeleteIfEmpty)
    {
     DirectoryInfo info = new DirectoryInfo(fldr.Path);
     progress.Event.CurrentItemName = info.FullName;
     task.OnProgressChanged(progress.Event);


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
    progress.Event.CurrentTargetStatus = S._("Emptying recycle bin...");
    task.OnProgressChanged(progress.Event);

    ShellApi.EmptyRecycleBin(EmptyRecycleBinOptions.NoConfirmation |
     EmptyRecycleBinOptions.NoProgressUI | EmptyRecycleBinOptions.NoSound);
   }
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
