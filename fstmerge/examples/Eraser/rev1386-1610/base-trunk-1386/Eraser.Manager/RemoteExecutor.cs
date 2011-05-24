

using System;
using System.Text;
using System.IO;
using System.IO.Pipes;
using System.Threading;
using System.Collections.Generic;

using System.Runtime.Serialization;
using System.Runtime.Serialization.Formatters.Binary;
using System.Security.Principal;
using System.Security.AccessControl;

namespace Eraser.Manager
{



 [Serializable]
 internal class RemoteExecutorRequest
 {






  public RemoteExecutorRequest(RemoteExecutorFunction func, params object[] data)
  {
   Func = func;
   Data = data;
  }




  public RemoteExecutorFunction Func { get; set; }




  public object[] Data { get; private set; }
 };




 public enum RemoteExecutorFunction
 {
  QueueTask,
  ScheduleTask,
  UnqueueTask,

  AddTask,
  DeleteTask,

  GetTaskCount,
  GetTask
 }





 public class RemoteExecutorServer : DirectExecutor
 {



  public static readonly string ServerName =
   "Eraser-FB6C5A7D-E47F-475f-ABA4-58F4D24BB67E-RemoteExecutor-" +
   WindowsIdentity.GetCurrent().User.ToString();




  public RemoteExecutorServer()
  {
   thread = new Thread(Main);
   serverLock = new Semaphore(maxServerInstances, maxServerInstances);

   thread.Start();
   Thread.Sleep(0);
  }

  protected override void Dispose(bool disposing)
  {
   if (disposing)
   {

    thread.Abort();


    lock (servers)
     foreach (NamedPipeServerStream server in servers)
      server.Close();



    for (int i = 0; i < maxServerInstances; ++i)
     serverLock.WaitOne();
    serverLock.Close();
   }

   base.Dispose(disposing);
  }




  private void Main()
  {
   while (Thread.CurrentThread.ThreadState != ThreadState.AbortRequested)
   {

    if (!serverLock.WaitOne())
     continue;

    PipeSecurity security = new PipeSecurity();
    security.AddAccessRule(new PipeAccessRule(
     WindowsIdentity.GetCurrent().User,
     PipeAccessRights.FullControl, AccessControlType.Allow));


    NamedPipeServerStream server = new NamedPipeServerStream(ServerName,
     PipeDirection.InOut, maxServerInstances, PipeTransmissionMode.Message,
     PipeOptions.Asynchronous, 128, 128, security);
    server.BeginWaitForConnection(EndWaitForConnection, server);

    lock (servers)
     servers.Add(server);
   }
  }






  private delegate object RequestHandler(object[] arguments);






  private void EndWaitForConnection(IAsyncResult result)
  {
   NamedPipeServerStream server = (NamedPipeServerStream)result.AsyncState;

   try
   {

    server.EndWaitForConnection(result);

    while (server.IsConnected)
    {

     RemoteExecutorRequest request = null;
     using (MemoryStream mstream = new MemoryStream())
     {
      byte[] buffer = new byte[65536];

      do
      {
       int lastRead = server.Read(buffer, 0, buffer.Length);
       mstream.Write(buffer, 0, lastRead);
      }
      while (!server.IsMessageComplete);


      if (!server.IsConnected)
       return;


      mstream.Position = 0;
      try
      {
       request = (RemoteExecutorRequest)new BinaryFormatter().Deserialize(
        new MemoryStream(buffer));
      }
      catch (SerializationException)
      {

       return;
      }
     }


     Dictionary<RemoteExecutorFunction, RequestHandler> functionMap =
      new Dictionary<RemoteExecutorFunction, RequestHandler>();
     functionMap.Add(RemoteExecutorFunction.QueueTask,
      delegate(object[] args) { QueueTask((Task)args[0]); return null; });
     functionMap.Add(RemoteExecutorFunction.ScheduleTask,
      delegate(object[] args) { ScheduleTask((Task)args[0]); return null; });
     functionMap.Add(RemoteExecutorFunction.UnqueueTask,
      delegate(object[] args) { UnqueueTask((Task)args[0]); return null; });

     functionMap.Add(RemoteExecutorFunction.AddTask,
      delegate(object[] args)
      {
       Tasks.Add((Task)args[0]);
       return null;
      });
     functionMap.Add(RemoteExecutorFunction.DeleteTask,
      delegate(object[] args)
      {
       Tasks.Remove((Task)args[0]);
       return null;
      });
     functionMap.Add(RemoteExecutorFunction.GetTaskCount,
      delegate(object[] args) { return Tasks.Count; });
     functionMap.Add(RemoteExecutorFunction.GetTask,
      delegate(object[] args) { return Tasks[(int)args[0]]; });


     object returnValue = functionMap[request.Func](request.Data);


     using (MemoryStream mStream = new MemoryStream())
     {
      if (returnValue != null)
      {
       byte[] header = BitConverter.GetBytes((Int32)1);
       byte[] buffer = null;
       new BinaryFormatter().Serialize(mStream, returnValue);

       server.Write(header, 0, header.Length);
       server.Write(buffer, 0, buffer.Length);
      }
      else
      {
       byte[] header = BitConverter.GetBytes((Int32)0);
       server.Write(header, 0, header.Length);
      }
     }

     server.WaitForPipeDrain();
    }
   }
   catch (OperationCanceledException)
   {
   }
   catch (ObjectDisposedException)
   {
   }
   finally
   {
    lock (servers)
     servers.Remove(server);
    server.Close();
    serverLock.Release();
   }
  }




  private Thread thread;




  private Semaphore serverLock;




  private List<NamedPipeServerStream> servers = new List<NamedPipeServerStream>();




  private const int maxServerInstances = 4;
 }





 public class RemoteExecutorClient : Executor
 {
  public RemoteExecutorClient()
  {
   client = new NamedPipeClientStream(".", RemoteExecutorServer.ServerName,
    PipeDirection.InOut);
   tasks = new RemoteExecutorClientTasksCollection(this);
  }

  protected override void Dispose(bool disposing)
  {
   if (disposing)
   {
    client.Close();
   }

   base.Dispose(disposing);
  }

  public override void Run()
  {
   try
   {
    client.Connect(500);
   }
   catch (TimeoutException)
   {
   }
  }
  internal ReturnType SendRequest<ReturnType>(RemoteExecutorFunction function, params object[] args)
  {
   object result = null;
   using (MemoryStream mStream = new MemoryStream())
   {
    new BinaryFormatter().Serialize(mStream, new RemoteExecutorRequest(function, args));
    byte[] buffer = mStream.ToArray();
    client.Write(buffer, 0, buffer.Length);
    mStream.Position = 0;
    buffer = new byte[65536];
    client.ReadMode = PipeTransmissionMode.Message;
    do
    {
     int lastRead = client.Read(buffer, 0, buffer.Length);
     mStream.Write(buffer, 0, lastRead);
    }
    while (!client.IsMessageComplete);
    if (BitConverter.ToInt32(mStream.ToArray(), 0) == 1)
    {
     mStream.Position = 0;
     do
     {
      int lastRead = client.Read(buffer, 0, buffer.Length);
      mStream.Write(buffer, 0, lastRead);
     }
     while (!client.IsMessageComplete);
     mStream.Position = 0;
     if (mStream.Length > 0)
      result = new BinaryFormatter().Deserialize(mStream);
    }
   }
   return (ReturnType)result;
  }
  public override void QueueTask(Task task)
  {
   SendRequest<object>(RemoteExecutorFunction.QueueTask, task);
  }
  public override void ScheduleTask(Task task)
  {
   SendRequest<object>(RemoteExecutorFunction.ScheduleTask, task);
  }
  public override void UnqueueTask(Task task)
  {
   SendRequest<object>(RemoteExecutorFunction.UnqueueTask, task);
  }
  public override void QueueRestartTasks()
  {
   throw new NotImplementedException();
  }
  internal override bool IsTaskQueued(Task task)
  {
   throw new NotImplementedException();
  }
  public override ExecutorTasksCollection Tasks
  {
   get
   {
    return tasks;
   }
  }
  public bool IsConnected
  {
   get { return client.IsConnected; }
  }
  private RemoteExecutorClientTasksCollection tasks;
  private NamedPipeClientStream client;
  private class RemoteExecutorClientTasksCollection : ExecutorTasksCollection
  {
   public RemoteExecutorClientTasksCollection(RemoteExecutorClient executor)
    : base(executor)
   {
   }
   private ReturnType SendRequest<ReturnType>(RemoteExecutorFunction function, params object[] args)
   {
    RemoteExecutorClient client = (RemoteExecutorClient)Owner;
    return client.SendRequest<ReturnType>(function, args);
   }
   public override int IndexOf(Task item)
   {
    throw new NotSupportedException();
   }
   public override void Insert(int index, Task item)
   {
    throw new NotSupportedException();
   }
   public override void RemoveAt(int index)
   {
    throw new NotSupportedException();
   }
   public override Task this[int index]
   {
    get
    {
     return SendRequest<Task>(RemoteExecutorFunction.GetTask, index);
    }
    set
    {
     throw new NotSupportedException();
    }
   }
   public override void Add(Task item)
   {
    item.Executor = Owner;
    SendRequest<object>(RemoteExecutorFunction.AddTask, item);
    Owner.OnTaskAdded(new TaskEventArgs(item));
   }
   public override void Clear()
   {
    throw new NotSupportedException();
   }
   public override bool Contains(Task item)
   {
    throw new NotSupportedException();
   }
   public override void CopyTo(Task[] array, int arrayIndex)
   {
    throw new NotSupportedException();
   }
   public override int Count
   {
    get { return SendRequest<int>(RemoteExecutorFunction.GetTaskCount); }
   }
   public override bool Remove(Task item)
   {
    item.Cancel();
    item.Executor = null;
    SendRequest<object>(RemoteExecutorFunction.DeleteTask, item);
    Owner.OnTaskDeleted(new TaskEventArgs(item));
    return true;
   }
   public override IEnumerator<Task> GetEnumerator()
   {
    throw new NotSupportedException();
   }
   public override void SaveToStream(Stream stream)
   {
    throw new NotSupportedException();
   }
   public override void LoadFromStream(Stream stream)
   {
    throw new NotSupportedException();
   }
  }
 }
}
