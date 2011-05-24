

using System;
using System.Collections.Generic;
using System.Windows.Forms;

using System.IO;
using System.IO.Pipes;
using System.Text;
using System.Threading;
using System.Globalization;
using System.ComponentModel;
using System.Security.Principal;
using System.Security.AccessControl;

using Eraser.Util;

namespace Eraser
{
 internal static partial class Program
 {



  class GuiProgram : IDisposable
  {







   public GuiProgram(string[] commandLine, string instanceID)
   {
    Application.EnableVisualStyles();
    Application.SetCompatibleTextRenderingDefault(false);
    InstanceID = instanceID;
    CommandLine = commandLine;


    bool isFirstInstance = false;
    GlobalMutex = new Mutex(true, instanceID, out isFirstInstance);
    IsFirstInstance = isFirstInstance;
   }


   ~GuiProgram()
   {
    Dispose(false);
   }

   protected virtual void Dispose(bool disposing)
   {
    if (disposing)
     GlobalMutex.Close();
   }

   public void Dispose()
   {
    Dispose(true);
    GC.SuppressFinalize(this);
   }
   public void Run()
   {
    if (IsFirstInstance)
    {
     try
     {
      PipeServer = new Thread(ServerMain);
      PipeServer.Start();
      InitInstanceEventArgs eventArgs = new InitInstanceEventArgs();
      OnInitInstance(this, eventArgs);
      if (MainForm == null)
       return;
      Application.ApplicationExit += OnExitInstance;
      MainForm.FormClosed += OnExitInstance;
      if (eventArgs.ShowMainForm)
       Application.Run(MainForm);
      else
       Application.Run();
     }
     finally
     {
      PipeServer.Abort();
     }
    }
    else
    {
     try
     {
      NamedPipeClientStream client = new NamedPipeClientStream(".", InstanceID,
       PipeDirection.Out);
      client.Connect(500);
      StringBuilder commandLineStr = new StringBuilder(CommandLine.Length * 64);
      foreach (string param in CommandLine)
       commandLineStr.Append(string.Format(
        CultureInfo.InvariantCulture, "{0}\0", param));
      byte[] buffer = new byte[commandLineStr.Length];
      int count = Encoding.UTF8.GetBytes(commandLineStr.ToString(), 0,
       commandLineStr.Length, buffer, 0);
      client.Write(buffer, 0, count);
     }
     catch (UnauthorizedAccessException)
     {
      MessageBox.Show(S._("Another instance of Eraser is already running but it " +
       "is running with higher privileges than this instance of Eraser.\n\n" +
       "Eraser will now exit."), S._("Eraser"), MessageBoxButtons.OK,
       MessageBoxIcon.Information, MessageBoxDefaultButton.Button1,
       S.IsRightToLeft(null) ?
        MessageBoxOptions.RtlReading | MessageBoxOptions.RightAlign : 0);
     }
     catch (IOException ex)
     {
      MessageBox.Show(S._("Another instance of Eraser is already running but " +
       "cannot be connected to.\n\nThe error returned was: {0}", ex.Message,
       S._("Eraser"), MessageBoxButtons.OK, MessageBoxIcon.Error,
       MessageBoxDefaultButton.Button1,
       S.IsRightToLeft(null) ?
        MessageBoxOptions.RtlReading | MessageBoxOptions.RightAlign : 0));
     }
     catch (TimeoutException)
     {
     }
    }
   }
   private struct ServerAsyncInfo
   {
    public NamedPipeServerStream Server;
    public AutoResetEvent WaitHandle;
   }
   private void ServerMain()
   {
    while (PipeServer.ThreadState != System.Threading.ThreadState.AbortRequested)
    {
     PipeSecurity security = new PipeSecurity();
     security.AddAccessRule(new PipeAccessRule(
      WindowsIdentity.GetCurrent().User,
      PipeAccessRights.FullControl,
      AccessControlType.Allow));
     using (NamedPipeServerStream server = new NamedPipeServerStream(InstanceID,
      PipeDirection.In, 1, PipeTransmissionMode.Message, PipeOptions.Asynchronous,
      128, 128, security))
     {
      ServerAsyncInfo async = new ServerAsyncInfo();
      async.Server = server;
      async.WaitHandle = new AutoResetEvent(false);
      IAsyncResult result = server.BeginWaitForConnection(WaitForConnection, async);
      if (result.AsyncWaitHandle.WaitOne())
       async.WaitHandle.WaitOne();
     }
    }
   }
   private void WaitForConnection(IAsyncResult result)
   {
    ServerAsyncInfo async = (ServerAsyncInfo)result.AsyncState;
    try
    {
     async.Server.EndWaitForConnection(result);
     if (async.Server.IsConnected)
     {
      byte[] buffer = new byte[8192];
      string[] commandLine = null;
      string message = string.Empty;
      do
      {
       int lastRead = async.Server.Read(buffer, 0, buffer.Length);
       message += Encoding.UTF8.GetString(buffer, 0, lastRead);
      }
      while (!async.Server.IsMessageComplete);
      OnNextInstance(this, new NextInstanceEventArgs(commandLine, message));
     }
    }
    catch (ObjectDisposedException)
    {
    }
    finally
    {
     async.WaitHandle.Set();
    }
   }
   public string[] CommandLine { get; private set; }
   public bool IsFirstInstance { get; private set; }
   public Form MainForm { get; set; }
   public delegate void InitInstanceEventHandler(object sender, InitInstanceEventArgs e);
   public event InitInstanceEventHandler InitInstance;
   private void OnInitInstance(object sender, InitInstanceEventArgs e)
   {
    if (InitInstance != null)
     InitInstance(sender, e);
   }
   public delegate void NextInstanceEventHandler(object sender, NextInstanceEventArgs e);
   public event NextInstanceEventHandler NextInstance;
   private void OnNextInstance(object sender, NextInstanceEventArgs e)
   {
    if (NextInstance != null)
     NextInstance(sender, e);
   }
   public delegate void ExitInstanceEventHandler(object sender, EventArgs e);
   public event ExitInstanceEventHandler ExitInstance;
   private void OnExitInstance(object sender, EventArgs e)
   {
    if (Exited)
     return;
    Exited = true;
    if (ExitInstance != null)
     ExitInstance(sender, e);
   }
   private string InstanceID;
   private Mutex GlobalMutex;
   private Thread PipeServer;
   private bool Exited;
  }
  class InitInstanceEventArgs : EventArgs
  {
   public InitInstanceEventArgs()
   {
    ShowMainForm = true;
   }
   public bool ShowMainForm { get; set; }
  }
  class NextInstanceEventArgs : EventArgs
  {
   public NextInstanceEventArgs(string[] commandLine, string message)
   {
    CommandLine = commandLine;
    Message = message;
   }
   public string[] CommandLine { get; private set; }
   public string Message { get; private set; }
  }
 }
}
