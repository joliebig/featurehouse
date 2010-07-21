

using System;
using System.Collections.Generic;
using System.Windows.Forms;

using System.IO;
using System.IO.Pipes;
using System.Text;
using System.Threading;
using System.Runtime.Serialization;
using System.Globalization;
using System.Reflection;
using System.Diagnostics;
using System.ComponentModel;
using System.Security.Principal;
using System.Security.AccessControl;

using Eraser.Manager;
using Eraser.Util;

namespace Eraser
{
 static class Program
 {



  [STAThread]
  static int Main(string[] commandLine)
  {

   if (commandLine.Length == 0)
    GUIMain(commandLine);



   else if (commandLine.Length == 1)
   {
    if (commandLine[0] == "--atRestart" || commandLine[0] == "--quiet")
    {
     GUIMain(commandLine);
    }
    else
    {
     return CommandMain(commandLine);
    }
   }


   else
    return CommandMain(commandLine);


   return 0;
  }





  private static int CommandMain(string[] commandLine)
  {

   bool isQuiet = false;

   try
   {
    CommandLineProgram program = new CommandLineProgram(commandLine);
    isQuiet = program.Arguments.Quiet;

    using (ManagerLibrary library = new ManagerLibrary(new Settings()))
     program.Run();

    return 0;
   }
   catch (UnauthorizedAccessException)
   {
    return 5;
   }
   catch (Win32Exception e)
   {
    Console.WriteLine(e.Message);
    return e.ErrorCode;
   }
   catch (Exception e)
   {
    Console.WriteLine(e.Message);
    return 1;
   }
   finally
   {

    Console.Out.Flush();


    if (!isQuiet)
    {
     Console.Write("\nPress enter to continue . . . ");
     Console.Out.Flush();
     Console.ReadLine();
    }

    KernelApi.FreeConsole();
   }
  }





  private static void GUIMain(string[] commandLine)
  {

   using (GUIProgram program = new GUIProgram(commandLine, "Eraser-BAD0DAC6-C9EE-4acc-" +
    "8701-C9B3C64BC65E-GUI-" +
    System.Security.Principal.WindowsIdentity.GetCurrent().User.ToString()))


   using (ManagerLibrary library = new ManagerLibrary(new Settings()))
   {
    program.InitInstance += OnGUIInitInstance;
    program.NextInstance += OnGUINextInstance;
    program.ExitInstance += OnGUIExitInstance;
    program.Run();
   }
  }






  private static bool OnGUIInitInstance(object sender)
  {
   GUIProgram program = (GUIProgram)sender;
   eraserClient = new RemoteExecutorServer();


   EraserSettings settings = EraserSettings.Get();
   System.Threading.Thread.CurrentThread.CurrentUICulture =
    new CultureInfo(settings.Language);
   Application.SafeTopLevelCaptionFormat = S._("Eraser");


   SettingsCompatibility.Execute();
   try
   {
    if (System.IO.File.Exists(TaskListPath))
    {
     using (FileStream stream = new FileStream(TaskListPath, FileMode.Open,
      FileAccess.Read, FileShare.Read))
     {
      eraserClient.Tasks.LoadFromStream(stream);
     }
    }
   }
   catch (SerializationException e)
   {
    System.IO.File.Delete(TaskListPath);
    MessageBox.Show(S._("Could not load task list. All task entries have " +
     "been lost. The error returned was: {0}", e.Message), S._("Eraser"),
     MessageBoxButtons.OK, MessageBoxIcon.Error,
     MessageBoxDefaultButton.Button1,
     S.IsRightToLeft(null) ? MessageBoxOptions.RtlReading : 0);
   }


   program.MainForm = new MainForm();
   program.MainForm.CreateControl();
   bool showMainForm = true;
   foreach (string param in program.CommandLine)
   {

    switch (param)
    {
     case "--atRestart":
      eraserClient.QueueRestartTasks();
      goto case "--quiet";



     case "--quiet":
      showMainForm = false;
      break;
    }
   }


   eraserClient.Run();
   return showMainForm;
  }






  private static void OnGUINextInstance(object sender, string message)
  {


   GUIProgram program = (GUIProgram)sender;


   if (program.MainForm.InvokeRequired)
   {
    program.MainForm.Invoke(new GUIProgram.NextInstanceFunction(
     OnGUINextInstance), new object[] { sender, message });
    return;
   }

   program.MainForm.Visible = true;
  }





  private static void OnGUIExitInstance(object sender)
  {

   if (!Directory.Exists(Program.AppDataPath))
    Directory.CreateDirectory(Program.AppDataPath);
   using (FileStream stream = new FileStream(TaskListPath, FileMode.Create,
    FileAccess.Write, FileShare.None))
   {
    eraserClient.Tasks.SaveToStream(stream);
   }


   eraserClient.Dispose();
  }




  public static Executor eraserClient;




  public static readonly string AppDataPath = Path.Combine(Environment.GetFolderPath(
   Environment.SpecialFolder.LocalApplicationData), @"Eraser 6");




  private const string TaskListFileName = @"Task List.ersx";




  public static readonly string TaskListPath = Path.Combine(AppDataPath, TaskListFileName);




  public const string SettingsPath = @"SOFTWARE\Eraser\Eraser 6";
 }

 class GUIProgram : IDisposable
 {







  public GUIProgram(string[] commandLine, string instanceID)
  {
   Application.EnableVisualStyles();
   Application.SetCompatibleTextRenderingDefault(false);
   this.instanceID = instanceID;
   this.CommandLine = commandLine;


   globalMutex = new Mutex(true, instanceID, out isFirstInstance);
  }

  ~GUIProgram()
  {
   Dispose(false);
  }

  protected virtual void Dispose(bool disposing)
  {
   if (disposing)
    globalMutex.Close();
  }

  public void Dispose()
  {
   Dispose(true);
   GC.SuppressFinalize(this);
  }
  public bool Run()
  {
   if (IsFirstInstance)
   {
    try
    {
     pipeServer = new Thread(ServerMain);
     pipeServer.Start();
     bool ShowMainForm = OnInitInstance(this);
     if (MainForm == null)
      return false;
     Application.ApplicationExit += OnExitInstance;
     MainForm.FormClosed += OnExitInstance;
     if (ShowMainForm)
      Application.Run(MainForm);
     else
      Application.Run();
     return true;
    }
    finally
    {
     pipeServer.Abort();
    }
   }
   else
   {
    try
    {
     NamedPipeClientStream client = new NamedPipeClientStream(".", instanceID,
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
     MessageBox.Show(S._("Another instance of Eraser is already running but it is " +
      "running with higher privileges than this instance of Eraser.\n\n" +
      "Eraser will now exit."), S._("Eraser"), MessageBoxButtons.OK,
      MessageBoxIcon.Information, MessageBoxDefaultButton.Button1,
      S.IsRightToLeft(null) ? MessageBoxOptions.RtlReading : 0);
    }
    catch (TimeoutException)
    {
    }
    return false;
   }
  }
  private struct ServerAsyncInfo
  {
   public NamedPipeServerStream Server;
   public AutoResetEvent WaitHandle;
  }
  private void ServerMain()
  {
   while (pipeServer.ThreadState != System.Threading.ThreadState.AbortRequested)
   {
    PipeSecurity security = new PipeSecurity();
    security.AddAccessRule(new PipeAccessRule(
     WindowsIdentity.GetCurrent().User,
     PipeAccessRights.FullControl, AccessControlType.Allow));
    using (NamedPipeServerStream server = new NamedPipeServerStream(instanceID,
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
     string message = string.Empty;
     do
     {
      int lastRead = async.Server.Read(buffer, 0, buffer.Length);
      message += Encoding.UTF8.GetString(buffer, 0, lastRead);
     }
     while (!async.Server.IsMessageComplete);
     OnNextInstance(this, message);
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
  public bool IsFirstInstance
  {
   get
   {
    return isFirstInstance;
   }
  }
  public Form MainForm { get; set; }
  public delegate bool InitInstanceFunction(object sender);
  public event InitInstanceFunction InitInstance;
  private bool OnInitInstance(object sender)
  {
   if (InitInstance != null)
    return InitInstance(sender);
   return true;
  }
  public delegate void NextInstanceFunction(object sender, string message);
  public event NextInstanceFunction NextInstance;
  private void OnNextInstance(object sender, string message)
  {
   if (NextInstance != null)
    NextInstance(sender, message);
  }
  public delegate void ExitInstanceFunction(object sender);
  public event ExitInstanceFunction ExitInstance;
  private void OnExitInstance(object sender, EventArgs e)
  {
   if (Exited)
    return;
   Exited = true;
   if (ExitInstance != null)
    ExitInstance(sender);
  }
  private bool Exited;
  private string instanceID;
  private Mutex globalMutex;
  private Thread pipeServer;
  private bool isFirstInstance;
 }
 class CommandLineProgram
 {
  public abstract class CommandLine
  {
   public static CommandLine Get(string[] cmdParams)
   {
    if (cmdParams.Length < 1)
     throw new ArgumentException("An action must be specified.");
    string action = cmdParams[0];
    CommandLine result = null;
    switch (action)
    {
     case "help":
      result = new HelpCommandLine();
      break;
     case "querymethods":
      result = new QueryMethodsCommandLine();
      break;
     case "importtasklist":
      result = new ImportTaskListCommandLine();
      break;
     case "addtask":
      result = new AddTaskCommandLine();
      break;
    }
    if (result != null)
    {
     result.Parse(cmdParams);
     return result;
    }
    else
     throw new ArgumentException(string.Format(CultureInfo.CurrentCulture,
      "Unknown action: {0}", action));
   }
   protected CommandLine()
   {
   }
   public bool Parse(string[] cmdParams)
   {
    for (int i = 1; i != cmdParams.Length; ++i)
    {
     if (IsParam(cmdParams[i], "quiet", "q"))
      Quiet = true;
     else if (!ResolveParameter(cmdParams[i]))
      throw new ArgumentException("Unknown argument: " + cmdParams[i]);
    }
    return true;
   }
   protected virtual bool ResolveParameter(string param)
   {
    return false;
   }
   protected static bool IsParam(string parameter, string expectedParameter,
    string shortParameter)
   {
    if (parameter.Length < 1)
     return false;
    {
     int equalPos = parameter.IndexOf('=');
     if (equalPos != -1)
      parameter = parameter.Substring(0, equalPos);
    }
    switch (parameter[0])
    {
     case '-':
      if (parameter.Length < 2)
       return false;
      if (parameter[1] == '-')
       return parameter.Substring(2) == expectedParameter;
      else if (string.IsNullOrEmpty(shortParameter))
       return parameter.Substring(1) == expectedParameter;
      else
       return parameter.Substring(1) == shortParameter;
     case '/':
      parameter = parameter.Substring(1);
      return parameter == expectedParameter || (
       !string.IsNullOrEmpty(shortParameter) && parameter == shortParameter
      );
     default:
      return false;
    }
   }
   protected static List<KeyValuePair<string, string> > GetSubParameters(string param)
   {
    List<KeyValuePair<string, string> > result =
     new List<KeyValuePair<string, string> >();
    int lastPos = 0;
    int commaPos = (param += ',').IndexOf(',');
    while (commaPos != -1)
    {
     if (commaPos == 0 ||
      (commaPos >= 1 && param[commaPos - 1] != '\\') ||
      (commaPos >= 2 && param[commaPos - 2] == '\\'))
     {
      string subParam = param.Substring(lastPos, commaPos - lastPos);
      int equalPos = -1;
      do
      {
       equalPos = subParam.IndexOf('=', equalPos + 1);
       if (equalPos == -1)
       {
        result.Add(new KeyValuePair<string, string>(
         UnescapeCommandLine(subParam), null));
       }
       else if (equalPos == 0 ||
        (equalPos >= 1 && subParam[equalPos - 1] != '\\') ||
        (equalPos >= 2 && subParam[equalPos - 2] == '\\'))
       {
        result.Add(new KeyValuePair<string, string>(
         UnescapeCommandLine(subParam.Substring(0, equalPos)),
         UnescapeCommandLine(subParam.Substring(equalPos + 1))));
        break;
       }
      }
      while (equalPos != -1);
      lastPos = ++commaPos;
     }
     else
      ++commaPos;
     commaPos = param.IndexOf(',', commaPos);
    }
    return result;
   }
   private static string UnescapeCommandLine(string param)
   {
    StringBuilder result = new StringBuilder(param.Length);
    for (int i = 0; i < param.Length; ++i)
     if (param[i] == '\\' && i < param.Length - 1)
      result.Append(param[++i]);
     else
      result.Append(param[i]);
    return result.ToString();
   }
   public bool Quiet { get; private set; }
  }
  class HelpCommandLine : CommandLine
  {
   public HelpCommandLine()
   {
   }
  }
  class QueryMethodsCommandLine : CommandLine
  {
   public QueryMethodsCommandLine()
   {
   }
  }
  class AddTaskCommandLine : CommandLine
  {
   public AddTaskCommandLine()
   {
    Schedule = Schedule.RunNow;
    Targets = new List<ErasureTarget>();
   }
   protected override bool ResolveParameter(string param)
   {
    int equalPos = param.IndexOf('=');
    if (IsParam(param, "method", "m"))
    {
     if (equalPos == -1)
      throw new ArgumentException("--method must be specified with an Erasure " +
       "method GUID.");
     List<KeyValuePair<string, string> > subParams =
      GetSubParameters(param.Substring(equalPos + 1));
     ErasureMethod = new Guid(subParams[0].Key);
    }
    else if (IsParam(param, "schedule", "s"))
    {
     if (equalPos == -1)
      throw new ArgumentException("--schedule must be specified with a Schedule " +
       "type.");
     List<KeyValuePair<string, string> > subParams =
      GetSubParameters(param.Substring(equalPos + 1));
     switch (subParams[0].Key)
     {
      case "now":
       Schedule = Schedule.RunNow;
       break;
      case "manually":
       Schedule = Schedule.RunManually;
       break;
      case "restart":
       Schedule = Schedule.RunOnRestart;
       break;
      default:
       throw new ArgumentException("Unknown schedule type: " + subParams[0].Key);
     }
    }
    else if (IsParam(param, "recycled", "r"))
    {
     Targets.Add(new RecycleBinTarget());
    }
    else if (IsParam(param, "unused", "u"))
    {
     if (equalPos == -1)
      throw new ArgumentException("--unused must be specified with the Volume " +
       "to erase.");
     UnusedSpaceTarget target = new UnusedSpaceTarget();
     target.EraseClusterTips = false;
     List<KeyValuePair<string, string> > subParams =
      GetSubParameters(param.Substring(equalPos + 1));
     foreach (KeyValuePair<string, string> kvp in subParams)
      if (kvp.Value == null && target.Drive == null)
       target.Drive = Path.GetFullPath(kvp.Key);
      else if (kvp.Key == "clusterTips")
       target.EraseClusterTips = true;
      else
       throw new ArgumentException("Unknown subparameter: " + kvp.Key);
     Targets.Add(target);
    }
    else if (IsParam(param, "dir", "d") || IsParam(param, "directory", null))
    {
     if (equalPos == -1)
      throw new ArgumentException("--directory must be specified with the " +
       "directory to erase.");
     FolderTarget target = new FolderTarget();
     List<KeyValuePair<string, string> > subParams =
      GetSubParameters(param.Substring(equalPos + 1));
     foreach (KeyValuePair<string, string> kvp in subParams)
      if (kvp.Value == null && target.Path == null)
       target.Path = Path.GetFullPath(kvp.Key);
      else if (kvp.Key == "excludeMask")
      {
       if (kvp.Value == null)
        throw new ArgumentException("The exclude mask must be specified " +
         "if the excludeMask subparameter is specified");
       target.ExcludeMask = kvp.Value;
      }
      else if (kvp.Key == "includeMask")
      {
       if (kvp.Value == null)
        throw new ArgumentException("The include mask must be specified " +
         "if the includeMask subparameter is specified");
       target.IncludeMask = kvp.Value;
      }
      else if (kvp.Key == "delete")
       target.DeleteIfEmpty = true;
      else
       throw new ArgumentException("Unknown subparameter: " + kvp.Key);
     Targets.Add(target);
    }
    else if (IsParam(param, "file", "f"))
    {
     if (equalPos == -1)
      throw new ArgumentException("--file must be specified with the " +
       "file to erase.");
     FileTarget target = new FileTarget();
     List<KeyValuePair<string, string> > subParams =
      GetSubParameters(param.Substring(equalPos + 1));
     foreach (KeyValuePair<string, string> kvp in subParams)
      if (kvp.Value == null && target.Path == null)
       target.Path = Path.GetFullPath(kvp.Key);
      else
       throw new ArgumentException("Unknown subparameter: " + kvp.Key);
     Targets.Add(target);
    }
    else
     return false;
    return true;
   }
   public Guid ErasureMethod { get; private set; }
   public Schedule Schedule { get; private set; }
   public List<ErasureTarget> Targets { get; private set; }
  }
  class ImportTaskListCommandLine : CommandLine
  {
   public ImportTaskListCommandLine()
   {
   }
   protected override bool ResolveParameter(string param)
   {
    if (!System.IO.File.Exists(param))
     throw new ArgumentException(string.Format(CultureInfo.CurrentCulture,
      "The file {0} does not exist.", param));
    files.Add(param);
    return true;
   }
   public ICollection<string> Files
   {
    get
    {
     return files.AsReadOnly();
    }
   }
   private List<string> files = new List<string>();
  }
  public CommandLineProgram(string[] cmdParams)
  {
   try
   {
    Arguments = CommandLine.Get(cmdParams);
    if (!Arguments.Quiet)
     CreateConsole();
    actionHandlers.Add(typeof(AddTaskCommandLine), AddTask);
    actionHandlers.Add(typeof(ImportTaskListCommandLine), ImportTaskList);
    actionHandlers.Add(typeof(QueryMethodsCommandLine), QueryMethods);
    actionHandlers.Add(typeof(HelpCommandLine), Help);
   }
   finally
   {
    if (Arguments == null || !Arguments.Quiet)
     CreateConsole();
   }
  }
  public void Run()
  {
   actionHandlers[Arguments.GetType()]();
  }
  private static void CreateConsole()
  {
   if (KernelApi.AllocConsole())
   {
    Console.SetOut(new StreamWriter(Console.OpenStandardOutput()));
    Console.SetIn(new StreamReader(Console.OpenStandardInput()));
   }
  }
  private static void CommandUsage()
  {
   Console.WriteLine(@"usage: Eraser <action> <arguments>
where action is
    help Show this help message.
    addtask Adds tasks to the current task list.
    querymethods Lists all registered Erasure methods.
global parameters:
    --quiet, -q Do not create a Console window to display progress.
parameters for help:
    eraser help
    no parameters to set.
parameters for addtask:
    eraser addtask [--method=<methodGUID>] [--schedule=(now|manually|restart)] (--recycled " +
@"| --unused=<volume> | --dir=<directory> | --file=<file>)[...]
    --method, -m The Erasure method to use.
    --schedule, -s The schedule the task will follow. The value must
                            be one of:
            now The task will be queued for immediate execution.
            manually The task will be created but not queued for execution.
            restart The task will be queued for execution when the
                            computer is next restarted.
                            This parameter defaults to now.
    --recycled, -r Erases files and folders in the recycle bin
    --unused, -u Erases unused space in the volume.
        optional arguments: --unused=<drive>[,clusterTips]
            clusterTips If specified, the drive's files will have their
                            cluster tips erased.
    --dir, --directory, -d Erases files and folders in the directory
        optional arguments: --dir=<directory>[,e=excludeMask][,i=includeMask][,delete]
            excludeMask A wildcard expression for files and folders to
                            exclude.
            includeMask A wildcard expression for files and folders to
                            include.
                            The include mask is applied before the exclude
                            mask.
            delete Deletes the folder at the end of the erasure if
                            specified.
    --file, -f Erases the specified file
parameters for querymethods:
    eraser querymethods
    no parameters to set.
All arguments are case sensitive.");
   Console.Out.Flush();
  }
  public CommandLine Arguments { get; private set; }
  private void Help()
  {
   Console.WriteLine(@"Eraser {0}
(c) 2008 The Eraser Project
Eraser is Open-Source Software: see http:
", Assembly.GetExecutingAssembly().GetName().Version);
   Console.Out.Flush();
   CommandUsage();
  }
  private void QueryMethods()
  {
   const string methodFormat = "{0,-2} {1,-39} {2}";
   Console.WriteLine(methodFormat, "", "Method", "GUID");
   Console.WriteLine(new string('-', 79));
   Dictionary<Guid, ErasureMethod> methods = ErasureMethodManager.Items;
   foreach (ErasureMethod method in methods.Values)
   {
    Console.WriteLine(methodFormat, (method is UnusedSpaceErasureMethod) ?
     "U" : "", method.Name, method.Guid.ToString());
   }
  }
  private void AddTask()
  {
   AddTaskCommandLine taskArgs = (AddTaskCommandLine)Arguments;
   Task task = new Task();
   ErasureMethod method = taskArgs.ErasureMethod == Guid.Empty ?
    ErasureMethodManager.Default :
    ErasureMethodManager.GetInstance(taskArgs.ErasureMethod);
   foreach (ErasureTarget target in taskArgs.Targets)
   {
    target.Method = method;
    task.Targets.Add(target);
   }
   if (task.Targets.Count == 0)
    throw new ArgumentException("Tasks must contain at least one erasure target.");
   task.Schedule = taskArgs.Schedule;
   try
   {
    using (RemoteExecutorClient client = new RemoteExecutorClient())
    {
     client.Run();
     if (!client.IsConnected)
     {
      Process eraserInstance = Process.Start(
       Assembly.GetExecutingAssembly().Location, "--quiet");
      eraserInstance.WaitForInputIdle();
      client.Run();
      if (!client.IsConnected)
       throw new IOException("Eraser cannot connect to the running " +
        "instance for erasures.");
     }
     client.Tasks.Add(task);
    }
   }
   catch (UnauthorizedAccessException e)
   {
    throw new UnauthorizedAccessException("Another instance of Eraser " +
     "is already running but it is running with higher privileges than " +
     "this instance of Eraser. Tasks cannot be added in this manner.\n\n" +
     "Close the running instance of Eraser and start it again without " +
     "administrator privileges, or run the command again as an " +
     "administrator.", e);
   }
  }
  private void ImportTaskList()
  {
   ImportTaskListCommandLine cmdLine = (ImportTaskListCommandLine)Arguments;
   try
   {
    using (RemoteExecutorClient client = new RemoteExecutorClient())
    {
     client.Run();
     if (!client.IsConnected)
     {
      Process eraserInstance = Process.Start(
       Assembly.GetExecutingAssembly().Location, "--quiet");
      eraserInstance.WaitForInputIdle();
      client.Run();
      if (!client.IsConnected)
       throw new IOException("Eraser cannot connect to the running " +
        "instance for erasures.");
     }
     foreach (string path in cmdLine.Files)
      using (FileStream stream = new FileStream(path, FileMode.Open, FileAccess.Read))
       client.Tasks.LoadFromStream(stream);
    }
   }
   catch (UnauthorizedAccessException e)
   {
    throw new UnauthorizedAccessException("Another instance of Eraser " +
     "is already running but it is running with higher privileges than " +
     "this instance of Eraser. Tasks cannot be added in this manner.\n\n" +
     "Close the running instance of Eraser and start it again without " +
     "administrator privileges, or run the command again as an " +
     "administrator.", e);
   }
  }
  private delegate void ActionHandler();
  private Dictionary<Type, ActionHandler> actionHandlers =
   new Dictionary<Type, ActionHandler>();
 }
}
