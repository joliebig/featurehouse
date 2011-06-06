using System;
using System.Collections.Generic;
using System.Windows.Forms;
using System.IO;
using System.Threading;
using System.Globalization;
using System.ComponentModel;
using System.Runtime.Serialization;
using System.Security.Principal;
using System.Text.RegularExpressions;
using System.Reflection;
using System.Diagnostics;
using ComLib.Arguments;
using Eraser.Manager;
using Eraser.Util;
namespace Eraser
{
 internal static partial class Program
 {
  class Arguments
  {
   [Arg("quiet", "The program should not be started with any user-visible interfaces. " +
    "Errors will be silently ignored.", typeof(bool), false, false, null)]
   public bool Quiet { get; set; }
  }
  class GuiArguments : Arguments
  {
   [Arg("atRestart", "The program should queue all tasks scheduled for running at " +
    "the system restart.", typeof(bool), false, false, null)]
   public bool AtRestart { get; set; }
  }
  class ConsoleArguments : Arguments
  {
   [Arg(0, "The action this command line is stating.", typeof(string), true, null, null)]
   public string Action { get; set; }
   public List<string> PositionalArguments { get; set; }
  }
  class AddTaskArguments : ConsoleArguments
  {
   [Arg("method", "The erasure method to use", typeof(Guid), false, null, null)]
   public Guid ErasureMethod { get; set; }
   [Arg("schedule", "The schedule to use", typeof(Schedule), true, null, null)]
   public string Schedule { get; set; }
  }
  [STAThread]
  static int Main(string[] commandLine)
  {
   BlackBox blackBox = BlackBox.Get();
   ComLib.BoolMessageItem argumentParser = Args.Parse(commandLine,
    CommandLinePrefixes, CommandLineSeparators);
   Args parsedArguments = (Args)argumentParser.Item;
   if (!argumentParser.Success || parsedArguments.IsEmpty ||
    parsedArguments.Positional.Count == 0 ||
    parsedArguments.Positional[0] != parsedArguments.Raw[0])
   {
    GUIMain(commandLine);
   }
   else
   {
    return CommandMain(commandLine);
   }
   return 0;
  }
  private static int CommandMain(string[] commandLine)
  {
   using (ConsoleProgram program = new ConsoleProgram(commandLine))
   using (ManagerLibrary library = new ManagerLibrary(new Settings()))
    try
    {
     program.Handlers.Add("help",
      new ConsoleActionData(CommandHelp, new ConsoleArguments()));
     program.Handlers.Add("querymethods",
      new ConsoleActionData(CommandQueryMethods, new ConsoleArguments()));
     program.Handlers.Add("addtask",
      new ConsoleActionData(CommandAddTask, new AddTaskArguments()));
     program.Handlers.Add("importtasklist",
      new ConsoleActionData(CommandImportTaskList, new ConsoleArguments()));
     program.Run();
     return 0;
    }
    catch (UnauthorizedAccessException)
    {
     return Win32ErrorCode.AccessDenied;
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
  }
  private static void PrintCommandHelp()
  {
   Console.WriteLine(@"usage: Eraser <action> <arguments>
where action is
  help Show this help message.
  addtask Adds tasks to the current task list.
  querymethods Lists all registered Erasure methods.
  importtasklist Imports an Eraser Task list to the current user's Task
                      List.
global parameters:
  --quiet, -q Do not create a Console window to display progress.
parameters for help:
  eraser help
  no parameters to set.
parameters for addtask:
  eraser addtask [--method=<methodGUID>] [--schedule=(now|manually|restart)] (recyclebin | unused=<volume> | dir=<directory> | file=<file>)[...]
  --method The Erasure method to use.
  --schedule The schedule the task will follow. The value must be one
                      of:
      now The task will be queued for immediate execution.
      manually The task will be created but not queued for execution.
      restart The task will be queued for execution when the computer
                      is next restarted.
  recyclebin Erases files and folders in the recycle bin
  unused Erases unused space in the volume.
    optional arguments: unused=<drive>[,clusterTips[=(true|false)]]
      clusterTips If specified, the drive's files will have their
                      cluster tips erased. This parameter accepts a Boolean
                      value (true/false) as an argument; if none is specified
                      true is assumed.
  dir Erases files and folders in the directory
    optional arguments: dir=<directory>[,-excludeMask][,+includeMask][,deleteIfEmpty]
      excludeMask A wildcard expression for files and folders to
                      exclude.
      includeMask A wildcard expression for files and folders to
                      include.
                      The include mask is applied before the exclude mask.
      deleteIfEmpty Deletes the folder at the end of the erasure if it is
                      empty.
  file Erases the specified file
parameters for querymethods:
  eraser querymethods
  no parameters to set.
parameters for importtasklist:
  eraser importtasklist (file)[...]
  [file] A list of one or more files to import.
All arguments are case sensitive.");
   Console.Out.Flush();
  }
  private static void CommandHelp(ConsoleArguments arguments)
  {
   Console.WriteLine(@"Eraser {0}
(c) 2008-2010 The Eraser Project
Eraser is Open-Source Software: see http:
", Assembly.GetExecutingAssembly().GetName().Version);
   PrintCommandHelp();
  }
  private static void CommandQueryMethods(ConsoleArguments arguments)
  {
   const string methodFormat = "{0,-2} {1,-39} {2}";
   Console.WriteLine(methodFormat, "", "Method", "GUID");
   Console.WriteLine(new string('-', 79));
   foreach (ErasureMethod method in ManagerLibrary.Instance.ErasureMethodRegistrar)
   {
    Console.WriteLine(methodFormat, (method is UnusedSpaceErasureMethod) ?
     "U" : "", method.Name, method.Guid.ToString());
   }
  }
  private static void CommandAddTask(ConsoleArguments arg)
  {
   AddTaskArguments arguments = (AddTaskArguments)arg;
   Task task = new Task();
   ErasureMethod method = arguments.ErasureMethod == Guid.Empty ?
    ErasureMethodRegistrar.Default :
    ManagerLibrary.Instance.ErasureMethodRegistrar[arguments.ErasureMethod];
   switch (arguments.Schedule.ToUpperInvariant())
   {
    case "NOW":
     task.Schedule = Schedule.RunNow;
     break;
    case "MANUALLY":
     task.Schedule = Schedule.RunManually;
     break;
    case "RESTART":
     task.Schedule = Schedule.RunOnRestart;
     break;
    default:
     throw new ArgumentException(string.Format(CultureInfo.CurrentCulture,
      "Unknown schedule type: {0}", arguments.Schedule), "--schedule");
   }
   List<string> trueValues = new List<string>(new string[] { "yes", "true" });
   string[] strings = new string[] {
    "(?<recycleBin>recyclebin)",
    "unused=(?<unusedVolume>.*)(?<unusedTips>,clusterTips(=(?<unusedTipsValue>true|false))?)?",
    "dir=(?<directoryName>.*)(?<directoryParams>(?<directoryExcludeMask>,-[^,]+)|(?<directoryIncludeMask>,\\+[^,]+)|(?<directoryDeleteIfEmpty>,deleteIfEmpty(=(?<directoryDeleteIfEmptyValue>true|false))?))*",
    "file=(?<fileName>.*)"
   };
   Regex regex = new Regex(string.Join("|", strings),
    RegexOptions.Compiled | RegexOptions.IgnoreCase | RegexOptions.RightToLeft);
   foreach (string argument in arguments.PositionalArguments)
   {
    Match match = regex.Match(argument);
    if (match.Captures.Count == 0)
    {
     Console.WriteLine("Unknown argument: {0}, skipped.", argument);
     continue;
    }
    ErasureTarget target = null;
    if (match.Groups["recycleBin"].Success)
    {
     target = new RecycleBinTarget();
    }
    else if (match.Groups["unusedVolume"].Success)
    {
     UnusedSpaceTarget unusedSpaceTarget = new UnusedSpaceTarget();
     target = unusedSpaceTarget;
     unusedSpaceTarget.Drive = match.Groups["unusedVolume"].Value;
     if (!match.Groups["unusedTips"].Success)
      unusedSpaceTarget.EraseClusterTips = false;
     else if (!match.Groups["unusedTipsValue"].Success)
      unusedSpaceTarget.EraseClusterTips = true;
     else
      unusedSpaceTarget.EraseClusterTips =
       trueValues.IndexOf(match.Groups["unusedTipsValue"].Value) != -1;
    }
    else if (match.Groups["directoryName"].Success)
    {
     FolderTarget folderTarget = new FolderTarget();
     target = folderTarget;
     folderTarget.Path = match.Groups["directoryName"].Value;
     if (!match.Groups["directoryDeleteIfEmpty"].Success)
      folderTarget.DeleteIfEmpty = false;
     else if (!match.Groups["directoryDeleteIfEmptyValue"].Success)
      folderTarget.DeleteIfEmpty = true;
     else
      folderTarget.DeleteIfEmpty =
       trueValues.IndexOf(match.Groups["directoryDeleteIfEmptyValue"].Value) != -1;
     if (match.Groups["directoryExcludeMask"].Success)
      folderTarget.ExcludeMask += match.Groups["directoryExcludeMask"].Value.Remove(0, 2) + ' ';
     if (match.Groups["directoryIncludeMask"].Success)
      folderTarget.IncludeMask += match.Groups["directoryIncludeMask"].Value.Remove(0, 2) + ' ';
    }
    else if (match.Groups["fileName"].Success)
    {
     FileTarget fileTarget = new FileTarget();
     target = fileTarget;
     fileTarget.Path = match.Groups["fileName"].Value;
    }
    if (target == null)
     continue;
    target.Method = method;
    task.Targets.Add(target);
   }
   if (task.Targets.Count == 0)
    throw new ArgumentException("Tasks must contain at least one erasure target.");
   try
   {
    using (RemoteExecutorClient client = new RemoteExecutorClient())
    {
     client.Run();
     if (!client.IsConnected)
     {
      Process eraserInstance = Process.Start(
       Assembly.GetExecutingAssembly().Location, "--quiet");
      Thread.Sleep(0);
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
  private static void CommandImportTaskList(ConsoleArguments args)
  {
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
     foreach (string path in args.PositionalArguments)
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
  private static void GUIMain(string[] commandLine)
  {
   string instanceId = "Eraser-BAD0DAC6-C9EE-4acc-8701-C9B3C64BC65E-GUI-" +
    WindowsIdentity.GetCurrent().User.ToString();
   using (GuiProgram program = new GuiProgram(commandLine, instanceId))
   using (ManagerLibrary library = new ManagerLibrary(new Settings()))
   {
    program.InitInstance += OnGUIInitInstance;
    program.NextInstance += OnGUINextInstance;
    program.ExitInstance += OnGUIExitInstance;
    program.Run();
   }
  }
  private static void OnGUIInitInstance(object sender, InitInstanceEventArgs e)
  {
   GuiProgram program = (GuiProgram)sender;
   eraserClient = new RemoteExecutorServer();
   EraserSettings settings = EraserSettings.Get();
   Thread.CurrentThread.CurrentUICulture = new CultureInfo(settings.Language);
   Application.SafeTopLevelCaptionFormat = S._("Eraser");
   Application.Idle += OnGUIIdle;
   try
   {
    if (File.Exists(TaskListPath))
    {
     using (FileStream stream = new FileStream(TaskListPath, FileMode.Open,
      FileAccess.Read, FileShare.Read))
     {
      eraserClient.Tasks.LoadFromStream(stream);
     }
    }
   }
   catch (SerializationException ex)
   {
    File.Delete(TaskListPath);
    MessageBox.Show(S._("Could not load task list. All task entries have " +
     "been lost. The error returned was: {0}", ex.Message), S._("Eraser"),
     MessageBoxButtons.OK, MessageBoxIcon.Error,
     MessageBoxDefaultButton.Button1,
     Localisation.IsRightToLeft(null) ?
      MessageBoxOptions.RtlReading | MessageBoxOptions.RightAlign : 0);
   }
   program.MainForm = new MainForm();
   GuiArguments arguments = new GuiArguments();
   Args.Parse(program.CommandLine, CommandLinePrefixes, CommandLineSeparators, arguments);
   e.ShowMainForm = !arguments.AtRestart && !arguments.Quiet;
   if (arguments.AtRestart)
    eraserClient.QueueRestartTasks();
   eraserClient.Run();
  }
  private static void OnGUIIdle(object sender, EventArgs e)
  {
   Application.Idle -= OnGUIIdle;
   BlackBox blackBox = BlackBox.Get();
   bool allSubmitted = true;
   foreach (BlackBoxReport report in blackBox.GetDumps())
    if (!report.Submitted)
    {
     allSubmitted = false;
     break;
    }
   if (allSubmitted)
    return;
   BlackBoxMainForm form = new BlackBoxMainForm();
   form.Show();
  }
  private static void OnGUINextInstance(object sender, NextInstanceEventArgs e)
  {
   GuiProgram program = (GuiProgram)sender;
   if (program.MainForm.InvokeRequired)
   {
    program.MainForm.Invoke(
     (GuiProgram.NextInstanceEventHandler)OnGUINextInstance,
     sender, e);
    return;
   }
   program.MainForm.Show();
  }
  private static void OnGUIExitInstance(object sender, EventArgs e)
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
  public const string CommandLinePrefixes = "(/|-|--)";
  public const string CommandLineSeparators = "(:|=)";
  public static Executor eraserClient;
  public static readonly string AppDataPath = Path.Combine(Environment.GetFolderPath(
   Environment.SpecialFolder.LocalApplicationData), @"Eraser 6");
  private const string TaskListFileName = @"Task List.ersx";
  public static readonly string TaskListPath = Path.Combine(AppDataPath, TaskListFileName);
  public const string SettingsPath = @"SOFTWARE\Eraser\Eraser 6";
 }
}
