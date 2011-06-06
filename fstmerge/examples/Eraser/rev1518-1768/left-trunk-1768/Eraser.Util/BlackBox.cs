using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Forms;
using System.IO;
using System.Runtime.InteropServices;
using Microsoft.Win32.SafeHandles;
using System.Diagnostics;
using System.Threading;
using System.Drawing;
using System.Drawing.Imaging;
using System.Reflection;
using System.Collections.ObjectModel;
using System.Globalization;
namespace Eraser.Util
{
 public class BlackBox
 {
  private static class NativeMethods
  {
   [DllImport("dbghelp.dll", SetLastError = true)]
   [return: MarshalAs(UnmanagedType.Bool)]
   public static extern bool MiniDumpWriteDump(IntPtr hProcess, uint ProcessId,
    SafeFileHandle hFile, MiniDumpType DumpType,
    ref MiniDumpExceptionInfo ExceptionParam, IntPtr UserStreamParam,
    IntPtr CallbackParam);
   public enum MiniDumpType
   {
    MiniDumpNormal = 0x00000000,
    MiniDumpWithDataSegs = 0x00000001,
    MiniDumpWithFullMemory = 0x00000002,
    MiniDumpWithHandleData = 0x00000004,
    MiniDumpFilterMemory = 0x00000008,
    MiniDumpScanMemory = 0x00000010,
    MiniDumpWithUnloadedModules = 0x00000020,
    MiniDumpWithIndirectlyReferencedMemory = 0x00000040,
    MiniDumpFilterModulePaths = 0x00000080,
    MiniDumpWithProcessThreadData = 0x00000100,
    MiniDumpWithPrivateReadWriteMemory = 0x00000200,
    MiniDumpWithoutOptionalData = 0x00000400,
    MiniDumpWithFullMemoryInfo = 0x00000800,
    MiniDumpWithThreadInfo = 0x00001000,
    MiniDumpWithCodeSegs = 0x00002000,
    MiniDumpWithoutAuxiliaryState = 0x00004000,
    MiniDumpWithFullAuxiliaryState = 0x00008000,
    MiniDumpWithPrivateWriteCopyMemory = 0x00010000,
    MiniDumpIgnoreInaccessibleMemory = 0x00020000,
    MiniDumpWithTokenInformation = 0x00040000
   }
   [StructLayout(LayoutKind.Sequential, Pack = 4)]
   public struct MiniDumpExceptionInfo
   {
    public uint ThreadId;
    public IntPtr ExceptionPointers;
    [MarshalAs(UnmanagedType.Bool)]
    public bool ClientPointers;
   }
  }
  public static BlackBox Get()
  {
   if (Instance == null)
    Instance = new BlackBox();
   return Instance;
  }
  public void CreateReport(Exception e)
  {
   if (e == null)
    throw new ArgumentNullException("e");
   string crashName = DateTime.Now.ToUniversalTime().ToString(
    CrashReportName, CultureInfo.InvariantCulture);
   string currentCrashReport = Path.Combine(CrashReportsPath, crashName);
   Directory.CreateDirectory(currentCrashReport);
   int currentStep = 0;
   try
   {
    WriteDebugLog(currentCrashReport, e);
    ++currentStep;
    WriteScreenshot(currentCrashReport);
    ++currentStep;
    WriteMemoryDump(currentCrashReport, e);
    ++currentStep;
   }
   catch
   {
    if (currentStep == 0)
     Directory.Delete(currentCrashReport);
   }
  }
  public BlackBoxReport[] GetDumps()
  {
   DirectoryInfo dirInfo = new DirectoryInfo(CrashReportsPath);
   List<BlackBoxReport> result = new List<BlackBoxReport>();
   if (dirInfo.Exists)
    foreach (DirectoryInfo subDir in dirInfo.GetDirectories())
     try
     {
      result.Add(new BlackBoxReport(Path.Combine(CrashReportsPath, subDir.Name)));
     }
     catch (InvalidDataException)
     {
     }
   return result.ToArray();
  }
  private BlackBox()
  {
   AppDomain.CurrentDomain.UnhandledException += OnUnhandledException;
   Application.SetUnhandledExceptionMode(UnhandledExceptionMode.ThrowException);
  }
  private void OnUnhandledException(object sender, UnhandledExceptionEventArgs e)
  {
   CreateReport(e.ExceptionObject as Exception);
  }
  private void WriteMemoryDump(string dumpFolder, Exception e)
  {
   using (FileStream stream = new FileStream(Path.Combine(dumpFolder, MemoryDumpFileName),
    FileMode.OpenOrCreate, FileAccess.Write, FileShare.None))
   {
    NativeMethods.MiniDumpExceptionInfo exception =
     new NativeMethods.MiniDumpExceptionInfo();
    exception.ClientPointers = false;
    exception.ExceptionPointers = Marshal.GetExceptionPointers();
    exception.ThreadId = (uint)AppDomain.GetCurrentThreadId();
    NativeMethods.MiniDumpWriteDump(Process.GetCurrentProcess().Handle,
     (uint)Process.GetCurrentProcess().Id, stream.SafeFileHandle,
     NativeMethods.MiniDumpType.MiniDumpWithFullMemory,
     ref exception, IntPtr.Zero, IntPtr.Zero);
   }
  }
  private void WriteDebugLog(string dumpFolder, Exception exception)
  {
   using (FileStream file = new FileStream(Path.Combine(dumpFolder, DebugLogFileName),
    FileMode.OpenOrCreate, FileAccess.Write, FileShare.None))
   using (StreamWriter stream = new StreamWriter(file))
   {
    string separator = new string('-', 76);
    string lineFormat = "{0,15}: {1}";
    stream.WriteLine("Application Information");
    stream.WriteLine(separator);
    stream.WriteLine(string.Format(lineFormat, "Version",
     Assembly.GetEntryAssembly().GetName().Version));
    StringBuilder commandLine = new StringBuilder();
    foreach (string param in Environment.GetCommandLineArgs())
    {
     commandLine.Append(param);
     commandLine.Append(' ');
    }
    stream.WriteLine(string.Format(lineFormat, "Command Line",
     commandLine.ToString().Trim()));
    stream.WriteLine();
    stream.WriteLine("Exception Information (Outermost to innermost)");
    stream.WriteLine(separator);
    using (StreamWriter stackTraceLog = new StreamWriter(
     Path.Combine(dumpFolder, BlackBoxReport.StackTraceFileName)))
    {
     Exception currentException = exception;
     for (uint i = 1; currentException != null; ++i)
     {
      stream.WriteLine(string.Format("Exception {0}:", i));
      stream.WriteLine(string.Format(lineFormat, "Message", currentException.Message));
      stream.WriteLine(string.Format(lineFormat, "Exception Type",
       currentException.GetType().FullName));
      stackTraceLog.WriteLine(string.Format("Exception {0}: {1}", i,
       currentException.GetType().FullName));
      string[] stackTrace = currentException.StackTrace.Split(new char[] { '\n' });
      for (uint j = 0; j < stackTrace.Length; ++j)
      {
       stream.WriteLine(string.Format(lineFormat,
        string.Format("Stack Trace [{0}]", j), stackTrace[j].Trim()));
       stackTraceLog.WriteLine(string.Format("{0}", stackTrace[j].Trim()));
      }
      uint k = 0;
      foreach (System.Collections.DictionaryEntry value in currentException.Data)
       stream.WriteLine(string.Format(lineFormat, string.Format("Data[{0}]", ++k),
        string.Format("{0} {1}", value.Key.ToString(), value.Value.ToString())));
      stream.WriteLine();
      currentException = currentException.InnerException;
     }
    }
   }
  }
  private void WriteScreenshot(string dumpFolder)
  {
   Rectangle rect = new Rectangle(int.MaxValue, int.MaxValue, int.MinValue, int.MinValue);
   foreach (Screen screen in Screen.AllScreens)
    rect = Rectangle.Union(rect, screen.Bounds);
   Bitmap screenShot = new Bitmap(rect.Width, rect.Height);
   Graphics bitmap = Graphics.FromImage(screenShot);
   bitmap.CopyFromScreen(0, 0, 0, 0, rect.Size, CopyPixelOperation.SourceCopy);
   screenShot.Save(Path.Combine(dumpFolder, ScreenshotFileName), ImageFormat.Png);
  }
  private static BlackBox Instance;
  private static readonly string CrashReportsPath = Path.Combine(Environment.GetFolderPath(
   Environment.SpecialFolder.LocalApplicationData), @"Eraser 6\Crash Reports");
  internal static readonly string CrashReportName = "yyyyMMdd HHmmss.FFF";
  internal static readonly string MemoryDumpFileName = "Memory.dmp";
  internal static readonly string DebugLogFileName = "Debug.log";
  internal static readonly string ScreenshotFileName = "Screenshot.png";
 }
 public class BlackBoxReport
 {
  internal BlackBoxReport(string path)
  {
   Path = path;
   string stackTracePath = System.IO.Path.Combine(Path, StackTraceFileName);
   if (!System.IO.File.Exists(stackTracePath))
   {
    Delete();
    throw new InvalidDataException("The BlackBox report is corrupt.");
   }
   string[] stackTrace = null;
   using (StreamReader reader = new StreamReader(stackTracePath))
    stackTrace = reader.ReadToEnd().Split(new char[] { '\n' });
   StackTraceCache = new List<BlackBoxExceptionEntry>();
   List<string> currentException = new List<string>();
   string exceptionType = null;
   foreach (string str in stackTrace)
   {
    if (str.StartsWith("Exception "))
    {
     if (currentException.Count != 0)
     {
      StackTraceCache.Add(new BlackBoxExceptionEntry(exceptionType,
       new List<string>(currentException)));
      currentException.Clear();
     }
     exceptionType = str.Substring(str.IndexOf(':') + 1).Trim();
    }
    else if (!string.IsNullOrEmpty(str.Trim()))
    {
     currentException.Add(str.Trim());
    }
   }
   if (currentException.Count != 0)
    StackTraceCache.Add(new BlackBoxExceptionEntry(exceptionType, currentException));
  }
  public void Delete()
  {
   Directory.Delete(Path, true);
  }
  public string Name
  {
   get
   {
    return System.IO.Path.GetFileName(Path);
   }
  }
  public DateTime Timestamp
  {
   get
   {
    return DateTime.ParseExact(Name, BlackBox.CrashReportName,
     CultureInfo.InvariantCulture).ToLocalTime();
   }
  }
  public string Path
  {
   get;
   private set;
  }
  public ReadOnlyCollection<FileInfo> Files
  {
   get
   {
    List<FileInfo> result = new List<FileInfo>();
    DirectoryInfo directory = new DirectoryInfo(Path);
    foreach (FileInfo file in directory.GetFiles())
     if (!InternalFiles.Contains(file.Name))
      result.Add(file);
    return result.AsReadOnly();
   }
  }
  public Stream DebugLog
  {
   get
   {
    return new FileStream(System.IO.Path.Combine(Path, BlackBox.DebugLogFileName),
     FileMode.Open, FileAccess.Read, FileShare.Read);
   }
  }
  public ReadOnlyCollection<BlackBoxExceptionEntry> StackTrace
  {
   get
   {
    return StackTraceCache.AsReadOnly();
   }
  }
  public bool Submitted
  {
   get
   {
    byte[] buffer = new byte[1];
    using (FileStream stream = new FileStream(System.IO.Path.Combine(Path, StatusFileName),
     FileMode.OpenOrCreate, FileAccess.Read, FileShare.Read))
    {
     stream.Read(buffer, 0, buffer.Length);
    }
    return buffer[0] == 1;
   }
   set
   {
    byte[] buffer = { Convert.ToByte(value) };
    using (FileStream stream = new FileStream(System.IO.Path.Combine(Path, StatusFileName),
     FileMode.OpenOrCreate, FileAccess.Write, FileShare.Read))
    {
     stream.Write(buffer, 0, buffer.Length);
    }
   }
  }
  public override string ToString()
  {
   return Name;
  }
  private List<BlackBoxExceptionEntry> StackTraceCache;
  private static readonly string StatusFileName = "Status.txt";
  internal static readonly string StackTraceFileName = "Stack Trace.log";
  private static readonly List<string> InternalFiles = new List<string>(
   new string[] {
     StackTraceFileName,
     StatusFileName
   }
  );
 }
 public class BlackBoxExceptionEntry
 {
  internal BlackBoxExceptionEntry(string exceptionType, List<string> stackTrace)
  {
   ExceptionType = exceptionType;
   StackTraceCache = stackTrace;
  }
  public string ExceptionType
  {
   get;
   private set;
  }
  public ReadOnlyCollection<string> StackTrace
  {
   get
   {
    return StackTraceCache.AsReadOnly();
   }
  }
  private List<string> StackTraceCache;
 }
}
