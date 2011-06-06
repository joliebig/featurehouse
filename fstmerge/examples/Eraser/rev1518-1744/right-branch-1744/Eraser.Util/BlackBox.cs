using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Forms;
using System.IO;
using System.Runtime.InteropServices;
using System.Diagnostics;
using System.Reflection;
using Microsoft.Win32.SafeHandles;
using System.Drawing;
using System.Drawing.Imaging;
using System.Collections.ObjectModel;
using System.Globalization;
using ICSharpCode.SharpZipLib.Tar;
using ICSharpCode.SharpZipLib.BZip2;
using System.Net;
using System.Xml;
namespace Eraser.Util
{
 public class BlackBox
 {
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
   if (!File.Exists(stackTracePath))
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
     FileMode.Open, FileAccess.Read);
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
 public class BlackBoxReportUploader
 {
  public BlackBoxReportUploader(BlackBoxReport report)
  {
   Report = report;
   if (!Directory.Exists(UploadTempDir))
    Directory.CreateDirectory(UploadTempDir);
   ReportBaseName = Path.Combine(UploadTempDir, Report.Name);
  }
  public bool IsNew
  {
   get
   {
    PostDataBuilder builder = new PostDataBuilder();
    builder.AddPart(new PostDataField("action", "status"));
    AddStackTraceToRequest(Report.StackTrace, builder);
    WebRequest reportRequest = HttpWebRequest.Create(BlackBoxServer);
    reportRequest.ContentType = builder.ContentType;
    reportRequest.Method = "POST";
    using (Stream formStream = builder.Stream)
    {
     reportRequest.ContentLength = formStream.Length;
     using (Stream requestStream = reportRequest.GetRequestStream())
     {
      int lastRead = 0;
      byte[] buffer = new byte[32768];
      while ((lastRead = formStream.Read(buffer, 0, buffer.Length)) != 0)
       requestStream.Write(buffer, 0, lastRead);
     }
    }
    try
    {
     HttpWebResponse response = reportRequest.GetResponse() as HttpWebResponse;
     using (Stream responseStream = response.GetResponseStream())
     {
      XmlReader reader = XmlReader.Create(responseStream);
      reader.ReadToFollowing("crashReport");
      string reportStatus = reader.GetAttribute("status");
      switch (reportStatus)
      {
       case "exists":
        Report.Submitted = true;
        return false;
       case "new":
        return true;
       default:
        throw new InvalidDataException(
         "Unknown crash report server response.");
      }
     }
    }
    catch (WebException e)
    {
     using (Stream responseStream = e.Response.GetResponseStream())
     {
      try
      {
       XmlReader reader = XmlReader.Create(responseStream);
       reader.ReadToFollowing("error");
       throw new InvalidDataException(string.Format(CultureInfo.CurrentCulture,
        "The server encountered a problem while processing the request: {0}",
        reader.ReadString()));
      }
      catch (XmlException)
      {
      }
     }
     throw new InvalidDataException(((HttpWebResponse)e.Response).StatusDescription);
    }
   }
  }
  private void Compress(SteppedProgressManager progress,
   ProgressChangedEventHandler progressChanged)
  {
   using (FileStream archiveStream = new FileStream(ReportBaseName + ".tar",
     FileMode.Create, FileAccess.Write))
   {
    TarArchive archive = TarArchive.CreateOutputTarArchive(archiveStream);
    foreach (FileInfo file in Report.Files)
    {
     TarEntry entry = TarEntry.CreateEntryFromFile(file.FullName);
     entry.Name = Path.GetFileName(entry.Name);
     archive.WriteEntry(entry, false);
    }
    archive.Close();
   }
   ProgressManager step = new ProgressManager();
   progress.Steps.Add(new SteppedProgressManagerStep(step, 0.5f, "Compressing"));
   using (FileStream bzipFile = new FileStream(ReportBaseName + ".tbz",
    FileMode.Create))
   using (FileStream tarStream = new FileStream(ReportBaseName + ".tar",
    FileMode.Open, FileAccess.Read, FileShare.Read, 262144, FileOptions.DeleteOnClose))
   using (BZip2OutputStream bzipStream = new BZip2OutputStream(bzipFile, 262144))
   {
    int lastRead = 0;
    byte[] buffer = new byte[524288];
    while ((lastRead = tarStream.Read(buffer, 0, buffer.Length)) != 0)
    {
     bzipStream.Write(buffer, 0, lastRead);
     step.Completed = tarStream.Position;
     step.Total = tarStream.Length;
     if (progressChanged != null)
      progressChanged(this, new ProgressChangedEventArgs(progress, null));
    }
   }
  }
  public void Submit(ProgressChangedEventHandler progressChanged)
  {
   SteppedProgressManager overallProgress = new SteppedProgressManager();
   Compress(overallProgress, progressChanged);
   using (FileStream bzipFile = new FileStream(ReportBaseName + ".tbz",
    FileMode.Open, FileAccess.Read, FileShare.Read, 131072, FileOptions.DeleteOnClose))
   using (Stream logFile = Report.DebugLog)
   {
    PostDataBuilder builder = new PostDataBuilder();
    builder.AddPart(new PostDataField("action", "upload"));
    builder.AddPart(new PostDataFileField("crashReport", "Report.tbz", bzipFile));
    AddStackTraceToRequest(Report.StackTrace, builder);
    WebRequest reportRequest = HttpWebRequest.Create(BlackBoxServer);
    reportRequest.ContentType = builder.ContentType;
    reportRequest.Method = "POST";
    reportRequest.Timeout = int.MaxValue;
    using (Stream formStream = builder.Stream)
    {
     ProgressManager progress = new ProgressManager();
     overallProgress.Steps.Add(new SteppedProgressManagerStep(
      progress, 0.5f, "Uploading"));
     reportRequest.ContentLength = formStream.Length;
     using (Stream requestStream = reportRequest.GetRequestStream())
     {
      int lastRead = 0;
      byte[] buffer = new byte[32768];
      while ((lastRead = formStream.Read(buffer, 0, buffer.Length)) != 0)
      {
       requestStream.Write(buffer, 0, lastRead);
       progress.Completed = formStream.Position;
       progress.Total = formStream.Length;
       progressChanged(this, new ProgressChangedEventArgs(overallProgress, null));
      }
     }
    }
    try
    {
     reportRequest.GetResponse();
     Report.Submitted = true;
    }
    catch (WebException e)
    {
     using (Stream responseStream = e.Response.GetResponseStream())
     {
      try
      {
       XmlReader reader = XmlReader.Create(responseStream);
       reader.ReadToFollowing("error");
       throw new InvalidDataException(string.Format(CultureInfo.CurrentCulture,
        "The server encountered a problem while processing the request: {0}",
        reader.ReadString()));
      }
      catch (XmlException)
      {
      }
     }
     throw new InvalidDataException(((HttpWebResponse)e.Response).StatusDescription);
    }
   }
  }
  private static void AddStackTraceToRequest(IList<BlackBoxExceptionEntry> stackTrace,
   PostDataBuilder builder)
  {
   int exceptionIndex = 0;
   foreach (BlackBoxExceptionEntry exceptionStack in stackTrace)
   {
    foreach (string stackFrame in exceptionStack.StackTrace)
     builder.AddPart(new PostDataField(
      string.Format("stackTrace[{0}][]", exceptionIndex), stackFrame));
    builder.AddPart(new PostDataField(string.Format(
     "stackTrace[{0}][exception]", exceptionIndex), exceptionStack.ExceptionType));
    ++exceptionIndex;
   }
  }
  private static readonly string UploadTempDir =
   Path.Combine(Path.GetTempPath(), "Eraser Crash Reports");
  private static readonly Uri BlackBoxServer =
   new Uri("http://eraser.heidi.ie/scripts/blackbox/upload.php");
  private BlackBoxReport Report;
  private readonly string ReportBaseName;
 }
}
