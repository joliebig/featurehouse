

using System;
using System.Diagnostics;
using System.Runtime.InteropServices;

namespace ProcessHacker.Common
{
    public delegate void LoggingDelegate(string message);

    public static class Logging
    {
        public enum Importance : int
        {
            Information = 0,
            Warning,
            Error,
            Critical
        }

        public static event LoggingDelegate Logged;

        [DllImport("kernel32.dll", CharSet = CharSet.Unicode)]
        private static extern void OutputDebugString(string OutputString);

        private static object _logLock = new object();

        [Conditional("DEBUG")]
        public static void Log(Importance importance, string message)
        {
            lock (_logLock)
            {
                string debugMessage =
                    DateTime.Now.ToString("hh:mm:ss:fff:") +
                    " ProcessHacker (T" + System.Threading.Thread.CurrentThread.ManagedThreadId +
                    "): (" + importance.ToString() + ") " + message + "\r\n\r\n" + Environment.StackTrace;

                OutputDebugString(debugMessage);

                if (Logged != null)
                    Logged(debugMessage);
            }
        }

        [Conditional("DEBUG")]
        public static void Log(Exception ex)
        {
            string message = ex.Message;

            if (ex.InnerException != null)
                message += "\r\nInner exception:\r\n" + ex.InnerException.ToString();
            if (ex.StackTrace != null)
                message += "\r\n" + ex.StackTrace;

            Log(Importance.Error, message);
        }
    }
}
