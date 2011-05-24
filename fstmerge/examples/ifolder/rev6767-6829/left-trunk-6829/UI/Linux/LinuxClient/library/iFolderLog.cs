

using System;
using System.Text;

using log4net;
using log4net.Layout;

namespace Novell.iFolder
{



    public class iFolderLog : IiFolderLog
    {
        private ILog log;





        internal iFolderLog(ILog log)
        {
            this.log = log;
        }






        public void Debug(string format, params object[] args)
        {
            if (log.IsDebugEnabled)
            {
                log.Debug(String.Format(format, args));
            }
        }






        public void Info(string format, params object[] args)
        {
            if (log.IsInfoEnabled)
            {
                log.Info(String.Format(format, args));
            }
        }






        public void Warn(string format, params object[] args)
        {
            if (log.IsWarnEnabled)
            {
                log.Warn(String.Format(format, args));
            }
        }






        public void Error(string format, params object[] args)
        {
            if (log.IsErrorEnabled)
            {
                log.Error(String.Format(format, args));
            }
        }






        public void Fatal(string format, params object[] args)
        {
            if (log.IsFatalEnabled)
            {
                log.Fatal(String.Format(format, args));
            }
        }







        public void Debug(Exception e, string format, params object[] args)
        {
            if (log.IsDebugEnabled)
            {
                log.Debug(String.Format(format, args), e);
            }
        }







        public void Info(Exception e, string format, params object[] args)
        {
            if (log.IsInfoEnabled)
            {
                log.Info(String.Format(format, args), e);
            }
        }







        public void Warn(Exception e, string format, params object[] args)
        {
            if (log.IsWarnEnabled)
            {
                log.Warn(String.Format(format, args), e);
            }
        }







        public void Error(Exception e, string format, params object[] args)
        {
            if (log.IsErrorEnabled)
            {
                log.Error(String.Format(format, args), e);
            }
        }







        public void Fatal(Exception e, string format, params object[] args)
        {
            if (log.IsFatalEnabled)
            {
                log.Fatal(String.Format(format, args), e);
            }
        }
    }
}
