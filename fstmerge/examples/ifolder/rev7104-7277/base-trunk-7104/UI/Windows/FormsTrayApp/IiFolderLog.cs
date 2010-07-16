
using System;

using log4net;

namespace Novell.iFolder
{



    public interface IiFolderLog
    {





        void Debug(string format, params object[] args);






        void Info(string format, params object[] args);






        void Warn(string format, params object[] args);






        void Error(string format, params object[] args);






        void Fatal(string format, params object[] args);







        void Debug(Exception e, string format, params object[] args);







        void Info(Exception e, string format, params object[] args);







        void Warn(Exception e, string format, params object[] args);







        void Error(Exception e, string format, params object[] args);







        void Fatal(Exception e, string format, params object[] args);
    }
}
