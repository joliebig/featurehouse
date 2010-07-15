

using System;
using System.Collections.Generic;
using System.Text;
using ProcessHacker.Native.Api;
using ProcessHacker.Native.Objects;

namespace ProcessHacker.Native.Threading
{



    public static class CurrentThread
    {



        public static void Sleep()
        {
            Yield();
        }





        public static void Sleep(int interval)
        {
            ThreadHandle.Sleep(interval * Win32.TimeMsTo100Ns, true);
        }





        public static void Sleep(DateTime time)
        {
            ThreadHandle.Sleep(time.ToFileTime(), false);
        }




        public static void Yield()
        {
            ThreadHandle.Yield();
        }
    }
}
