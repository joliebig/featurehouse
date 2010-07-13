

using System;
using System.Runtime.InteropServices;

namespace ProcessHackerRestartRecovery
{







    public delegate int RecoveryCallback(object state);




    public class RecoveryData
    {





        public RecoveryData(RecoveryCallback callback, object state)
        {
            Callback = callback;
            State = state;
        }




        public RecoveryCallback Callback { get; set; }




        public object State { get; set; }




        public void Invoke()
        {
            if(Callback != null)
                Callback(State);
        }
    }
}
