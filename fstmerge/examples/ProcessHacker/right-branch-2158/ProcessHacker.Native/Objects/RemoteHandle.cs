


using System;
namespace ProcessHacker.Native.Objects
{



    public class RemoteHandle
    {
        private ProcessHandle _phandle;
        private IntPtr _handle;

        public RemoteHandle(ProcessHandle phandle, IntPtr handle)
        {
            _phandle = phandle;
            _handle = handle;
        }

        public ProcessHandle ProcessHandle
        {
            get { return _phandle; }
        }

        public IntPtr Handle
        {
            get { return _handle; }
        }
        public int GetHandle(int access)
        {
            return new GenericHandle(_phandle, _handle, access);
        }
    }
}
