

using ProcessHacker.Native.Objects;
using System;

namespace ProcessHacker.Structs
{
    public class ProcessMemoryIO : IStructIOProvider
    {
        private ProcessHandle _phandleR;
        private ProcessHandle _phandleW;

        public ProcessMemoryIO(int pid)
        {
            try { _phandleR = new ProcessHandle(pid, Program.MinProcessReadMemoryRights); }
            catch { }
            try
            {
                _phandleW = new ProcessHandle(pid, Program.MinProcessWriteMemoryRights);
            }
            catch { }
        }

        public byte[] ReadBytes(IntPtr offset, int length)
        {
            return _phandleR.ReadMemory(offset, length);
        }

        public void WriteBytes(IntPtr offset, byte[] bytes)
        {
            _phandleW.WriteMemory(offset, bytes);
        }
    }
}
