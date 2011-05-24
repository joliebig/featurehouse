

using System;
using System.Collections.Generic;
using System.Text;

namespace ProcessHacker.Structs
{
    public interface IStructIOProvider
    {
        byte[] ReadBytes(IntPtr offset, int length);
        void WriteBytes(IntPtr offset, byte[] bytes);
    }
}
