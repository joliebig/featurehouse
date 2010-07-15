

using System;
using ProcessHacker.Native.Api;

namespace ProcessHacker.Native.Debugging
{
    public class ModuleInformation : ILoadedModule
    {
        internal ModuleInformation(RtlProcessModuleInformation moduleInfo)
        {
            this.BaseAddress = moduleInfo.ImageBase;
            this.Size = moduleInfo.ImageSize;
            this.Flags = moduleInfo.Flags;
            this.LoadCount = moduleInfo.LoadCount;

            int nullIndex = Array.IndexOf<char>(moduleInfo.FullPathName, '\0');

            if (nullIndex != -1)
                this.FileName = new string(moduleInfo.FullPathName, 0, nullIndex);
            else
                this.FileName = new string(moduleInfo.FullPathName);

            this.BaseName = this.FileName.Substring(moduleInfo.OffsetToFileName);
        }

        public IntPtr BaseAddress { get; private set; }
        public int Size { get; private set; }
        public LdrpDataTableEntryFlags Flags { get; private set; }
        public ushort LoadCount { get; private set; }
        public string BaseName { get; private set; }
        public string FileName { get; private set; }
    }
}
