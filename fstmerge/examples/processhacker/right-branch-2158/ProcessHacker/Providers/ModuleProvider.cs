

using System;
using System.Collections.Generic;
using ProcessHacker.Common;
using ProcessHacker.Native;
using ProcessHacker.Native.Api;
using ProcessHacker.Native.Debugging;
using ProcessHacker.Native.Objects;
using ProcessHacker.Native.Security;

namespace ProcessHacker
{
    public class ModuleItem : ICloneable
    {
        public object Clone()
        {
            return this.MemberwiseClone();
        }

        public int RunId;
        public IntPtr BaseAddress;
        public int Size;
        public LdrpDataTableEntryFlags Flags;
        public string Name;
        public string FileName;
        public string FileDescription;
        public string FileVersion;
    }

    public class ModuleProvider : Provider<IntPtr, ModuleItem>
    {
        private ProcessHandle _processHandle;
        private int _pid;
        private bool _isWow64 = false;

        public ModuleProvider(int pid)
            : base()
        {
            this.Name = this.GetType().Name;
            _pid = pid;

            try
            {
                _processHandle = new ProcessHandle(_pid,
                    ProcessAccess.QueryInformation | Program.MinProcessReadMemoryRights);
            }
            catch
            {
                try
                {
                    _processHandle = new ProcessHandle(_pid,
                        Program.MinProcessQueryRights | Program.MinProcessReadMemoryRights);
                }
                catch
                { }
            }

            if (_processHandle != null && IntPtr.Size == 8)
            {
                try
                {
                    _isWow64 = _processHandle.IsWow64();
                }
                catch
                { }
            }

            this.ProviderUpdate += new ProviderUpdateOnce(UpdateOnce);
            this.Disposed += (provider) => { if (_processHandle != null) _processHandle.Dispose(); };
        }

        private void UpdateOnce()
        {
            if (_pid != 4 && _processHandle == null)
            {
                Logging.Log(Logging.Importance.Warning, "ModuleProvider: Process Handle is null, exiting...");
                return;
            }

            var modules = new Dictionary<IntPtr, ILoadedModule>();
            var newdictionary = new Dictionary<IntPtr, ModuleItem>(this.Dictionary);

            if (_pid != 4)
            {

                if (!_isWow64)
                {
                    _processHandle.EnumModules((module) =>
                        {
                            if (!modules.ContainsKey(module.BaseAddress))
                                modules.Add(module.BaseAddress, module);

                            return true;
                        });
                }
                else
                {
                    using (DebugBuffer buffer = new DebugBuffer())
                    {
                        buffer.Query(_pid, RtlQueryProcessDebugFlags.Modules32);

                        var processModules = buffer.GetModules();

                        foreach (var m in processModules)
                        {


                            if (!modules.ContainsKey(m.BaseAddress))
                            {
                                modules.Add(
                                    m.BaseAddress,
                                    new ProcessModule(
                                        m.BaseAddress,
                                        m.Size,
                                        IntPtr.Zero,
                                        m.Flags,
                                        System.IO.Path.GetFileName(m.FileName),
                                        m.FileName
                                        )
                                    );
                            }
                        }
                    }
                }


                _processHandle.EnumMemory((info) =>
                {
                    if (info.Type == MemoryType.Mapped)
                    {
                        try
                        {
                            string fileName = _processHandle.GetMappedFileName(info.BaseAddress);

                            if (fileName != null)
                            {
                                var fi = new System.IO.FileInfo(fileName);

                                modules.Add(info.BaseAddress,
                                    new ProcessModule(
                                        info.BaseAddress,
                                        info.RegionSize.ToInt32(),
                                        IntPtr.Zero,
                                        0,
                                        fi.Name, fi.FullName));
                            }
                        }
                        catch
                        { }
                    }

                    return true;
                });
            }
            else
            {

                Windows.EnumKernelModules((module) =>
                {
                    if (!modules.ContainsKey(module.BaseAddress))
                        modules.Add(module.BaseAddress, module);

                    return true;
                });
            }


            foreach (IntPtr b in Dictionary.Keys)
            {
                if (!modules.ContainsKey(b))
                {
                    this.OnDictionaryRemoved(this.Dictionary[b]);
                    newdictionary.Remove(b);
                }
            }


            foreach (IntPtr b in modules.Keys)
            {
                if (!Dictionary.ContainsKey(b))
                {
                    var m = modules[b];
                    ModuleItem item = new ModuleItem();

                    item.RunId = this.RunCount;
                    item.Name = m.BaseName;

                    try
                    {
                        item.FileName = FileUtils.GetFileName(m.FileName);
                    }
                    catch
                    { }

                    item.BaseAddress = b;
                    item.Size = m.Size;
                    item.Flags = m.Flags;

                    try
                    {
                        var info = System.Diagnostics.FileVersionInfo.GetVersionInfo(item.FileName);

                        item.FileDescription = info.FileDescription;
                        item.FileVersion = info.FileVersion;
                    }
                    catch
                    { }

                    newdictionary.Add(b, item);
                    this.OnDictionaryAdded(item);
                }
            }

            this.Dictionary = newdictionary;
        }

        public int Pid
        {
            get { return _pid; }
        }
    }
}
