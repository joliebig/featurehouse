

using System;
using System.Collections.Generic;
using ProcessHacker.Native;
using ProcessHacker.Native.Api;
using ProcessHacker.Native.Objects;

namespace ProcessHacker
{
    public class HandleItem : ICloneable
    {
        public object Clone()
        {
            return this.MemberwiseClone();
        }

        public int RunId;
        public SystemHandleEntry Handle;
        public ObjectInformation ObjectInfo;
    }

    public class HandleProvider : Provider<short, HandleItem>
    {
        private ProcessHandle _processHandle;
        private int _pid;

        public HandleProvider(int pid)
            : base()
        {
            this.Name = this.GetType().Name;
            _pid = pid;

            try
            {
                _processHandle = new ProcessHandle(_pid, ProcessHacker.Native.Security.ProcessAccess.DupHandle);
            }
            catch
            {
                try
                {
                    _processHandle = new ProcessHandle(_pid, Program.MinProcessGetHandleInformationRights);
                }
                catch
                { }
            }

            this.ProviderUpdate += new ProviderUpdateOnce(UpdateOnce);
            this.Disposed += (provider) => { if (_processHandle != null) _processHandle.Dispose(); };
        }

        private void UpdateOnce()
        {
            var handles = Windows.GetHandles();
            var processHandles = new Dictionary<short, SystemHandleEntry>();
            var newdictionary = new Dictionary<short, HandleItem>(this.Dictionary);

            foreach (var handle in handles)
            {
                if (handle.ProcessId == _pid)
                {
                    processHandles.Add(handle.Handle, handle);
                }
            }


            foreach (short h in this.Dictionary.Keys)
            {

                if (!processHandles.ContainsKey(h) ||
                    processHandles[h].Object != this.Dictionary[h].Handle.Object)
                {
                    this.OnDictionaryRemoved(this.Dictionary[h]);
                    newdictionary.Remove(h);
                }
            }


            foreach (short h in processHandles.Keys)
            {
                if (!this.Dictionary.ContainsKey(h))
                {
                    ObjectInformation info;
                    HandleItem item = new HandleItem();

                    try
                    {
                        info = processHandles[h].GetHandleInfo(_processHandle);

                        if ((info.BestName == null || info.BestName == "") &&
                            HideHandlesWithNoName)
                            continue;
                    }
                    catch
                    {
                        continue;
                    }

                    item.RunId = this.RunCount;
                    item.Handle = processHandles[h];
                    item.ObjectInfo = info;

                    newdictionary.Add(h, item);
                    this.OnDictionaryAdded(item);
                }
                else
                {

                    if (this.Dictionary[h].Handle.Flags != processHandles[h].Flags)
                    {
                        this.Dictionary[h].Handle.Flags = processHandles[h].Flags;
                        this.OnDictionaryModified(null, this.Dictionary[h]);
                    }
                }
            }

            this.Dictionary = newdictionary;
        }

        public bool HideHandlesWithNoName { get; set; }

        public int Pid
        {
            get { return _pid; }
        }
    }
}
