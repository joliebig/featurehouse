

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading;
using Microsoft.Win32;
using TaskbarLib.Interop;
using System.Runtime.CompilerServices;
using ProcessHacker.Native.Api;

namespace TaskbarLib
{
    public sealed class JumpListManager : IDisposable
    {
        string _appId;
        uint _maxSlotsInList;
        JumpListTasks _tasks;
        JumpListDestinations _destinations;
        EventHandler _displaySettingsChangeHandler;
        ICustomDestinationList _customDestinationList;
        ApplicationDestinationType _enabledAutoDestinationType;
        public JumpListManager(string appId)
        {
            _appId = appId;
            _destinations = new JumpListDestinations();
            _tasks = new JumpListTasks();
            _customDestinationList = (ICustomDestinationList)new CDestinationList();
            if (String.IsNullOrEmpty(_appId))
            {
                _appId = Windows7Taskbar.ProcessAppId;
            }
            if (!String.IsNullOrEmpty(_appId))
            {
                _customDestinationList.SetAppID(_appId);
            }
            _displaySettingsChangeHandler = delegate
            {
                RefreshMaxSlots();
            };
            SystemEvents.DisplaySettingsChanged += _displaySettingsChangeHandler;
        }
        public JumpListManager()
            : this(Windows7Taskbar.AppId)
        {
        }
        public void AddUserTask(IJumpListTask task)
        {
            _tasks.AddTask(task);
        }
        public IEnumerable<IJumpListTask> Tasks
        {
            get { return _tasks.Tasks; }
        }
        public void DeleteTask(IJumpListTask task)
        {
            _tasks.DeleteTask(task);
        }
        public void ClearTasks()
        {
            _tasks.Clear();
        }
        public void AddCustomDestination(IJumpListDestination destination)
        {
        }
        public void DeleteCustomDestination(IJumpListDestination destination)
        {
            _destinations.DeleteDestination(destination);
        }
        public ApplicationDestinationType EnabledAutoDestinationType
        {
            get
            {
                return _enabledAutoDestinationType;
            }
            set
            {
                if (_enabledAutoDestinationType == value)
                    return;
                _enabledAutoDestinationType = value;
            }
        }
        public void ClearAllDestinations()
        {
            ClearApplicationDestinations();
            ClearCustomDestinations();
        }
        public void ClearApplicationDestinations()
        {
            IApplicationDestinations destinations = (IApplicationDestinations)new CApplicationDestinations();
            if (!String.IsNullOrEmpty(_appId))
            {
                HResult setAppIDResult = destinations.SetAppID(_appId);
                setAppIDResult.ThrowIf();
            }
            try
            {
                HResult removeAllDestinationsResult = destinations.RemoveAllDestinations();
                removeAllDestinationsResult.ThrowIf();
            }
            catch (FileNotFoundException)
            { }
        }
        public IEnumerable<IJumpListDestination> GetApplicationDestinations(ApplicationDestinationType type)
        {
            if (type == ApplicationDestinationType.None)
                throw new ArgumentException("ApplicationDestinationType can't be NONE");
            IApplicationDocumentLists destinations = (IApplicationDocumentLists)new CApplicationDocumentLists();
            Guid iidObjectArray = typeof(IObjectArray).GUID;
            object obj;
            HResult getListResult = destinations.GetList((AppDocListType)type, 100, ref iidObjectArray, out obj);
            getListResult.ThrowIf();
            List<IJumpListDestination> returnValue = new List<IJumpListDestination>();
            Guid iidShellItem = typeof(IShellItem).GUID;
            Guid iidShellLink = typeof(IShellLinkW).GUID;
            IObjectArray array = (IObjectArray)obj;
            uint count;
            HResult getCountResult = array.GetCount(out count);
            getCountResult.ThrowIf();
            for (uint i = 0; i < count; ++i)
            {
                try
                {
                    array.GetAt(i, ref iidShellItem, out obj);
                }
                catch (Exception)
                { }
                if (obj == null)
                {
                    HResult getAtResult = array.GetAt(i, ref iidShellLink, out obj);
                    getAtResult.ThrowIf();
                    IShellLinkW link = (IShellLinkW)obj;
                    ShellLink wrapper = new ShellLink();
                    StringBuilder sb = new StringBuilder(256);
                    HResult getPathResult = link.GetPath(sb, sb.Capacity, IntPtr.Zero, 2);
                    getPathResult.ThrowIf();
                    wrapper.Path = sb.ToString();
                    HResult getArgumentsResult = link.GetArguments(sb, sb.Capacity);
                    getArgumentsResult.ThrowIf();
                    wrapper.Arguments = sb.ToString();
                    int iconId;
                    HResult getIconLocationResult = link.GetIconLocation(sb, sb.Capacity, out iconId);
                    getIconLocationResult.ThrowIf();
                    wrapper.IconIndex = iconId;
                    wrapper.IconLocation = sb.ToString();
                    uint showCmd;
                    HResult getShowCmdResult = link.GetShowCmd(out showCmd);
                    getShowCmdResult.ThrowIf();
                    wrapper.ShowCommand = (WindowShowCommand)showCmd;
                    HResult getWorkingDirectoryResult = link.GetWorkingDirectory(sb, sb.Capacity);
                    getWorkingDirectoryResult.ThrowIf();
                    wrapper.WorkingDirectory = sb.ToString();
                    returnValue.Add(wrapper);
                }
                else
                {
                    IShellItem item = (IShellItem)obj;
                    ShellItem wrapper = new ShellItem();
                    string path;
                    HResult getDisplayNameResult = item.GetDisplayName(SIGDN.SIGDN_FILESYSPATH, out path);
                    getDisplayNameResult.ThrowIf();
                    wrapper.Path = path;
                    returnValue.Add(wrapper);
                }
            }
            return returnValue;
        }
        public void DeleteApplicationDestination(IJumpListDestination destination)
        {
            IApplicationDestinations destinations = (IApplicationDestinations)new CApplicationDestinations();
            if (!String.IsNullOrEmpty(_appId))
            {
               HResult setAppIDResult = destinations.SetAppID(_appId);
               setAppIDResult.ThrowIf();
            }
            HResult removeDestinationResult = destinations.RemoveDestination(destination.GetShellRepresentation());
            removeDestinationResult.ThrowIf();
        }
        public void ClearCustomDestinations()
        {
            try
            {
                HResult deleteListResult = _customDestinationList.DeleteList(_appId);
                deleteListResult.ThrowIf();
            }
            catch (FileNotFoundException)
            { }
            _destinations.Clear();
        }
        public bool Refresh()
        {
            if (!BeginList())
                return false;
            _tasks.RefreshTasks(_customDestinationList);
            _destinations.RefreshDestinations(_customDestinationList);
            switch (EnabledAutoDestinationType)
            {
                case ApplicationDestinationType.Frequent:
                    HResult appendKnownCategoryFrequentResult = _customDestinationList.AppendKnownCategory(KnownDestCategory.FREQUENT);
                    appendKnownCategoryFrequentResult.ThrowIf();
                    break;
                case ApplicationDestinationType.Recent:
                    HResult appendKnownCategoryRecentResult = _customDestinationList.AppendKnownCategory(KnownDestCategory.RECENT);
                    appendKnownCategoryRecentResult.ThrowIf();
                    break;
            }
            CommitList();
            return true;
        }
        public uint MaximumSlotsInList
        {
            get
            {
                if (_maxSlotsInList == 0)
                {
                    RefreshMaxSlots();
                }
                return _maxSlotsInList;
            }
        }
        public void Dispose()
        {
            SystemEvents.DisplaySettingsChanged -= _displaySettingsChangeHandler;
            if (_customDestinationList != null)
                Marshal.ReleaseComObject(_customDestinationList);
        }
        public event EventHandler<UserRemovedItemsEventArgs> UserRemovedItems;
        private void RefreshMaxSlots()
        {
            object obj;
            _customDestinationList.BeginList(out _maxSlotsInList, ref SafeNativeMethods.IID_IObjectArray, out obj);
            _customDestinationList.AbortList();
        }
        private bool BeginList()
        {
            if (UserRemovedItems == null)
            {
                throw new InvalidOperationException("You must register for the JumpListManager.UserRemovedItems event before adding any items");
            }
            object obj;
            _customDestinationList.BeginList(out _maxSlotsInList, ref SafeNativeMethods.IID_IObjectArray, out obj);
            IObjectArray removedItems = (IObjectArray)obj;
            uint count;
            removedItems.GetCount(out count);
            if (count == 0)
                return true;
            string[] removedItemsArr = new string[count];
            for (uint i = 0; i < count; ++i)
            {
                object item;
                removedItems.GetAt(i, ref SafeNativeMethods.IID_IUnknown, out item);
                try
                {
                    IShellLinkW shellLink = (IShellLinkW)item;
                    if (shellLink != null)
                    {
                        StringBuilder sb = new StringBuilder(256);
                        shellLink.GetPath(sb, sb.Capacity, IntPtr.Zero, 2);
                        removedItemsArr[i] = sb.ToString();
                    }
                    continue;
                }
                catch (InvalidCastException)
                { }
                try
                {
                    IShellItem shellItem = (IShellItem)item;
                    if (shellItem != null)
                    {
                        string path;
                        shellItem.GetDisplayName(SIGDN.SIGDN_FILESYSPATH, out path);
                        removedItemsArr[i] = path;
                    }
                }
                catch (InvalidCastException)
                {
                    Debug.Assert(false,
                        "List of removed items contains something that is neither a shell item nor a shell link");
                }
            }
            UserRemovedItemsEventArgs args = new UserRemovedItemsEventArgs(removedItemsArr);
            UserRemovedItems(this, args);
            if (args.Cancel)
            {
                _customDestinationList.AbortList();
            }
            return !args.Cancel;
        }
        private void CommitList()
        {
            _customDestinationList.CommitList();
        }
    }
    public enum ApplicationDestinationType
    {
        None = -1,
        Recent = 0,
        Frequent
    }
    public class UserRemovedItemsEventArgs : EventArgs
    {
        readonly string[] _removedItems;
        internal UserRemovedItemsEventArgs(string[] removedItems)
        {
            _removedItems = removedItems;
        }
        public string[] RemovedItems
        {
            get
            {
                return _removedItems;
            }
        }
        public bool Cancel { get; set; }
    }
}
