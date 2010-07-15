

using System;
using System.Collections.Generic;
using System.Linq;
using TaskbarLib.Interop;
using System.Runtime.InteropServices;
using ProcessHacker.Native.Api;

namespace TaskbarLib
{



    internal sealed class JumpListDestinations
    {



        private SortedDictionary<int, List<IJumpListDestination> > _categorizedDestinations =
            new SortedDictionary<int, List<IJumpListDestination> >();

        public void AddDestination(IJumpListDestination destination)
        {
            List<IJumpListDestination> destinations =
                _categorizedDestinations.Values.FirstOrDefault(
                    list => list.First().Category == destination.Category);
            if (destinations == null)
            {
                destinations = new List<IJumpListDestination>();
                _categorizedDestinations.Add(
                    _categorizedDestinations.Keys.LastOrDefault() + 1, destinations);
            }

            destinations.Add(destination);
        }

        public void DeleteDestination(IJumpListDestination destination)
        {
            List<IJumpListDestination> destinations =
                _categorizedDestinations.Values.First(
                    list => list.First().Category == destination.Category);
            IJumpListDestination toDelete = destinations.Find(
                d => d.Path == destination.Path && d.Category == destination.Category && d.Title == destination.Title);
            if (toDelete != null)
                destinations.Remove(toDelete);
        }

        internal void RefreshDestinations(ICustomDestinationList destinationList)
        {
            if (_categorizedDestinations.Count == 0)
                return;

            foreach (int key in _categorizedDestinations.Keys)
            {
                IObjectCollection categoryContents =
                    (IObjectCollection)new CEnumerableObjectCollection();
                var destinations = _categorizedDestinations[key];
                foreach (IJumpListDestination destination in destinations)
                {
                   HResult addObjectResult = categoryContents.AddObject(destination.GetShellRepresentation());
                   addObjectResult.ThrowIf();
                }

                HResult appendCategoryResult = destinationList.AppendCategory(
                    destinations.First().Category, (IObjectArray)categoryContents);
                appendCategoryResult.ThrowIf();
            }
        }

        public IEnumerable<string> Categories
        {
            get
            {
                return
                    (from d in _categorizedDestinations.Keys
                     select _categorizedDestinations[d].First().Category);
            }
        }
        public IEnumerable<IJumpListDestination> GetDestinationsByCategory(
            string category)
        {
            return
                (from k in _categorizedDestinations.Keys
                 let d = _categorizedDestinations[k]
                 where d.First().Category == category
                 select d).Single();
        }

        public void Clear()
        {
            _categorizedDestinations.Clear();
        }
    }




    internal sealed class JumpListTasks
    {
        private List<IJumpListTask> _tasks = new List<IJumpListTask>();

        public void AddTask(IJumpListTask task)
        {
            _tasks.Add(task);
        }

        public void DeleteTask(IJumpListTask task)
        {
            IJumpListTask toDelete = _tasks.Find(t => t.Path == task.Path && t.Arguments == task.Arguments);
            if (toDelete != null)
                _tasks.Remove(toDelete);
        }

        internal void RefreshTasks(ICustomDestinationList destinationList)
        {
            if (_tasks.Count == 0)
                return;

            IObjectCollection taskCollection = (IObjectCollection)new CEnumerableObjectCollection();
            foreach (IJumpListTask task in _tasks)
            {
                HResult addObjectResult = taskCollection.AddObject(task.GetShellRepresentation());
                addObjectResult.ThrowIf();
            }
            HResult addUserTasksResult = destinationList.AddUserTasks((IObjectArray)taskCollection);
            addUserTasksResult.ThrowIf();
        }

        public IEnumerable<IJumpListTask> Tasks
        {
            get { return _tasks; }
        }

        public void Clear()
        {
            _tasks.Clear();
        }
    }





    public interface IJumpListShellObject
    {



        string Title { get; }



        string Path { get; }






        object GetShellRepresentation();
    }




    public interface IJumpListDestination : IJumpListShellObject
    {



        string Category { get; }
    }




    public interface IJumpListTask : IJumpListShellObject
    {



        string Arguments { get; }
    }




    [Flags]
    public enum WindowShowCommand : uint
    {



        Hide = 0,




        Normal = 1,



        Minimized = 2,



        Maximized = 3,




        ShowNoActivate = 4,




        Show = 5,



        Minimize = 6,



        ShowMinimizedNoActivate = 7,




        ShowNA = 8,




        Restore = 9,




        Default = 10,





        ForceMinimize = 11
    }





    public sealed class Separator : IJumpListTask
    {
        string IJumpListTask.Arguments
        {
            get { throw new NotImplementedException(); }
        }

        string IJumpListShellObject.Title
        {
            get { throw new NotImplementedException(); }
        }

        string IJumpListShellObject.Path
        {
            get { throw new NotImplementedException(); }
        }

        object IJumpListShellObject.GetShellRepresentation()
        {
            ShellLink shellLink = new ShellLink()
            {
                IsSeparator = true
            };
            return shellLink.GetShellRepresentation();
        }
    }




    public sealed class ShellLink : IJumpListTask, IJumpListDestination
    {



        public string Title { get; set; }



        public string Category { get; set; }




        public string Path { get; set; }



        public string IconLocation { get; set; }




        public int IconIndex { get; set; }




        public string Arguments { get; set; }



        public string WorkingDirectory { get; set; }




        public WindowShowCommand ShowCommand { get; set; }






        internal bool IsSeparator { get; set; }






        public object GetShellRepresentation()
        {
            IShellLinkW shellLink = (IShellLinkW)new CShellLink();
            IPropertyStore propertyStore = (IPropertyStore)shellLink;
            PropVariant propVariant = new PropVariant();

            if (IsSeparator)
            {
                propVariant.SetValue(true);

                HResult setValueResult = propertyStore.SetValue(ref PropertyKey.PKEY_AppUserModel_IsDestListSeparator, ref propVariant);
                setValueResult.ThrowIf();

                propVariant.Clear();
                propVariant.Dispose();
            }
            else
            {
                HResult setPathResult = shellLink.SetPath(Path);
                setPathResult.ThrowIf();

                if (!String.IsNullOrEmpty(IconLocation))
                {
                    HResult setIconLocationResult = shellLink.SetIconLocation(IconLocation, IconIndex);
                    setIconLocationResult.ThrowIf();
                }
                if (!String.IsNullOrEmpty(Arguments))
                {
                    HResult setArgumentsResult = shellLink.SetArguments(Arguments);
                    setArgumentsResult.ThrowIf();
                }
                if (!String.IsNullOrEmpty(WorkingDirectory))
                {
                    HResult setWorkingDirectoryResult = shellLink.SetWorkingDirectory(WorkingDirectory);
                    setWorkingDirectoryResult.ThrowIf();
                }

                HResult setShowCmdResult = shellLink.SetShowCmd((uint)ShowCommand);
                setShowCmdResult.ThrowIf();

                propVariant.SetValue(Title);

                HResult setValueResult = propertyStore.SetValue(ref PropertyKey.PKEY_Title, ref propVariant);
                setValueResult.ThrowIf();

                propVariant.Clear();
                propVariant.Dispose();
            }

            HResult commitResult = propertyStore.Commit();
            commitResult.ThrowIf();



            return shellLink;
        }
    }




    public sealed class ShellItem : IJumpListDestination
    {
        string IJumpListShellObject.Title { get { return null; } }




        public string Category { get; set; }




        public string Path { get; set; }






        public object GetShellRepresentation()
        {
            return GetShellItemFromPath(Path);
        }

        internal static IShellItem GetShellItemFromPath(string path)
        {
            if (String.IsNullOrEmpty(path))
                throw new ArgumentNullException(
                "path", "Shell item cannot be generated from null or empty path.");

            IShellItem resultItem = default(IShellItem);
            Guid shellItemGuid = new Guid("43826D1E-E718-42EE-BC55-A1E261C37BFE");
            HResult result = UnsafeNativeMethods.SHCreateItemFromParsingName(
                path, IntPtr.Zero, ref shellItemGuid, out resultItem);
            result.ThrowIf();

            return resultItem;
        }
    }
}
