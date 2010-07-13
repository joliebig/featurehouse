




using System;
using System.Collections.Generic;
using System.Windows.Forms;
using Aga.Controls.Tree;
using ProcessHacker.Common;
using ProcessHacker.Native;

namespace ProcessHacker
{



    public class ProcessTreeModel : ITreeModel
    {
        private ProcessTree _tree;
        private Dictionary<int, ProcessNode> _processes = new Dictionary<int, ProcessNode>();
        private List<ProcessNode> _roots = new List<ProcessNode>();

        public ProcessTreeModel(ProcessTree tree)
        {
            _tree = tree;
        }

        public void Add(ProcessItem item)
        {
            ProcessNode itemNode = new ProcessNode(item);


            _processes.Add(item.Pid, itemNode);


            if (item.HasParent && _processes.ContainsKey(item.ParentPid))
            {
                ProcessNode parent = _processes[item.ParentPid];

                parent.Children.Add(itemNode);
                itemNode.Parent = parent;
            }
            else
            {

                _roots.Add(itemNode);
            }

            itemNode.RefreshTreePath();





            ProcessNode[] roots = _roots.ToArray();

            foreach (ProcessNode node in roots)
            {



                if (node.Parent == null && node.ProcessItem.HasParent && node.PPid == item.Pid)
                {


                    _roots.Remove(node);
                    itemNode.Children.Add(node);
                    node.Parent = itemNode;
                    node.RefreshTreePathRecursive();
                }
            }

            this.StructureChanged(this, new TreePathEventArgs(new TreePath()));
        }

        public void Modify(ProcessItem oldItem, ProcessItem newItem)
        {
            ProcessNode node = _processes[newItem.Pid];

            node.ProcessItem = newItem;





        }

        public void Remove(ProcessItem item)
        {
            ProcessNode itemNode = _processes[item.Pid];
            ProcessNode[] itemChildren = null;


            itemNode.Dispose();

            itemChildren = itemNode.Children.ToArray();


            if (itemNode.Parent == null)
            {
                if (_roots.Contains(itemNode))
                {

                    _roots.Remove(itemNode);
                    this.MoveChildrenToRoot(itemNode);
                }
            }
            else
            {
                if (itemNode.Parent.Children.Contains(itemNode))
                {

                    itemNode.Parent.Children.Remove(itemNode);
                    this.MoveChildrenToRoot(itemNode);
                }
            }


            _processes.Remove(item.Pid);
            this.StructureChanged(this, new TreePathEventArgs(new TreePath()));


            if (itemChildren != null)
            {
                foreach (ProcessNode n in itemChildren)
                {
                    try
                    {
                        _tree.FindTreeNode(n).ExpandAll();
                    }
                    catch (Exception ex)
                    {
                        Logging.Log(ex);
                    }
                }
            }

            _tree.Invalidate();
        }

        public TreePath GetPath(ProcessNode node)
        {
            if (node == null)
                return TreePath.Empty;

            if (this.GetSortColumn() != "")
            {
                return new TreePath(node);
            }
            else
            {
                return node.TreePath;
            }
        }

        public void MoveChildrenToRoot(ProcessNode node)
        {
            ProcessNode[] children = node.Children.ToArray();

            foreach (ProcessNode child in children)
            {
                child.Parent = null;
                child.RefreshTreePathRecursive();
            }

            _roots.AddRange(children);
        }

        public Dictionary<int, ProcessNode> Nodes
        {
            get { return _processes; }
        }

        public ProcessNode[] Roots
        {
            get { return _roots.ToArray(); }
        }

        public string GetSortColumn()
        {
            foreach (TreeColumn column in _tree.Tree.Columns)
                if (column.SortOrder != SortOrder.None)
                    return column.Header.ToLower();

            return "";
        }

        public SortOrder GetSortOrder()
        {
            foreach (TreeColumn column in _tree.Tree.Columns)
                if (column.SortOrder != SortOrder.None)
                    return column.SortOrder;

            return SortOrder.None;
        }

        public int ModifySort(int sortResult, SortOrder order)
        {
            if (order == SortOrder.Ascending)
                return -sortResult;
            else if (order == SortOrder.Descending)
                return sortResult;
            else
                return 0;
        }

        public System.Collections.IEnumerable GetChildren(TreePath treePath)
        {
            if (this.GetSortColumn() != "")
            {
                List<ProcessNode> nodes = new List<ProcessNode>();
                string sortC = this.GetSortColumn();
                SortOrder sortO = this.GetSortOrder();

                nodes.AddRange(_processes.Values);

                nodes.Sort(new Comparison<ProcessNode>(delegate(ProcessNode n1, ProcessNode n2)
                    {






                        if (n1 == n2)
                            return 0;

                        switch (sortC)
                        {
                            case "name":
                                return ModifySort(n1.Name.CompareTo(n2.Name), sortO);
                            case "pid":
                                return ModifySort(n1.Pid.CompareTo(n2.Pid), sortO);
                            case "pvt. memory":
                                return ModifySort(n1.ProcessItem.Process.VirtualMemoryCounters.PrivatePageCount.CompareTo(
                                    n2.ProcessItem.Process.VirtualMemoryCounters.PrivatePageCount), sortO);
                            case "working set":
                                return ModifySort(n1.ProcessItem.Process.VirtualMemoryCounters.WorkingSetSize.CompareTo(
                                    n2.ProcessItem.Process.VirtualMemoryCounters.WorkingSetSize), sortO);
                            case "peak working set":
                                return ModifySort(n1.ProcessItem.Process.VirtualMemoryCounters.PeakWorkingSetSize.CompareTo(
                                    n2.ProcessItem.Process.VirtualMemoryCounters.PeakWorkingSetSize), sortO);
                            case "private ws":
                                return ModifySort(n1.PrivateWorkingSetNumber.CompareTo(n2.PrivateWorkingSetNumber), sortO);
                            case "shared ws":
                                return ModifySort(n1.SharedWorkingSetNumber.CompareTo(n2.SharedWorkingSetNumber), sortO);
                            case "shareable ws":
                                return ModifySort(n1.ShareableWorkingSetNumber.CompareTo(n2.ShareableWorkingSetNumber), sortO);
                            case "virtual size":
                                return ModifySort(n1.ProcessItem.Process.VirtualMemoryCounters.VirtualSize.CompareTo(
                                    n2.ProcessItem.Process.VirtualMemoryCounters.VirtualSize), sortO);
                            case "peak virtual size":
                                return ModifySort(n1.ProcessItem.Process.VirtualMemoryCounters.PeakVirtualSize.CompareTo(
                                    n2.ProcessItem.Process.VirtualMemoryCounters.PeakVirtualSize), sortO);
                            case "pagefile usage":
                                return ModifySort(n1.ProcessItem.Process.VirtualMemoryCounters.PagefileUsage.CompareTo(
                                    n2.ProcessItem.Process.VirtualMemoryCounters.PagefileUsage), sortO);
                            case "peak pagefile usage":
                                return ModifySort(n1.ProcessItem.Process.VirtualMemoryCounters.PeakPagefileUsage.CompareTo(
                                    n2.ProcessItem.Process.VirtualMemoryCounters.PeakPagefileUsage), sortO);
                            case "page faults":
                                return ModifySort(n1.ProcessItem.Process.VirtualMemoryCounters.PageFaultCount.CompareTo(
                                    n2.ProcessItem.Process.VirtualMemoryCounters.PageFaultCount), sortO);
                            case "cpu":
                                return ModifySort(n1.ProcessItem.CpuUsage.CompareTo(n2.ProcessItem.CpuUsage), sortO);
                            case "username":
                                return ModifySort(n1.Username.CompareTo(n2.Username), sortO);
                            case "session id":
                                return ModifySort(n1.ProcessItem.SessionId.CompareTo(n2.ProcessItem.SessionId), sortO);
                            case "priority class":
                            case "base priority":
                                return ModifySort(n1.ProcessItem.Process.BasePriority.CompareTo(
                                    n2.ProcessItem.Process.BasePriority), sortO);
                            case "description":
                                return ModifySort(n1.Description.CompareTo(n2.Description), sortO);
                            case "company":
                                return ModifySort(n1.Company.CompareTo(n2.Company), sortO);
                            case "file name":
                                return ModifySort(n1.FileName.CompareTo(n2.FileName), sortO);
                            case "command line":
                                return ModifySort(n1.CommandLine.CompareTo(n2.CommandLine), sortO);
                            case "threads":
                                return ModifySort(n1.ProcessItem.Process.NumberOfThreads.CompareTo(
                                    n2.ProcessItem.Process.NumberOfThreads), sortO);
                            case "handles":
                                return ModifySort(n1.ProcessItem.Process.HandleCount.CompareTo(
                                    n2.ProcessItem.Process.HandleCount), sortO);
                            case "gdi handles":
                                return ModifySort(n1.GdiHandlesNumber.CompareTo(n2.GdiHandlesNumber), sortO);
                            case "user handles":
                                return ModifySort(n1.UserHandlesNumber.CompareTo(n2.UserHandlesNumber), sortO);
                            case "i/o total":
                                return ModifySort(n1.IoTotalNumber.CompareTo(n2.IoTotalNumber), sortO);
                            case "i/o ro":
                                return ModifySort(n1.IoReadOtherNumber.CompareTo(n2.IoReadOtherNumber), sortO);
                            case "i/o w":
                                return ModifySort(n1.IoWriteNumber.CompareTo(n2.IoWriteNumber), sortO);
                            case "integrity":
                                return ModifySort(n1.IntegrityLevel.CompareTo(n2.IntegrityLevel), sortO);
                            case "i/o priority":
                                return ModifySort(n1.IoPriority.CompareTo(n2.IoPriority), sortO);
                            case "page priority":
                                return ModifySort(n1.PagePriority.CompareTo(n2.PagePriority), sortO);
                            case "start time":
                                return ModifySort(n1.ProcessItem.CreateTime.CompareTo(n2.ProcessItem.CreateTime), sortO);
                            case "start time (relative)":

                                return -ModifySort(n1.ProcessItem.CreateTime.CompareTo(n2.ProcessItem.CreateTime), sortO);
                            case "total cpu time":
                                return ModifySort((n1.ProcessItem.Process.KernelTime + n1.ProcessItem.Process.UserTime).
                                    CompareTo(n2.ProcessItem.Process.KernelTime + n2.ProcessItem.Process.UserTime), sortO);
                            case "kernel cpu time":
                                return ModifySort(n1.ProcessItem.Process.KernelTime.CompareTo(
                                    n2.ProcessItem.Process.KernelTime), sortO);
                            case "user cpu time":
                                return ModifySort(n1.ProcessItem.Process.UserTime.CompareTo(
                                    n2.ProcessItem.Process.UserTime), sortO);
                            case "verification status":
                                return ModifySort(n1.VerificationStatus.CompareTo(n2.VerificationStatus), sortO);
                            default:
                                return 0;
                        }
                    }));

                return nodes;
            }

            if (treePath.IsEmpty())
                return _roots;
            else
                return (treePath.LastNode as ProcessNode).Children;
        }

        public bool IsLeaf(TreePath treePath)
        {

            if (this.GetSortColumn() != "")
                return true;

            if (treePath.IsEmpty())
                return false;
            else
                return (treePath.LastNode as ProcessNode).Children.Count == 0;
        }

        public event EventHandler<TreeModelEventArgs> NodesChanged;

        public event EventHandler<TreeModelEventArgs> NodesInserted;

        public event EventHandler<TreeModelEventArgs> NodesRemoved;

        public event EventHandler<TreePathEventArgs> StructureChanged;

        public void CallStructureChanged(TreePathEventArgs args)
        {
            this.StructureChanged(this, args);
        }
    }
}
