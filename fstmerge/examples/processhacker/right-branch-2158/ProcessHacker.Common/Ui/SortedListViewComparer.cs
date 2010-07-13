

using System;
using System.Collections;
using System.Collections.Generic;
using System.Windows.Forms;
using System.Globalization;

namespace ProcessHacker.Common.Ui
{
    public interface ISortedListViewComparer
    {
        int Compare(ListViewItem x, ListViewItem y, int column);
    }







    public class SortedListViewComparer : IComparer
    {
        private class DefaultComparer : ISortedListViewComparer
        {
            private SortedListViewComparer _sortedListComparer;

            public DefaultComparer(SortedListViewComparer sortedListComparer)
            {
                _sortedListComparer = sortedListComparer;
            }

            public int Compare(ListViewItem x, ListViewItem y, int column)
            {
                string sx, sy;
                long ix, iy;
                IComparable cx, cy;

                sx = x.SubItems[column].Text.Replace(",", "");
                sy = y.SubItems[column].Text.Replace(",", "");

                if (!long.TryParse(sx.StartsWith("0x") ? sx.Substring(2) : sx,
                    sx.StartsWith("0x") ? NumberStyles.AllowHexSpecifier : 0,
                    null, out ix) ||
                    !long.TryParse(sy.StartsWith("0x") ? sy.Substring(2) : sy,
                    sy.StartsWith("0x") ? NumberStyles.AllowHexSpecifier : 0,
                    null, out iy))
                {
                    cx = x.SubItems[column].Text;
                    cy = y.SubItems[column].Text;
                }
                else
                {
                    cx = ix;
                    cy = iy;
                }

                return cx.CompareTo(cy);
            }
        }

        private ListView _list;
        private bool _virtualMode = false;
        private RetrieveVirtualItemEventHandler _retrieveVirtualItem;
        private bool _triState = false;
        private ISortedListViewComparer _comparer;
        private ISortedListViewComparer _triStateComparer;
        private int _sortColumn;
        private SortOrder _sortOrder;
        private Dictionary<int, Comparison<ListViewItem> > _customSorters =
            new Dictionary<int, Comparison<ListViewItem> >();
        private List<int> _columnSortOrder = new List<int>();





        public SortedListViewComparer(ListView list)
        {
            _list = list;
            _list.ColumnClick += new ColumnClickEventHandler(list_ColumnClick);
            _sortColumn = 0;
            _sortOrder = SortOrder.Ascending;
            _comparer = new DefaultComparer(this);
            this.SetSortIcon();
        }





        public bool VirtualMode
        {
            get { return _virtualMode; }
            set { _virtualMode = value; }
        }

        public RetrieveVirtualItemEventHandler RetrieveVirtualItem
        {
            get { return _retrieveVirtualItem; }
            set { _retrieveVirtualItem = value; }
        }






        public bool TriState
        {
            get { return _triState; }
            set { _triState = value; }
        }





        public ISortedListViewComparer Comparer
        {
            get { return _comparer; }
            set
            {
                if (value == null)
                    _comparer = new DefaultComparer(this);
                else
                    _comparer = value;
            }
        }




        public ISortedListViewComparer TriStateComparer
        {
            get { return _triStateComparer; }
            set { _triStateComparer = value; }
        }

        public ListView ListView
        {
            get { return _list; }
        }




        public int SortColumn
        {
            get { return _sortColumn; }
            set
            {
                _sortColumn = value;
                this.SetSortIcon();
            }
        }




        public SortOrder SortOrder
        {
            get { return _sortOrder; }
            set
            {
                _sortOrder = value;
                this.SetSortIcon();
            }
        }




        public IDictionary<int, Comparison<ListViewItem> > CustomSorters
        {
            get { return _customSorters; }
        }

        public IList<int> ColumnSortOrder
        {
            get { return _columnSortOrder; }
        }

        private void list_ColumnClick(object sender, ColumnClickEventArgs e)
        {
            if (e.Column == _sortColumn)
            {
                if (_triState)
                {
                    if (_sortOrder == SortOrder.Ascending)
                        _sortOrder = SortOrder.Descending;
                    else if (_sortOrder == SortOrder.Descending)
                        _sortOrder = SortOrder.None;
                    else
                        _sortOrder = SortOrder.Ascending;
                }
                else
                {
                    _sortOrder = _sortOrder == SortOrder.Ascending ? SortOrder.Descending : SortOrder.Ascending;
                }
            }
            else
            {
                _sortColumn = e.Column;
                _sortOrder = SortOrder.Ascending;
            }

            this.SetSortIcon();

            if (!_virtualMode)
                _list.Sort();
        }

        private void SetSortIcon()
        {



            _list.DoDelayed((control) => _list.Columns[_sortColumn].SetSortIcon(_sortOrder));
        }

        private ListViewItem GetItem(int index)
        {
            if (_virtualMode)
            {
                var args = new RetrieveVirtualItemEventArgs(index);
                _retrieveVirtualItem(this, args);
                return args.Item;
            }
            else
            {
                return _list.Items[index];
            }
        }

        private int ModifySort(int result, SortOrder order)
        {
            if (order == SortOrder.Ascending)
                return result;
            else if (order == SortOrder.Descending)
                return -result;
            else
                return result;
        }

        private int Compare(ListViewItem x, ListViewItem y, int column)
        {
            int result = 0;

            if (_triState && _sortOrder == SortOrder.None)
                result = _triStateComparer.Compare(x, y, column);

            if (result != 0)
                return result;

            if (_customSorters.ContainsKey(column))
                result = ModifySort(_customSorters[column](x, y), _sortOrder);

            if (result != 0)
                return result;

            return ModifySort(_comparer.Compare(x, y, column), _sortOrder);
        }

        public int Compare(ListViewItem x, ListViewItem y)
        {
            int result = this.Compare(x, y, _sortColumn);

            if (result != 0)
                return result;

            foreach (int column in _columnSortOrder)
            {
                if (column == _sortColumn)
                    continue;

                result = this.Compare(x, y, column);

                if (result != 0)
                    return result;
            }

            return 0;
        }







        public int Compare(object x, object y)
        {
            return this.Compare(x as ListViewItem, y as ListViewItem);
        }
    }
}
