

using System;
using System.Windows.Forms;
using System.Collections.Generic;
using System.Collections;
using System.Text;
using Aga.Controls.Tree;
using Aga.Controls.Tree.NodeControls;

namespace ProcessHacker.UI
{



    public static class ColumnSettings
    {





        public static string SaveSettings(ListView lv)
        {
            StringBuilder result = new StringBuilder();

            try
            {
                foreach (ColumnHeader ch in lv.Columns)
                {
                    result.Append(ch.DisplayIndex.ToString() + "," + ch.Width.ToString() + "|");
                }
            }
            catch
            { }

            if (result.Length > 0)
                result.Remove(result.Length - 1, 1);

            return result.ToString();
        }






        public static string SaveSettings(TreeViewAdv tv)
        {
            StringBuilder result = new StringBuilder();

            try
            {
                for (int i = 0; i < tv.Columns.Count; i++)
                {
                    TreeColumn c = tv.Columns[i];
                    result.Append(c.Header + "," + c.Width.ToString() + "," + c.SortOrder.ToString() +
                        "," + c.IsVisible.ToString() + "|");
                }
            }
            catch
            { }

            if (result.Length > 0)
                result.Remove(result.Length - 1, 1);

            return result.ToString();
        }






        public static void LoadSettings(string settings, ListView lv)
        {
            if (settings.EndsWith("|"))
                settings = settings.Remove(settings.Length - 1, 1);

            string[] list = settings.Split('|');

            if (settings == "")
                return;


            if (list.Length != lv.Columns.Count)
                return;

            for (int i = 0; i < list.Length; i++)
            {
                string[] s = list[i].Split(',');

                if (s.Length != 2)
                    break;

                lv.Columns[i].DisplayIndex = Int32.Parse(s[0]);
                lv.Columns[i].Width = Int32.Parse(s[1]);
            }
        }






        public static void LoadSettings(string settings, TreeViewAdv tv)
        {
            if (settings.EndsWith("|"))
                settings = settings.Remove(settings.Length - 1, 1);

            string[] list = settings.Split('|');

            try
            {
                Dictionary<NodeControl, string> oldAssoc = new Dictionary<NodeControl, string>();

                foreach (NodeControl control in tv.NodeControls)
                {
                    oldAssoc.Add(control, control.ParentColumn.Header);
                }

                TreeColumn[] newColumns = new TreeColumn[tv.Columns.Count];
                Dictionary<string, TreeColumn> newColumnsD = new Dictionary<string,TreeColumn>();

                for (int i = 0; i < tv.Columns.Count; i++)
                {
                    string[] s = list[i].Split(',');

                    newColumns[i] = new TreeColumn(s[0], Int32.Parse(s[1]));
                    newColumns[i].SortOrder = (SortOrder)Enum.Parse(typeof(SortOrder), s[2]);
                    newColumns[i].IsVisible = bool.Parse(s[3]);
                    newColumns[i].MinColumnWidth = 3;
                    newColumnsD.Add(s[0], newColumns[i]);
                }

                tv.Columns.Clear();

                foreach (TreeColumn column in newColumns)
                    tv.Columns.Add(column);

                foreach (NodeControl c in oldAssoc.Keys)
                    c.ParentColumn = newColumnsD[oldAssoc[c]];
            }
            catch
            { }
        }
    }
}
