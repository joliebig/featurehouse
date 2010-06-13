// ****************************************************************************
// 
// Copyright (C) 2005-2009  Doom9 & al
// 
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
// 
// ****************************************************************************

using System;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.Drawing;
using System.Globalization;
using System.IO;
using System.Reflection;
using System.Text;
using System.Windows.Forms;
using System.Xml.Serialization;

namespace MeGUI.core.util
{
    public delegate R Delegate<R>();
    public delegate R Delegate<R, P1>(P1 p1);
    public delegate R Delegate<R, P1, P2>(P1 p1, P2 p2);
    public delegate R Delegate<R, P1, P2, P3>(P1 p1, P2 p2, P3 p3);
    public delegate R Delegate<R, P1, P2, P3, P4>(P1 p1, P2 P2, P3 p3);

    public class Pair<T1, T2>
    {
        public T1 fst;
        public T2 snd;
        public Pair(T1 f, T2 s)
        {
            fst = f;
            snd = s;
        }

        public Pair() { }
    }

    public delegate T Getter<T>();
    public delegate void Setter<T>(T thing);

    public delegate void Action();

    public class Util
    {
        public static void CatchAndTellUser<TException>(string processDescription, Action action)
            where TException : Exception
        {
            try { action(); }
            catch (TException e)
            {
                MessageBox.Show(processDescription + ". Error message: " + e.Message, "Error occurred", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        public static void CatchAndTellUser<TException>(Action action)
            where TException : Exception
        {
            CatchAndTellUser<TException>("An error occurred", action);
        }

        public static void CatchExceptionsAndTellUser(string processDescription, Action action)
        {
            CatchAndTellUser<Exception>(processDescription, action);
        }

        public static void CatchExceptionsAndTellUser(Action action)
        {
            CatchAndTellUser<Exception>(action);
        }

        public static void ThreadSafeRun(Control c, MethodInvoker m)
        {
            if (c != null && c.InvokeRequired)
                c.Invoke(m);
            else
                m();
        }

        public static void XmlSerialize(object o, string path)
        {
            FileUtil.ensureDirectoryExists(Path.GetDirectoryName(path));
            XmlSerializer ser = new XmlSerializer(o.GetType());
            using (Stream s = File.Open(path, System.IO.FileMode.Create, System.IO.FileAccess.Write))
            {
                try
                {
                    ser.Serialize(s, o);
                }
                catch (Exception e)
                {
                    s.Close();
                    try
                    {
                        File.Delete(path);
                    }
                    catch (IOException) { }
                    Console.Write(e.Message);
                }
            }
        }

        public static T XmlDeserializeOrDefault<T>(string path)
            where T : class, new()
        {
            XmlSerializer ser = new XmlSerializer(typeof(T));
            if (File.Exists(path))
            {
                using (Stream s = File.OpenRead(path))
                {
                    try
                    {
                        return (T)ser.Deserialize(s);
                    }
                    catch (Exception e)
                    {
                        s.Close();
                        MessageBox.Show("File '" + path + "' could not be loaded!\n\nIt will be moved to the backup directory.", "Error loading File", MessageBoxButtons.OK, MessageBoxIcon.Error);
                        FileUtil.BackupFile(path, true);
                        Console.Write(e.Message);
                        return null;
                    }
                }
            }
            else return new T();
        }

        public static object XmlDeserialize(string path, Type t)
        {
            MethodInfo ms = (MethodInfo)Array.Find(typeof(Util).GetMember("XmlDeserialize"),
                delegate(MemberInfo m) { return (m is MethodInfo) && (m as MethodInfo).IsGenericMethod; });
            ms = ms.MakeGenericMethod(t);
            return ms.Invoke(null, new object[] { path });
        }

        public static T XmlDeserialize<T>(string path)
            where T : class
        {
            XmlSerializer ser = new XmlSerializer(typeof(T));
            if (File.Exists(path))
            {
                using (Stream s = File.OpenRead(path))
                {
                    try
                    {
                        return (T)ser.Deserialize(s);
                    }
                    catch (Exception e)
                    {
                        s.Close();
                        MessageBox.Show("File '" + path + "' could not be loaded!\n\nIt will be moved to the backup directory.", "Error loading File", MessageBoxButtons.OK, MessageBoxIcon.Error);
                        FileUtil.BackupFile(path, true);
                        Console.Write(e.Message);
                        return null;
                    }
                }
            }
            else return null;
        }


        private static readonly System.Text.RegularExpressions.Regex _cleanUpStringRegex = new System.Text.RegularExpressions.Regex(@"\n[^\n]+\r", System.Text.RegularExpressions.RegexOptions.Compiled | System.Text.RegularExpressions.RegexOptions.CultureInvariant);
        public static string cleanUpString(string s)
        {
            return _cleanUpStringRegex.Replace(s.Replace(Environment.NewLine, "\n"), Environment.NewLine);
        }

        public static void ensureExists(string file)
        {
            if (file == null || !System.IO.File.Exists(file))
                throw new MissingFileException(file);
        }

        public static void ensureExistsIfNeeded(string file)
        {
            if (!string.IsNullOrEmpty(file)) ensureExists(file);
        }

        public static string ToString(TimeSpan? t1)
        {
            if (!t1.HasValue) return null;
            TimeSpan t = t1.Value;
            return (new TimeSpan(t.Hours, t.Minutes, t.Seconds)).ToString();
        }

        /// <summary>
        /// Formats the decimal according to what looks nice in MeGUI (ensures consistency
        /// and not too many decimal places)
        /// </summary>
        /// <param name="d"></param>
        /// <returns></returns>
        public static string ToString(decimal? d)
        {
            if (!d.HasValue) return null;
            return d.Value.ToString("#####.##");
        }

        public static string ToStringOrNull<T>(T t)
            where T : class
        {
            if (t == null) return null;
            return t.ToString();
        }

        public static string ToStringOrNull<T>(T? t)
            where T : struct
        {
            if (t == null) return null;
            return t.Value.ToString();
        }
        public static int CountStrings(string src, char find)
        {
            int ret = 0;
            foreach (char s in src)
            {
                if (s == find)
                {
                    ++ret;
                }
            }
            return ret;
        }

        public static T ByID<T>(IEnumerable<T> i, string id)
            where T : IIDable
        {
            foreach (T t in i)
                if (t.ID == id)
                    return t;

            return default(T);
        }

        public static List<T> Unique<T>(List<T> l, Delegate<bool, T, T> cmp)
            where T : class
        {
            for (int i = 0; i < l.Count; ++i)
            {
                if (l.FindIndex(0, i, delegate(T t) { return cmp(t, l[i]); }) >= 0)
                {
                    l.RemoveAt(i);
                    --i;
                }
            }
            return l.FindAll(delegate(T t) { 
                return l.FindIndex(delegate(T t2) { return cmp(t, t2); }) == l.IndexOf(t); });
        }

        public static List<T> Unique<T>(List<T> l)
            where T : class
        {
            return Unique(l, delegate(T t1, T t2) { return t1.Equals(t2); });
        }

        public static List<T> UniqueByIDs<T>(List<T> l)
            where T : class, IIDable
        {
            return Unique(l, delegate(T t1, T t2) { return t1.ID == t2.ID; });
        }

        public static List<T> RemoveDuds<T>(List<T> ps)
            where T : class, IIDable
        {
            ps = ps.FindAll(delegate(T p) { return p != null; });

            // eliminate duplicate names
            return Util.UniqueByIDs(ps);
        }


        public static IEnumerable<To> CastAll<To>(IEnumerable i)
        {
            foreach (To t in i)
                yield return t;
        }


        public static IEnumerable<To> CastAll<From, To>(IEnumerable<From> i)
            where To : class
        {
            foreach (From f in i)
                yield return f as To;
        }

        public static List<To> CastAll<From, To>(List<From> i)
            where To : class
        {
            return i.ConvertAll<To>(delegate(From f) { return f as To; });
        }

        public static To[] CastAll<From, To>(From[] fr)
            where To : class
        {
            if (fr == null)
                return null;
            return Array.ConvertAll<From, To>(fr, delegate(From f) { return f as To; });
        }

        public static To[] CastAll<To>(object[] os)
        {
            return Array.ConvertAll<object, To>(os, delegate(object o) { return (To)o; });
        }

        public static void RegisterTypeConverter<T, TC>() where TC : TypeConverter
        {
            Attribute[] attr = new Attribute[1];
            TypeConverterAttribute vConv = new TypeConverterAttribute(typeof(TC));
            attr[0] = vConv;
            TypeDescriptor.AddAttributes(typeof(T), attr);
        }

        public static T[] ToArray<T>(IEnumerable<T> i)
        {
            return new List<T>(i).ToArray();
        }

        public static object[] ToArray(IEnumerable i)
        {
            List<object> l = new List<object>();
            foreach (object o in i)
                l.Add(o);
            return l.ToArray();
        }

        public static IEnumerable<T> Append<T>(params IEnumerable<T>[] lists)
        {
            foreach (IEnumerable<T> list in lists)
                foreach (T t in list)
                    yield return t;
        }

        public static IEnumerable<TOut> ConvertAll<TIn, TOut>(IEnumerable<TIn> input, Converter<TIn, TOut> c)
        {
            foreach (TIn t in input)
                yield return c(t);
        }

        
        public static void clampedSet(NumericUpDown box, decimal value)
        {
            box.Value = clamp(value, box.Minimum, box.Maximum);
        }

        public static decimal clamp(decimal val, decimal min, decimal max)
        {
            Debug.Assert(max >= min);
            if (val < min) return min;
            if (val > max) return max;
            return val;
        }

        public static int clamp(int val, int min, int max)
        {
            return (int)clamp((decimal)val, min, max);
        }

        public static uint clamp(uint val, uint min, uint max)
        {
            return (uint)clamp((decimal)val, min, max);
        }

        public static ulong clamp(ulong val, ulong min, ulong max)
        {
            return (ulong)clamp((decimal)val, min, max);
        }

        public static long clamp(long val, long min, long max)
        {
            return (long)clamp((decimal)val, min, max);
        }

        public static ulong clamp(long val, ulong min, ulong max)
        {
            return (ulong)clamp((decimal)val, min, max);
        }

        public static ulong clampPositive(long val)
        {
            if (val < 0) return 0u;
            return (ulong)val;
        }
        public static uint clampPositive(int val)
        {
            return (uint)clampPositive((long)val);
        }
        


        public static void ChangeItemsKeepingSelectedSame<T>(ComboBox box, T[] newItems)
        {
            T sel = (T)box.SelectedItem;
            if (Array.IndexOf(newItems, sel) < 0 || sel == null)
                sel = newItems[0];

            box.Items.Clear();
            box.Items.AddRange(Util.CastAll<T, object>(newItems));
            box.SelectedItem = sel;
        }

        
        /// <summary>
        /// convers a timecode to a framenumber
        /// </summary>
        /// <param name="timeCode">position in the movie in milliseconds</param>
        /// <param name="framerate">framerate of the movie</param>
        /// <returns>the frame corresponding to the timecode</returns>
        public static int convertTimecodeToFrameNumber(int timeCode, double framerate)
        {
            double millisecondsPerFrame = (double)(1000 / framerate);
            double frameNumber = (double)timeCode / millisecondsPerFrame;
            return (int)frameNumber;

        }
        /// <summary>
        /// converts a framenumber into a chapter format compatible timecode given the framerate of the video
        /// </summary>
        /// <param name="frameNumber">the position of the video</param>
        /// <param name="framerate">the framerate of the video</param>
        /// <returns>the chapter compatible timecode</returns>
        public static string converFrameNumberToTimecode(int frameNumber, double framerate)
        {
            double millisecondsPerFrame = (double)(1000 / framerate);
            int milliseconds = (int)(millisecondsPerFrame * (double)frameNumber);
            int hours = milliseconds / (3600 * 1000);
            milliseconds -= hours * 3600 * 1000;
            int minutes = milliseconds / (60 * 1000);
            milliseconds -= minutes * 60 * 1000;
            int seconds = milliseconds / 1000;
            milliseconds -= seconds * 1000;
            string retval = "";
            if (hours < 10)
                retval += "0";
            retval += hours + ":";
            if (minutes < 10)
                retval += "0";
            retval += minutes + ":";
            if (seconds < 10)
                retval += "0";
            retval += seconds + ".";
            if (milliseconds < 100)
                retval += "0";
            if (milliseconds < 10)
                retval += "0";
            retval += milliseconds;
            return retval;
        }
        /// <summary>
        /// converts a string timecode into number of milliseconds
        /// </summary>
        /// <param name="timecode">the time string to be analyzed</param>
        /// <returns>the time in milliseconds from the string or -1 if there was an error parsing</returns>
        public static int getTimeCode(string timecode)
        {
            if (timecode.Equals(""))
                return -1;
            else if (timecode.Length == 12) // must be 12 chars
            {
                char[] separator = new char[] { ':' };
                string[] subItems = timecode.Split(separator);
                if (subItems.Length == 3)
                {
                    int hours, minutes, seconds, milliseconds;
                    try
                    {
                        hours = Int32.Parse(subItems[0]);
                        minutes = Int32.Parse(subItems[1]);
                        separator = new char[] { '.' };
                        string[] str = subItems[2].Split(separator);
                        if (str.Length == 2)
                        {
                            seconds = Int32.Parse(str[0]);
                            milliseconds = Int32.Parse(str[1]);
                        }
                        else
                            return -1;
                        if (hours > 24 || minutes > 59 || seconds > 59)
                            return -1;
                        int retval = milliseconds + seconds * 1000 + minutes * 60 * 1000 + hours * 60 * 60 * 1000;
                        return retval;
                    }
                    catch (Exception e) // integer parsing error
                    {
                        Console.Write(e.Message);
                        return -1;
                    }
                }
                else
                    return -1;
            }
            else // incorrect length
                return -1;
        }
        
    }

    public delegate TOut Converter<TIn, TOut>(TIn iinput);
}
