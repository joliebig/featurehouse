

using System;
using System.Collections.Generic;
using System.Drawing;
using System.IO;
using System.Reflection;
using System.Text;
using System.Windows.Forms;

namespace ProcessHacker.Common
{



    public static class Utils
    {
        public enum Endianness
        {
            Little, Big
        }



        public static int[] Primes =
        {
            3, 7, 11, 0x11, 0x17, 0x1d, 0x25, 0x2f, 0x3b, 0x47, 0x59, 0x6b, 0x83, 0xa3, 0xc5, 0xef,
            0x125, 0x161, 0x1af, 0x209, 0x277, 0x2f9, 0x397, 0x44f, 0x52f, 0x63d, 0x78b, 0x91d, 0xaf1,
            0xd2b, 0xfd1, 0x12fd, 0x16cf, 0x1b65, 0x20e3, 0x2777, 0x2f6f, 0x38ff, 0x446f, 0x521f, 0x628d,
            0x7655, 0x8e01, 0xaa6b, 0xcc89, 0xf583, 0x126a7, 0x1619b, 0x1a857, 0x1fd3b, 0x26315, 0x2dd67,
            0x3701b, 0x42023, 0x4f361, 0x5f0ed, 0x72125, 0x88e31, 0xa443b, 0xc51eb, 0xec8c1, 0x11bdbf,
            0x154a3f, 0x198c4f, 0x1ea867, 0x24ca19, 0x2c25c1, 0x34fa1b, 0x3f928f, 0x4c4987, 0x5b8b6f, 0x6dda89
        };

        public static string[] SizeUnitNames = { "B", "kB", "MB", "GB", "TB", "PB", "EB" };






        public static int UnitSpecifier = 4;
        public static T[] Concat<T>(params T[][] ap)
        {
            int tl = 0;
            foreach (var array in ap)
                if (array != null)
                    tl += array.Length;
            T[] na = new T[tl];
            int i = 0;
            foreach (var array in ap)
            {
                if (array != null)
                {
                    Array.Copy(array, 0, na, i, array.Length);
                    i += array.Length;
                }
            }
            return na;
        }
        public static bool Contains<T>(this T[] array, T value)
        {
            return Array.IndexOf<T>(array, value) != -1;
        }
        public static int CountBits(this int value)
        {
            int count = 0;
            while (value != 0)
            {
                count++;
                value &= value - 1;
            }
            return count;
        }
        public static int CountBits(this long value)
        {
            int count = 0;
            while (value != 0)
            {
                count++;
                value &= value - 1;
            }
            return count;
        }
        public unsafe static byte[] Create(byte* ptr, int length)
        {
            byte[] array = new byte[length];
            for (int i = 0; i < length; i++)
                array[i] = ptr[i];
            return array;
        }
        public static string CreateEllipsis(string s, int len)
        {
            if (s.Length <= len)
                return s;
            else
                return s.Substring(0, len - 4) + " ...";
        }
        public static string CreateRandomString(int length)
        {
            Random r = new Random((int)(DateTime.Now.ToFileTime() & 0xffffffff));
            StringBuilder sb = new StringBuilder(length);
            for (int i = 0; i < length; i++)
                sb.Append((char)('A' + r.Next(25)));
            return sb.ToString();
        }
        public static void DisposeAndClear(this Menu.MenuItemCollection items)
        {
            items.Clear();
        }
        public static void DisableAllMenuItems(Menu menu)
        {
            foreach (MenuItem item in menu.MenuItems)
                item.Enabled = false;
        }
        public static void DisableAll(this Menu menu)
        {
            DisableAllMenuItems(menu);
        }
        public static int DivideUp(int dividend, int divisor)
        {
            return (dividend - 1) / divisor + 1;
        }
        public static void DoDelayed(this Control control, Action<Control> action)
        {
            if (control.IsHandleCreated)
            {
                action(control);
            }
            else
            {
                LayoutEventHandler handler = null;
                handler = (sender, e) =>
                {
                    if (control.IsHandleCreated)
                    {
                        control.Layout -= handler;
                        action(control);
                    }
                };
                control.Layout += handler;
            }
        }
        public static T[] Duplicate<T>(this T[] array)
        {
            T[] newArray = new T[array.Length];
            array.CopyTo(newArray, 0);
            return newArray;
        }
        public static void EnableAllMenuItems(Menu menu)
        {
            foreach (MenuItem item in menu.MenuItems)
                item.Enabled = true;
        }
        public static void EnableAll(this Menu menu)
        {
            EnableAllMenuItems(menu);
        }
        public static bool Equals<T>(this T[] array, T[] other)
        {
            return Equals(array, other, 0);
        }
        public static bool Equals<T>(this T[] array, T[] other, int startIndex)
        {
            return Equals(array, other, startIndex, array.Length);
        }
        public static bool Equals<T>(this T[] array, T[] other, int startIndex, int length)
        {
            for (int i = startIndex; i < startIndex + length; i++)
                if (!array[i].Equals(other[i]))
                    return false;
            return true;
        }
        public static string Escape(this string str)
        {
            str = str.Replace("\\", "\\\\");
            str = str.Replace("\"", "\\\"");
            return str;
        }
        public static void Fill<T>(this T[] array, T value)
        {
            for (int i = 0; i < array.Length; i++)
                array[i] = value;
        }
        public static void Fill(this ComboBox box, Type t)
        {
            foreach (string s in Enum.GetNames(t))
                box.Items.Add(s);
        }
        public static Rectangle FitRectangle(Rectangle rect, Control c)
        {
            return FitRectangle(rect, Screen.GetWorkingArea(c));
        }
        public static Rectangle FitRectangle(Rectangle rect, Rectangle bounds)
        {
            if (rect.X < bounds.Left)
                rect.X = bounds.Left;
            if (rect.Y < bounds.Top)
                rect.Y = bounds.Top;
            if (rect.X + rect.Width > bounds.Width)
                rect.X = bounds.Width - rect.Width;
            if (rect.Y + rect.Height > bounds.Height)
                rect.Y = bounds.Height - rect.Height;
            return rect;
        }
        public static string FormatAddress(int address)
        {
            return "0x" + address.ToString("x");
        }
        public static string FormatAddress(uint address)
        {
            return "0x" + address.ToString("x");
        }
        public static string FormatAddress(long address)
        {
            return "0x" + address.ToString("x");
        }
        public static string FormatAddress(ulong address)
        {
            return "0x" + address.ToString("x");
        }
        public static string FormatAddress(IntPtr address)
        {
            return "0x" + address.ToString("x");
        }
        public static string FormatFlags(Type e, long value)
        {
            string r = "";
            for (int i = 0; i < 32; i++)
            {
                long fv = 1 << i;
                if ((value & fv) == fv)
                {
                    r += Enum.GetName(e, fv) + ", ";
                }
            }
            if (r.EndsWith(", "))
                r = r.Remove(r.Length - 2, 2);
            return r;
        }
        public static string FormatLongTimeSpan(TimeSpan time)
        {
            return String.Format(
                "{0}{1:d2}:{2:d2}:{3:d2}",
                time.Days != 0 ? (time.Days.ToString() + ".") : "",
                time.Hours,
                time.Minutes,
                time.Seconds
                );
        }
        public static string FormatRelativeDateTime(DateTime time)
        {
            TimeSpan span = DateTime.Now.Subtract(time);
            double weeks = span.TotalDays / 7;
            double fortnights = weeks / 2;
            double months = span.TotalDays * 12 / 365;
            double years = months / 12;
            double centuries = years / 100;
            string str = "";
            if (centuries >= 1)
                str = (int)centuries + " " + ((int)centuries == 1 ? "century" : "centuries");
            else if (years >= 1)
                str = (int)years + " " + ((int)years == 1 ? "year" : "years");
            else if (months >= 1)
                str = (int)months + " " + ((int)months == 1 ? "month" : "months");
            else if (fortnights >= 1)
                str = (int)fortnights + " " + ((int)fortnights == 1 ? "fortnight" : "fortnights");
            else if (weeks >= 1)
                str = (int)weeks + " " + ((int)weeks == 1 ? "week" : "weeks");
            else if (span.TotalDays >= 1)
            {
                str = (int)span.TotalDays + " " + ((int)span.TotalDays == 1 ? "day" : "days");
                if (span.Hours >= 1)
                    str += " and " + span.Hours + " " +
                        (span.Hours == 1 ? "hour" : "hours");
            }
            else if (span.Hours >= 1)
            {
                str = span.Hours + " " + (span.Hours == 1 ? "hour" : "hours");
                if (span.Minutes >= 1)
                    str += " and " + span.Minutes + " " +
                        (span.Minutes == 1 ? "minute" : "minutes");
            }
            else if (span.Minutes >= 1)
            {
                str = span.Minutes + " " + (span.Minutes == 1 ? "minute" : "minutes");
                if (span.Seconds >= 1)
                    str += " and " + span.Seconds + " " +
                        (span.Seconds == 1 ? "second" : "seconds");
            }
            else if (span.Seconds >= 1)
                str = span.Seconds + " " + (span.Seconds == 1 ? "second" : "seconds");
            else if (span.Milliseconds >= 1)
                str = span.Milliseconds + " " + (span.Milliseconds == 1 ? "millisecond" : "milliseconds");
            else
                str = "a very short time";
            if (str.StartsWith("1 "))
            {
                if (str[2] != 'h')
                    str = "a " + str.Substring(2);
                else
                    str = "an " + str.Substring(2);
            }
            return str + " ago";
        }
        public static string FormatSize(int size)
        {
            return FormatSize((uint)size);
        }
        public static string FormatSize(uint size)
        {
            int i = 0;
            double s = (double)size;
            while (s > 1024 && i < SizeUnitNames.Length && i < UnitSpecifier)
            {
                s /= 1024;
                i++;
            }
            return (s == 0 ? "0" : s.ToString("#,#.##")) + " " + SizeUnitNames[i];
        }
        public static string FormatSize(IntPtr size)
        {
            unchecked
            {
                return FormatSize((ulong)size.ToInt64());
            }
        }
        public static string FormatSize(long size)
        {
            return FormatSize((ulong)size);
        }
        public static string FormatSize(ulong size)
        {
            int i = 0;
            double s = (double)size;
            while (s > 1024 && i < SizeUnitNames.Length && i < UnitSpecifier)
            {
                s /= 1024;
                i++;
            }
            return (s == 0 ? "0" : s.ToString("#,#.##")) + " " + SizeUnitNames[i];
        }
        public static string FormatTimeSpan(TimeSpan time)
        {
            return String.Format("{0:d2}:{1:d2}:{2:d2}.{3:d3}",
                time.Hours,
                time.Minutes,
                time.Seconds,
                time.Milliseconds);
        }
        public static DateTime GetAssemblyBuildDate(Assembly assembly, bool forceFileDate)
        {
            Version AssemblyVersion = assembly.GetName().Version;
            DateTime dt;
            if (forceFileDate)
            {
                dt = GetAssemblyLastWriteTime(assembly);
            }
            else
            {
                dt = DateTime.Parse("01/01/2000").AddDays(AssemblyVersion.Build).AddSeconds(AssemblyVersion.Revision * 2);
                if (TimeZone.IsDaylightSavingTime(dt, TimeZone.CurrentTimeZone.GetDaylightChanges(dt.Year)))
                {
                    dt = dt.AddHours(1);
                }
                if (dt > DateTime.Now || AssemblyVersion.Build < 730 || AssemblyVersion.Revision == 0)
                {
                    dt = GetAssemblyLastWriteTime(assembly);
                }
            }
            return dt;
        }
        public static DateTime GetAssemblyLastWriteTime(Assembly assembly)
        {
            if (assembly.Location == null || assembly.Location == "")
                return DateTime.MaxValue;
            try
            {
                return File.GetLastWriteTime(assembly.Location);
            }
            catch
            {
                return DateTime.MaxValue;
            }
        }
        public static byte[] GetBytes(this int n)
        {
            return n.GetBytes(Endianness.Little);
        }
        public static byte[] GetBytes(this int n, Endianness type)
        {
            byte[] data = new byte[4];
            if (type == Endianness.Little)
            {
                data[0] = (byte)(n & 0xff);
                data[1] = (byte)((n >> 8) & 0xff);
                data[2] = (byte)((n >> 16) & 0xff);
                data[3] = (byte)((n >> 24) & 0xff);
            }
            else if (type == Endianness.Big)
            {
                data[0] = (byte)((n >> 24) & 0xff);
                data[1] = (byte)((n >> 16) & 0xff);
                data[2] = (byte)((n >> 8) & 0xff);
                data[3] = (byte)(n & 0xff);
            }
            else
            {
                throw new ArgumentException();
            }
            return data;
        }
        public static byte[] GetBytes(this uint n)
        {
            return n.GetBytes(Endianness.Little);
        }
        public static byte[] GetBytes(this uint n, Endianness type)
        {
            byte[] data = new byte[4];
            if (type == Endianness.Little)
            {
                data[0] = (byte)(n & 0xff);
                data[1] = (byte)((n >> 8) & 0xff);
                data[2] = (byte)((n >> 16) & 0xff);
                data[3] = (byte)((n >> 24) & 0xff);
            }
            else if (type == Endianness.Big)
            {
                data[0] = (byte)((n >> 24) & 0xff);
                data[1] = (byte)((n >> 16) & 0xff);
                data[2] = (byte)((n >> 8) & 0xff);
                data[3] = (byte)(n & 0xff);
            }
            else
            {
                throw new ArgumentException();
            }
            return data;
        }
        public static byte[] GetBytes(this ushort n)
        {
            return n.GetBytes(Endianness.Little);
        }
        public static byte[] GetBytes(this ushort n, Endianness type)
        {
            byte[] data = new byte[2];
            if (type == Endianness.Little)
            {
                data[0] = (byte)(n & 0xff);
                data[1] = (byte)((n >> 8) & 0xff);
            }
            else if (type == Endianness.Big)
            {
                data[0] = (byte)((n >> 8) & 0xff);
                data[1] = (byte)(n & 0xff);
            }
            else
            {
                throw new ArgumentException();
            }
            return data;
        }
        public static DateTime GetDateTimeFromUnixTime(uint time)
        {
            return (new DateTime(1970, 1, 1, 0, 0, 0)).Add(new TimeSpan(0, 0, 0, (int)time));
        }
        public static int GetPrime(int minimum)
        {
            if (minimum < 0)
                throw new ArgumentOutOfRangeException("minimum");
            for (int i = 0; i < Primes.Length; i++)
            {
                if (Primes[i] >= minimum)
                    return Primes[i];
            }
            for (int i = minimum | 1; i < int.MaxValue; i += 2)
            {
                if (IsPrime(i))
                    return i;
            }
            return minimum;
        }
        public static Rectangle GetRectangle(string s)
        {
            var split = s.Split(',');
            return new Rectangle(int.Parse(split[0]), int.Parse(split[1]),
                int.Parse(split[2]), int.Parse(split[3]));
        }
        public static System.Diagnostics.ProcessThread GetThreadFromId(System.Diagnostics.Process p, int id)
        {
            foreach (System.Diagnostics.ProcessThread t in p.Threads)
                if (t.Id == id)
                    return t;
            return null;
        }
        public static bool IsEmpty(this byte[] array)
        {
            foreach (byte b in array)
            {
                if (b != 0)
                    return false;
            }
            return true;
        }
        public static bool IsPrime(this int number)
        {
            int x;
            if ((number & 1) == 0)
                return number == 2;
            x = (int)Math.Sqrt(number);
            for (int i = 3; i <= x; i += 2)
            {
                if ((number % i) == 0)
                    return false;
            }
            return true;
        }
        public static char MakePrintable(char c)
        {
            if (c >= ' ' && c <= '~')
                return c;
            else
                return '.';
        }
        public static string MakePrintable(string s)
        {
            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < s.Length; i++)
                sb.Append(MakePrintable(s[i]));
            return sb.ToString();
        }
        public static bool MatchWildcards(string pattern, string text)
        {
            return MatchWildcards(pattern, 0, text, 0);
        }
        private static bool MatchWildcards(string pattern, int patternStart, string text, int textStart)
        {
            int patternIndex = patternStart;
            int textIndex = textStart;
            if (pattern.Length == 0 || patternIndex >= pattern.Length)
                return true;
            if (text.Length == 0 || textIndex >= text.Length)
                return false;
            while (true)
            {
                if (patternIndex >= pattern.Length)
                    return textIndex >= text.Length;
                if (pattern[patternIndex] == '*')
                {
                    patternIndex++;
                    while (patternIndex < pattern.Length)
                    {
                        if (pattern[patternIndex] != '*')
                            break;
                        patternIndex++;
                    }
                    break;
                }
                if (textIndex >= text.Length)
                    return false;
                if (pattern[patternIndex] != text[textIndex] && pattern[patternIndex] != '?')
                    return false;
                patternIndex++;
                textIndex++;
            }
            if (patternIndex >= pattern.Length)
                return true;
            while (textIndex < text.Length)
            {
                if (MatchWildcards(pattern, patternIndex, text, textIndex))
                    return true;
                textIndex++;
            }
            return false;
        }
        public static Dictionary<string, string> ParseCommandLine(string[] args)
        {
            Dictionary<string, string> dict = new Dictionary<string, string>();
            string argPending = null;
            foreach (string s in args)
            {
                if (s.StartsWith("-"))
                {
                    if (dict.ContainsKey(s))
                        throw new ArgumentException("Option already specified.");
                    dict.Add(s, "");
                    argPending = s;
                }
                else
                {
                    if (argPending != null)
                    {
                        dict[argPending] = s;
                        argPending = null;
                    }
                    else
                    {
                        if (!dict.ContainsKey(""))
                            dict.Add("", s);
                    }
                }
            }
            return dict;
        }
        public static int ReadInt32(Stream s, Endianness type)
        {
            byte[] buffer = new byte[4];
            if (s.Read(buffer, 0, 4) == 0)
                throw new EndOfStreamException();
            return ToInt32(buffer, type);
        }
        public static string ReadString(Stream s)
        {
            StringBuilder str = new StringBuilder();
            while (true)
            {
                int b = s.ReadByte();
                if (b == 0 || b == -1)
                    break;
                str.Append((char)(byte)b);
            }
            return str.ToString();
        }
        public static string ReadString(Stream s, int length)
        {
            byte[] buffer = new byte[length];
            if (s.Read(buffer, 0, length) == 0)
                throw new EndOfStreamException();
            return System.Text.ASCIIEncoding.ASCII.GetString(buffer);
        }
        public static uint ReadUInt32(Stream s, Endianness type)
        {
            byte[] buffer = new byte[4];
            if (s.Read(buffer, 0, 4) == 0)
                throw new EndOfStreamException();
            return ToUInt32(buffer, type);
        }
        public static string ReadUnicodeString(Stream s)
        {
            StringBuilder str = new StringBuilder();
            while (true)
            {
                int b = s.ReadByte();
                if (b == -1)
                    break;
                int b2 = s.ReadByte();
                if (b2 == -1)
                    break;
                if (b == 0 && b2 == 0)
                    break;
                str.Append(UnicodeEncoding.Unicode.GetChars(new byte[] { (byte)b, (byte)b2 }));
            }
            return str.ToString();
        }
        public static string ReadUnicodeString(Stream s, int length)
        {
            StringBuilder str = new StringBuilder();
            int i = 0;
            while (i < length)
            {
                int b = s.ReadByte();
                if (b == -1)
                    break;
                int b2 = s.ReadByte();
                if (b2 == -1)
                    break;
                str.Append(UnicodeEncoding.Unicode.GetChars(new byte[] { (byte)b, (byte)b2 }));
                i += 2;
            }
            return str.ToString();
        }
        public static int Reverse(this int v)
        {
            byte b1 = (byte)v;
            byte b2 = (byte)(v >> 8);
            byte b3 = (byte)(v >> 16);
            byte b4 = (byte)(v >> 24);
            return b4 | (b3 << 8) | (b2 << 16) | (b1 << 24);
        }
        public static uint Reverse(this uint v)
        {
            uint b0 = v & 0xff;
            uint b1 = (v >> 8) & 0xff;
            uint b2 = (v >> 16) & 0xff;
            uint b3 = (v >> 24) & 0xff;
            b0 <<= 24;
            b1 <<= 16;
            b2 <<= 8;
            return b0 | b1 | b2 | b3;
        }
        public static ushort Reverse(this ushort v)
        {
            byte b1 = (byte)v;
            byte b2 = (byte)(v >> 8);
            return (ushort)(b2 | (b1 << 8));
        }
        public static T[] Reverse<T>(this T[] data)
        {
            T[] newData = new T[data.Length];
            for (int i = 0; i < data.Length; i++)
                newData[i] = data[data.Length - i - 1];
            return newData;
        }
        public static void SelectAll(this ListView.ListViewItemCollection items)
        {
            foreach (ListViewItem item in items)
                item.Selected = true;
        }
        public static void SelectAll(this ListView items)
        {
            for (int i = 0; i < items.VirtualListSize; i++)
                if (!items.SelectedIndices.Contains(i))
                    items.SelectedIndices.Add(i);
        }
        public static void SetDoubleBuffered(this Control c, Type t, bool value)
        {
            PropertyInfo property = t.GetProperty("DoubleBuffered",
               BindingFlags.NonPublic | BindingFlags.Instance);
            property.SetValue(c, value, null);
        }
        public static void SetDoubleBuffered(this Control c, bool value)
        {
            c.SetDoubleBuffered(c.GetType(), value);
        }
        public static void ShowFileInExplorer(string fileName)
        {
            System.Diagnostics.Process.Start("explorer.exe", "/select," + fileName);
        }
        public static int SizeOf<T>()
        {
            return System.Runtime.InteropServices.Marshal.SizeOf(typeof(T));
        }
        public static int SizeOf<T>(int alignment)
        {
            return SizeOf<T>() + alignment;
        }
        public static List<KeyValuePair<string, long> > SortFlagNames(Type enumType)
        {
            List<KeyValuePair<string, long> > nameList = new List<KeyValuePair<string, long> >();
            foreach (string name in Enum.GetNames(enumType))
            {
                long nameLong = Convert.ToInt64(Enum.Parse(enumType, name));
                nameList.Add(new KeyValuePair<string, long>(name, nameLong));
            }
            nameList.Sort((kvp1, kvp2) => kvp2.Value.CountBits().CompareTo(kvp1.Value.CountBits()));
            return nameList;
        }
        public static int ToInt32(this byte[] data)
        {
            return data.ToInt32(Endianness.Little);
        }
        public static int ToInt32(this byte[] data, Endianness type)
        {
            if (type == Endianness.Little)
            {
                return (data[0]) | (data[1] << 8) | (data[2] << 16) | (data[3] << 24);
            }
            else if (type == Endianness.Big)
            {
                return (data[0] << 24) | (data[1] << 16) | (data[2] << 8) | (data[3]);
            }
            else
            {
                throw new ArgumentException();
            }
        }
        public static long ToInt64(this byte[] data)
        {
            return data.ToInt64(Endianness.Little);
        }
        public static long ToInt64(this byte[] data, Endianness type)
        {
            if (type == Endianness.Little)
            {
                return (data[0]) | (data[1] << 8) | (data[2] << 16) | (data[3] << 24) |
                    (data[4] << 32) | (data[5] << 40) | (data[6] << 48) | (data[7] << 56);
            }
            else if (type == Endianness.Big)
            {
                return (data[0] << 56) | (data[1] << 48) | (data[2] << 40) | (data[3] << 32) |
                    (data[4] << 24) | (data[5] << 16) | (data[6] << 8) | (data[7]);
            }
            else
            {
                throw new ArgumentException();
            }
        }
        public static IntPtr ToIntPtr(this byte[] data)
        {
            if (IntPtr.Size != data.Length)
                throw new ArgumentException("data");
            if (IntPtr.Size == sizeof(int))
                return new IntPtr(data.ToInt32(Endianness.Little));
            else if (IntPtr.Size == sizeof(long))
                return new IntPtr(data.ToInt64(Endianness.Little));
            else
                throw new ArgumentException("data");
        }
        public static ushort ToUInt16(this byte[] data, Endianness type)
        {
            return ToUInt16(data, 0, type);
        }
        public static ushort ToUInt16(this byte[] data, int offset, Endianness type)
        {
            if (type == Endianness.Little)
            {
                return (ushort)(data[offset] | (data[offset + 1] << 8));
            }
            else if (type == Endianness.Big)
            {
                return (ushort)((data[offset] << 8) | data[offset + 1]);
            }
            else
            {
                throw new ArgumentException();
            }
        }
        public static uint ToUInt32(this byte[] data, Endianness type)
        {
            return ToUInt32(data, 0, type);
        }
        public static uint ToUInt32(this byte[] data, int offset, Endianness type)
        {
            if (type == Endianness.Little)
            {
                return (uint)(data[offset]) | (uint)(data[offset + 1] << 8) |
                    (uint)(data[offset + 2] << 16) | (uint)(data[offset + 3] << 24);
            }
            else if (type == Endianness.Big)
            {
                return (uint)(data[offset] << 24) | (uint)(data[offset + 1] << 16) |
                    (uint)(data[offset + 2] << 8) | (uint)(data[offset + 3]);
            }
            else
            {
                throw new ArgumentException();
            }
        }
        public static void ValidateBuffer(byte[] buffer, int offset, int length)
        {
            ValidateBuffer(buffer, offset, length, false);
        }
        public static void ValidateBuffer(byte[] buffer, int offset, int length, bool canBeNull)
        {
            if (offset < 0)
                throw new ArgumentOutOfRangeException("offset");
            if (length < 0)
                throw new ArgumentOutOfRangeException("length");
            if (buffer != null)
            {
                if (buffer.Length - offset < length)
                    throw new ArgumentOutOfRangeException("The buffer is too small for the specified offset and length.");
            }
            else
            {
                if (!canBeNull)
                    throw new ArgumentException("The buffer cannot be null.");
                if (offset != 0 || length != 0)
                    throw new ArgumentOutOfRangeException("The offset and length must be zero for a null buffer.");
            }
        }
    }
}
