

using System;
using System.Text;
using ProcessHacker.Common;
using ProcessHacker.Native.Api;
using ProcessHacker.Native.Objects;
using ProcessHacker.Native.Security;

namespace ProcessHacker
{
    public class StringSearcher : Searcher
    {
        public StringSearcher(int PID) : base(PID) { }

        private bool IsChar(byte b)
        {
            return (b >= ' ' && b <= '~') || b == '\n' || b == '\r' || b == '\t';
        }

        public override void Search()
        {
            Results.Clear();

            byte[] text = (byte[])Params["text"];
            ProcessHandle phandle;
            int count = 0;

            int minsize = (int)BaseConverter.ToNumberParse((string)Params["s_ms"]);
            bool unicode = (bool)Params["unicode"];

            bool opt_priv = (bool)Params["private"];
            bool opt_img = (bool)Params["image"];
            bool opt_map = (bool)Params["mapped"];

            try
            {
                phandle = new ProcessHandle(PID,
                    ProcessAccess.QueryInformation |
                    Program.MinProcessReadMemoryRights);
            }
            catch
            {
                CallSearchError("Could not open process: " + Win32.GetLastErrorMessage());
                return;
            }

            phandle.EnumMemory((info) =>
                {

                    if (info.Protect == MemoryProtection.AccessDenied)
                        return true;
                    if (info.State != MemoryState.Commit)
                        return true;

                    if ((!opt_priv) && (info.Type == MemoryType.Private))
                        return true;

                    if ((!opt_img) && (info.Type == MemoryType.Image))
                        return true;

                    if ((!opt_map) && (info.Type == MemoryType.Mapped))
                        return true;

                    byte[] data = new byte[info.RegionSize.ToInt32()];
                    int bytesRead = 0;

                    CallSearchProgressChanged(
                        String.Format("Searching 0x{0} ({1} found)...", info.BaseAddress.ToString("x"), count));

                    try
                    {
                        bytesRead = phandle.ReadMemory(info.BaseAddress, data, data.Length);

                        if (bytesRead == 0)
                            return true;
                    }
                    catch
                    {
                        return true;
                    }

                    StringBuilder curstr = new StringBuilder();
                    bool isUnicode = false;
                    byte byte2 = 0;
                    byte byte1 = 0;

                    for (int i = 0; i < bytesRead; i++)
                    {
                        bool isChar = IsChar(data[i]);

                        if (unicode && isChar && isUnicode && byte1 != 0)
                        {
                            isUnicode = false;

                            if (curstr.Length > 0)
                                curstr.Remove(curstr.Length - 1, 1);

                            curstr.Append((char)data[i]);
                        }
                        else if (isChar)
                        {
                            curstr.Append((char)data[i]);
                        }
                        else if (unicode && data[i] == 0 && IsChar(byte1) && !IsChar(byte2))
                        {

                            isUnicode = true;
                        }
                        else if (unicode &&
                            data[i] == 0 && IsChar(byte1) && IsChar(byte2) && curstr.Length < minsize)
                        {


                            isUnicode = true;
                            curstr = new StringBuilder();
                            curstr.Append((char)byte1);
                        }
                        else
                        {
                            if (curstr.Length >= minsize)
                            {
                                int length = curstr.Length;

                                if (isUnicode)
                                    length *= 2;

                                Results.Add(new string[] { Utils.FormatAddress(info.BaseAddress),
                                    String.Format("0x{0:x}", i - length), length.ToString(),
                                    curstr.ToString() });

                                count++;
                            }

                            isUnicode = false;
                            curstr = new StringBuilder();
                        }

                        byte2 = byte1;
                        byte1 = data[i];
                    }

                    data = null;

                    return true;
                });

            phandle.Dispose();

            CallSearchFinished();
        }
    }
}
