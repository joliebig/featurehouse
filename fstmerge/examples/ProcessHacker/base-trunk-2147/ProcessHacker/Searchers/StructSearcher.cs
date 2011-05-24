

using System;
using ProcessHacker.Common;
using ProcessHacker.Native;
using ProcessHacker.Native.Api;
using ProcessHacker.Native.Objects;
using ProcessHacker.Structs;

namespace ProcessHacker
{
    public class StructSearcher : Searcher
    {
        public StructSearcher(int PID) : base(PID) { }

        public override void Search()
        {
            Results.Clear();

            ProcessHandle phandle;
            int count = 0;

            bool opt_priv = (bool)Params["private"];
            bool opt_img = (bool)Params["image"];
            bool opt_map = (bool)Params["mapped"];

            string structName = (string)Params["struct"];
            int align = (int)BaseConverter.ToNumberParse((string)Params["struct_align"]);

            if (!Program.Structs.ContainsKey(structName))
            {
                CallSearchError("Struct '" + structName + "' is not defined.");
                return;
            }

            StructDef structDef = Program.Structs[structName];
            string structLen = structDef.Size.ToString();

            structDef.IOProvider = new ProcessMemoryIO(PID);

            try
            {
                phandle = new ProcessHandle(PID, ProcessHacker.Native.Security.ProcessAccess.QueryInformation);
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

                    CallSearchProgressChanged(
                        String.Format("Searching 0x{0} ({1} found)...", info.BaseAddress.ToString("x"), count));

                    for (int i = 0; i < info.RegionSize.ToInt32(); i += align)
                    {
                        try
                        {
                            structDef.Offset = info.BaseAddress.Increment(i);
                            structDef.Read();


                            Results.Add(new string[] { Utils.FormatAddress(info.BaseAddress),
                                String.Format("0x{0:x}", i), structLen, "" });
                            count++;
                        }
                        catch
                        { }
                    }

                    return true;
                });

            phandle.Dispose();

            CallSearchFinished();
        }
    }
}
