

using System;
using System.Runtime.InteropServices;
using ProcessHacker.Common;
using ProcessHacker.Native.Api;

namespace ProcessHacker
{
    public class HeapSearcher : Searcher
    {
        public HeapSearcher(int PID) : base(PID) { }

        public override void Search()
        {
            Results.Clear();

            IntPtr snapshot;
            var hlist = new HeapList32();
            var heap = new HeapEntry32();
            int minsize = (int)BaseConverter.ToNumberParse((string)Params["h_ms"]);
            int count = 0;

            snapshot = Win32.CreateToolhelp32Snapshot(SnapshotFlags.HeapList, PID);

            hlist.dwSize = Marshal.SizeOf(hlist);
            heap.dwSize = Marshal.SizeOf(heap);

            if (snapshot != IntPtr.Zero && Marshal.GetLastWin32Error() == 0)
            {
                Win32.Heap32ListFirst(snapshot, ref hlist);

                do
                {
                    Win32.Heap32First(ref heap, hlist.th32ProcessID, hlist.th32HeapID);

                    do
                    {
                        CallSearchProgressChanged(
                            String.Format("Searching 0x{0} ({1} found)...", heap.dwAddress.ToString("x"), count));

                        if (heap.dwBlockSize <= minsize)
                            continue;

                        Results.Add(new string[] { Utils.FormatAddress(heap.dwAddress),
                            "0x0", heap.dwBlockSize.ToString(), heap.dwFlags.ToString().Replace("LF32_", "") });

                        count++;
                    } while (Win32.Heap32Next(out heap) != 0);
                } while (Win32.Heap32ListNext(snapshot, out hlist));
            }
            else
            {
                CallSearchError(Win32.GetLastErrorMessage());
                return;
            }

            CallSearchFinished();
        }
    }
}
