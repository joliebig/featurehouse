

using System;
using System.Runtime.InteropServices;

namespace ProcessHacker.Native.Api
{
    public delegate void ApcCallbackDelegate(NtStatus ioStatus, IntPtr apcContext, IntPtr context);
    public delegate void ApcRoutine(IntPtr parameter);
    public delegate void IoApcRoutine(IntPtr apcContext, ref IoStatusBlock ioStatusBlock, int reserved);
    public delegate void TimerApcRoutine(IntPtr context, int lowValue, int highValue);
    public delegate void WaitOrTimerCallbackDelegate(IntPtr context, bool timeout);
    public delegate void WorkerCallbackDelegate(IntPtr context);

    public static partial class Win32
    {
        public const int AclRevision = 2;
        public const int AclRevisionDs = 4;
        public const int CsrSrvServerDllIndex = 0;
        public const int CsrSrvFirstApiNumber = 0;
        public const int BaseSrvServerDllIndex = 1;
        public const int BaseSrvFirstApiNumber = 0;
        public const int ConSrvServerDllIndex = 2;
        public const int ConSrvFirstApiNumber = 512;
        public const int UserSrvServerDllIndex = 3;
        public const int UserSrvFirstApiNumber = 1024;
        public const int ExceptionMaximumParameters = 15;
        public const uint FileWriteToEndOfFile = 0xffffffff;
        public const uint FileUseFilePointerPosition = 0xfffffffe;
        public const int FlsMaximumAvailable = 128;



        public const int GdiHandleBufferSize = 34;

        public const int MaximumSupportedExtension = 512;
        public const int MaximumWaitObjects = 64;
        public const int MaxKeyNameLength = 512;
        public const int MaxKeyValueNameLength = 32767;
        public const int MaxStackDepth = 32;
        public const int MaxWow64SharedEntries = 16;
        public const short Pe32Magic = 0x10b;
        public const short Pe32PlusMagic = 0x20b;
        public const short RomMagic = 0x107;
        public const int PortMessageMaxDataLength = 0x130;
        public const int PortMessageMaxLength = 0x148;
        public const int ProcessHandleTracingMaxStacks = 16;
        public const int ProcessorFeatureMax = 64;
        public const int SecurityDescriptorRevision = 1;
        public const int SidMaxSubAuthorities = 15;
        public const int SidRecommendedSubAuthorities = 1;
        public const int SidRevision = 1;
        public const int SizeOf80387Registers = 80;
        public const int TimeMsTo100Ns = 10000;


        public const string BeepDeviceName = @"\Device\Beep";
        public const string EnlistmentPath = @"\Enlistment";
        public const string MailslotPath = @"\Device\Mailslot";
        public const string MountMgrDeviceName = @"\Device\MountPointManager";
        public const string NamedPipePath = @"\Device\NamedPipe";
        public const string ResourceManagerPath = @"\ResourceManager";
        public const string TransactionPath = @"\Transaction";
        public const string TransactionManagerPath = @"\TransactionManager";

        public static readonly IntPtr KnownAceSidStartOffset = Marshal.OffsetOf(typeof(KnownAceStruct), "SidStart");
        public static readonly int SecurityDescriptorMinLength = Marshal.SizeOf(typeof(SecurityDescriptorStruct));
        public static readonly int SecurityMaxSidSize =
            Marshal.SizeOf(typeof(SidStruct)) - sizeof(int) + (SidMaxSubAuthorities * sizeof(int));
        public static readonly IntPtr UserSharedData = new IntPtr(0x7ffe0000);

        public static int CsrMakeApiNumber(int dllIndex, int apiIndex)
        {
            return (dllIndex << 16) | apiIndex;
        }

        public static int CtlCode(DeviceType type, int function, DeviceControlMethod method, DeviceControlAccess access)
        {
            return ((int)type << 16) |
                ((int)access << 14) |
                (function << 2) |
                (int)method;
        }
    }
}
