

using System;
using ProcessHacker.Common;
using ProcessHacker.Native.Api;
using ProcessHacker.Native.Security;

namespace ProcessHacker.Native.Objects
{



    public sealed class NamedPipeHandle : FileHandle
    {
        public static readonly int FsCtlAssignEvent = Win32.CtlCode(DeviceType.NamedPipe, 0, DeviceControlMethod.Buffered, DeviceControlAccess.Any);
        public static readonly int FsCtlDisconnect = Win32.CtlCode(DeviceType.NamedPipe, 1, DeviceControlMethod.Buffered, DeviceControlAccess.Any);
        public static readonly int FsCtlListen = Win32.CtlCode(DeviceType.NamedPipe, 2, DeviceControlMethod.Buffered, DeviceControlAccess.Any);
        public static readonly int FsCtlPeek = Win32.CtlCode(DeviceType.NamedPipe, 3, DeviceControlMethod.Buffered, DeviceControlAccess.Read);
        public static readonly int FsCtlQueryEvent = Win32.CtlCode(DeviceType.NamedPipe, 4, DeviceControlMethod.Buffered, DeviceControlAccess.Any);
        public static readonly int FsCtlTransceive = Win32.CtlCode(DeviceType.NamedPipe, 5, DeviceControlMethod.Neither, DeviceControlAccess.Read | DeviceControlAccess.Write);
        public static readonly int FsCtlWait = Win32.CtlCode(DeviceType.NamedPipe, 6, DeviceControlMethod.Buffered, DeviceControlAccess.Any);
        public static readonly int FsCtlImpersonate = Win32.CtlCode(DeviceType.NamedPipe, 7, DeviceControlMethod.Buffered, DeviceControlAccess.Any);
        public static readonly int FsCtlSetClientProcess = Win32.CtlCode(DeviceType.NamedPipe, 8, DeviceControlMethod.Buffered, DeviceControlAccess.Any);
        public static readonly int FsCtlQueryClientProcess = Win32.CtlCode(DeviceType.NamedPipe, 9, DeviceControlMethod.Buffered, DeviceControlAccess.Any);

        public static NamedPipeHandle Create(
            FileAccess access,
            string fileName,
            PipeType type,
            int maximumInstances,
            long defaultTimeout
            )
        {
            return Create(
                access,
                fileName,
                ObjectFlags.CaseInsensitive,
                null,
                FileShareMode.ReadWrite,
                FileCreationDisposition.OpenIf,
                0,
                type,
                type,
                PipeCompletionMode.Queue,
                maximumInstances,
                0,
                0,
                defaultTimeout
                );
        }

        public static NamedPipeHandle Create(
            FileAccess access,
            string fileName,
            ObjectFlags objectFlags,
            FileHandle rootDirectory,
            FileShareMode shareMode,
            FileCreationDisposition creationDisposition,
            FileCreateOptions createOptions,
            PipeType type,
            PipeType readMode,
            PipeCompletionMode completionMode,
            int maximumInstances,
            int inboundQuota,
            int outboundQuota,
            long defaultTimeout
            )
        {
            NtStatus status;
            ObjectAttributes oa = new ObjectAttributes(fileName, objectFlags, rootDirectory);
            IoStatusBlock isb;
            IntPtr handle;

            try
            {
                if ((status = Win32.NtCreateNamedPipeFile(
                    out handle,
                    access,
                    ref oa,
                    out isb,
                    shareMode,
                    creationDisposition,
                    createOptions,
                    type,
                    readMode,
                    completionMode,
                    maximumInstances,
                    inboundQuota,
                    outboundQuota,
                    ref defaultTimeout
                    )) >= NtStatus.Error)
                    Win32.ThrowLastError(status);
            }
            finally
            {
                oa.Dispose();
            }

            return new NamedPipeHandle(handle, true);
        }

        public new static NamedPipeHandle FromHandle(IntPtr handle)
        {
            return new NamedPipeHandle(handle, false);
        }

        public static bool Wait(string name)
        {
            return Wait(name, long.MinValue, false);
        }
        public static bool Wait(string name, long timeout)
        {
            return Wait(name, timeout, true);
        }
        public static bool Wait(string name, long timeout, bool relative)
        {
            using (var npfsHandle = new FileHandle(
                Win32.NamedPipePath + "\\",
                FileShareMode.ReadWrite,
                FileCreateOptions.SynchronousIoNonAlert,
                FileAccess.ReadAttributes | (FileAccess)StandardRights.Synchronize
                ))
            {
                using (var data = new MemoryAlloc(FilePipeWaitForBuffer.NameOffset + name.Length * 2))
                {
                    FilePipeWaitForBuffer info = new FilePipeWaitForBuffer();
                    info.Timeout = timeout;
                    info.TimeoutSpecified = true;
                    info.NameLength = name.Length * 2;
                    data.WriteStruct<FilePipeWaitForBuffer>(info);
                    data.WriteUnicodeString(FilePipeWaitForBuffer.NameOffset, name);
                    NtStatus status;
                    int returnLength;
                    status = npfsHandle.FsControl(FsCtlWait, data, data.Size, IntPtr.Zero, 0, out returnLength);
                    if (status == NtStatus.IoTimeout)
                        return false;
                    if (status >= NtStatus.Error)
                        Win32.ThrowLastError(status);
                    return true;
                }
            }
        }
        private NamedPipeHandle(IntPtr handle, bool owned)
            : base(handle, owned)
        { }
        public NamedPipeHandle(string fileName, FileAccess access)
            : base(fileName, access)
        { }
        public NamedPipeHandle(string fileName, FileShareMode shareMode, FileAccess access)
            : base(fileName, shareMode, access)
        { }
        public NamedPipeHandle(string fileName, FileShareMode shareMode, FileCreateOptions openOptions, FileAccess access)
            : base(fileName, shareMode, openOptions, access)
        { }
        public NamedPipeHandle(
            string fileName,
            ObjectFlags objectFlags,
            FileHandle rootDirectory,
            FileShareMode shareMode,
            FileCreateOptions openOptions,
            FileAccess access
            )
            : base(fileName, objectFlags, rootDirectory, shareMode, openOptions, access)
        { }
        public AsyncIoContext BeginListen()
        {
            return this.BeginFsControl(FsCtlListen, null, null);
        }
        public AsyncIoContext BeginTransceive(
            byte[] inBuffer,
            int inBufferOffset,
            int inBufferLength,
            byte[] outBuffer,
            int outBufferOffset,
            int outBufferLength
            )
        {
            return this.BeginFsControl(
                FsCtlTransceive,
                inBuffer,
                inBufferOffset,
                inBufferLength,
                outBuffer,
                outBufferOffset,
                outBufferLength
                );
        }
        public AsyncIoContext BeginTransceive(MemoryRegion inBuffer, MemoryRegion outBuffer)
        {
            return this.BeginFsControl(FsCtlTransceive, inBuffer, outBuffer);
        }
        public bool EndListen(AsyncIoContext asyncContext)
        {
            asyncContext.Wait();
            asyncContext.NotifyEnd();
            if (asyncContext.Status == NtStatus.PipeConnected)
                return true;
            if (asyncContext.StatusBlock.Status >= NtStatus.Error)
                Win32.ThrowLastError(asyncContext.StatusBlock.Status);
            return false;
        }
        public int EndTransceive(AsyncIoContext asyncContext)
        {
            return this.EndCommonIo(asyncContext);
        }
        public void Disconnect()
        {
            this.FsControl(FsCtlDisconnect, IntPtr.Zero, 0, IntPtr.Zero, 0);
        }
        private FilePipeInformation GetInformation()
        {
            return this.QueryStruct<FilePipeInformation>(FileInformationClass.FilePipeInformation);
        }
        private FilePipeLocalInformation GetLocalInformation()
        {
            return this.QueryStruct<FilePipeLocalInformation>(FileInformationClass.FilePipeLocalInformation);
        }
        public PipeType GetPipeType()
        {
            return this.GetInformation().ReadMode;
        }
        public void ImpersonateClient()
        {
            this.FsControl(FsCtlImpersonate, null, null);
        }
        public bool Listen()
        {
            NtStatus status;
            int returnLength;
            status = this.FsControl(FsCtlListen, IntPtr.Zero, 0, IntPtr.Zero, 0, out returnLength);
            if (status == NtStatus.PipeConnected)
                return true;
            if (status >= NtStatus.Error)
                Win32.ThrowLastError(status);
            return false;
        }
        public int Peek(byte[] buffer)
        {
            return this.Peek(buffer, 0, 0);
        }
        public int Peek(byte[] buffer, int offset, int length)
        {
            int bytesAvailable;
            return this.Peek(buffer, offset, length, out bytesAvailable);
        }
        public int Peek(IntPtr buffer, int length)
        {
            int bytesAvailable;
            return this.Peek(buffer, length, out bytesAvailable);
        }
        public int Peek(byte[] buffer, out int bytesAvailable)
        {
            int bytesLeftInMessage;
            return this.Peek(buffer, out bytesAvailable, out bytesLeftInMessage);
        }
        public int Peek(byte[] buffer, int offset, int length, out int bytesAvailable)
        {
            int bytesLeftInMessage;
            return this.Peek(buffer, offset, length, out bytesAvailable, out bytesLeftInMessage);
        }
        public int Peek(IntPtr buffer, int length, out int bytesAvailable)
        {
            int bytesLeftInMessage;
            return this.Peek(buffer, length, out bytesAvailable, out bytesLeftInMessage);
        }
        public int Peek(byte[] buffer, out int bytesAvailable, out int bytesLeftInMessage)
        {
            return this.Peek(buffer, 0, buffer.Length, out bytesAvailable, out bytesLeftInMessage);
        }
        public int Peek(byte[] buffer, int offset, int length, out int bytesAvailable, out int bytesLeftInMessage)
        {
            Utils.ValidateBuffer(buffer, offset, length);
            unsafe
            {
                fixed (byte* bufferPtr = buffer)
                {
                    return this.Peek(new IntPtr(&bufferPtr[offset]), length, out bytesAvailable, out bytesLeftInMessage);
                }
            }
        }
        public int Peek(IntPtr buffer, int length, out int bytesAvailable, out int bytesLeftInMessage)
        {
            using (var data = new MemoryAlloc(FilePipePeekBuffer.DataOffset + length))
            {
                NtStatus status;
                int returnLength;
                status = this.FsControl(FsCtlPeek, IntPtr.Zero, 0, data, data.Size, out returnLength);
                if (status == NtStatus.BufferOverflow)
                    status = NtStatus.Success;
                if (status >= NtStatus.Error)
                    Win32.ThrowLastError(status);
                FilePipePeekBuffer info = data.ReadStruct<FilePipePeekBuffer>();
                int bytesRead;
                bytesAvailable = info.ReadDataAvailable;
                bytesRead = returnLength - FilePipePeekBuffer.DataOffset;
                bytesLeftInMessage = info.MessageLength - bytesRead;
                if (buffer != IntPtr.Zero)
                    data.ReadMemory(buffer, 0, FilePipePeekBuffer.DataOffset, bytesRead);
                return bytesRead;
            }
        }
        public int Transceive(byte[] inBuffer, byte[] outBuffer)
        {
            return this.FsControl(FsCtlTransceive, inBuffer, outBuffer);
        }
        public int Transceive(IntPtr inBuffer, int inBufferLength, IntPtr outBuffer, int outBufferLength)
        {
            return this.FsControl(FsCtlTransceive, inBuffer, inBufferLength, outBuffer, outBufferLength);
        }
    }
}
