

using System;
using ProcessHacker.Native.Api;
using ProcessHacker.Native.Lpc;
using ProcessHacker.Native.Security;

namespace ProcessHacker.Native.Objects
{
    public sealed class PortComHandle : NativeHandle<PortAccess>
    {
        public static PortComHandle Connect(string portName)
        {
            NtStatus status;
            UnicodeString portNameStr = new UnicodeString(portName);
            SecurityQualityOfService securityQos =
                new SecurityQualityOfService(SecurityImpersonationLevel.SecurityImpersonation, true, false);
            IntPtr handle;

            try
            {
                if ((status = Win32.NtConnectPort(
                    out handle,
                    ref portNameStr,
                    ref securityQos,
                    IntPtr.Zero,
                    IntPtr.Zero,
                    IntPtr.Zero,
                    IntPtr.Zero,
                    IntPtr.Zero
                    )) >= NtStatus.Error)
                    Win32.ThrowLastError(status);
            }
            finally
            {
                portNameStr.Dispose();
            }

            return new PortComHandle(handle, true);
        }

        internal PortComHandle(IntPtr handle, bool owned)
            : base(handle, owned)
        { }

        public void Reply(PortMessage message)
        {
            NtStatus status;

            using (var messageMemory = message.ToMemory())
            {
                if ((status = Win32.NtReplyPort(this, messageMemory)) >= NtStatus.Error)
                    Win32.ThrowLastError(status);

                message.SetHeader(messageMemory);
            }
        }

        public PortMessage ReplyWaitReceive()
        {
            return this.ReplyWaitReceive(null);
        }

        public PortMessage ReplyWaitReceive(PortMessage message)
        {
            NtStatus status;
            IntPtr context;

            using (var buffer = PortMessage.AllocateBuffer())
            {
                MemoryAlloc messageMemory = null;

                if (message != null)
                    messageMemory = message.ToMemory();

                try
                {
                    if ((status = Win32.NtReplyWaitReceivePort(
                        this,
                        out context,
                        messageMemory ?? IntPtr.Zero,
                        buffer
                        )) >= NtStatus.Error)
                        Win32.ThrowLastError(status);

                    if (message != null)
                        message.SetHeader(messageMemory);
                }
                finally
                {
                    if (messageMemory != null)
                        messageMemory.Dispose();
                }

                return new PortMessage(buffer);
            }
        }

        public PortMessage ReplyWaitReply(PortMessage message)
        {
            NtStatus status;

            using (var messageMemory = message.ToMemory())
            {
                if ((status = Win32.NtReplyWaitReplyPort(
                    this,
                    messageMemory
                    )) >= NtStatus.Error)
                    Win32.ThrowLastError(status);

                return new PortMessage(messageMemory);
            }
        }

        public void Request(PortMessage message)
        {
            NtStatus status;

            using (var messageMemory = message.ToMemory())
            {
                if ((status = Win32.NtRequestPort(this, messageMemory)) >= NtStatus.Error)
                    Win32.ThrowLastError(status);

                message.SetHeader(messageMemory);
            }
        }

        public PortMessage RequestWaitReply(PortMessage message)
        {
            NtStatus status;

            using (var buffer = PortMessage.AllocateBuffer())
            using (var messageMemory = message.ToMemory())
            {
                if ((status = Win32.NtRequestWaitReplyPort(
                    this,
                    messageMemory,
                    buffer
                    )) >= NtStatus.Error)
                    Win32.ThrowLastError(status);

                message.SetHeader(messageMemory);

                return new PortMessage(buffer);
            }
        }
    }
}
