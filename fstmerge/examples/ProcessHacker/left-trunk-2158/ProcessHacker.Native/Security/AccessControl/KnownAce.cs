

using System;
using System.Runtime.InteropServices;
using ProcessHacker.Native.Api;

namespace ProcessHacker.Native.Security.AccessControl
{
    public class KnownAce : Ace
    {
        private int _mask;
        private Sid _sid;

        protected KnownAce()
        { }

        public KnownAce(AceType type, AceFlags flags, int mask, Sid sid)
        {
            if (
                type != AceType.AccessAllowed &&
                type != AceType.AccessDenied &&
                type != AceType.SystemAlarm &&
                type != AceType.SystemAudit
                )
                throw new ArgumentException("Invalid ACE type.");

            this.MemoryRegion = new MemoryAlloc(
                Marshal.SizeOf(typeof(KnownAceStruct)) -
                sizeof(int) +
                sid.Length
                );

            KnownAceStruct knownAce = new KnownAceStruct();


            knownAce.Header.AceType = type;
            knownAce.Header.AceFlags = flags;
            knownAce.Header.AceSize = (ushort)this.MemoryRegion.Size;
            knownAce.Mask = mask;

            this.MemoryRegion.WriteStruct<KnownAceStruct>(knownAce);

            this.MemoryRegion.WriteMemory(Win32.KnownAceSidStartOffset.ToInt32(), sid, 0, sid.Length);

            this.Read();
        }

        public KnownAce(IntPtr memory)
            : base(memory)
        { }

        public int Mask
        {
            get { return _mask; }
        }

        public Sid Sid
        {
            get { return _sid; }
        }

        protected override void Read()
        {
            var knownAce = this.MemoryRegion.ReadStruct<KnownAceStruct>();

            _mask = knownAce.Mask;
            _sid = Sid.FromPointer(this.Memory.Increment(Win32.KnownAceSidStartOffset));

            base.Read();
        }
    }
}
