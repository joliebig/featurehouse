

using System;
using ProcessHacker.Common.Objects;
using ProcessHacker.Native.Api;

namespace ProcessHacker.Native.Security.AccessControl
{
    public class Ace : BaseObject
    {
        public static Ace GetAce(IntPtr ace)
        {
            var type = GetType(ace);

            switch (type)
            {
                case AceType.AccessAllowed:
                case AceType.AccessDenied:
                case AceType.SystemAlarm:
                case AceType.SystemAudit:
                    return new KnownAce(ace);
                default:
                    return new Ace(ace);
            }
        }

        public static AceType GetType(IntPtr ace)
        {
            MemoryRegion memory = new MemoryRegion(ace);

            return memory.ReadStruct<AceHeader>().AceType;
        }

        public static implicit operator IntPtr(Ace ace)
        {
            return ace.Memory;
        }

        private MemoryRegion _memory;
        private AceFlags _flags;
        private int _size;
        private AceType _type;

        protected Ace()
        { }

        public Ace(IntPtr memory)
            : this(memory, false)
        { }

        public Ace(IntPtr memory, bool copy)
            : base(copy)
        {
            if (copy)
            {
                Ace existingAce = new Ace(memory);

                _memory = new MemoryAlloc(existingAce.Size);
                _memory.WriteMemory(0, existingAce, 0, existingAce.Size);
            }
            else
            {
                _memory = new MemoryRegion(memory);
            }

            this.Read();
        }

        protected override void DisposeObject(bool disposing)
        {
            if (_memory != null)
                _memory.Dispose();
        }

        public AceFlags Flags
        {
            get { return _flags; }
        }

        public IntPtr Memory
        {
            get { return _memory; }
        }

        protected MemoryRegion MemoryRegion
        {
            get { return _memory; }
            set { _memory = value; }
        }

        public int Size
        {
            get { return _size; }
        }

        public AceType Type
        {
            get { return _type; }
        }

        protected virtual void Read()
        {
            var aceHeader = _memory.ReadStruct<AceHeader>();

            _flags = aceHeader.AceFlags;
            _size = aceHeader.AceSize;
            _type = aceHeader.AceType;
        }
    }
}
