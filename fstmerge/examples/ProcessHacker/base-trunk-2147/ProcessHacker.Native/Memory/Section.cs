using ProcessHacker.Native.Api;
using ProcessHacker.Native.Objects;
using ProcessHacker.Native.Security;

namespace ProcessHacker.Native
{



    public sealed class Section : NativeObject<SectionHandle>
    {
        private MemoryProtection _originalProtection = MemoryProtection.ReadWrite;






        public Section(string name, SectionAccess access)
        {
            this.Handle = new SectionHandle(name, access);
        }





        public Section(FileHandle fileHandle)
            : this(fileHandle, MemoryProtection.ReadWrite)
        { }






        public Section(FileHandle fileHandle, MemoryProtection protection)
            : this(fileHandle, false, protection)
        { }







        public Section(FileHandle fileHandle, bool image, MemoryProtection protection)
            : this(null, fileHandle, image, protection)
        { }
        public Section(string name, FileHandle fileHandle, bool image, MemoryProtection protection)
        {
            _originalProtection = protection;
            this.Handle = SectionHandle.Create(
                SectionAccess.All,
                name,
                ObjectFlags.OpenIf,
                null,
                fileHandle.GetSize(),
                image ? SectionAttributes.Image : SectionAttributes.Commit,
                protection,
                fileHandle
                );
        }
        public Section(long maximumSize)
            : this(maximumSize, MemoryProtection.ReadWrite)
        { }
        public Section(long maximumSize, MemoryProtection protection)
            : this(null, maximumSize, protection)
        { }
        public Section(string name, long maximumSize, MemoryProtection protection)
        {
            _originalProtection = protection;
            this.Handle = SectionHandle.Create(
                SectionAccess.All,
                name,
                ObjectFlags.OpenIf,
                null,
                maximumSize,
                SectionAttributes.Commit,
                protection,
                null
                );
        }
        public void Extend(long newSize)
        {
            this.Handle.Extend(newSize);
        }
        public SectionView MapView(int size)
        {
            return this.MapView(size, _originalProtection);
        }
        public SectionView MapView(int size, MemoryProtection protection)
        {
            return this.Handle.MapView(0, size, protection);
        }
        public SectionView MapView(int offset, int size, MemoryProtection protection)
        {
            return this.Handle.MapView(offset, size, protection);
        }
    }
}
