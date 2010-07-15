

using System;
using System.Runtime.InteropServices;

namespace ProcessHacker.Native
{
    public static class IntPtrExtensions
    {
        public static IntPtr And(this IntPtr ptr, int value)
        {
            if (IntPtr.Size == sizeof(Int32))
                return new IntPtr(ptr.ToInt32() & value);
            else
                return new IntPtr(ptr.ToInt64() & value);
        }

        public static IntPtr And(this IntPtr ptr, IntPtr value)
        {
            if (IntPtr.Size == sizeof(Int32))
                return new IntPtr(ptr.ToInt32() & value.ToInt32());
            else
                return new IntPtr(ptr.ToInt64() & value.ToInt64());
        }

        public static int CompareTo(this IntPtr ptr, IntPtr ptr2)
        {
            if (ptr.ToUInt64() > ptr2.ToUInt64())
                return 1;
            if (ptr.ToUInt64() < ptr2.ToUInt64())
                return -1;
            return 0;
        }

        public static int CompareTo(this IntPtr ptr, int ptr2)
        {
            return ptr.CompareTo((uint)ptr2);
        }

        public static int CompareTo(this IntPtr ptr, uint ptr2)
        {
            if (ptr.ToUInt64() > ptr2)
                return 1;
            if (ptr.ToUInt64() < ptr2)
                return -1;
            return 0;
        }

        public static IntPtr Decrement(this IntPtr ptr, IntPtr ptr2)
        {
            if (IntPtr.Size == sizeof(Int32))
                return new IntPtr(ptr.ToInt32() - ptr2.ToInt32());
            else
                return new IntPtr(ptr.ToInt64() - ptr2.ToInt64());
        }

        public static IntPtr Decrement(this IntPtr ptr, int value)
        {
            return Increment(ptr, -value);
        }

        public static IntPtr Decrement(this IntPtr ptr, long value)
        {
            return Increment(ptr, -value);
        }

        public static T ElementAt<T>(this IntPtr ptr, int index)
        {
            var offset = Marshal.SizeOf(typeof(T)) * index;
            var offsetPtr = ptr.Increment(offset);
            return (T)Marshal.PtrToStructure(offsetPtr, typeof(T));
        }

        public static bool Equals(this IntPtr ptr, IntPtr ptr2)
        {
            return ptr == ptr2;
        }

        public static bool Equals(this IntPtr ptr, int value)
        {
            return ptr.ToInt32() == value;
        }

        public static bool Equals(this IntPtr ptr, uint value)
        {
            return ptr.ToUInt32() == value;
        }

        public static bool Equals(this IntPtr ptr, long value)
        {
            return ptr.ToInt64() == value;
        }

        public static bool Equals(this IntPtr ptr, ulong value)
        {
            return ptr.ToUInt64() == value;
        }

        public static IntPtr Increment(this IntPtr ptr, int value)
        {
            unchecked
            {
                if (IntPtr.Size == sizeof(Int32))
                    return new IntPtr(ptr.ToInt32() + value);
                else
                    return new IntPtr(ptr.ToInt64() + value);
            }
        }

        public static IntPtr Increment(this IntPtr ptr, long value)
        {
            unchecked
            {
                if (IntPtr.Size == sizeof(Int32))
                    return new IntPtr((int)(ptr.ToInt32() + value));
                else
                    return new IntPtr(ptr.ToInt64() + value);
            }
        }

        public static IntPtr Increment(this IntPtr ptr, IntPtr ptr2)
        {
            unchecked
            {
                if (IntPtr.Size == sizeof(Int32))
                    return new IntPtr(ptr.ToInt32() + ptr2.ToInt32());
                else
                    return new IntPtr(ptr.ToInt64() + ptr2.ToInt64());
            }
        }

        public static IntPtr Increment<T>(this IntPtr ptr)
        {
            return ptr.Increment(Marshal.SizeOf(typeof(T)));
        }

        public static bool IsGreaterThanOrEqualTo(this IntPtr ptr, IntPtr ptr2)
        {
            return ptr.CompareTo(ptr2) >= 0;
        }

        public static bool IsLessThanOrEqualTo(this IntPtr ptr, IntPtr ptr2)
        {
            return ptr.CompareTo(ptr2) <= 0;
        }

        public static IntPtr Not(this IntPtr ptr)
        {
            if (IntPtr.Size == sizeof(Int32))
                return new IntPtr(~ptr.ToInt32());
            else
                return new IntPtr(~ptr.ToInt64());
        }

        public static IntPtr Or(this IntPtr ptr, IntPtr value)
        {
            if (IntPtr.Size == sizeof(Int32))
                return new IntPtr(ptr.ToInt32() | value.ToInt32());
            else
                return new IntPtr(ptr.ToInt64() | value.ToInt64());
        }

        public static uint ToUInt32(this IntPtr ptr)
        {

            unsafe
            {
                void* voidPtr = (void*)ptr;

                return (uint)voidPtr;
            }
        }

        public static ulong ToUInt64(this IntPtr ptr)
        {

            unsafe
            {
                void* voidPtr = (void*)ptr;

                return (ulong)voidPtr;
            }
        }

        public static IntPtr ToIntPtr(this int value)
        {
            return new IntPtr(value);
        }

        public static IntPtr ToIntPtr(this uint value)
        {
            unchecked
            {
                return new IntPtr((int)value);
            }
        }

        public static IntPtr ToIntPtr(this long value)
        {
            unchecked
            {
                if (value > 0 && value <= 0xffffffff)
                    return new IntPtr((int)value);
            }

            return new IntPtr(value);
        }

        public static IntPtr ToIntPtr(this ulong value)
        {
            unchecked
            {
                return ((long)value).ToIntPtr();
            }
        }

        public static IntPtr Xor(this IntPtr ptr, IntPtr value)
        {
            if (IntPtr.Size == sizeof(Int32))
                return new IntPtr(ptr.ToInt32() ^ value.ToInt32());
            else
                return new IntPtr(ptr.ToInt64() ^ value.ToInt64());
        }
    }
}
