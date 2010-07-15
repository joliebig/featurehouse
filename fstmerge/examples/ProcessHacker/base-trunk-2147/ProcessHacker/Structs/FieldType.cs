

using System;
using System.Collections.Generic;
using System.Text;

namespace ProcessHacker.Structs
{
    public enum FieldType : uint
    {
        Bool8 = 0x1,
        Bool32,
        CharASCII,
        CharUTF16,
        Int8,
        Int16,
        Int32,
        Int64,
        UInt8,
        UInt16,
        UInt32,
        UInt64,
        Single,
        Double,
        StringASCII,
        StringUTF16,
        Struct,
        PVoid,
        Pointer = 0x4000000,
        Array = 0x8000000
    }
}
