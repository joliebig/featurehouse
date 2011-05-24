

using System;
using System.Collections.Generic;
using System.Text;

namespace ProcessHacker.Structs
{
    public struct FieldValue
    {
        public string Name;
        public FieldType FieldType;
        public object Value;
        public string StructName;
        public int PointerValue;
    }
}
