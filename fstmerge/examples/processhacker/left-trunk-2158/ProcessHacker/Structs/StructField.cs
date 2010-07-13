

using System;
using System.Collections.Generic;
using System.Text;

namespace ProcessHacker.Structs
{
    public class StructField
    {
        private FieldType _type;

        public StructField(string name, FieldType type)
        {
            VarLength = -1;
            VarArrayLength = 0;
            Name = name;
            _type = type;
        }

        public bool IsArray
        {
            get { return (_type & FieldType.Array) != 0; }
        }

        public bool IsPointer
        {
            get { return (_type & FieldType.Pointer) != 0; }
        }




        public int Size
        {
            get
            {
                if (this.IsPointer)
                {
                    return 4;
                }
                else
                {
                    int size;

                    switch (_type)
                    {
                        case FieldType.Bool32:
                            size = 4;
                            break;
                        case FieldType.Bool8:
                            size = 1;
                            break;
                        case FieldType.CharASCII:
                            size = 1;
                            break;
                        case FieldType.CharUTF16:
                            size = 2;
                            break;
                        case FieldType.Double:
                            size = 8;
                            break;
                        case FieldType.Int16:
                            size = 2;
                            break;
                        case FieldType.Int32:
                            size = 4;
                            break;
                        case FieldType.Int64:
                            size = 8;
                            break;
                        case FieldType.Int8:
                            size = 1;
                            break;
                        case FieldType.PVoid:
                            size = 4;
                            break;
                        case FieldType.Single:
                            size = 4;
                            break;
                        case FieldType.StringASCII:
                            size = VarLength;
                            break;
                        case FieldType.StringUTF16:
                            size = VarLength * 2;
                            break;
                        case FieldType.Struct:
                            size = 0;
                            break;
                        case FieldType.UInt16:
                            size = 2;
                            break;
                        case FieldType.UInt32:
                            size = 4;
                            break;
                        case FieldType.UInt64:
                            size = 8;
                            break;
                        case FieldType.UInt8:
                            size = 1;
                            break;
                        default:
                            size = 0;
                            break;
                    }

                    if (this.IsArray)
                        return size * VarArrayLength;
                    else
                        return size;
                }
            }
        }

        public string Name { get; set; }

        internal int VarArrayLength { get; set; }

        internal int VarLength { get; set; }

        public string SetsVarOn { get; set; }

        public int SetsVarOnAdd { get; set; }

        public float SetsVarOnMultiply { get; set; }

        public string StructName { get; set; }

        public FieldType Type
        {
            get { return _type & (~FieldType.Pointer) & (~FieldType.Array); }
        }

        public FieldType RawType
        {
            get { return _type; }
        }
    }
}
