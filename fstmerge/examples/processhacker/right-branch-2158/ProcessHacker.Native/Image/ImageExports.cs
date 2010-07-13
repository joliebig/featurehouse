

using System;
using System.Collections.Generic;
using System.Text;
using ProcessHacker.Native.Api;
using ProcessHacker.Native.Objects;
using ProcessHacker.Native.Security;

namespace ProcessHacker.Native.Image
{
    public unsafe sealed class ImageExports
    {
        public delegate bool EnumEntriesDelegate(ImageExportEntry entry);

        private MappedImage _mappedImage;
        private ImageDataDirectory* _dataDirectory;
        private ImageExportDirectory* _exportDirectory;
        private int* _addressTable;
        private int* _namePointerTable;
        private short* _ordinalTable;

        internal ImageExports(MappedImage mappedImage)
        {
            _mappedImage = mappedImage;
            _dataDirectory = mappedImage.GetDataEntry(ImageDataEntry.Export);
            _exportDirectory = mappedImage.GetExportDirectory();

            if (_exportDirectory != null)
            {
                _addressTable = (int*)mappedImage.RvaToVa(_exportDirectory->AddressOfFunctions);
                _namePointerTable = (int*)mappedImage.RvaToVa(_exportDirectory->AddressOfNames);
                _ordinalTable = (short*)mappedImage.RvaToVa(_exportDirectory->AddressOfNameOrdinals);
            }
        }

        public int Count
        {
            get
            {
                if (_exportDirectory != null)
                    return _exportDirectory->NumberOfFunctions;
                else
                    return 0;
            }
        }

        public ImageExportEntry GetEntry(int index)
        {
            if (_exportDirectory == null || _namePointerTable == null || _ordinalTable == null)
                return ImageExportEntry.Empty;
            if (index >= _exportDirectory->NumberOfFunctions)
                return ImageExportEntry.Empty;

            ImageExportEntry entry = new ImageExportEntry();

            entry.Ordinal = (short)(_ordinalTable[index] + _exportDirectory->Base);

            if (index < _exportDirectory->NumberOfNames)
                entry.Name = new string((sbyte*)_mappedImage.RvaToVa(_namePointerTable[index]));

            return entry;
        }

        public ImageExportFunction GetFunction(string name)
        {
            if (_exportDirectory == null || _namePointerTable == null || _ordinalTable == null)
                return ImageExportFunction.Empty;

            int index;

            index = this.LookupName(name);

            if (index == -1)
                return ImageExportFunction.Empty;

            return this.GetFunction((short)(_ordinalTable[index] + _exportDirectory->Base));
        }

        public ImageExportFunction GetFunction(short ordinal)
        {
            if (_exportDirectory == null || _addressTable == null)
                return ImageExportFunction.Empty;
            if (ordinal - _exportDirectory->Base >= _exportDirectory->NumberOfFunctions)
                return ImageExportFunction.Empty;

            int rva = _addressTable[ordinal - _exportDirectory->Base];

            if (
                rva >= _dataDirectory->VirtualAddress &&
                rva < _dataDirectory->VirtualAddress + _dataDirectory->Size
                )
            {

                return new ImageExportFunction() { ForwardedName = new string((sbyte*)_mappedImage.RvaToVa(rva)) };
            }
            else
            {

                return new ImageExportFunction() { Function = (IntPtr)_mappedImage.RvaToVa(rva) };
            }
        }

        private int LookupName(string name)
        {
            int low = 0;
            int high = _exportDirectory->NumberOfNames - 1;


            while (low <= high)
            {
                int i;
                string n;

                i = (low + high) / 2;
                n = new string((sbyte*)_mappedImage.RvaToVa(_namePointerTable[i]));

                if (name == n)
                {
                    return i;
                }
                else if (name.CompareTo(n) > 0)
                {
                    low = i + 1;
                }
                else
                {
                    high = i - 1;
                }
            }

            return -1;
        }
    }

    public struct ImageExportEntry
    {
        public static readonly ImageExportEntry Empty = new ImageExportEntry();

        public string Name;
        public short Ordinal;
    }

    public struct ImageExportFunction
    {
        public static readonly ImageExportFunction Empty = new ImageExportFunction();

        public IntPtr Function;
        public string ForwardedName;
    }
}
