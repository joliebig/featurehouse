

using System;
using System.Collections.Generic;
using System.Windows.Forms;
using ProcessHacker.Common;
using ProcessHacker.UI;
using ProcessHacker.Native;
using ProcessHacker.Native.Image;
using ProcessHacker.Native.Api;

namespace ProcessHacker
{
    public partial class PEWindow : Form
    {
        private string _path;
        private MappedImage _mappedImage;

        public PEWindow(string path)
        {
            InitializeComponent();
            this.AddEscapeToClose();
            this.SetTopMost();

            _path = path;
            this.Text = "PE File - " + path;
            Program.PEWindows.Add(Id, this);

            this.InitializeLists();

            try
            {
                _mappedImage = new MappedImage(path);
                this.Read();
            }
            catch (Exception ex)
            {
                PhUtils.ShowException("Unable to load the specified file", ex);

                this.Close();
            }
        }

        private void PEWindow_Load(object sender, EventArgs e)
        {
            this.Size = Properties.Settings.Default.PEWindowSize;

            this.SetPhParent();
        }

        private void PEWindow_FormClosing(object sender, FormClosingEventArgs e)
        {
            this.Visible = false;

            Properties.Settings.Default.PECOFFHColumns = ColumnSettings.SaveSettings(listCOFFHeader);
            Properties.Settings.Default.PECOFFOHColumns = ColumnSettings.SaveSettings(listCOFFOptionalHeader);
            Properties.Settings.Default.PEImageDataColumns = ColumnSettings.SaveSettings(listImageData);
            Properties.Settings.Default.PESectionsColumns = ColumnSettings.SaveSettings(listSections);
            Properties.Settings.Default.PEExportsColumns = ColumnSettings.SaveSettings(listExports);
            Properties.Settings.Default.PEImportsColumns = ColumnSettings.SaveSettings(listImports);
            Properties.Settings.Default.PEWindowSize = this.Size;

            if (_mappedImage != null)
                _mappedImage.Dispose();
        }

        private void InitializeLists()
        {
            listCOFFHeader.SetDoubleBuffered(true);
            listCOFFHeader.SetTheme("explorer");
            listCOFFHeader.ContextMenu = listCOFFHeader.GetCopyMenu();
            listCOFFHeader.AddShortcuts();
            ColumnSettings.LoadSettings(Properties.Settings.Default.PECOFFHColumns, listCOFFHeader);

            listCOFFOptionalHeader.SetDoubleBuffered(true);
            listCOFFOptionalHeader.SetTheme("explorer");
            listCOFFOptionalHeader.ContextMenu = listCOFFOptionalHeader.GetCopyMenu();
            listCOFFOptionalHeader.AddShortcuts();
            ColumnSettings.LoadSettings(Properties.Settings.Default.PECOFFOHColumns, listCOFFOptionalHeader);

            listImageData.SetDoubleBuffered(true);
            listImageData.SetTheme("explorer");
            listImageData.ContextMenu = listImageData.GetCopyMenu();
            listImageData.AddShortcuts();
            ColumnSettings.LoadSettings(Properties.Settings.Default.PEImageDataColumns, listImageData);

            listSections.SetDoubleBuffered(true);
            listSections.SetTheme("explorer");
            listSections.ContextMenu = listSections.GetCopyMenu();
            listSections.AddShortcuts();
            ColumnSettings.LoadSettings(Properties.Settings.Default.PESectionsColumns, listSections);

            listExports.SetDoubleBuffered(true);
            listExports.SetTheme("explorer");
            listExports.ContextMenu = listExports.GetCopyMenu(listExports_RetrieveVirtualItem);
            listExports.AddShortcuts(this.listExports_RetrieveVirtualItem);
            ColumnSettings.LoadSettings(Properties.Settings.Default.PEExportsColumns, listExports);

            listImports.ContextMenu = listImports.GetCopyMenu();
            listImports.AddShortcuts();
            ColumnSettings.LoadSettings(Properties.Settings.Default.PEImportsColumns, listImports);
        }

        public string Id
        {
            get { return _path; }
        }

        private unsafe void Read()
        {





            listCOFFHeader.Items.Clear();
            listCOFFHeader.Items.Add(new ListViewItem(new string[] { "Target Machine",
                _mappedImage.NtHeaders->FileHeader.Machine.ToString() }));
            listCOFFHeader.Items.Add(new ListViewItem(new string[] { "Number of Sections",
                _mappedImage.NtHeaders->FileHeader.NumberOfSections.ToString() }));
            listCOFFHeader.Items.Add(new ListViewItem(new string[] { "Time/Date Stamp",
                Utils.GetDateTimeFromUnixTime((uint)_mappedImage.NtHeaders->FileHeader.TimeDateStamp).ToString() }));
            listCOFFHeader.Items.Add(new ListViewItem(new string[] { "Pointer to Symbol Table",
                Utils.FormatAddress(_mappedImage.NtHeaders->FileHeader.PointerToSymbolTable) }));
            listCOFFHeader.Items.Add(new ListViewItem(new string[] { "Number of Symbols",
                _mappedImage.NtHeaders->FileHeader.NumberOfSymbols.ToString() }));
            listCOFFHeader.Items.Add(new ListViewItem(new string[] { "Size of Optional Header",
                _mappedImage.NtHeaders->FileHeader.SizeOfOptionalHeader.ToString() }));
            listCOFFHeader.Items.Add(new ListViewItem(new string[] { "Characteristics",
                _mappedImage.NtHeaders->FileHeader.Characteristics.ToString() }));






            listCOFFOptionalHeader.Items.Clear();
            listCOFFOptionalHeader.Items.Add(new ListViewItem(new string[] { "Magic",
                _mappedImage.NtHeaders->OptionalHeader.Magic == Win32.Pe32Magic ? "PE32 (0x10b)" :
                (_mappedImage.NtHeaders->OptionalHeader.Magic == Win32.Pe32PlusMagic ? "PE32+ (0x20b)" :
                "Unknown (0x" + _mappedImage.NtHeaders->OptionalHeader.Magic.ToString("x") + ")") }));
            listCOFFOptionalHeader.Items.Add(new ListViewItem(new string[] { "Linker Version",
                _mappedImage.NtHeaders->OptionalHeader.MajorLinkerVersion.ToString() + "." +
                _mappedImage.NtHeaders->OptionalHeader.MinorLinkerVersion.ToString() }));
            listCOFFOptionalHeader.Items.Add(new ListViewItem(new string[] { "Size of Code",
                "0x" + _mappedImage.NtHeaders->OptionalHeader.SizeOfCode.ToString("x") }));
            listCOFFOptionalHeader.Items.Add(new ListViewItem(new string[] { "Size of Initialized Data",
                "0x" + _mappedImage.NtHeaders->OptionalHeader.SizeOfInitializedData.ToString("x") }));
            listCOFFOptionalHeader.Items.Add(new ListViewItem(new string[] { "Size of Uninitialized Data",
                "0x" + _mappedImage.NtHeaders->OptionalHeader.SizeOfUninitializedData.ToString("x") }));
            listCOFFOptionalHeader.Items.Add(new ListViewItem(new string[] { "Entry Point RVA",
                "0x" + _mappedImage.NtHeaders->OptionalHeader.AddressOfEntryPoint.ToString("x") }));
            listCOFFOptionalHeader.Items.Add(new ListViewItem(new string[] { "Base of Code",
                "0x" + _mappedImage.NtHeaders->OptionalHeader.BaseOfCode.ToString("x") }));
            listCOFFOptionalHeader.Items.Add(new ListViewItem(new string[] { "Preferred Image Base",
                "0x" + _mappedImage.NtHeaders->OptionalHeader.ImageBase.ToString("x") }));
            listCOFFOptionalHeader.Items.Add(new ListViewItem(new string[] { "Section Alignment",
                _mappedImage.NtHeaders->OptionalHeader.SectionAlignment.ToString() }));
            listCOFFOptionalHeader.Items.Add(new ListViewItem(new string[] { "File Alignment",
                _mappedImage.NtHeaders->OptionalHeader.FileAlignment.ToString() }));
            listCOFFOptionalHeader.Items.Add(new ListViewItem(new string[] { "Operating System Version",
                _mappedImage.NtHeaders->OptionalHeader.MajorOperatingSystemVersion.ToString() + "." +
                _mappedImage.NtHeaders->OptionalHeader.MinorOperatingSystemVersion.ToString() }));
            listCOFFOptionalHeader.Items.Add(new ListViewItem(new string[] { "Image Version",
                _mappedImage.NtHeaders->OptionalHeader.MajorImageVersion.ToString() + "." +
                _mappedImage.NtHeaders->OptionalHeader.MinorImageVersion.ToString() }));
            listCOFFOptionalHeader.Items.Add(new ListViewItem(new string[] { "Subsystem Version",
                _mappedImage.NtHeaders->OptionalHeader.MajorSubsystemVersion.ToString() + "." +
                _mappedImage.NtHeaders->OptionalHeader.MinorSubsystemVersion.ToString() }));
            listCOFFOptionalHeader.Items.Add(new ListViewItem(new string[] { "Size of Image",
                "0x" + _mappedImage.NtHeaders->OptionalHeader.SizeOfImage.ToString("x") }));
            listCOFFOptionalHeader.Items.Add(new ListViewItem(new string[] { "Size of Headers",
                "0x" + _mappedImage.NtHeaders->OptionalHeader.SizeOfHeaders.ToString("x") }));
            listCOFFOptionalHeader.Items.Add(new ListViewItem(new string[] { "Checksum",
                "0x" + _mappedImage.NtHeaders->OptionalHeader.CheckSum.ToString("x") }));
            listCOFFOptionalHeader.Items.Add(new ListViewItem(new string[] { "Subsystem",
                _mappedImage.NtHeaders->OptionalHeader.Subsystem.ToString() }));
            listCOFFOptionalHeader.Items.Add(new ListViewItem(new string[] { "DLL Characteristics",
                _mappedImage.NtHeaders->OptionalHeader.DllCharacteristics.ToString() }));
            listCOFFOptionalHeader.Items.Add(new ListViewItem(new string[] { "Size of Stack Reserve",
                "0x" + _mappedImage.NtHeaders->OptionalHeader.SizeOfStackReserve.ToString("x") }));
            listCOFFOptionalHeader.Items.Add(new ListViewItem(new string[] { "Size of Stack Commit",
                "0x" + _mappedImage.NtHeaders->OptionalHeader.SizeOfStackCommit.ToString("x") }));
            listCOFFOptionalHeader.Items.Add(new ListViewItem(new string[] { "Size of Heap Reserve",
                "0x" + _mappedImage.NtHeaders->OptionalHeader.SizeOfHeapReserve.ToString("x") }));
            listCOFFOptionalHeader.Items.Add(new ListViewItem(new string[] { "Size of Heap Commit",
                "0x" + _mappedImage.NtHeaders->OptionalHeader.SizeOfHeapCommit.ToString("x") }));
            listCOFFOptionalHeader.Items.Add(new ListViewItem(new string[] { "Number of Data Directory Entries",
                _mappedImage.NtHeaders->OptionalHeader.NumberOfRvaAndSizes.ToString() }));





            listImageData.Items.Clear();

            for (int i = 0; i < _mappedImage.NumberOfDataEntries; i++)
            {
                ImageDataDirectory* dataEntry;

                dataEntry = _mappedImage.GetDataEntry((ImageDataEntry)i);

                if (dataEntry != null && dataEntry->VirtualAddress != 0)
                {
                    ListViewItem item = new ListViewItem();

                    item.Text = ((ImageDataEntry)i).ToString();
                    item.SubItems.Add(new ListViewItem.ListViewSubItem(item, "0x" + dataEntry->VirtualAddress.ToString("x")));
                    item.SubItems.Add(new ListViewItem.ListViewSubItem(item, "0x" + dataEntry->Size.ToString("x")));

                    listImageData.Items.Add(item);
                }
            }





            listSections.Items.Clear();

            for (int i = 0; i < _mappedImage.NumberOfSections; i++)
            {
                ImageSectionHeader* section = &_mappedImage.Sections[i];
                ListViewItem item = new ListViewItem();

                item.Text = _mappedImage.GetSectionName(section);
                item.SubItems.Add(new ListViewItem.ListViewSubItem(item, "0x" + section->VirtualAddress.ToString("x")));
                item.SubItems.Add(new ListViewItem.ListViewSubItem(item, "0x" + section->SizeOfRawData.ToString("x")));
                item.SubItems.Add(new ListViewItem.ListViewSubItem(item, "0x" + section->PointerToRawData.ToString("x")));
                item.SubItems.Add(new ListViewItem.ListViewSubItem(item, section->Characteristics.ToString()));

                listSections.Items.Add(item);
            }





            listExports.VirtualListSize = _mappedImage.Exports.Count;





            listImports.Items.Clear();
            listImports.Groups.Clear();

            var list = new List<KeyValuePair<string,int> >();

            for (int i = 0; i < _mappedImage.Imports.Count; i++)
                list.Add(new KeyValuePair<string, int>(_mappedImage.Imports[i].Name, i));

            list.Sort((kvp1, kvp2) => StringComparer.CurrentCultureIgnoreCase.Compare(kvp1.Key, kvp2.Key));

            for (int i = 0; i < list.Count; i++)
            {
                var dll = _mappedImage.Imports[list[i].Value];
                int index = list[i].Value;

                listImports.Groups.Add(new ListViewGroup(list[i].Key));

                for (int j = 0; j < dll.Count; j++)
                {
                    var entry = dll[j];
                    ListViewItem item = new ListViewItem(listImports.Groups[listImports.Groups.Count - 1]);

                    if (entry.Name == null)
                    {
                        item.Text = "(Ordinal " + entry.Ordinal.ToString() + ")";
                        item.SubItems.Add(new ListViewItem.ListViewSubItem());
                    }
                    else
                    {
                        item.Text = entry.Name;
                        item.SubItems.Add(new ListViewItem.ListViewSubItem(item, entry.NameHint.ToString()));
                    }

                    listImports.Items.Add(item);
                }
            }


            listImports.SetGroupState(ListViewGroupState.Collapsed | ListViewGroupState.Collapsible, "Properties");


        }

        private void listExports_RetrieveVirtualItem(object sender, RetrieveVirtualItemEventArgs e)
        {
            unsafe
            {
                var entry = _mappedImage.Exports.GetEntry(e.ItemIndex);
                var function = _mappedImage.Exports.GetFunction(entry.Ordinal);

                e.Item = new ListViewItem(new string[]
                    {
                        entry.Ordinal.ToString(),
                        function.ForwardedName != null ? entry.Name + " > " + function.ForwardedName : entry.Name,
                        function.ForwardedName == null ?
                        "0x" + function.Function.Decrement(new IntPtr(_mappedImage.Memory)).ToString("x") :
                        ""
                    });
            }
        }

        private void listExports_DoubleClick(object sender, EventArgs e)
        {




        }
    }
}
