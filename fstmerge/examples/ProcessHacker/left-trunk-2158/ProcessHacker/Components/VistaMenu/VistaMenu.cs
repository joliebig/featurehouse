using System;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Windows.Forms;
using ProcessHacker.Native;





namespace wyDay.Controls
{

    internal class Properties
    {
        public Image Image;
        public IntPtr renderBmpHbitmap = IntPtr.Zero;
        public Bitmap PreVistaBitmap;
    }



    [ProvideProperty("Image", typeof(MenuItem))]
    public partial class VistaMenu : Component, IExtenderProvider, ISupportInitialize
    {
        private Container components;
        private readonly Hashtable properties = new Hashtable();
        private readonly Hashtable menuParents = new Hashtable();

        private bool formHasBeenIntialized;


        private Queue<KeyValuePair<MenuItem, Image> > _pendingSetImageCalls =
            new Queue<KeyValuePair<MenuItem, Image> >();

        private static bool _firstVistaMenu = true;
        private bool _delaySetImageCalls = false;

        public bool DelaySetImageCalls
        {
            get { return _delaySetImageCalls; }
            set { _delaySetImageCalls = value; }
        }



        [DllImport("user32.dll", CharSet = CharSet.Auto)]
        public static extern bool SetMenuItemInfo(HandleRef hMenu, int uItem, bool fByPosition, MENUITEMINFO_T_RW lpmii);

        [DllImport("user32.dll", CharSet = CharSet.Auto)]
        public static extern bool SetMenuInfo(HandleRef hMenu, MENUINFO lpcmi);

        [DllImport("gdi32.dll")]
        public static extern bool DeleteObject(IntPtr hObject);




        public VistaMenu()
        {
            InitializeComponent();
        }

        public VistaMenu(IContainer container)
            : this()
        {
            container.Add(this);

            if (_firstVistaMenu)
            {
                _delaySetImageCalls = true;
                _firstVistaMenu = false;
            }
        }





        private void InitializeComponent()
        {
            components = new Container();
        }




        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {

                foreach (DictionaryEntry de in properties)
                {
                    if (((Properties)de.Value).renderBmpHbitmap != IntPtr.Zero)
                        DeleteObject(((Properties)de.Value).renderBmpHbitmap);
                    if (((Properties)de.Value).PreVistaBitmap != null)
                        ((Properties)de.Value).PreVistaBitmap.Dispose();
                }


                if (components != null)
                {
                    components.Dispose();
                }
            }
            base.Dispose(disposing);
        }

        bool IExtenderProvider.CanExtend(object o)
        {
            if (o is MenuItem)
            {

                if (((MenuItem)o).Parent != null)
                    return ((MenuItem)o).Parent.GetType() != typeof(MainMenu);


                return true;
            }

            if (o is Form)
                return true;

            return false;
        }

        private Properties EnsurePropertiesExists(MenuItem key)
        {
            Properties p = (Properties)properties[key];

            if (p == null)
            {
                p = new Properties();

                properties[key] = p;
            }

            return p;
        }




        [DefaultValue(null)]
        [Description("The Image for the MenuItem")]
        [Category("Appearance")]
        public Image GetImage(MenuItem mnuItem)
        {
            return EnsurePropertiesExists(mnuItem).Image;
        }

        [DefaultValue(null)]
        public void SetImage(MenuItem mnuItem, Image value)
        {
            this.SetImage(mnuItem, value, false);
        }

        public void SetImage(MenuItem mnuItem, Image value, bool ignorePending)
        {
            if (_delaySetImageCalls && !ignorePending)
            {
                _pendingSetImageCalls.Enqueue(new KeyValuePair<MenuItem, Image>(mnuItem, value));
                return;
            }

            Properties prop = EnsurePropertiesExists(mnuItem);

            if (DesignMode)
                prop.Image = value;

            if (!DesignMode && OSVersion.IsAboveOrEqual(WindowsVersion.Vista))
            {

                if (prop.renderBmpHbitmap != IntPtr.Zero)
                {
                    DeleteObject(prop.renderBmpHbitmap);
                    prop.renderBmpHbitmap = IntPtr.Zero;
                }


                if (value == null)
                {

                    RemoveVistaMenuItem(mnuItem);
                    return;
                }


                Bitmap renderBmp = new Bitmap(value.Width, value.Height, System.Drawing.Imaging.PixelFormat.Format32bppPArgb);
                Graphics g = Graphics.FromImage(renderBmp);

                g.DrawImage(value, 0, 0, value.Width, value.Height);
                g.Dispose();

                prop.renderBmpHbitmap = renderBmp.GetHbitmap(Color.FromArgb(0, 0, 0, 0));
                renderBmp.Dispose();

                if (formHasBeenIntialized)
                {
                    AddVistaMenuItem(mnuItem);
                }
            }
            else if (!DesignMode && OSVersion.IsBelow(WindowsVersion.Vista))
            {
                if (prop.PreVistaBitmap != null)
                {
                    prop.PreVistaBitmap.Dispose();
                    prop.PreVistaBitmap = null;
                }

                if (value == null)
                {
                    RemoveVistaMenuItem(mnuItem);
                    return;
                }

                Bitmap bmp = new Bitmap(value.Width, value.Height, System.Drawing.Imaging.PixelFormat.Format32bppPArgb);
                Graphics g = Graphics.FromImage(bmp);

                g.DrawImage(value, 0, 0, value.Width, value.Height);
                g.Dispose();

                prop.PreVistaBitmap = bmp;


                if (formHasBeenIntialized)
                {
                    AddPreVistaMenuItem(mnuItem);
                }
            }
        }

        public void PerformPendingSetImageCalls()
        {
            while (_pendingSetImageCalls.Count > 0)
            {
                var call = _pendingSetImageCalls.Dequeue();

                this.SetImage(call.Key, call.Value, true);
            }
        }





        void ISupportInitialize.BeginInit()
        {
        }

        readonly MENUINFO mnuInfo = new MENUINFO();

        void AddVistaMenuItem(MenuItem mnuItem)
        {

            List<MenuItem> mnuBitmapChildren = (List<MenuItem>)menuParents[mnuItem.Parent.Handle];


            if (mnuBitmapChildren == null)
            {
                if (mnuItem.Parent.GetType() == typeof(ContextMenu))
                    ((ContextMenu)mnuItem.Parent).Popup += MenuItem_Popup;
                else
                    ((MenuItem)mnuItem.Parent).Popup += MenuItem_Popup;


                SetMenuInfo(new HandleRef(null, mnuItem.Parent.Handle), mnuInfo);


                mnuBitmapChildren = new List<MenuItem>() { mnuItem };


                menuParents[mnuItem.Parent.Handle] = mnuBitmapChildren;
            }
            else
            {
                mnuBitmapChildren.Add(mnuItem);
            }
        }

        void AddPreVistaMenuItem(MenuItem mnuItem)
        {
            if (menuParents[mnuItem.Parent] == null)
            {
                menuParents[mnuItem.Parent] = true;

                if (formHasBeenIntialized)
                {

                    foreach (MenuItem menu in mnuItem.Parent.MenuItems)
                    {
                        menu.DrawItem += MenuItem_DrawItem;
                        menu.MeasureItem += MenuItem_MeasureItem;
                        menu.OwnerDraw = true;
                    }
                }
            }
        }

        public void RemoveVistaMenuItem(MenuItem mnuItem)
        {
            if (menuParents[mnuItem.Parent.Handle] != null)
            {
                List<MenuItem> mnuBitmapChildren = (List<MenuItem>)menuParents[mnuItem.Parent.Handle];

                mnuBitmapChildren.Remove(mnuItem);
            }
        }

        public void RemovePreVistaMenuItem(MenuItem mnuItem)
        {
            mnuItem.DrawItem -= MenuItem_DrawItem;
            mnuItem.MeasureItem -= MenuItem_MeasureItem;
            mnuItem.OwnerDraw = false;
        }

        void ISupportInitialize.EndInit()
        {
            if (!DesignMode)
            {
                if (OSVersion.IsAboveOrEqual(WindowsVersion.Vista))
                {
                    foreach (DictionaryEntry de in properties)
                    {
                        AddVistaMenuItem((MenuItem)de.Key);
                    }
                }
                else
                {
                    if (ownerForm != null)
                        ownerForm.ChangeUICues += ownerForm_ChangeUICues;

                    foreach (DictionaryEntry de in properties)
                    {
                        AddPreVistaMenuItem((MenuItem)de.Key);
                    }


                    foreach (DictionaryEntry parent in menuParents)
                    {
                        foreach (MenuItem mnuItem in ((Menu)parent.Key).MenuItems)
                        {
                            mnuItem.DrawItem += MenuItem_DrawItem;
                            mnuItem.MeasureItem += MenuItem_MeasureItem;
                            mnuItem.OwnerDraw = true;
                        }
                    }
                }

                formHasBeenIntialized = true;
            }
        }

        void MenuItem_Popup(object sender, EventArgs e)
        {

            IntPtr parentHandle = ((Menu)sender).Handle;


            List<MenuItem> mnuBitmapChildren = (List<MenuItem>)menuParents[parentHandle];

            MENUITEMINFO_T_RW menuItemInfo = new MENUITEMINFO_T_RW();

            foreach (MenuItem menuItem in mnuBitmapChildren)
            {

                menuItemInfo.hbmpItem = ((Properties)properties[menuItem]).renderBmpHbitmap;


                SetMenuItemInfo(new HandleRef(null, parentHandle),
                    (int)typeof(MenuItem).InvokeMember("MenuID", BindingFlags.DeclaredOnly | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.GetProperty, null, menuItem, null),
                    false,
                    menuItemInfo);
            }
        }
    }

    [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Auto)]
    public class MENUITEMINFO_T_RW
    {
        public int cbSize = Marshal.SizeOf(typeof(MENUITEMINFO_T_RW));
        public int fMask = 0x00000080;
        public int fType;
        public int fState;
        public int wID;
        public IntPtr hSubMenu = IntPtr.Zero;
        public IntPtr hbmpChecked = IntPtr.Zero;
        public IntPtr hbmpUnchecked = IntPtr.Zero;
        public IntPtr dwItemData = IntPtr.Zero;
        public IntPtr dwTypeData = IntPtr.Zero;
        public int cch;
        public IntPtr hbmpItem = IntPtr.Zero;
    }

    [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Auto)]
    public class MENUINFO
    {
        public int cbSize = Marshal.SizeOf(typeof(MENUINFO));
        public int fMask = 0x00000010;
        public int dwStyle = 0x04000000;
        public uint cyMax;
        public IntPtr hbrBack = IntPtr.Zero;
        public int dwContextHelpID;
        public IntPtr dwMenuData = IntPtr.Zero;
    }
}
