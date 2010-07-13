

using System;
using System.Drawing;
using System.Runtime.InteropServices;
using System.Windows.Forms;
using ProcessHacker;
using ProcessHacker.Native;
using ProcessHacker.Native.Api;
using TaskbarLib.Interop;

namespace TaskbarLib
{





    public static class Windows7Taskbar
    {



        [ThreadStatic]
        private static ITaskbarList3 _taskbarList;
        internal static ITaskbarList3 TaskbarList
        {
            get
            {
                if (_taskbarList == null)
                {
                    _taskbarList = (ITaskbarList3)new CTaskbarList();
                    HResult result = _taskbarList.HrInit();

                    result.ThrowIf();
                }
                return _taskbarList;
            }
        }






        public static JumpListManager CreateJumpListManager()
        {
            return new JumpListManager();
        }

        private static IPropertyStore InternalGetWindowPropertyStore(IntPtr hwnd)
        {
            IPropertyStore propStore;
            HResult shGetPropertyStoreResult = UnsafeNativeMethods.SHGetPropertyStoreForWindow(
                hwnd, ref SafeNativeMethods.IID_IPropertyStore, out propStore);
            shGetPropertyStoreResult.ThrowIf();

            return propStore;
        }

        private static void InternalEnableCustomWindowPreview(IntPtr hwnd, bool enable)
        {
            int t = enable ? 1 : 0;

            HResult setFirstAttributeResult = UnsafeNativeMethods.DwmSetWindowAttribute(
                hwnd, SafeNativeMethods.DWMWA_HAS_ICONIC_BITMAP, ref t, 4);
            setFirstAttributeResult.ThrowIf();

            HResult setSecondAttributeResult = UnsafeNativeMethods.DwmSetWindowAttribute(
                hwnd, SafeNativeMethods.DWMWA_FORCE_ICONIC_REPRESENTATION, ref t, 4);
            setSecondAttributeResult.ThrowIf();
        }
        public static string AppId
        {
            get
            {
                IPropertyStore propStore = InternalGetWindowPropertyStore(Program.HackerWindowHandle);
                PropVariant pv;
                HResult getValueResult = propStore.GetValue(ref PropertyKey.PKEY_AppUserModel_ID, out pv);
                getValueResult.ThrowIf();
                string appId = pv.GetValue();
                Marshal.ReleaseComObject(propStore);
                pv.Dispose();
                return appId;
            }
            set
            {
                IPropertyStore propStore = InternalGetWindowPropertyStore(Program.HackerWindowHandle);
                PropVariant pv = new PropVariant();
                pv.SetValue(value);
                HResult setValueResult = propStore.SetValue(ref PropertyKey.PKEY_AppUserModel_ID, ref pv);
                setValueResult.ThrowIf();
                Marshal.ReleaseComObject(propStore);
                pv.Dispose();
            }
        }
        public static string ProcessAppId
        {
            get
            {
                string appId;
                HResult getProcessAppUserModeIDResult = UnsafeNativeMethods.GetCurrentProcessExplicitAppUserModelID(out appId);
                getProcessAppUserModeIDResult.ThrowIf();
                return appId;
            }
            set
            {
                HResult setProcessAppUserModeIDResult = UnsafeNativeMethods.SetCurrentProcessExplicitAppUserModelID(value);
                setProcessAppUserModeIDResult.ThrowIf();
            }
        }
        public static void EnableCustomWindowPreview(this Form form)
        {
            InternalEnableCustomWindowPreview(form.Handle, true);
        }
        public static void DisableCustomWindowPreview(this Form form)
        {
            InternalEnableCustomWindowPreview(form.Handle, false);
        }
        public static void SetIconicThumbnail(this Form form, Bitmap bitmap)
        {
            HResult dwmSetIconicThumbnailResult = UnsafeNativeMethods.DwmSetIconicThumbnail(
                form.Handle, bitmap.GetHbitmap(), SafeNativeMethods.DWM_SIT_DISPLAYFRAME);
            dwmSetIconicThumbnailResult.ThrowIf();
        }
        public static void SetPeekBitmap(this Form form, Bitmap bitmap, bool displayFrame)
        {
            HResult dwmSetIconicLivePreviewBitmapResult = UnsafeNativeMethods.DwmSetIconicLivePreviewBitmap(
                form.Handle, bitmap.GetHbitmap(), IntPtr.Zero, displayFrame ? SafeNativeMethods.DWM_SIT_DISPLAYFRAME : (uint)0);
            dwmSetIconicLivePreviewBitmapResult.ThrowIf();
        }
        public static void SetPeekBitmap(this Form form, Bitmap bitmap, Point offset, bool displayFrame)
        {
            var nativePoint = new POINT(offset.X, offset.Y);
            HResult dwmSetIconicLivePreviewResult =
                UnsafeNativeMethods.DwmSetIconicLivePreviewBitmap(
                form.Handle, bitmap.GetHbitmap(), ref nativePoint, displayFrame ? SafeNativeMethods.DWM_SIT_DISPLAYFRAME : (uint)0);
            dwmSetIconicLivePreviewResult.ThrowIf();
        }
        public static void SetTaskbarOverlayIcon(Icon icon, string description)
        {
            HResult result = TaskbarList.SetOverlayIcon(
                Program.HackerWindowHandle, icon == null ? IntPtr.Zero : icon.Handle, description);
            result.ThrowIf();
        }
        public static void SetTaskbarOverlayIcon(this Form form, Icon icon, string description)
        {
            HResult result = TaskbarList.SetOverlayIcon(
                form.Handle, icon == null ? IntPtr.Zero : icon.Handle, description);
            result.ThrowIf();
        }
        public static void SetTaskbarProgress(this Form form, ProgressBar progressBar)
        {
            if (!form.IsDisposed && form.IsHandleCreated)
            {
                ulong maximum = Convert.ToUInt64(progressBar.Maximum);
                ulong progress = Convert.ToUInt64(progressBar.Value);
                SetTaskbarProgress(form.Handle, progress, maximum);
            }
        }
        public static void SetTaskbarProgress(this Form form, ToolStripProgressBar progressBar)
        {
            if (!form.IsDisposed && form.IsHandleCreated)
            {
                ulong maximum = Convert.ToUInt64(progressBar.Maximum);
                ulong progress = Convert.ToUInt64(progressBar.Value);
                SetTaskbarProgress(form.Handle, progress, maximum);
            }
        }
        public static void SetTaskbarProgress(IntPtr hwnd, ulong progress, ulong maximum)
        {
            HResult valueResult = TaskbarList.SetProgressValue(hwnd, progress, maximum);
            valueResult.ThrowIf();
        }
        public static void SetTaskbarProgressState(this Form form, ThumbnailProgressState state)
        {
            SetTaskbarProgressState(form.Handle, state);
        }
        public static void SetTaskbarProgressState(IntPtr hwnd, ThumbnailProgressState state)
        {
            HResult result = TaskbarList.SetProgressState(hwnd, (uint)state);
            result.ThrowIf();
        }
        [Flags]
        public enum ThumbnailProgressState
        {
            NoProgress = 0,
            Indeterminate = 0x1,
            Normal = 0x2,
            Error = 0x4,
            Paused = 0x8
        }
        private static void SetThumbnailClip(this Form form, Rectangle clipRect)
        {
            Rect rect = new Rect(clipRect.Left, clipRect.Top, clipRect.Right, clipRect.Bottom);
            HResult setThumbnailClipResult = TaskbarList.SetThumbnailClip(form.Handle, ref rect);
            setThumbnailClipResult.ThrowIf();
        }
        private static void SetThumbnailTooltip(this Form form, string tooltip)
        {
            HResult setThumbnailTooltipResult = TaskbarList.SetThumbnailTooltip(form.Handle, tooltip);
            setThumbnailTooltipResult.ThrowIf();
        }
        public static void AllowWindowMessagesThroughUipi()
        {
            if (OSVersion.HasTaskDialogs && Program.ElevationType == TokenElevationType.Full)
            {
                Win32.ChangeWindowMessageFilter((WindowMessage)UnsafeNativeMethods.WM_TaskbarButtonCreated, UipiFilterFlag.Add);
                Win32.ChangeWindowMessageFilter(WindowMessage.DwmSendIconicThumbnail, UipiFilterFlag.Add);
                Win32.ChangeWindowMessageFilter(WindowMessage.DwmSendIconicLivePreviewBitmap, UipiFilterFlag.Add);
                Win32.ChangeWindowMessageFilter(WindowMessage.Command, UipiFilterFlag.Add);
                Win32.ChangeWindowMessageFilter(WindowMessage.SysCommand, UipiFilterFlag.Add);
                Win32.ChangeWindowMessageFilter(WindowMessage.Activate, UipiFilterFlag.Add);
            }
        }
        public static uint TaskbarButtonCreatedMessage
        {
            get { return UnsafeNativeMethods.WM_TaskbarButtonCreated; }
        }
    }
}
