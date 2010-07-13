

using System.Runtime.InteropServices;
using ProcessHacker.Native.Api;
using ProcessHacker.Native;
using System;

public class VistaTreeView : System.Windows.Forms.TreeView
{



    private const int TV_FIRST = 0x1100;
    private const int TVM_SETEXTENDEDSTYLE = TV_FIRST + 44;
    private const int TVS_EX_AUTOHSCROLL = 0x0020;
    private const int TVS_EX_FADEINOUTEXPANDOS = 0x0040;

    protected override void OnHandleCreated(System.EventArgs e)
    {
        if (OSVersion.IsAboveOrEqual(WindowsVersion.Vista))
        {
            Win32.SendMessage(this.Handle, (WindowMessage)TVM_SETEXTENDEDSTYLE, 0, TVS_EX_FADEINOUTEXPANDOS);
            HResult setThemeResult = Win32.SetWindowTheme(this.Handle, "explorer", null);
            setThemeResult.ThrowIf();
        }
        base.OnHandleCreated(e);
    }
}
