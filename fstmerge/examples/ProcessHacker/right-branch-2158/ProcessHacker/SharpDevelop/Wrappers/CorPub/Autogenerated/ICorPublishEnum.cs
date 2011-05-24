namespace Debugger.Interop.CorPub
{
    using System;
    using System.Runtime.CompilerServices;
    using System.Runtime.InteropServices;
 [ComImport, Guid("C0B22967-5A69-11D3-8F84-00A0C9B4D50C"), InterfaceType((short) 1)]
 public interface ICorPublishEnum
 {
  [MethodImpl(MethodImplOptions.InternalCall, MethodCodeType=MethodCodeType.Runtime)]
  void Skip([In] uint celt);
  [MethodImpl(MethodImplOptions.InternalCall, MethodCodeType=MethodCodeType.Runtime)]
  void Reset();
  [MethodImpl(MethodImplOptions.InternalCall, MethodCodeType=MethodCodeType.Runtime)]
  void Clone([MarshalAs(UnmanagedType.Interface)] out ICorPublishEnum ppEnum);
  [MethodImpl(MethodImplOptions.InternalCall, MethodCodeType=MethodCodeType.Runtime)]
  void GetCount(out uint pcelt);
 }
}
