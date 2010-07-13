namespace Debugger.Interop.CorPub
{
    using System;
    using System.Runtime.CompilerServices;
    using System.Runtime.InteropServices;
 public enum COR_PUB_ENUMPROCESS
 {
  COR_PUB_MANAGEDONLY = 1
 }
 [ComImport, Guid("9613A0E7-5A68-11D3-8F84-00A0C9B4D50C"), InterfaceType((short) 1)]
 public interface ICorPublish
 {
  [MethodImpl(MethodImplOptions.InternalCall, MethodCodeType=MethodCodeType.Runtime)]
  void EnumProcesses([In, ComAliasName("CorpubProcessLib.COR_PUB_ENUMPROCESS")] COR_PUB_ENUMPROCESS Type, [MarshalAs(UnmanagedType.Interface)] out ICorPublishProcessEnum ppIEnum);
  [MethodImpl(MethodImplOptions.InternalCall, MethodCodeType=MethodCodeType.Runtime)]
  void GetProcess([In] uint pid, [MarshalAs(UnmanagedType.Interface)] out ICorPublishProcess ppProcess);
 }
}
