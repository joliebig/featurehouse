

using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;

namespace ProcessHacker.Native.Api
{
    [ComImport, Guid("965fc360-16ff-11d0-91cb-00aa00bbb723"), InterfaceType(ComInterfaceType.InterfaceIsIUnknown)]
    public interface ISecurityInformation
    {
        [PreserveSig]
        HResult GetObjectInformation(
            [Out] out SiObjectInfo ObjectInfo
            );

        [PreserveSig]
        HResult GetSecurity(
            [In] SecurityInformation RequestedInformation,
            [Out] out IntPtr SecurityDescriptor,
            [In] bool Default
            );

        [PreserveSig]
        HResult SetSecurity(
            [In] SecurityInformation SecurityInformation,
            [In] IntPtr SecurityDescriptor
            );

        [PreserveSig]
        HResult GetAccessRights(
            [In] ref Guid ObjectType,
            [In] SiObjectInfoFlags Flags,
            [Out] out IntPtr Access,
            [Out] out int Accesses,
            [Out] out int DefaultAccess
            );

        [PreserveSig]
        HResult MapGeneric(
            [In] ref Guid ObjectType,
            [In] ref AceFlags AceFlags,
            [In] ref int Mask
            );

        [PreserveSig]
        HResult GetInheritTypes(
            [Out] out IntPtr InheritTypes,
            [Out] out int InheritTypesCount
            );

        [PreserveSig]
        HResult PropertySheetPageCallback(
            [In] IntPtr hWnd,
            [In] SiCallbackMessage Msg,
            [In] SiPageType Page
            );
    }
}
