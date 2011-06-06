using System;
using System.Runtime.InteropServices;
namespace Novell.FormsTrayApp
{
 public class Security
 {
  public Security()
  {
  }
  private static readonly int SECURITY_BUILTIN_DOMAIN_RID = 0x00000020;
  private static readonly int DOMAIN_ALIAS_RID_USERS = 0x00000221;
  private static readonly int DACL_SECURITY_INFORMATION = 0x00000004;
  private static readonly int ERROR_INSUFFICIENT_BUFFER = 122;
  private static readonly int SECURITY_DESCRIPTOR_REVISION = 1;
  private static readonly int ACL_REVISION = 2;
  private static readonly int INHERITED_ACE = 0x10;
  private static readonly uint MAXDWORD = 0xffffffff;
  public static readonly uint GENERIC_READ = 0x80000000;
  public static readonly uint GENERIC_WRITE = 0x40000000;
  public static readonly uint GENERIC_EXECUTE = 0x20000000;
  public static readonly int OBJECT_INHERIT_ACE = 0x1;
  public static readonly int CONTAINER_INHERIT_ACE = 0x2;
  private static readonly int SE_DACL_AUTO_INHERIT_REQ = 0x0100;
  private static readonly int SE_DACL_AUTO_INHERITED = 0x0400;
  private static readonly int SE_DACL_PROTECTED = 0x1000;
  [DllImport("advapi32.dll")]
  static extern bool AllocateAndInitializeSid(ref SID_IDENTIFIER_AUTHORITY pAuthority, byte subAuthorityCount, int subAuthority0, int subAuthority1, int subAuthority2, int subAuthority3, int subAuthority4, int subAuthority5, int subAuthority6, int subAuthority7, ref IntPtr pSid);
  [DllImport("advapi32.dll")]
  static extern void FreeSid(IntPtr pSid);
  [DllImport("advapi32.dll", CallingConvention=CallingConvention.Winapi, SetLastError=true, CharSet=CharSet.Unicode)]
  static extern bool GetFileSecurity(string fileName, int RequestedInformation, IntPtr pSecurityDescriptor, int length, out int lengthNeeded);
  [DllImport("advapi32.dll", CallingConvention=CallingConvention.Winapi, SetLastError=true, CharSet=CharSet.Unicode)]
  static extern bool SetFileSecurity(string lpFileName, int SecurityInformation, ref SECURITY_DESCRIPTOR pSecurityDescriptor);
  [DllImport("advapi32.dll", CallingConvention=CallingConvention.Winapi, SetLastError=true)]
  static extern bool GetSecurityDescriptorDacl(IntPtr pSecurityDescriptor, out bool bDaclPresent, ref IntPtr pDacl, out bool bDaclDefaulted);
  [DllImport("advapi32.dll")]
  static extern int GetLengthSid(IntPtr pSid);
  [DllImport("advapi32.dll")]
  static extern bool InitializeAcl(IntPtr pAcl, int nAclLength, int dwAclRevision);
  [DllImport("advapi32.dll")]
  static extern bool GetAce(IntPtr pAcl, int dwAceIndex, ref IntPtr pAce);
  [DllImport("advapi32.dll")]
  static extern bool EqualSid(IntPtr pSid1, IntPtr pSid2);
  [DllImport("advapi32.dll")]
  static extern bool AddAce(IntPtr pAcl, int dwAceRevision, uint dwStartingAceIndex, IntPtr pAceList, int nAceListLength);
  [DllImport("advapi32.dll")]
  static extern bool AddAccessAllowedAce(IntPtr pAcl, int dwAceRevision, uint AccessMask, IntPtr pSid);
  [DllImport("advapi32.dll")]
  static extern bool AddAccessAllowedAceEx(IntPtr pAcl, int dwAceRevision, int AceFlags, uint AccessMask, IntPtr pSid);
  [DllImport("advapi32.dll")]
  static extern bool SetSecurityDescriptorDacl(ref SECURITY_DESCRIPTOR pSecurityDescriptor, bool bDaclPresent, IntPtr pDacl, bool bDaclDefaulted);
  [DllImport("advapi32.dll")]
  static extern bool GetSecurityDescriptorControl(IntPtr pSecurityDescriptor, out short pControl, out int lpdwRevision);
  [DllImport("advapi32.dll")]
  static extern bool SetSecurityDescriptorControl(ref SECURITY_DESCRIPTOR pSecurityDescriptor, short ControlBitsOfInterest, short ControlBitsToSet);
  [StructLayout(LayoutKind.Sequential)]
  public struct ACCESS_ALLOWED_ACE
  {
   public ACE_HEADER Header;
   public int Mask;
   public int SidStart;
  }
  [StructLayout(LayoutKind.Sequential)]
  public struct ACE_HEADER
  {
   public byte AceType;
   public byte AceFlags;
   public short AceSize;
  }
  [StructLayout(LayoutKind.Sequential)]
  public struct SID_IDENTIFIER_AUTHORITY
  {
   public byte Value0;
   public byte Value1;
   public byte Value2;
   public byte Value3;
   public byte Value4;
   public byte Value5;
  }
  [StructLayout(LayoutKind.Sequential)]
  public struct ACL
  {
   public byte AclRevision;
   public byte Sbz1;
   public ushort AclSize;
   public ushort AceCount;
   public ushort Sbz2;
  }
  [StructLayout(LayoutKind.Sequential)]
  public struct SECURITY_DESCRIPTOR
  {
   public byte Revision;
   public byte Sbz1;
   public short Control;
   public IntPtr Owner;
   public IntPtr Group;
   public IntPtr Sacl;
   public IntPtr Dacl;
  }
  public bool SetAccess(string Path, int AceFlags, uint AccessMask)
  {
   bool retValue = false;
   IntPtr pSid = IntPtr.Zero;
   IntPtr fileSD = IntPtr.Zero;
   IntPtr pNewAcl = IntPtr.Zero;
   try
   {
    try
    {
     SID_IDENTIFIER_AUTHORITY ntAuthority;
     ntAuthority.Value0 = ntAuthority.Value1 = ntAuthority.Value2 = ntAuthority.Value3 = ntAuthority.Value4 = 0;
     ntAuthority.Value5 = 5;
     if (!AllocateAndInitializeSid(
      ref ntAuthority,
      2,
      SECURITY_BUILTIN_DOMAIN_RID,
      DOMAIN_ALIAS_RID_USERS,
      0, 0, 0, 0, 0, 0,
      ref pSid))
     {
      return false;
     }
     int sdLength = 0;
     if (!GetFileSecurity(Path, DACL_SECURITY_INFORMATION, fileSD, 0, out sdLength))
     {
      if (Marshal.GetLastWin32Error() != ERROR_INSUFFICIENT_BUFFER)
      {
       return false;
      }
      fileSD = Marshal.AllocHGlobal(sdLength);
      if (!GetFileSecurity(Path, DACL_SECURITY_INFORMATION, fileSD, sdLength, out sdLength))
      {
       return false;
      }
      bool bDaclPresent;
      bool bDaclDefaulted;
      IntPtr aclPtr = IntPtr.Zero;
      if (!GetSecurityDescriptorDacl(fileSD, out bDaclPresent, ref aclPtr, out bDaclDefaulted))
      {
       return false;
      }
      if (aclPtr.Equals(IntPtr.Zero))
      {
       return false;
      }
      MemoryMarshaler mm = new MemoryMarshaler(aclPtr);
      ACL acl = (ACL)mm.ParseStruct(typeof(ACL));
      ACCESS_ALLOWED_ACE accessAllowedAce = new ACCESS_ALLOWED_ACE();
      int n1 = Marshal.SizeOf(accessAllowedAce);
      int n2 = Marshal.SizeOf(n1);
      int cbNewAcl = acl.AclSize + 12 + GetLengthSid(pSid) - 4 ;
      pNewAcl = Marshal.AllocHGlobal(cbNewAcl);
      if (!InitializeAcl(pNewAcl, cbNewAcl, ACL_REVISION))
      {
       return false;
      }
      uint newAceIndex = 0;
      IntPtr acePtr = IntPtr.Zero;
      int CurrentAceIndex = 0;
      if (bDaclPresent && (acl.AceCount > 0))
      {
       for (CurrentAceIndex = 0; CurrentAceIndex < acl.AceCount; CurrentAceIndex++)
       {
        if (!GetAce(aclPtr, CurrentAceIndex, ref acePtr))
        {
         return false;
        }
        mm.Ptr = acePtr;
        accessAllowedAce = (ACCESS_ALLOWED_ACE)mm.ParseStruct(typeof(ACCESS_ALLOWED_ACE));
        if ((accessAllowedAce.Header.AceFlags & INHERITED_ACE) == INHERITED_ACE)
         break;
        mm.Ptr = acePtr;
        mm.Advance(8);
        if (EqualSid(pSid, mm.Ptr))
         continue;
        if (!AddAce(pNewAcl, ACL_REVISION, MAXDWORD, acePtr, accessAllowedAce.Header.AceSize))
        {
         return false;
        }
        newAceIndex++;
       }
      }
      if (!AddAccessAllowedAceEx(pNewAcl, ACL_REVISION, AceFlags, AccessMask, pSid))
      {
       return false;
      }
      if (bDaclPresent && (acl.AceCount > 0))
      {
       for (; CurrentAceIndex < acl.AceCount; CurrentAceIndex++)
       {
        if (!GetAce(aclPtr, CurrentAceIndex, ref acePtr))
        {
         return false;
        }
        mm.Ptr = acePtr;
        accessAllowedAce = (ACCESS_ALLOWED_ACE)mm.ParseStruct(typeof(ACCESS_ALLOWED_ACE));
        if (!AddAce(pNewAcl, ACL_REVISION, MAXDWORD, acePtr, accessAllowedAce.Header.AceSize))
        {
         return false;
        }
       }
      }
      SECURITY_DESCRIPTOR newSD;
      newSD.Revision = (byte)SECURITY_DESCRIPTOR_REVISION;
      newSD.Sbz1 = 0;
      newSD.Control = 0;
      newSD.Owner = IntPtr.Zero;
      newSD.Group = IntPtr.Zero;
      newSD.Sacl = IntPtr.Zero;
      newSD.Dacl = IntPtr.Zero;
      if (!SetSecurityDescriptorDacl(ref newSD, true, pNewAcl, false))
      {
       return false;
      }
      short controlBitsOfInterest = 0;
      short controlBitsToSet = 0;
      short oldControlBits = 0;
      int revision = 0;
      if (!GetSecurityDescriptorControl(fileSD, out oldControlBits, out revision))
      {
       return false;
      }
      if ((oldControlBits & SE_DACL_AUTO_INHERITED) == SE_DACL_AUTO_INHERITED)
      {
       controlBitsOfInterest = (short)(SE_DACL_AUTO_INHERIT_REQ | SE_DACL_AUTO_INHERITED);
       controlBitsToSet = controlBitsOfInterest;
      }
      else if ((oldControlBits & SE_DACL_PROTECTED) == SE_DACL_PROTECTED)
      {
       controlBitsOfInterest = (short)SE_DACL_PROTECTED;
       controlBitsToSet = controlBitsOfInterest;
      }
      if (controlBitsOfInterest > 0)
      {
       if (!SetSecurityDescriptorControl(ref newSD, controlBitsOfInterest, controlBitsToSet))
       {
        return false;
       }
      }
      if (!SetFileSecurity(Path, DACL_SECURITY_INFORMATION, ref newSD))
      {
       return false;
      }
      retValue = true;
     }
    }
    catch {}
   }
   finally
   {
    if (pSid != IntPtr.Zero)
    {
     FreeSid(pSid);
    }
    if (fileSD != IntPtr.Zero)
    {
     Marshal.FreeHGlobal(fileSD);
    }
    if (pNewAcl != IntPtr.Zero)
    {
     Marshal.FreeHGlobal(pNewAcl);
    }
   }
   return retValue;
  }
 }
 public class MemoryMarshaler
 {
  private IntPtr ptr;
  public MemoryMarshaler(IntPtr ptr)
  {
   this.ptr = ptr;
  }
  public void Advance(int cbLength)
  {
   long p = ptr.ToInt64();
   p += cbLength;
   ptr = (IntPtr)p;
  }
  public object ParseStruct(System.Type type)
  {
   return ParseStruct(type, true);
  }
  public object ParseStruct(System.Type type, bool moveOffset)
  {
   object o = Marshal.PtrToStructure(ptr, type);
   if (moveOffset)
    Advance(Marshal.SizeOf(type));
   return o;
  }
  public IntPtr Ptr
  {
   get { return ptr; }
   set { ptr = value; }
  }
 }
}
