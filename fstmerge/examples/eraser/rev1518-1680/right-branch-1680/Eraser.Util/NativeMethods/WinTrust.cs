

using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;

namespace Eraser.Util
{
 internal static partial class NativeMethods
 {
  [DllImport("Wintrust.dll", CharSet = CharSet.Unicode)]
  public static extern int WinVerifyTrust(IntPtr hWnd, ref Guid pgActionID,
   ref WINTRUST_DATA pWVTData);
  [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)]
  public struct WINTRUST_FILE_INFO
  {
   public uint cbStruct;
   public string pcwszFilePath;
   public IntPtr hFile;
   public IntPtr pgKnownSubject;
  }
  [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)]
  public struct WINTRUST_DATA
  {
   public uint cbStruct;
   public IntPtr pPolicyCallbackData;
   public IntPtr pSIPClientData;
   public UIChoices dwUIChoice;
   public RevocationChecks fdwRevocationChecks;
   public UnionChoices dwUnionChoice;
   public IntPtr pUnion;
   public StateActions dwStateAction;
   public IntPtr hWVTStateData;
   private string pwszURLReference;
   public ProviderFlags dwProvFlags;
   public UIContexts dwUIContext;
   public enum UIChoices : uint
   {
    WTD_UI_ALL = 1,
    WTD_UI_NONE = 2,
    WTD_UI_NOBAD = 3,
    WTD_UI_NOGOOD = 4,
   }
   public enum RevocationChecks : uint
   {
    WTD_REVOKE_NONE = 0x00000000,
    WTD_REVOKE_WHOLECHAIN = 0x00000001
   }
   public enum UnionChoices : uint
   {
    WTD_CHOICE_FILE = 1,
    WTD_CHOICE_CATALOG = 2,
    WTD_CHOICE_BLOB = 3,
    WTD_CHOICE_SIGNER = 4,
    WTD_CHOICE_CERT = 5
   }
   public enum StateActions : uint
   {
    WTD_STATEACTION_IGNORE = 0x00000000,
    WTD_STATEACTION_VERIFY = 0x00000001,
    WTD_STATEACTION_CLOSE = 0x00000002,
    WTD_STATEACTION_AUTO_CACHE = 0x00000003,
    WTD_STATEACTION_AUTO_CACHE_FLUSH = 0x00000004
   }
   public enum ProviderFlags : uint
   {
    WTD_PROV_FLAGS_MASK = 0x0000FFFF,
    WTD_USE_IE4_TRUST_FLAG = 0x00000001,
    WTD_NO_IE4_CHAIN_FLAG = 0x00000002,
    WTD_NO_POLICY_USAGE_FLAG = 0x00000004,
    WTD_REVOCATION_CHECK_NONE = 0x00000010,
    WTD_REVOCATION_CHECK_END_CERT = 0x00000020,
    WTD_REVOCATION_CHECK_CHAIN = 0x00000040,
    WTD_REVOCATION_CHECK_CHAIN_EXCLUDE_ROOT = 0x00000080,
    WTD_SAFER_FLAG = 0x00000100,
    WTD_HASH_ONLY_FLAG = 0x00000200,
    WTD_USE_DEFAULT_OSVER_CHECK = 0x00000400,
    WTD_LIFETIME_SIGNING_FLAG = 0x00000800,
    WTD_CACHE_ONLY_URL_RETRIEVAL = 0x00001000
   }
   public enum UIContexts
   {
    WTD_UICONTEXT_EXECUTE = 0,
    WTD_UICONTEXT_INSTALL = 1
   }
  }
  public static readonly Guid WINTRUST_ACTION_GENERIC_VERIFY_V2 = new Guid(0xaac56b,
   unchecked((short)0xcd44), 0x11d0, new byte[] { 0x8c, 0xc2, 0x0, 0xc0, 0x4f, 0xc2, 0x95, 0xee });
 }
}
