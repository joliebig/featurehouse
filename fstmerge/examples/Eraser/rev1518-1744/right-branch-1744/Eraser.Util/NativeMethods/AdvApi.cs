

using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;

namespace Eraser.Util
{
 internal static partial class NativeMethods
 {
  [DllImport("Advapi32.dll", SetLastError = true, CharSet = CharSet.Unicode)]
  [return: MarshalAs(UnmanagedType.Bool)]
  public static extern bool CryptAcquireContext(out SafeCryptHandle phProv,
   string pszContainer, string pszProvider, uint dwProvType, uint dwFlags);
  [DllImport("Advapi32.dll", SetLastError = true)]
  [return: MarshalAs(UnmanagedType.Bool)]
  public static extern bool CryptGenRandom(SafeCryptHandle hProv, uint dwLen,
   byte[] pbBuffer);
  [DllImport("Advapi32.dll", SetLastError = true)]
  [return: MarshalAs(UnmanagedType.Bool)]
  public static extern bool CryptReleaseContext(IntPtr hProv, uint dwFlags);
  public const uint PROV_RSA_FULL = 1;
  public const uint PROV_RSA_SIG = 2;
  public const uint PROV_DSS = 3;
  public const uint PROV_FORTEZZA = 4;
  public const uint PROV_MS_EXCHANGE = 5;
  public const uint PROV_SSL = 6;
  public const uint PROV_RSA_SCHANNEL = 12;
  public const uint PROV_DSS_DH = 13;
  public const uint PROV_EC_ECDSA_SIG = 14;
  public const uint PROV_EC_ECNRA_SIG = 15;
  public const uint PROV_EC_ECDSA_FULL = 16;
  public const uint PROV_EC_ECNRA_FULL = 17;
  public const uint PROV_DH_SCHANNEL = 18;
  public const uint PROV_SPYRUS_LYNKS = 20;
  public const uint PROV_RNG = 21;
  public const uint PROV_INTEL_SEC = 22;
  public const int NTE_BAD_KEYSET = unchecked((int)0x80090016);
  public const uint CRYPT_VERIFYCONTEXT = 0xF0000000;
  public const uint CRYPT_NEWKEYSET = 0x00000008;
  public const uint CRYPT_DELETEKEYSET = 0x00000010;
  public const uint CRYPT_MACHINE_KEYSET = 0x00000020;
  public const uint CRYPT_SILENT = 0x00000040;
 }
}
