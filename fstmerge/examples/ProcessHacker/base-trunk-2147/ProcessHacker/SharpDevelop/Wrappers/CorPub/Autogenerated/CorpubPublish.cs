namespace Debugger.Interop.CorPub
{
 using System;
 using System.Runtime.CompilerServices;
 using System.Runtime.InteropServices;
 [ComImport, CoClass(typeof(CorpubPublishClass)), Guid("9613A0E7-5A68-11D3-8F84-00A0C9B4D50C")]
 public interface CorpubPublish : ICorPublish
 {
 }
}
