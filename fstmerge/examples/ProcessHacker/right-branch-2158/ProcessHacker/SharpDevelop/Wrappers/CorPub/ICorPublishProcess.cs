namespace Debugger.Core.Wrappers.CorPub
{
 using System;
 using System.Runtime.InteropServices;
 using System.Text;
 using Debugger.Wrappers;
 public partial class ICorPublishProcess
 {
  private Debugger.Interop.CorPub.ICorPublishProcess wrappedObject;
  internal Debugger.Interop.CorPub.ICorPublishProcess WrappedObject
  {
   get
   {
    return this.wrappedObject;
   }
  }
  public ICorPublishProcess(Debugger.Interop.CorPub.ICorPublishProcess wrappedObject)
  {
   this.wrappedObject = wrappedObject;
   ResourceManager.TrackCOMObject(wrappedObject, typeof(ICorPublishProcess));
  }
  public static ICorPublishProcess Wrap(Debugger.Interop.CorPub.ICorPublishProcess objectToWrap)
  {
   if ((objectToWrap != null))
   {
    return new ICorPublishProcess(objectToWrap);
   } else
   {
    return null;
   }
  }
  public int ProcessId
  {
   get
   {
    uint id;
    wrappedObject.GetProcessID(out id);
    return (int)id;
   }
  }
  public bool IsManaged
  {
   get
   {
    int managed;
    wrappedObject.IsManaged(out managed);
    return managed != 0;
   }
  }
 }
}
