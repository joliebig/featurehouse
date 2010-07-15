






using System;
using System.Collections.Generic;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Threading;
using System.Windows.Forms;

namespace Debugger.Interop
{
 public delegate T MethodInvokerWithReturnValue<T>();

 public enum CallMethod {DirectCall, Manual, HiddenForm, HiddenFormWithTimeout};

 public class MTA2STA
 {
  Form hiddenForm;
  IntPtr hiddenFormHandle;

  System.Threading.Thread targetThread;
  CallMethod callMethod = CallMethod.HiddenFormWithTimeout;

  Queue<MethodInvoker> pendingCalls = new Queue<MethodInvoker>();
  ManualResetEvent pendingCallsNotEmpty = new ManualResetEvent(false);

  WaitHandle EnqueueCall(MethodInvoker callDelegate)
  {
   lock (pendingCalls) {
    ManualResetEvent callDone = new ManualResetEvent(false);
    pendingCalls.Enqueue(delegate{
                          callDelegate();
                          callDone.Set();
                         });
    pendingCallsNotEmpty.Set();
    return callDone;
   }
  }




  public void WaitForCall()
  {
   pendingCallsNotEmpty.WaitOne();
  }

  public void WaitForCall(TimeSpan timeout)
  {
   pendingCallsNotEmpty.WaitOne(timeout, false);
  }




  public void PerformAllCalls()
  {
   while (true) {
    if (!PerformCall()) {
     return;
    }
   }
  }




  public bool PerformCall()
  {
   MethodInvoker nextMethod;
   lock (pendingCalls) {
    if (pendingCalls.Count > 0) {
     nextMethod = pendingCalls.Dequeue();
    } else {
     pendingCallsNotEmpty.Reset();
     return false;
    }
   }
   nextMethod();
   return true;
  }

  public CallMethod CallMethod {
   get {
    return callMethod;
   }
   set {
    callMethod = value;
   }
  }

  public MTA2STA()
  {
   targetThread = System.Threading.Thread.CurrentThread;

   hiddenForm = new Form();

   hiddenFormHandle = hiddenForm.Handle;
  }




  public int SoftWait(params WaitHandle[] waitFor)
  {
   List<WaitHandle> waits = new List<WaitHandle> (waitFor);
   waits.Add(pendingCallsNotEmpty);
   while(true) {
    int i = WaitHandle.WaitAny(waits.ToArray());
    PerformAllCalls();
    if (i < waits.Count - 1) {
     return i;
    }
   }
  }




  public WaitHandle AsyncCall(MethodInvoker callDelegate)
  {
   WaitHandle callDone = EnqueueCall(callDelegate);
   TriggerInvoke();
   return callDone;
  }

  public T Call<T>(MethodInvokerWithReturnValue<T> callDelegate)
  {
   T returnValue = default(T);
   Call(delegate { returnValue = callDelegate(); }, true);
   return returnValue;
  }

  public void Call(MethodInvoker callDelegate)
  {
   Call(callDelegate, false);
  }

  void Call(MethodInvoker callDelegate, bool hasReturnValue)
  {

   WaitHandle callDone = EnqueueCall(callDelegate);

   if (targetThread == System.Threading.Thread.CurrentThread) {
    PerformAllCalls();
    return;
   }


   TriggerInvoke();


   if (!hasReturnValue && callMethod == CallMethod.HiddenFormWithTimeout) {

    if (!callDone.WaitOne(5000, true)) {
     System.Console.WriteLine("Call time out! (continuing)");
     System.Console.WriteLine(new System.Diagnostics.StackTrace(true).ToString());
    }
   } else {
    callDone.WaitOne();
   }
  }

  void TriggerInvoke()
  {
   switch (callMethod) {
    case CallMethod.DirectCall:
     PerformAllCalls();
     break;
    case CallMethod.Manual:

     break;
    case CallMethod.HiddenForm:
    case CallMethod.HiddenFormWithTimeout:
     hiddenForm.BeginInvoke((MethodInvoker)PerformAllCalls);
     break;
   }
  }

  public static object MarshalParamTo(object param, Type outputType)
  {
   if (param is IntPtr) {
    return MarshalIntPtrTo((IntPtr)param, outputType);
   } else {
    return param;
   }
  }

  public static T MarshalIntPtrTo<T>(IntPtr param)
  {
   return (T)MarshalIntPtrTo(param, typeof(T));
  }

  public static object MarshalIntPtrTo(IntPtr param, Type outputType)
  {

   if (outputType == typeof(IntPtr)) {
    return param;
   }

   if ((IntPtr)param == IntPtr.Zero) {
    return null;
   }

   if (outputType == typeof(string)) {
    return Marshal.PtrToStringAuto((IntPtr)param);
   }

   object comObject = Marshal.GetObjectForIUnknown(param);
   return Activator.CreateInstance(outputType, comObject);
  }
  public static object InvokeMethod(object targetObject, string functionName, object[] functionParameters)
  {
   System.Reflection.MethodInfo method;
   if (targetObject is Type) {
    method = ((Type)targetObject).GetMethod(functionName);
   } else {
    method = targetObject.GetType().GetMethod(functionName);
   }
   ParameterInfo[] methodParamsInfo = method.GetParameters();
   object[] convertedParams = new object[methodParamsInfo.Length];
   for (int i = 0; i < convertedParams.Length; i++) {
    convertedParams[i] = MarshalParamTo(functionParameters[i], methodParamsInfo[i].ParameterType);
   }
   try {
    if (targetObject is Type) {
     return method.Invoke(null, convertedParams);
    } else {
     return method.Invoke(targetObject, convertedParams);
    }
   } catch (System.Exception exception) {
    throw new Exception("Invoke of " + functionName + " failed.", exception);
   }
  }
 }
}
