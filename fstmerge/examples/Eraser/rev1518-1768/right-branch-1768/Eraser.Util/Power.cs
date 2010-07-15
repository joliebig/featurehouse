

using System;
using System.Collections.Generic;
using System.Text;

namespace Eraser.Util
{
 public static class Power
 {
  public static ExecutionState ExecutionState
  {
   set
   {
    NativeMethods.SetThreadExecutionState((NativeMethods.EXECUTION_STATE)value);
   }
  }
 }
 public enum ExecutionState
 {
  None = 0,
  AwayModeRequired = (int)NativeMethods.EXECUTION_STATE.ES_AWAYMODE_REQUIRED,
  Continuous = unchecked((int)NativeMethods.EXECUTION_STATE.ES_CONTINUOUS),
  DisplayRequired = (int)NativeMethods.EXECUTION_STATE.ES_DISPLAY_REQUIRED,
  SystemRequired = (int)NativeMethods.EXECUTION_STATE.ES_SYSTEM_REQUIRED,
  UserPresent = (int)NativeMethods.EXECUTION_STATE.ES_USER_PRESENT
 }
}
