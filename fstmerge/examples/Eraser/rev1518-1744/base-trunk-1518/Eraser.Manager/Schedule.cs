using System;
using System.Collections.Generic;
using System.Text;
using System.Diagnostics;
using System.Runtime.Serialization;
using System.Security.Permissions;
using System.Globalization;
using Eraser.Util;
namespace Eraser.Manager
{
 [Serializable]
 public abstract class Schedule : ISerializable
 {
  [Serializable]
  private class RunManuallySchedule : Schedule
  {
   public RunManuallySchedule(SerializationInfo info, StreamingContext context)
   {
   }
   [SecurityPermission(SecurityAction.Demand, SerializationFormatter = true)]
   public override void GetObjectData(SerializationInfo info, StreamingContext context)
   {
   }
   public RunManuallySchedule()
   {
   }
   public override string UIText
   {
    get { return string.Empty; }
   }
  }
  [Serializable]
  private class RunNowSchedule : Schedule
  {
   public RunNowSchedule(SerializationInfo info, StreamingContext context)
   {
   }
   [SecurityPermission(SecurityAction.Demand, SerializationFormatter = true)]
   public override void GetObjectData(SerializationInfo info, StreamingContext context)
   {
   }
   public RunNowSchedule()
   {
   }
   public override string UIText
   {
    get { return string.Empty; }
   }
  }
  [Serializable]
  private class RunOnRestartSchedule : Schedule
  {
   public RunOnRestartSchedule(SerializationInfo info, StreamingContext context)
   {
   }
   [SecurityPermission(SecurityAction.Demand, SerializationFormatter = true)]
   public override void GetObjectData(SerializationInfo info, StreamingContext context)
   {
   }
   public RunOnRestartSchedule()
   {
   }
   public override string UIText
   {
    get { return S._("Running on restart"); }
   }
  }
  public abstract string UIText
  {
   get;
  }
  public Task Owner
  {
   get;
   internal set;
  }
  [SecurityPermission(SecurityAction.Demand, SerializationFormatter = true)]
  public abstract void GetObjectData(SerializationInfo info, StreamingContext context);
  [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
  public static readonly Schedule RunManually = new RunManuallySchedule();
  [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
  public static readonly Schedule RunNow = new RunNowSchedule();
  [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
  public static readonly Schedule RunOnRestart = new RunOnRestartSchedule();
 }
 [Serializable]
 public class RecurringSchedule : Schedule
 {
  public override string UIText
  {
   get
   {
    string result = string.Empty;
    switch (type)
    {
     case RecurringScheduleUnit.Daily:
      if (frequency != 1)
       result = S._("Once every {0} days", frequency);
      else
       result = S._("Once every day");
      break;
     case RecurringScheduleUnit.Weekdays:
      result = S._("Every weekday");
      break;
     case RecurringScheduleUnit.Weekly:
      if ((weeklySchedule & DaysOfWeek.Monday) != 0)
       result = S._("Every Monday, {0}");
      if ((weeklySchedule & DaysOfWeek.Tuesday) != 0)
       result += S._("Every Tuesday, {0}");
      if ((weeklySchedule & DaysOfWeek.Wednesday) != 0)
       result += S._("Every Wednesday, {0}");
      if ((weeklySchedule & DaysOfWeek.Thursday) != 0)
       result += S._("Every Thursday, {0}");
      if ((weeklySchedule & DaysOfWeek.Friday) != 0)
       result += S._("Every Friday, {0}");
      if ((weeklySchedule & DaysOfWeek.Saturday) != 0)
       result += S._("Every Saturday, {0}");
      if ((weeklySchedule & DaysOfWeek.Sunday) != 0)
       result += S._("Every Sunday, {0}");
      result = string.Format(CultureInfo.CurrentCulture, result,
       frequency == 1 ?
        S._("once every {0} week.", frequency) :
        S._("once every {0} weeks.", frequency));
      break;
     case RecurringScheduleUnit.Monthly:
      if (frequency == 1)
       result = S._("On day {0} of every month", monthlySchedule);
      else
       result = S._("On day {0} of every {1} months", monthlySchedule,
        frequency);
      break;
    }
    return result + S._(", at {0}", executionTime.TimeOfDay.ToString());
   }
  }
  protected RecurringSchedule(SerializationInfo info, StreamingContext context)
  {
   type = (RecurringScheduleUnit)info.GetValue("Type", typeof(RecurringScheduleUnit));
   frequency = (int)info.GetValue("Frequency", typeof(int));
   executionTime = (DateTime)info.GetValue("ExecutionTime", typeof(DateTime));
   weeklySchedule = (DaysOfWeek)info.GetValue("WeeklySchedule", typeof(DaysOfWeek));
   monthlySchedule = (int)info.GetValue("MonthlySchedule", typeof(int));
   LastRun = (DateTime)info.GetDateTime("LastRun");
   NextRunCache = (DateTime)info.GetDateTime("NextRun");
  }
  [SecurityPermission(SecurityAction.Demand, SerializationFormatter = true)]
  public override void GetObjectData(SerializationInfo info, StreamingContext context)
  {
   info.AddValue("Type", type);
   info.AddValue("Frequency", frequency);
   info.AddValue("ExecutionTime", executionTime);
   info.AddValue("WeeklySchedule", weeklySchedule);
   info.AddValue("MonthlySchedule", monthlySchedule);
   info.AddValue("LastRun", LastRun);
   info.AddValue("NextRun", NextRunCache);
  }
  public RecurringSchedule()
  {
  }
  public RecurringScheduleUnit ScheduleType
  {
   get { return type; }
   set
   {
    type = value;
    if (Owner != null)
     Owner.OnTaskEdited();
   }
  }
  public int Frequency
  {
   get
   {
    if (ScheduleType != RecurringScheduleUnit.Daily && ScheduleType != RecurringScheduleUnit.Weekly &&
     ScheduleType != RecurringScheduleUnit.Monthly)
     throw new InvalidOperationException(S._("The ScheduleUnit of the schedule " +
      "does not require a frequency value, this field would contain garbage."));
    return frequency;
   }
   set
   {
    if (value == 0)
     throw new ArgumentException(S._("The frequency of the recurrence should " +
      "be greater than one"));
    frequency = value;
    if (Owner != null)
     Owner.OnTaskEdited();
   }
  }
  public DateTime ExecutionTime
  {
   get { return executionTime; }
   set
   {
    executionTime = value;
    if (Owner != null)
     Owner.OnTaskEdited();
   }
  }
  public DaysOfWeek WeeklySchedule
  {
   get
   {
    if (ScheduleType != RecurringScheduleUnit.Weekly)
     throw new InvalidOperationException(S._("The ScheduleUnit of the schedule " +
      "does not require the WeeklySchedule value, this field would contain garbage"));
    return weeklySchedule;
   }
   set
   {
    if (value == 0)
     throw new ArgumentException(S._("The WeeklySchedule should have at " +
      "least one day where the task should be run."));
    weeklySchedule = value;
    if (Owner != null)
     Owner.OnTaskEdited();
   }
  }
  public int MonthlySchedule
  {
   get
   {
    if (ScheduleType != RecurringScheduleUnit.Monthly)
     throw new InvalidOperationException(S._("The ScheduleUnit of the schedule does " +
      "not require the MonthlySchedule value, this field would contain garbage"));
    return monthlySchedule;
   }
   set
   {
    monthlySchedule = value;
    if (Owner != null)
     Owner.OnTaskEdited();
   }
  }
  public DateTime LastRun
  {
   get;
   private set;
  }
  public DateTime NextRun
  {
   get
   {
    DateTime nextRun = LastRun;
    if (nextRun == DateTime.MinValue)
     nextRun = DateTime.Now;
    nextRun = new DateTime(nextRun.Year, nextRun.Month, nextRun.Day, executionTime.Hour,
     executionTime.Minute, executionTime.Second);
    switch (ScheduleType)
    {
     case RecurringScheduleUnit.Daily:
     {
      long daysToAdd = (DateTime.Now - nextRun).Days;
      nextRun = nextRun.AddDays(daysToAdd);
      if (nextRun < DateTime.Now)
       nextRun = nextRun.AddDays(frequency);
      break;
     }
     case RecurringScheduleUnit.Weekdays:
     {
      while (nextRun < DateTime.Now ||
       LastRun.DayOfWeek == DayOfWeek.Saturday ||
       LastRun.DayOfWeek == DayOfWeek.Sunday)
       nextRun = nextRun.AddDays(1);
      break;
     }
     case RecurringScheduleUnit.Weekly:
     {
      if (weeklySchedule == 0)
       break;
      do
      {
       if (CanRunOnDay(nextRun) && nextRun >= DateTime.Now)
        break;
       nextRun = nextRun.AddDays(1);
      }
      while (nextRun.DayOfWeek < DayOfWeek.Saturday);
      while (nextRun < DateTime.Now || !CanRunOnDay(nextRun))
      {
       nextRun = nextRun.AddDays(7 * (frequency - 1));
       for (int daysInWeek = 7; daysInWeek-- != 0; nextRun = nextRun.AddDays(1))
       {
        if (CanRunOnDay(nextRun) && nextRun >= DateTime.Now)
         break;
       }
      }
      break;
     }
     case RecurringScheduleUnit.Monthly:
      if (LastRun != DateTime.MinValue)
       nextRun = nextRun.AddMonths(frequency);
      while (monthlySchedule < nextRun.Day)
       nextRun = nextRun.AddDays(1);
      nextRun = nextRun.AddDays(-((int)monthlySchedule - nextRun.Day));
      while (nextRun < DateTime.Now)
       nextRun = nextRun.AddMonths(frequency);
      break;
    }
    return nextRun;
   }
  }
  public bool MissedPreviousSchedule
  {
   get
   {
    return LastRun != DateTime.MinValue && NextRun != NextRunCache;
   }
  }
  private bool CanRunOnDay(DateTime date)
  {
   if (ScheduleType != RecurringScheduleUnit.Weekly)
    throw new ArgumentException(S._("The ScheduleUnit of the schedule does " +
     "not use the WeeklySchedule value, this field would contain garbage"));
   return ((int)weeklySchedule & (1 << (int)date.DayOfWeek)) != 0;
  }
  internal void Reschedule(DateTime lastRun)
  {
   LastRun = lastRun;
   NextRunCache = NextRun;
  }
  private RecurringScheduleUnit type;
  private int frequency;
  private DateTime executionTime;
  private DaysOfWeek weeklySchedule;
  private int monthlySchedule;
  private DateTime NextRunCache;
 }
 public enum RecurringScheduleUnit
 {
  Daily,
  Weekdays,
  Weekly,
  Monthly
 }
 [Flags]
 [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA1714:FlagsEnumsShouldHavePluralNames")]
 public enum DaysOfWeek
 {
  None = 0,
  Sunday = 1 << DayOfWeek.Sunday,
  Monday = 1 << DayOfWeek.Monday,
  Tuesday = 1 << DayOfWeek.Tuesday,
  Wednesday = 1 << DayOfWeek.Wednesday,
  Thursday = 1 << DayOfWeek.Thursday,
  Friday = 1 << DayOfWeek.Friday,
  Saturday = 1 << DayOfWeek.Saturday
 }
}
