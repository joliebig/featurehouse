using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
namespace Eraser.Manager
{
 public abstract class Executor : IDisposable
 {
  ~Executor()
  {
   Dispose(false);
  }
  protected virtual void Dispose(bool disposing)
  {
  }
  public void Dispose()
  {
   Dispose(true);
   GC.SuppressFinalize(this);
  }
  public abstract void Run();
  public abstract void QueueTask(Task task);
  public abstract void ScheduleTask(Task task);
  public abstract void UnqueueTask(Task task);
  internal abstract bool IsTaskQueued(Task task);
  public abstract void QueueRestartTasks();
  public abstract ExecutorTasksCollection Tasks { get; }
  public EventHandler<TaskEventArgs> TaskAdded { get; set; }
  internal void OnTaskAdded(TaskEventArgs e)
  {
   if (TaskAdded != null)
    TaskAdded(this, e);
  }
  public EventHandler<TaskEventArgs> TaskDeleted { get; set; }
  internal void OnTaskDeleted(TaskEventArgs e)
  {
   if (TaskDeleted != null)
    TaskDeleted(this, e);
  }
  public EventHandler<TaskEventArgs> TaskProcessing { get; set; }
  protected void OnTaskProcessing(TaskEventArgs e)
  {
   if (TaskProcessing != null)
    TaskProcessing(this, e);
  }
  public EventHandler<TaskEventArgs> TaskProcessed { get; set; }
  protected void OnTaskProcessed(TaskEventArgs e)
  {
   if (TaskProcessed != null)
    TaskProcessed(this, e);
  }
 }
 public abstract class ExecutorTasksCollection : IList<Task>
 {
  protected ExecutorTasksCollection(Executor executor)
  {
   Owner = executor;
  }
  public abstract int IndexOf(Task item);
  public abstract void Insert(int index, Task item);
  public abstract void RemoveAt(int index);
  public abstract Task this[int index] { get; set; }
  public abstract void Add(Task item);
  public abstract void Clear();
  public abstract bool Contains(Task item);
  public abstract void CopyTo(Task[] array, int arrayIndex);
  public abstract int Count { get; }
  public bool IsReadOnly { get { return false; } }
  public abstract bool Remove(Task item);
  public abstract IEnumerator<Task> GetEnumerator();
  System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
  {
   return GetEnumerator();
  }
  public abstract void SaveToStream(Stream stream);
  public abstract void LoadFromStream(Stream stream);
  protected Executor Owner { get; private set; }
 }
}
