

using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.Serialization;
using System.Security.Permissions;

using Eraser.Util;
using Eraser.Manager;

namespace Eraser.DefaultPlugins
{
 [Serializable]
 class EraseCustom : PassBasedErasureMethod
 {




  public EraseCustom(CustomErasureMethod method)
  {
   this.method = method;
  }




  internal static void RegisterAll()
  {
   if (DefaultPlugin.Settings.EraseCustom == null)
    return;

   Dictionary<Guid, CustomErasureMethod> methods =
    DefaultPlugin.Settings.EraseCustom;
   foreach (Guid guid in methods.Keys)
   {
    CustomErasureMethod method = methods[guid];
    ManagerLibrary.Instance.ErasureMethodRegistrar.Add(new EraseCustom(method));
   }
  }

  public override string Name
  {
   get { return method.Name; }
  }

  public override Guid Guid
  {
   get { return method.Guid; }
  }

  protected override bool RandomizePasses
  {
   get { return method.RandomizePasses; }
  }

  protected override ErasureMethodPass[] PassesSet
  {
   get { return method.Passes; }
  }

  CustomErasureMethod method;
 }




 [Serializable]
 internal class CustomErasureMethod : ISerializable
 {
  public CustomErasureMethod()
  {
   Name = string.Empty;
   Guid = Guid.Empty;
   RandomizePasses = true;
  }

  protected CustomErasureMethod(SerializationInfo info, StreamingContext context)
  {
   Name = info.GetString("Name");
   Guid = (Guid)info.GetValue("GUID", Guid.GetType());
   RandomizePasses = info.GetBoolean("RandomizePasses");
   List<PassData> passes = (List<PassData>)
    info.GetValue("Passes", typeof(List<PassData>));

   Passes = new ErasureMethodPass[passes.Count];
   for (int i = 0; i != passes.Count; ++i)
    Passes[i] = passes[i];
  }

  public string Name { get; set; }
  public Guid Guid { get; set; }
  public bool RandomizePasses { get; set; }
  public ErasureMethodPass[] Passes { get; set; }


  [SecurityPermission(SecurityAction.Demand, SerializationFormatter = true)]
  public virtual void GetObjectData(SerializationInfo info, StreamingContext context)
  {
   info.AddValue("Name", Name);
   info.AddValue("GUID", Guid);
   info.AddValue("RandomizePasses", RandomizePasses);

   List<PassData> passes = new List<PassData>(Passes.Length);
   foreach (ErasureMethodPass pass in Passes)
    passes.Add(new PassData(pass));
   info.AddValue("Passes", passes);
  }

  [Serializable]
  private class PassData
  {
   public PassData(ErasureMethodPass pass)
   {
    if (pass.Function == ErasureMethod.WriteConstant)
    {
     Random = false;
     OpaqueValue = pass.OpaqueValue;
    }
    else if (pass.Function == ErasureMethod.WriteRandom)
    {
     Random = true;
    }
    else
     throw new ArgumentException(S._("The custom erasure method can only comprise " +
      "passes containing constant or random passes"));
   }

   public static implicit operator ErasureMethodPass(PassData pass)
   {
    return new ErasureMethodPass(pass.Random ?
     new ErasureMethodPassFunction(ErasureMethod.WriteRandom) :
      new ErasureMethodPassFunction(ErasureMethod.WriteConstant),
     pass.OpaqueValue);
   }

   object OpaqueValue;
   bool Random;
  }

 }
}
