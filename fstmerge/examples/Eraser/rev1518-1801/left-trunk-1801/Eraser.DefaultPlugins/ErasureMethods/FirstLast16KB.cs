

using System;
using System.Collections.Generic;
using System.Text;

using Eraser.Manager;
using Eraser.Util;
using System.IO;
using System.Threading;
using System.Windows.Forms;

namespace Eraser.DefaultPlugins
{
 sealed class FirstLast16KB : ErasureMethod
 {
  public FirstLast16KB()
  {
   try
   {

    if (DefaultPlugin.Settings.FL16Method != Guid.Empty)
     method = ErasureMethodManager.GetInstance(
      DefaultPlugin.Settings.FL16Method);
    else if (ManagerLibrary.Settings.DefaultFileErasureMethod != Guid)
     method = ErasureMethodManager.GetInstance(
      ManagerLibrary.Settings.DefaultFileErasureMethod);
    else
     method = ErasureMethodManager.GetInstance(new Gutmann().Guid);
   }
   catch (ErasureMethodNotFoundException)
   {
    MessageBox.Show(S._("The First/last 16KB erasure method " +
     "requires another erasure method to erase the file.\n\nThis must " +
     "be set in the Plugin Settings dialog."), Name, MessageBoxButtons.OK,
     MessageBoxIcon.Error, MessageBoxDefaultButton.Button1,
     S.IsRightToLeft(null) ? MessageBoxOptions.RtlReading : 0);
   }
  }

  public override string Name
  {
   get { return S._("First/last 16KB Erasure"); }
  }

  public override int Passes
  {
   get { return 0; }
  }

  public override Guid Guid
  {
   get { return new Guid("{0C2E07BF-0207-49a3-ADE8-46F9E1499C01}"); }
  }

  public override long CalculateEraseDataSize(ICollection<string> paths, long targetSize)
  {

   if (method == null || method.Guid == Guid)
    throw new InvalidOperationException(S._("The First/last 16KB erasure method " +
     "requires another erasure method to erase the file.\n\nThis must " +
     "be set in the Plugin Settings dialog."));


   long amountToWrite = 0;
   if (paths == null)
   {
    if (targetSize <= DataSize)
     amountToWrite = targetSize;
    else
     amountToWrite = DataSize * 2;
   }
   else
    amountToWrite = paths.Count * DataSize * 2;


   return amountToWrite * method.Passes;
  }

  public override void Erase(Stream strm, long erasureLength, Prng prng,
   ErasureMethodProgressFunction callback)
  {

   if (method == null || method.Guid == Guid)
    throw new InvalidOperationException(S._("The First/last 16KB erasure method " +
     "requires another erasure method to erase the file.\n\nThis must " +
     "be set in the Plugin Settings dialog."));




   if (erasureLength != long.MaxValue)
    throw new ArgumentException(S._("The amount of data erased should not be " +
     "limited, since this is a self-limiting erasure method."));



   if (strm.Length < DataSize * 2)
   {
    method.Erase(strm, erasureLength, prng, callback);
    return;
   }



   long dataSize = method.CalculateEraseDataSize(null, DataSize * 2);
   ErasureMethodProgressFunction customCallback =
    delegate(long lastWritten, long totalData, int currentPass)
    {
     callback(lastWritten, dataSize, currentPass);
    };


   strm.Seek(0, SeekOrigin.Begin);
   method.Erase(strm, dataSize, prng, callback == null ? null: customCallback);


   strm.Seek(-dataSize, SeekOrigin.End);
   method.Erase(strm, long.MaxValue, prng, callback == null ? null : customCallback);
  }




  private const long DataSize = 16 * 1024;

  private ErasureMethod method;
 }
}
