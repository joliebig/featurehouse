using System;

namespace Be.Windows.Forms
{



 public interface IByteProvider
 {





  byte ReadByte(long index);





  void WriteByte(long index, byte value);






  void InsertBytes(long index, byte[] bs);






  void DeleteBytes(long index, long length);




  long Length {get;}



  event EventHandler LengthChanged;




  bool HasChanges();



  void ApplyChanges();



  event EventHandler Changed;





  bool SupportsWriteByte();




  bool SupportsInsertBytes();




  bool SupportsDeleteBytes();
 }
}
