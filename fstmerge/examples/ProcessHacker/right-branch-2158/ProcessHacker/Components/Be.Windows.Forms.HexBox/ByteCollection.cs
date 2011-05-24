using System;

using System.Collections;

namespace Be.Windows.Forms
{



 public class ByteCollection : CollectionBase
 {



  public ByteCollection()
  {}





  public ByteCollection(byte[] bs)
  { AddRange(bs); }




  public byte this[int index]
  {
   get { return (byte)List[index]; }
   set { List[index] = value; }
  }





  public void Add(byte b)
  { List.Add(b); }





  public void AddRange(byte[] bs)
  { InnerList.AddRange(bs); }





  public void Remove(byte b)
  { List.Remove(b); }






  public void RemoveRange(int index, int count)
  { InnerList.RemoveRange(index, count); }






  public void InsertRange(int index, byte[] bs)
  { InnerList.InsertRange(index, bs); }





  public byte[] GetBytes()
  {
   byte[] bytes = new byte[Count];
   InnerList.CopyTo(0, bytes, 0, bytes.Length);
   return bytes;
  }






  public void Insert(int index, byte b)
  {
   InnerList.Insert(index, b);
  }




  public int IndexOf(byte b)
  {
   return InnerList.IndexOf(b);
  }




  public bool Contains(bool b)
  {
   return InnerList.Contains(b);
  }




  public void CopyTo(byte[] bs, int index)
  {
   InnerList.CopyTo(bs, index);
  }





  public byte[] ToArray()
  {
   byte[] data = new byte[this.Count];
   this.CopyTo(data, 0);
   return data;
  }

 }
}
