using System;
using System.Collections.Generic;
using System.Text;
using System.Globalization;
namespace Eraser.Util
{
 public struct FileSize : IConvertible
 {
  public FileSize(long filesize)
   : this()
  {
   Size = filesize;
  }
  public TypeCode GetTypeCode()
  {
   return TypeCode.Int64;
  }
  public bool ToBoolean(IFormatProvider provider)
  {
   throw new InvalidCastException();
  }
  public byte ToByte(IFormatProvider provider)
  {
   return Convert.ToByte(Size);
  }
  public char ToChar(IFormatProvider provider)
  {
   throw new InvalidCastException();
  }
  public DateTime ToDateTime(IFormatProvider provider)
  {
   throw new InvalidCastException();
  }
  public decimal ToDecimal(IFormatProvider provider)
  {
   return Convert.ToDecimal(Size);
  }
  public double ToDouble(IFormatProvider provider)
  {
   return Convert.ToDouble(Size);
  }
  public short ToInt16(IFormatProvider provider)
  {
   return Convert.ToInt16(Size);
  }
  public int ToInt32(IFormatProvider provider)
  {
   return Convert.ToInt32(Size);
  }
  public long ToInt64(IFormatProvider provider)
  {
   return Size;
  }
  public sbyte ToSByte(IFormatProvider provider)
  {
   return Convert.ToSByte(Size);
  }
  public float ToSingle(IFormatProvider provider)
  {
   return Convert.ToSingle(Size);
  }
  public string ToString(IFormatProvider provider)
  {
   return ToString(Size);
  }
  public object ToType(Type conversionType, IFormatProvider provider)
  {
   return Convert.ChangeType(Size, conversionType, provider);
  }
  public ushort ToUInt16(IFormatProvider provider)
  {
   return Convert.ToUInt16(Size);
  }
  public uint ToUInt32(IFormatProvider provider)
  {
   return Convert.ToUInt32(Size);
  }
  public ulong ToUInt64(IFormatProvider provider)
  {
   return Convert.ToUInt64(Size);
  }
  public long Size
  {
   get;
   private set;
  }
  public override string ToString()
  {
   return ToString(CultureInfo.CurrentCulture);
  }
  public static string ToString(long size)
  {
   string[] units = new string[] {
    "bytes",
    "KB",
    "MB",
    "GB",
    "TB",
    "PB",
    "EB"
   };
   double dSize = (double)size;
   for (int i = 0; i != units.Length; ++i)
   {
    if (dSize < 1000.0)
     if (i <= 1)
      return string.Format(CultureInfo.CurrentCulture,
       "{0} {1}", (int)dSize, units[i]);
     else
      return string.Format(CultureInfo.CurrentCulture,
       "{0:0.00} {1}", dSize, units[i]);
    dSize /= 1024.0;
   }
   return string.Format(CultureInfo.CurrentCulture, "{0, 2} {1}",
    dSize, units[units.Length - 1]);
  }
 }
}
