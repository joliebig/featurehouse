

package bsh.org.objectweb.asm;



final class ByteVector {

  

  byte[] data;

  

  int length;

  

  public ByteVector () {
    data = new byte[64];
  }

  

  public ByteVector (final int initialSize) {
    data = new byte[initialSize];
  }

  

  public ByteVector put1 (final int b) {
    int length = this.length;
    if (length + 1 > data.length) {
      enlarge(1);
    }
    data[length++] = (byte)b;
    this.length = length;
    return this;
  }

  

  public ByteVector put11 (final int b1, final int b2) {
    int length = this.length;
    if (length + 2 > data.length) {
      enlarge(2);
    }
    byte[] data = this.data;
    data[length++] = (byte)b1;
    data[length++] = (byte)b2;
    this.length = length;
    return this;
  }

  

  public ByteVector put2 (final int s) {
    int length = this.length;
    if (length + 2 > data.length) {
      enlarge(2);
    }
    byte[] data = this.data;
    data[length++] = (byte)(s >>> 8);
    data[length++] = (byte)s;
    this.length = length;
    return this;
  }

  

  public ByteVector put12 (final int b, final int s) {
    int length = this.length;
    if (length + 3 > data.length) {
      enlarge(3);
    }
    byte[] data = this.data;
    data[length++] = (byte)b;
    data[length++] = (byte)(s >>> 8);
    data[length++] = (byte)s;
    this.length = length;
    return this;
  }

  

  public ByteVector put4 (final int i) {
    int length = this.length;
    if (length + 4 > data.length) {
      enlarge(4);
    }
    byte[] data = this.data;
    data[length++] = (byte)(i >>> 24);
    data[length++] = (byte)(i >>> 16);
    data[length++] = (byte)(i >>> 8);
    data[length++] = (byte)i;
    this.length = length;
    return this;
  }

  

  public ByteVector put8 (final long l) {
    int length = this.length;
    if (length + 8 > data.length) {
      enlarge(8);
    }
    byte[] data = this.data;
    int i = (int)(l >>> 32);
    data[length++] = (byte)(i >>> 24);
    data[length++] = (byte)(i >>> 16);
    data[length++] = (byte)(i >>> 8);
    data[length++] = (byte)i;
    i = (int)l;
    data[length++] = (byte)(i >>> 24);
    data[length++] = (byte)(i >>> 16);
    data[length++] = (byte)(i >>> 8);
    data[length++] = (byte)i;
    this.length = length;
    return this;
  }

  

  public ByteVector putUTF (final String s) {
    int charLength = s.length();
    int byteLength = 0;
    for (int i = 0; i < charLength; ++i) {
      char c = s.charAt(i);
      if (c >= '\001' && c <= '\177') {
        byteLength++;
      } else if (c > '\u') {
        byteLength += 3;
      } else {
        byteLength += 2;
      }
    }
    if (byteLength > 65535) {
      throw new IllegalArgumentException();
    }
    int length = this.length;
    if (length + 2 + byteLength > data.length) {
      enlarge(2 + byteLength);
    }
    byte[] data = this.data;
    data[length++] = (byte)(byteLength >>> 8);
    data[length++] = (byte)(byteLength);
    for (int i = 0; i < charLength; ++i) {
      char c = s.charAt(i);
      if (c >= '\001' && c <= '\177') {
        data[length++] = (byte)c;
      } else if (c > '\u') {
        data[length++] = (byte)(0xE0 | c >> 12 & 0xF);
        data[length++] = (byte)(0x80 | c >> 6 & 0x3F);
        data[length++] = (byte)(0x80 | c & 0x3F);
      } else {
        data[length++] = (byte)(0xC0 | c >> 6 & 0x1F);
        data[length++] = (byte)(0x80 | c & 0x3F);
      }
    }
    this.length = length;
    return this;
  }

  

  public ByteVector putByteArray (
    final byte[] b,
    final int off,
    final int len)
  {
    if (length + len > data.length) {
      enlarge(len);
    }
    if (b != null) {
      System.arraycopy(b, off, data, length, len);
    }
    length += len;
    return this;
  }

  

  private void enlarge (final int size) {
    byte[] newData = new byte[Math.max(2*data.length, length + size)];
    System.arraycopy(data, 0, newData, 0, length);
    data = newData;
  }
}
