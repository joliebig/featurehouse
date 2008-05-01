package com.sleepycat.util; 
import de.ovgu.cide.jakutil.*; 
public  class  UtfOps {
	 private static byte[] EMPTY_BYTES={};

	 private static String EMPTY_STRING="";

	 public static int getZeroTerminatedByteLength( byte[] bytes, int offset) throws IndexOutOfBoundsException { int len=0; while (bytes[offset++] != 0) { len++; } return len; }

	 public static int getByteLength( char[] chars){ return getByteLength(chars,0,chars.length); }

	 public static int getByteLength( char[] chars, int offset, int length){ int len=0; length+=offset; for (int i=offset; i < length; i++) { int c=chars[i]; if ((c >= 0x0001) && (c <= 0x007F)) { len++; } else if (c > 0x07FF) { len+=3; } else { len+=2; } } return len; }

	 public static int getCharLength( byte[] bytes) throws IllegalArgumentException, IndexOutOfBoundsException { return getCharLength(bytes,0,bytes.length); }

	 public static int getCharLength( byte[] bytes, int offset, int length) throws IllegalArgumentException, IndexOutOfBoundsException { int charCount=0; length+=offset; while (offset < length) {
switch ((bytes[offset] & 0xff) >> 4) {
case 0:
case 1:
case 2:
case 3:
case 4:
case 5:
case 6:
case 7: offset++; break;
case 12:
case 13: offset+=2; break;
case 14:
offset+=3;
break;
default :
throw new IllegalArgumentException();
}
charCount++;
}
return charCount;
}

	
public static int bytesToChars(byte[] bytes,int byteOffset,char[] chars,int charOffset,int len,boolean isByteLen) throws IllegalArgumentException, IndexOutOfBoundsException {
int char1, char2, char3;
len+=isByteLen ? byteOffset : charOffset;
while ((isByteLen ? byteOffset : charOffset) < len) {
char1=bytes[byteOffset++] & 0xff;
switch ((char1 & 0xff) >> 4) {
case 0:
case 1:
case 2:
case 3:
case 4:
case 5:
case 6:
case 7:
chars[charOffset++]=(char)char1;
break;
case 12:
case 13:
char2=bytes[byteOffset++];
if ((char2 & 0xC0) != 0x80) {
throw new IllegalArgumentException();
}
chars[charOffset++]=(char)(((char1 & 0x1F) << 6) | (char2 & 0x3F));
break;
case 14:
char2=bytes[byteOffset++];
char3=bytes[byteOffset++];
if (((char2 & 0xC0) != 0x80) || ((char3 & 0xC0) != 0x80)) throw new IllegalArgumentException();
chars[charOffset++]=(char)(((char1 & 0x0F) << 12) | ((char2 & 0x3F) << 6) | ((char3 & 0x3F) << 0));
break;
default :
throw new IllegalArgumentException();
}
}
return byteOffset;
}

	
public static void charsToBytes(char[] chars,int charOffset,byte[] bytes,int byteOffset,int charLength){
charLength+=charOffset;
for (int i=charOffset; i < charLength; i++) {
int c=chars[i];
if ((c >= 0x0001) && (c <= 0x007F)) {
bytes[byteOffset++]=(byte)c;
} else if (c > 0x07FF) {
bytes[byteOffset++]=(byte)(0xE0 | ((c >> 12) & 0x0F));
bytes[byteOffset++]=(byte)(0x80 | ((c >> 6) & 0x3F));
bytes[byteOffset++]=(byte)(0x80 | ((c >> 0) & 0x3F));
} else {
bytes[byteOffset++]=(byte)(0xC0 | ((c >> 6) & 0x1F));
bytes[byteOffset++]=(byte)(0x80 | ((c >> 0) & 0x3F));
}
}
}

	
public static String bytesToString(byte[] bytes,int offset,int length) throws IllegalArgumentException, IndexOutOfBoundsException {
if (length == 0) return EMPTY_STRING;
int charLen=UtfOps.getCharLength(bytes,offset,length);
char[] chars=new char[charLen];
UtfOps.bytesToChars(bytes,offset,chars,0,length,true);
return new String(chars,0,charLen);
}

	
public static byte[] stringToBytes(String string){
if (string.length() == 0) return EMPTY_BYTES;
char[] chars=string.toCharArray();
byte[] bytes=new byte[UtfOps.getByteLength(chars)];
UtfOps.charsToBytes(chars,0,bytes,0,chars.length);
return bytes;
}


}
