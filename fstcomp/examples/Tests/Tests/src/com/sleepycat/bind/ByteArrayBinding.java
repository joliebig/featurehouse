package com.sleepycat.bind; 
import com.sleepycat.je.DatabaseEntry; 
import de.ovgu.cide.jakutil.*; 
public  class  ByteArrayBinding  implements EntryBinding {
	 private static byte[] ZERO_LENGTH_BYTE_ARRAY=new byte[0];

	 public ByteArrayBinding(){ }

	 public Object entryToObject( DatabaseEntry entry){ int len=entry.getSize(); if (len == 0) { return ZERO_LENGTH_BYTE_ARRAY; } else { byte[] bytes=new byte[len]; System.arraycopy(entry.getData(),entry.getOffset(),bytes,0,bytes.length); return bytes; } }

	 public void objectToEntry( Object object, DatabaseEntry entry){ byte[] bytes=(byte[])object; entry.setData(bytes,0,bytes.length); }


}
