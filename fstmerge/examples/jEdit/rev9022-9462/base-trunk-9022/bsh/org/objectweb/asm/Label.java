

package bsh.org.objectweb.asm;



public class Label {

  

  CodeWriter owner;

  

  boolean resolved;

  

  int position;

  

  private int referenceCount;

  

  private int[] srcAndRefPositions;

  
  
  
  
  
  
  
  
  

  

  int beginStackSize;

  

  int maxStackSize;

  

  Edge successors;

  

  Label next;

  

  boolean pushed;

  
  
  

  

  public Label () {
  }

  
  
  

  

  void put (
    final CodeWriter owner,
    final ByteVector out,
    final int source,
    final boolean wideOffset)
  {
    if (CodeWriter.CHECK) {
      if (this.owner == null) {
        this.owner = owner;
      } else if (this.owner != owner) {
        throw new IllegalArgumentException();
      }
    }
    if (resolved) {
      if (wideOffset) {
        out.put4(position - source);
      } else {
        out.put2(position - source);
      }
    } else {
      if (wideOffset) {
        addReference(-1 - source, out.length);
        out.put4(-1);
      } else {
        addReference(source, out.length);
        out.put2(-1);
      }
    }
  }

  

  private void addReference (
    final int sourcePosition,
    final int referencePosition)
  {
    if (srcAndRefPositions == null) {
      srcAndRefPositions = new int[6];
    }
    if (referenceCount >= srcAndRefPositions.length) {
      int[] a = new int[srcAndRefPositions.length + 6];
      System.arraycopy(srcAndRefPositions, 0, a, 0, srcAndRefPositions.length);
      srcAndRefPositions = a;
    }
    srcAndRefPositions[referenceCount++] = sourcePosition;
    srcAndRefPositions[referenceCount++] = referencePosition;
  }

  

  boolean resolve (
    final CodeWriter owner,
    final int position,
    final byte[] data)
  {
    if (CodeWriter.CHECK) {
      if (this.owner == null) {
        this.owner = owner;
      }
      if (resolved || this.owner != owner) {
        throw new IllegalArgumentException();
      }
    }
    boolean needUpdate = false;
    this.resolved = true;
    this.position = position;
    int i = 0;
    while (i < referenceCount) {
      int source = srcAndRefPositions[i++];
      int reference = srcAndRefPositions[i++];
      int offset;
      if (source >= 0) {
        offset = position - source;
        if (offset < Short.MIN_VALUE || offset > Short.MAX_VALUE) {
          
          
          
          
          
          
          int opcode = data[reference - 1] & 0xFF;
          if (opcode <= Constants.JSR) {
            
            data[reference - 1] = (byte)(opcode + 49);
          } else {
            
            data[reference - 1] = (byte)(opcode + 20);
          }
          needUpdate = true;
        }
        data[reference++] = (byte)(offset >>> 8);
        data[reference] = (byte)offset;
      } else {
        offset = position + source + 1;
        data[reference++] = (byte)(offset >>> 24);
        data[reference++] = (byte)(offset >>> 16);
        data[reference++] = (byte)(offset >>> 8);
        data[reference] = (byte)offset;
      }
    }
    return needUpdate;
  }
}
