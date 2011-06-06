

package org.objectweb.asm;



public class ClassWriter implements ClassVisitor {

  

  final static int CLASS = 7;

  

  final static int FIELD = 9;

  

  final static int METH = 10;

  

  final static int IMETH = 11;

  

  final static int STR = 8;

  

  final static int INT = 3;

  

  final static int FLOAT = 4;

  

  final static int LONG = 5;

  

  final static int DOUBLE = 6;

  

  final static int NAME_TYPE = 12;

  

  final static int UTF8 = 1;

  

  private short index;

  

  private ByteVector pool;

  

  private Item[] table;

  

  private int threshold;

  

  private int access;

  

  private int name;

  

  private int superName;

  

  private int interfaceCount;

  

  private int[] interfaces;

  

  private Item sourceFile;

  

  private int fieldCount;

  

  private ByteVector fields;

  

  private boolean computeMaxs;

  

  CodeWriter firstMethod;

  

  CodeWriter lastMethod;

  

  private int innerClassesCount;

  

  private ByteVector innerClasses;

  

  Item key;

  

  Item key2;

  

  Item key3;

  

  final static int NOARG_INSN = 0;

  

  final static int SBYTE_INSN = 1;

  

  final static int SHORT_INSN = 2;

  

  final static int VAR_INSN = 3;

  

  final static int IMPLVAR_INSN = 4;

  

  final static int TYPE_INSN = 5;

  

  final static int FIELDORMETH_INSN = 6;

  

  final static int ITFMETH_INSN = 7;

  

  final static int LABEL_INSN = 8;

  

  final static int LABELW_INSN = 9;

  

  final static int LDC_INSN = 10;

  

  final static int LDCW_INSN = 11;

  

  final static int IINC_INSN = 12;

  

  final static int TABL_INSN = 13;

  

  final static int LOOK_INSN = 14;

  

  final static int MANA_INSN = 15;

  

  final static int WIDE_INSN = 16;

  

  static byte[] TYPE;

  
  
  

  

  static {
    int i;
    byte[] b = new byte[220];
    String s =
      "AAAAAAAAAAAAAAAABCKLLDDDDDEEEEEEEEEEEEEEEEEEEEAAAAAAAADDDDDEEEEEEEEE" +
      "EEEEEEEEEEEAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMAAA" +
      "AAAAAAAAAAAAAAAAAIIIIIIIIIIIIIIIIDNOAAAAAAGGGGGGGHAFBFAAFFAAQPIIJJII" +
      "IIIIIIIIIIIIIIII";
    for (i = 0; i < b.length; ++i) {
      b[i] = (byte)(s.charAt(i) - 'A');
    }
    TYPE = b;

    
  }

  
  
  

  

  public ClassWriter (final boolean computeMaxs) {
    index = 1;
    pool = new ByteVector();
    table = new Item[64];
    threshold = (int)(0.75d*table.length);
    key = new Item();
    key2 = new Item();
    key3 = new Item();
    this.computeMaxs = computeMaxs;
  }

  
  
  

  public void visit (
    final int access,
    final String name,
    final String superName,
    final String[] interfaces,
    final String sourceFile)
  {
    this.access = access;
    this.name = newClass(name).index;
    this.superName = superName == null ? 0 : newClass(superName).index;
    if (interfaces != null && interfaces.length > 0) {
      interfaceCount = interfaces.length;
      this.interfaces = new int[interfaceCount];
      for (int i = 0; i < interfaceCount; ++i) {
        this.interfaces[i] = newClass(interfaces[i]).index;
      }
    }
    if (sourceFile != null) {
      newUTF8("SourceFile");
      this.sourceFile = newUTF8(sourceFile);
    }
    if ((access & Constants.ACC_DEPRECATED) != 0) {
      newUTF8("Deprecated");
    }
  }

  public void visitInnerClass (
    final String name,
    final String outerName,
    final String innerName,
    final int access)
  {
    if (innerClasses == null) {
      newUTF8("InnerClasses");
      innerClasses = new ByteVector();
    }
    ++innerClassesCount;
    innerClasses.put2(name == null ? 0 : newClass(name).index);
    innerClasses.put2(outerName == null ? 0 : newClass(outerName).index);
    innerClasses.put2(innerName == null ? 0 : newUTF8(innerName).index);
    innerClasses.put2(access);
  }

  public void visitField (
    final int access,
    final String name,
    final String desc,
    final Object value)
  {
    ++fieldCount;
    if (fields == null) {
      fields = new ByteVector();
    }
    fields.put2(access).put2(newUTF8(name).index).put2(newUTF8(desc).index);
    int attributeCount = 0;
    if (value != null) {
      ++attributeCount;
    }
    if ((access & Constants.ACC_SYNTHETIC) != 0) {
      ++attributeCount;
    }
    if ((access & Constants.ACC_DEPRECATED) != 0) {
      ++attributeCount;
    }
    fields.put2(attributeCount);
    if (value != null) {
      fields.put2(newUTF8("ConstantValue").index);
      fields.put4(2).put2(newCst(value).index);
    }
    if ((access & Constants.ACC_SYNTHETIC) != 0) {
      fields.put2(newUTF8("Synthetic").index).put4(0);
    }
    if ((access & Constants.ACC_DEPRECATED) != 0) {
      fields.put2(newUTF8("Deprecated").index).put4(0);
    }
  }

  public CodeVisitor visitMethod (
    final int access,
    final String name,
    final String desc,
    final String[] exceptions)
  {
    CodeWriter cw = new CodeWriter(this, computeMaxs);
    cw.init(access, name, desc, exceptions);
    return cw;
  }

  public void visitEnd () {
  }

  
  
  

  

  public byte[] toByteArray () {
    
    int size = 24 + 2*interfaceCount;
    if (fields != null) {
      size += fields.length;
    }
    int nbMethods = 0;
    CodeWriter cb = firstMethod;
    while (cb != null) {
      ++nbMethods;
      size += cb.getSize();
      cb = cb.next;
    }
    size += pool.length;
    int attributeCount = 0;
    if (sourceFile != null) {
      ++attributeCount;
      size += 8;
    }
    if ((access & Constants.ACC_DEPRECATED) != 0) {
      ++attributeCount;
      size += 6;
    }
    if (innerClasses != null) {
      ++attributeCount;
      size += 8 + innerClasses.length;
    }
    
    
    ByteVector out = new ByteVector(size);
    out.put4(0xCAFEBABE).put2(3).put2(45);
    out.put2(index).putByteArray(pool.data, 0, pool.length);
    out.put2(access).put2(name).put2(superName);
    out.put2(interfaceCount);
    for (int i = 0; i < interfaceCount; ++i) {
      out.put2(interfaces[i]);
    }
    out.put2(fieldCount);
    if (fields != null) {
      out.putByteArray(fields.data, 0, fields.length);
    }
    out.put2(nbMethods);
    cb = firstMethod;
    while (cb != null) {
      cb.put(out);
      cb = cb.next;
    }
    out.put2(attributeCount);
    if (sourceFile != null) {
      out.put2(newUTF8("SourceFile").index).put4(2).put2(sourceFile.index);
    }
    if ((access & Constants.ACC_DEPRECATED) != 0) {
      out.put2(newUTF8("Deprecated").index).put4(0);
    }
    if (innerClasses != null) {
      out.put2(newUTF8("InnerClasses").index);
      out.put4(innerClasses.length + 2).put2(innerClassesCount);
      out.putByteArray(innerClasses.data, 0, innerClasses.length);
    }
    return out.data;
  }

  
  
  

  

  Item newCst (final Object cst) {
    if (cst instanceof Integer) {
      int val = ((Integer)cst).intValue();
      return newInteger(val);
    } else if (cst instanceof Float) {
      float val = ((Float)cst).floatValue();
      return newFloat(val);
    } else if (cst instanceof Long) {
      long val = ((Long)cst).longValue();
      return newLong(val);
    } else if (cst instanceof Double) {
      double val = ((Double)cst).doubleValue();
      return newDouble(val);
    } else if (cst instanceof String) {
      return newString((String)cst);
    } else {
      throw new IllegalArgumentException("value " + cst);
    }
  }

  

  Item newUTF8 (final String value) {
    key.set(UTF8, value, null, null);
    Item result = get(key);
    if (result == null) {
      pool.put1(UTF8).putUTF(value);
      result = new Item(index++, key);
      put(result);
    }
    return result;
  }

  

  Item newClass (final String value) {
    key2.set(CLASS, value, null, null);
    Item result = get(key2);
    if (result == null) {
      pool.put12(CLASS, newUTF8(value).index);
      result = new Item(index++, key2);
      put(result);
    }
    return result;
  }

  

  Item newField (
    final String owner,
    final String name,
    final String desc)
  {
    key3.set(FIELD, owner, name, desc);
    Item result = get(key3);
    if (result == null) {
      put122(FIELD, newClass(owner).index, newNameType(name, desc).index);
      result = new Item(index++, key3);
      put(result);
    }
    return result;
  }

  

  Item newMethod (
    final String owner,
    final String name,
    final String desc)
  {
    key3.set(METH, owner, name, desc);
    Item result = get(key3);
    if (result == null) {
      put122(METH, newClass(owner).index, newNameType(name, desc).index);
      result = new Item(index++, key3);
      put(result);
    }
    return result;
  }

  

  Item newItfMethod (
    final String ownerItf,
    final String name,
    final String desc)
  {
    key3.set(IMETH, ownerItf, name, desc);
    Item result = get(key3);
    if (result == null) {
      put122(IMETH, newClass(ownerItf).index, newNameType(name, desc).index);
      result = new Item(index++, key3);
      put(result);
    }
    return result;
  }

  

  private Item newInteger (final int value) {
    key.set(value);
    Item result = get(key);
    if (result == null) {
      pool.put1(INT).put4(value);
      result = new Item(index++, key);
      put(result);
    }
    return result;
  }

  

  private Item newFloat (final float value) {
    key.set(value);
    Item result = get(key);
    if (result == null) {
      pool.put1(FLOAT).put4(Float.floatToIntBits(value));
      result = new Item(index++, key);
      put(result);
    }
    return result;
  }

  

  private Item newLong (final long value) {
    key.set(value);
    Item result = get(key);
    if (result == null) {
      pool.put1(LONG).put8(value);
      result = new Item(index, key);
      put(result);
      index += 2;
    }
    return result;
  }

  

  private Item newDouble (final double value) {
    key.set(value);
    Item result = get(key);
    if (result == null) {
      pool.put1(DOUBLE).put8(Double.doubleToLongBits(value));
      result = new Item(index, key);
      put(result);
      index += 2;
    }
    return result;
  }

  

  private Item newString (final String value) {
    key2.set(STR, value, null, null);
    Item result = get(key2);
    if (result == null) {
      pool.put12(STR, newUTF8(value).index);
      result = new Item(index++, key2);
      put(result);
    }
    return result;
  }

  

  private Item newNameType (final String name, final String desc) {
    key2.set(NAME_TYPE, name, desc, null);
    Item result = get(key2);
    if (result == null) {
      put122(NAME_TYPE, newUTF8(name).index, newUTF8(desc).index);
      result = new Item(index++, key2);
      put(result);
    }
    return result;
  }

  

  private Item get (final Item key) {
    Item tab[] = table;
    int hashCode = key.hashCode;
    int index = (hashCode & 0x7FFFFFFF) % tab.length;
    for (Item i = tab[index]; i != null; i = i.next) {
      if (i.hashCode == hashCode && key.isEqualTo(i)) {
        return i;
      }
    }
    return null;
  }

  

  private void put (final Item i) {
    if (index > threshold) {
      int oldCapacity = table.length;
      Item oldMap[] = table;
      int newCapacity = oldCapacity * 2 + 1;
      Item newMap[] = new Item[newCapacity];
      threshold = (int)(newCapacity * 0.75);
      table = newMap;
      for (int j = oldCapacity; j-- > 0; ) {
        for (Item old = oldMap[j]; old != null; ) {
          Item e = old;
          old = old.next;
          int index = (e.hashCode & 0x7FFFFFFF) % newCapacity;
          e.next = newMap[index];
          newMap[index] = e;
        }
      }
    }
    int index = (i.hashCode & 0x7FFFFFFF) % table.length;
    i.next = table[index];
    table[index] = i;
  }

  

  private void put122 (final int b, final int s1, final int s2) {
    pool.put12(b, s1).put2(s2);
  }
}
