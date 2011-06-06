

package org.gjt.sp.jedit.bsh.org.objectweb.asm;



final class Item {

  

  short index;

  

  int type;

  

  int intVal;

  

  long longVal;

  

  float floatVal;

  

  double doubleVal;

  

  String strVal1;

  

  String strVal2;

  

  String strVal3;

  

  int hashCode;

  

  Item next;

  

  Item () {
  }

  

  Item (final short index, final Item i) {
    this.index = index;
    type = i.type;
    intVal = i.intVal;
    longVal = i.longVal;
    floatVal = i.floatVal;
    doubleVal = i.doubleVal;
    strVal1 = i.strVal1;
    strVal2 = i.strVal2;
    strVal3 = i.strVal3;
    hashCode = i.hashCode;
  }

  

  void set (final int intVal) {
    this.type = ClassWriter.INT;
    this.intVal = intVal;
    this.hashCode = type + intVal;
  }

  

  void set (final long longVal) {
    this.type = ClassWriter.LONG;
    this.longVal = longVal;
    this.hashCode = type + (int)longVal;
  }

  

  void set (final float floatVal) {
    this.type = ClassWriter.FLOAT;
    this.floatVal = floatVal;
    this.hashCode = type + (int)floatVal;
  }

  

  void set (final double doubleVal) {
    this.type = ClassWriter.DOUBLE;
    this.doubleVal = doubleVal;
    this.hashCode = type + (int)doubleVal;
  }

  

  void set (
    final int type,
    final String strVal1,
    final String strVal2,
    final String strVal3)
  {
    this.type = type;
    this.strVal1 = strVal1;
    this.strVal2 = strVal2;
    this.strVal3 = strVal3;
    switch (type) {
      case ClassWriter.UTF8:
      case ClassWriter.STR:
      case ClassWriter.CLASS:
        hashCode = type + strVal1.hashCode();
        return;
      case ClassWriter.NAME_TYPE:
        hashCode = type + strVal1.hashCode()*strVal2.hashCode();
        return;
      
      
      
      default:
        hashCode = type + strVal1.hashCode()*strVal2.hashCode()*strVal3.hashCode();
        return;
    }
  }

  

  boolean isEqualTo (final Item i) {
    if (i.type == type) {
      switch (type) {
        case ClassWriter.INT:
          return i.intVal == intVal;
        case ClassWriter.LONG:
          return i.longVal == longVal;
        case ClassWriter.FLOAT:
          return i.floatVal == floatVal;
        case ClassWriter.DOUBLE:
          return i.doubleVal == doubleVal;
        case ClassWriter.UTF8:
        case ClassWriter.STR:
        case ClassWriter.CLASS:
          return i.strVal1.equals(strVal1);
        case ClassWriter.NAME_TYPE:
          return i.strVal1.equals(strVal1) &&
                 i.strVal2.equals(strVal2);
        
        
        
        default:
          return i.strVal1.equals(strVal1) &&
                 i.strVal2.equals(strVal2) &&
                 i.strVal3.equals(strVal3);
      }
    }
    return false;
  }
}
