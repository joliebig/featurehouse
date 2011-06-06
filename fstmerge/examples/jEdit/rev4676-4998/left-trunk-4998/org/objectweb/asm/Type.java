

package org.objectweb.asm;

import java.lang.reflect.Method;



public class Type {

  

  public final static int VOID = 0;

  

  public final static int BOOLEAN = 1;

  

  public final static int CHAR = 2;

  

  public final static int BYTE = 3;

  

  public final static int SHORT = 4;

  

  public final static int INT = 5;

  

  public final static int FLOAT = 6;

  

  public final static int LONG = 7;

  

  public final static int DOUBLE = 8;

  

  public final static int ARRAY = 9;

  

  public final static int OBJECT = 10;

  

  public final static Type VOID_TYPE = new Type(VOID);

  

  public final static Type BOOLEAN_TYPE = new Type(BOOLEAN);

  

  public final static Type CHAR_TYPE = new Type(CHAR);

  

  public final static Type BYTE_TYPE = new Type(BYTE);

  

  public final static Type SHORT_TYPE = new Type(SHORT);

  

  public final static Type INT_TYPE = new Type(INT);

  

  public final static Type FLOAT_TYPE = new Type(FLOAT);

  

  public final static Type LONG_TYPE = new Type(LONG);

  

  public final static Type DOUBLE_TYPE = new Type(DOUBLE);

  
  
  

  

  private final int sort;

  

  private char[] buf;

  

  private int off;

  

  private int len;

  
  
  

  

  private Type (final int sort) {
    this.sort = sort;
    this.len = 1;
  }

  

  private Type (
    final int sort,
    final char[] buf,
    final int off,
    final int len)
  {
    this.sort = sort;
    this.buf = buf;
    this.off = off;
    this.len = len;
  }

  

  public static Type getType (final String typeDescriptor) {
    return getType(typeDescriptor.toCharArray(), 0);
  }

  

  public static Type getType (final Class c) {
    if (c.isPrimitive()) {
      if (c == Integer.TYPE) {
        return INT_TYPE;
      } else if (c == Void.TYPE) {
        return VOID_TYPE;
      } else if (c == Boolean.TYPE) {
        return BOOLEAN_TYPE;
      } else if (c == Byte.TYPE) {
        return BYTE_TYPE;
      } else if (c == Character.TYPE) {
        return CHAR_TYPE;
      } else if (c == Short.TYPE) {
        return SHORT_TYPE;
      } else if (c == Double.TYPE) {
        return DOUBLE_TYPE;
      } else if (c == Float.TYPE) {
        return FLOAT_TYPE;
      } else  {
        return LONG_TYPE;
      }
    } else {
      return getType(getDescriptor(c));
    }
  }

  

  public static Type[] getArgumentTypes (final String methodDescriptor) {
    char[] buf = methodDescriptor.toCharArray();
    int off = 1;
    int size = 0;
    while (true) {
      char car = buf[off++];
      if (car == ')') {
        break;
      } else if (car == 'L') {
        while (buf[off++] != ';') {
        }
        ++size;
      } else if (car != '[') {
        ++size;
      }
    }
    Type[] args = new Type[size];
    off = 1;
    size = 0;
    while (buf[off] != ')') {
      args[size] = getType(buf, off);
      off += args[size].len;
      size += 1;
    }
    return args;
  }

  

  public static Type[] getArgumentTypes (final Method method) {
    Class[] classes = method.getParameterTypes();
    Type[] types = new Type[classes.length];
    for (int i = classes.length - 1; i >= 0; --i) {
      types[i] = getType(classes[i]);
    }
    return types;
  }

  

  public static Type getReturnType (final String methodDescriptor) {
    char[] buf = methodDescriptor.toCharArray();
    return getType(buf, methodDescriptor.indexOf(')') + 1);
  }

  

  public static Type getReturnType (final Method method) {
    return getType(method.getReturnType());
  }

  

  private static Type getType (final char[] buf, final int off) {
    int len;
    switch (buf[off]) {
      case 'V': return VOID_TYPE;
      case 'Z': return BOOLEAN_TYPE;
      case 'C': return CHAR_TYPE;
      case 'B': return BYTE_TYPE;
      case 'S': return SHORT_TYPE;
      case 'I': return INT_TYPE;
      case 'F': return FLOAT_TYPE;
      case 'J': return LONG_TYPE;
      case 'D': return DOUBLE_TYPE;
      case '[':
        len = 1;
        while (buf[off + len] == '[') {
          ++len;
        }
        if (buf[off + len] == 'L') {
          ++len;
          while (buf[off + len] != ';') {
            ++len;
          }
        }
        return new Type(ARRAY, buf, off, len + 1);
      
      default:
        len = 1;
        while (buf[off + len] != ';') {
          ++len;
        }
        return new Type(OBJECT, buf, off, len + 1);
    }
  }

  
  
  

  

  public int getSort () {
    return sort;
  }

  

  public int getDimensions () {
    int i = 1;
    while (buf[off + i] == '[') {
      ++i;
    }
    return i;
  }

  

  public Type getElementType () {
    return getType(buf, off + getDimensions());
  }

  

  public String getClassName () {
    return new String(buf, off + 1, len - 2).replace('/', '.');
  }

  

  public String getInternalName () {
    return new String(buf, off + 1, len - 2);
  }

  
  
  

  

  public String getDescriptor () {
    StringBuffer buf = new StringBuffer();
    getDescriptor(buf);
    return buf.toString();
  }

  

  public static String getMethodDescriptor (
    final Type returnType,
    final Type[] argumentTypes)
  {
    StringBuffer buf = new StringBuffer();
    buf.append('(');
    for (int i = 0; i < argumentTypes.length; ++i) {
      argumentTypes[i].getDescriptor(buf);
    }
    buf.append(')');
    returnType.getDescriptor(buf);
    return buf.toString();
  }

  

  private void getDescriptor (final StringBuffer buf) {
    switch (sort) {
      case VOID:    buf.append('V'); return;
      case BOOLEAN: buf.append('Z'); return;
      case CHAR:    buf.append('C'); return;
      case BYTE:    buf.append('B'); return;
      case SHORT:   buf.append('S'); return;
      case INT:     buf.append('I'); return;
      case FLOAT:   buf.append('F'); return;
      case LONG:    buf.append('J'); return;
      case DOUBLE:  buf.append('D'); return;
      
      
      default:      buf.append(this.buf, off, len);
    }
  }

  
  
  
  

  

  public static String getInternalName (final Class c) {
    return c.getName().replace('.', '/');
  }

  

  public static String getDescriptor (final Class c) {
    StringBuffer buf = new StringBuffer();
    getDescriptor(buf, c);
    return buf.toString();
  }

  

  public static String getMethodDescriptor (final Method m) {
    Class[] parameters = m.getParameterTypes();
    StringBuffer buf = new StringBuffer();
    buf.append('(');
    for (int i = 0; i < parameters.length; ++i) {
      getDescriptor(buf, parameters[i]);
    }
    buf.append(')');
    getDescriptor(buf, m.getReturnType());
    return buf.toString();
  }

  

  private static void getDescriptor (final StringBuffer buf, final Class c) {
    Class d = c;
    while (true) {
      if (d.isPrimitive()) {
        char car;
        if (d == Integer.TYPE) {
          car = 'I';
        } else if (d == Void.TYPE) {
          car = 'V';
        } else if (d == Boolean.TYPE) {
          car = 'Z';
        } else if (d == Byte.TYPE) {
          car = 'B';
        } else if (d == Character.TYPE) {
          car = 'C';
        } else if (d == Short.TYPE) {
          car = 'S';
        } else if (d == Double.TYPE) {
          car = 'D';
        } else if (d == Float.TYPE) {
          car = 'F';
        } else  {
          car = 'J';
        }
        buf.append(car);
        return;
      } else if (d.isArray()) {
        buf.append('[');
        d = d.getComponentType();
      } else {
        buf.append('L');
        String name = d.getName();
        int len = name.length();
        for (int i = 0; i < len; ++i) {
          char car = name.charAt(i);
          buf.append(car == '.' ? '/' : car);
        }
        buf.append(';');
        return;
      }
    }
  }

  
  
  

  

  public int getSize () {
    return (sort == LONG || sort == DOUBLE ? 2 : 1);
  }

  

  public int getOpcode (final int opcode) {
    if (opcode == Constants.IALOAD || opcode == Constants.IASTORE) {
      switch (sort) {
        case VOID:
          return opcode + 5;
        case BOOLEAN:
        case BYTE:
          return opcode + 6;
        case CHAR:
          return opcode + 7;
        case SHORT:
          return opcode + 8;
        case INT:
          return opcode;
        case FLOAT:
          return opcode + 2;
        case LONG:
          return opcode + 1;
        case DOUBLE:
          return opcode + 3;
        
        
        default:
          return opcode + 4;
      }
    } else {
      switch (sort) {
        case VOID:
          return opcode + 5;
        case BOOLEAN:
        case CHAR:
        case BYTE:
        case SHORT:
        case INT:
          return opcode;
        case FLOAT:
          return opcode + 2;
        case LONG:
          return opcode + 1;
        case DOUBLE:
          return opcode + 3;
        
        
        default:
          return opcode + 4;
      }
    }
  }
}
