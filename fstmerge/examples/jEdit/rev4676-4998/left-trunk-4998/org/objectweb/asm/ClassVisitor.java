

package org.objectweb.asm;



public interface ClassVisitor {

  

  void visit (
    int access,
    String name,
    String superName,
    String[] interfaces,
    String sourceFile);

  

  void visitInnerClass (
    String name,
    String outerName,
    String innerName,
    int access);

  

  void visitField (int access, String name, String desc, Object value);

  

  CodeVisitor visitMethod (
    int access,
    String name,
    String desc,
    String[] exceptions);

  

  void visitEnd ();
}
