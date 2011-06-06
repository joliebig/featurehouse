

package bsh.org.objectweb.asm;



public interface CodeVisitor {

  

  void visitInsn (int opcode);

  

  void visitIntInsn (int opcode, int operand);

  

  void visitVarInsn (int opcode, int var);

  

  void visitTypeInsn (int opcode, String desc);

  

  void visitFieldInsn (int opcode, String owner, String name, String desc);

  

  void visitMethodInsn (int opcode, String owner, String name, String desc);

  

  void visitJumpInsn (int opcode, Label label);

  

  void visitLabel (Label label);

  
  
  

  

  void visitLdcInsn (Object cst);

  

  void visitIincInsn (int var, int increment);

  

  void visitTableSwitchInsn (int min, int max, Label dflt, Label labels[]);

  

  void visitLookupSwitchInsn (Label dflt, int keys[], Label labels[]);

  

  void visitMultiANewArrayInsn (String desc, int dims);

  
  
  

  

  void visitTryCatchBlock (Label start, Label end, Label handler, String type);

  

  void visitMaxs (int maxStack, int maxLocals);

  
  
  

  

  void visitLocalVariable (
    String name,
    String desc,
    Label start,
    Label end,
    int index);

  

  void visitLineNumber (int line, Label start);
}
