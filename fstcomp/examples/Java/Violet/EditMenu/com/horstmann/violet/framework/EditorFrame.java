package com.horstmann.violet.framework;

public class EditorFrame {
  protected JMenu editMenu;

  EditorFrame(Class appClass) {
      editMenu = factory.createMenu("edit");
      menuBar.add(editMenu);
  }
}