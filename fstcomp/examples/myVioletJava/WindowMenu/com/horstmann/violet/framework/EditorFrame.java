package com.horstmann.violet.framework;

public class EditorFrame {
  protected JMenu windowMenu;
  EditorFrame(Class appClass) {
      windowMenu = factory.createMenu("window");
      menuBar.add(windowMenu);
  }
}