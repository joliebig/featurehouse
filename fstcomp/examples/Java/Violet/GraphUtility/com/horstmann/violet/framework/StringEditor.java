package com.horstmann.violet.framework;

import java.awt.*;
import java.awt.event.*;
import java.beans.*;
import java.lang.reflect.*;
import java.util.*;
import javax.swing.*;
import javax.swing.event.*;

// workaround for Web Start bug
public class StringEditor extends PropertyEditorSupport
   {
      public String getAsText() { return (String) getValue(); }
      public void setAsText(String s) { setValue(s); }
   }

