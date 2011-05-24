package com.horstmann.violet.framework;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.util.ResourceBundle;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

public class JSave implements Save
      {
         public JSave(File f) throws FileNotFoundException
         {
            if (f != null)
            {
               name = f.getPath();
               out = new FileOutputStream(f);
            }
         }

         public String getName() { return name; }
         public OutputStream getOutputStream() { return out; }

         private String name;
         private OutputStream out;
      }