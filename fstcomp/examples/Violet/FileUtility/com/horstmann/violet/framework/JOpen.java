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

public class JOpen implements Open
      {
         public JOpen(JFileChooserService jfcs) throws FileNotFoundException
         {
            if (jfcs.file != null)
            {
               name = jfcs.file.getPath();
               in = new FileInputStream(jfcs.file);
            }
         }

         public String getName() { return name; }
         public InputStream getInputStream() { return in; }

         private String name;
         private InputStream in;
      }