package com.horstmann.violet.framework;

public class EditorFrame {

  EditorFrame(Class appClass) {

      helpMenu.add(factory.createMenuItem(
         "help.license", new
         ActionListener()
         {
            public void actionPerformed(ActionEvent event)
            {
               try
               {
                  BufferedReader reader 
                     = new BufferedReader(
                        new InputStreamReader(
                           getClass().getResourceAsStream(
                              "license.txt")));
                  JTextArea text = new JTextArea(10, 50);
                  String line;
                  while ((line = reader.readLine()) != null)
                  {
                     text.append(line);
                     text.append("\n");
                  }   
                  text.setCaretPosition(0);
                  text.setEditable(false);
                  JOptionPane.showInternalMessageDialog(
                     desktop, 
                     new JScrollPane(text),
                     null, 
                     JOptionPane.INFORMATION_MESSAGE);
               }
               catch (IOException exception) {}
            }
         }));
  }

}