package net.sf.jabref; 


import java.awt.*; 
import java.awt.event.ActionEvent; 
import java.awt.event.ActionListener; 
import java.util.Vector; 

import javax.swing.*; 
import javax.swing.border.EmptyBorder; 
import javax.swing.border.TitledBorder; 
import javax.swing.event.ListSelectionEvent; 
import javax.swing.event.ListSelectionListener; 



 



class  FontSelector  extends JButton {
	

	private static final long serialVersionUID = 7745223550102664896L;

	

	static final String PLAIN="plain";

	
        static final String BOLD="bold";

	
        static final String BOLD_ITALIC="bold-italic";

	
        static final String ITALIC="italic";

	

        
        public FontSelector(){
                this(new Font("SansSerif", Font.PLAIN, 10));
        }


	

        
        public FontSelector(Font font){
                setFont(font);
                setRequestFocusEnabled(false);
                addActionListener(new ActionHandler());
        }


	

        public void setFont(Font font){
                super.setFont(font);
                updateText();
        }


	

        
        private void updateText(){
                Font font = getFont();
                String styleString;
                switch(font.getStyle()){
                case Font.PLAIN:
                        styleString = PLAIN;
                        break;
                case Font.BOLD:
                        styleString = BOLD;
                        break;
                case Font.ITALIC:
                        styleString = ITALIC;
                        break;
                case Font.BOLD | Font.ITALIC:
                        styleString = BOLD_ITALIC;
                        break;
                default:
                        styleString = "UNKNOWN!!!???";
                        break;
                }

                setText(font.getFamily() + " " + font.getSize() + " " + styleString);
        }


	

        
         

        
        class  ActionHandler implements  ActionListener {
		
                public void actionPerformed(ActionEvent evt) {
                        Font font = new FontSelectorDialog(FontSelector.this,getFont()).getSelectedFont();
                        if(font != null){
                                setFont(font);
                        }
                }



	}


} 





public  class  FontSelectorDialog  extends JDialog {
	

	private static final long serialVersionUID = -8670346696048738055L;

	

	static final String PLAIN="plain";

	
        static final String BOLD="bold";

	
        static final String BOLD_ITALIC="bold-italic";

	
        static final String ITALIC="italic";

	

        public FontSelectorDialog(Component comp, Font font) {

            
            super(JOptionPane.getFrameForComponent(comp),Globals.lang("FontSelector"),true); 
                JPanel content = new JPanel(new BorderLayout());
                content.setBorder(new EmptyBorder(12,12,12,12));
                setContentPane(content);

                JPanel listPanel = new JPanel(new GridLayout(1,3,6,6));

                JPanel familyPanel = createTextFieldAndListPanel(
                                                                 Globals.lang("Font Family"),
                                                                 familyField = new JTextField(),
                                                                 familyList = new JList(getFontList()));
                listPanel.add(familyPanel);

                String[] sizes = { "9", "10", "12", "14", "16", "18", "24" };
                JPanel sizePanel = createTextFieldAndListPanel(
                                                               Globals.lang("Font Size"),
                                       sizeField = new JTextField(),
                                       sizeList = new JList(sizes));
                listPanel.add(sizePanel);

                String[] styles = {PLAIN,BOLD,ITALIC,BOLD_ITALIC};

                JPanel stylePanel = createTextFieldAndListPanel(
                                                                Globals.lang("Font Style"),
                                        styleField = new JTextField(),
                                        styleList = new JList(styles));
                styleField.setEditable(false);
                listPanel.add(stylePanel);

                familyList.setSelectedValue(font.getFamily(),true);
                familyField.setText(font.getFamily());
                sizeList.setSelectedValue(String.valueOf(font.getSize()),true);
                sizeField.setText(String.valueOf(font.getSize()));
                styleList.setSelectedIndex(font.getStyle());
                styleField.setText((String)styleList.getSelectedValue());

                ListHandler listHandler = new ListHandler();
                familyList.addListSelectionListener(listHandler);
                sizeList.addListSelectionListener(listHandler);
                styleList.addListSelectionListener(listHandler);

                content.add(BorderLayout.NORTH,listPanel);

                

                
                preview = new JLabel(Globals.lang("Font Preview")) {
					private static final long serialVersionUID = -4191591634265068189L;
						public void paint(Graphics g) {
                            Graphics2D g2 = (Graphics2D)g;
                            g2.setRenderingHint
                                (RenderingHints.KEY_ANTIALIASING,
                                 RenderingHints.VALUE_ANTIALIAS_ON);
                            super.paint(g2);
                        }

                    };



                preview.setBorder(new TitledBorder(Globals.lang("Font Preview")));

                updatePreview();

                Dimension prefSize = preview.getPreferredSize();
                prefSize.height = 50;
                preview.setPreferredSize(prefSize);

                content.add(BorderLayout.CENTER,preview);

                JPanel buttons = new JPanel();
                buttons.setLayout(new BoxLayout(buttons,BoxLayout.X_AXIS));
                buttons.setBorder(new EmptyBorder(12,0,0,0));
                buttons.add(Box.createGlue());

                ok = new JButton(Globals.lang("OK"));
                ok.addActionListener(new ActionHandler());
                getRootPane().setDefaultButton(ok);
                buttons.add(ok);

                buttons.add(Box.createHorizontalStrut(6));

                cancel = new JButton(Globals.lang("Cancel"));
                cancel.addActionListener(new ActionHandler());
                buttons.add(cancel);

                buttons.add(Box.createGlue());

                content.add(BorderLayout.SOUTH,buttons);

                pack();
                setLocationRelativeTo(JOptionPane.getFrameForComponent(comp));
                setVisible(true); 
        }


	

        public void ok(){
                isOK = true;
                dispose();
        }


	

        public void cancel(){
                dispose();
        }


	

        public Font getSelectedFont(){
                if(!isOK)
                        return null;

                int size;
                try{
                        size = Integer.parseInt(sizeField.getText());
                }
                catch(Exception e){
                        size = 14;
                }

                return new Font(familyField.getText(),styleList.getSelectedIndex(),size);
        }


	

        
        private boolean isOK;

	
        private JTextField familyField;

	
        private JList familyList;

	
        private JTextField sizeField;

	
        private JList sizeList;

	
        private JTextField styleField;

	
        private JList styleList;

	
        private JLabel preview;

	
        private JButton ok;

	
        private JButton cancel;

	

        
        private static final String[] HIDEFONTS = {".bold",".italic"};

	

        
        
        private String[] getFontList(){
                try{
                        Class<?> GEClass = Class.forName("java.awt.GraphicsEnvironment");
                        Object GEInstance = GEClass.getMethod("getLocalGraphicsEnvironment").invoke(null);

                        String[] nameArray = (String[])GEClass.getMethod("getAvailableFontFamilyNames").invoke(GEInstance);
                        Vector<String> nameVector = new Vector<String>(nameArray.length);

                        for(int i = 0, j; i < nameArray.length; i++){
                                for(j = 0; j < HIDEFONTS.length; j++){
                                        if(nameArray[i].indexOf(HIDEFONTS[j]) >= 0) break;
                                }

                                if(j == HIDEFONTS.length) nameVector.addElement(nameArray[i]);
                        }

                        String[] _array = new String[nameVector.size()];
                        nameVector.copyInto(_array);
                        return _array;
                }
                catch(Exception ex){
                    return null;
                }
        }


	

        private JPanel createTextFieldAndListPanel(String label,JTextField textField, JList list){
                GridBagLayout layout = new GridBagLayout();
                JPanel panel = new JPanel(layout);

                GridBagConstraints cons = new GridBagConstraints();
                cons.gridx = cons.gridy = 0;
                cons.gridwidth = cons.gridheight = 1;
                cons.fill = GridBagConstraints.BOTH;
                cons.weightx = 1.0f;

                JLabel _label = new JLabel(label);
                layout.setConstraints(_label,cons);
                panel.add(_label);

                cons.gridy = 1;
                Component vs = Box.createVerticalStrut(6);
                layout.setConstraints(vs,cons);
                panel.add(vs);

                cons.gridy = 2;
                layout.setConstraints(textField,cons);
                panel.add(textField);

                cons.gridy = 3;
                vs = Box.createVerticalStrut(6);
                layout.setConstraints(vs,cons);
                panel.add(vs);

                cons.gridy = 4;
                cons.gridheight = GridBagConstraints.REMAINDER;
                cons.weighty = 1.0f;
                JScrollPane scroller = new JScrollPane(list);
                layout.setConstraints(scroller,cons);
                panel.add(scroller);

                return panel;
        }


	

        private void updatePreview(){
                String family = familyField.getText();
                int size;
                try{
                        size = Integer.parseInt(sizeField.getText());
                }
                catch(Exception e){
                        size = 14;
                }
                int style = styleList.getSelectedIndex();
                preview.setFont(new Font(family,style,size));
        }


	

         

        class  ActionHandler implements  ActionListener {
		
                public void actionPerformed(ActionEvent evt){
                        if(evt.getSource() == ok)ok();
                        else if(evt.getSource() == cancel)cancel();
                }



	}

	

         

        class  ListHandler implements  ListSelectionListener {
		
                public void valueChanged(ListSelectionEvent evt)
                {
                        Object source = evt.getSource();
                        if(source == familyList) {
                                String family = (String)familyList.getSelectedValue();
                                if(family != null)
                                        familyField.setText(family);
                        }
                        else if(source == sizeList) {
                                String size = (String)sizeList.getSelectedValue();
                                if(size != null)
                                        sizeField.setText(size);
                        }
                        else if(source == styleList) {
                                String style = (String)styleList.getSelectedValue();
                                if(style != null)
                                        styleField.setText(style);
                        }
                        updatePreview();
                }



	}


}
