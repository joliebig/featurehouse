package net.sf.jabref.gui; 

import java.util.* ; 

import java.awt.* ; 
import java.awt.event.* ; 

import javax.swing.*; 

import com.jgoodies.forms.builder.* ; 
import com.jgoodies.forms.layout.* ; 
import net.sf.jabref.* ; 

import java.awt.BorderLayout; 
import java.awt.event.ActionEvent; 
import java.awt.event.ActionListener; 
import java.util.HashMap; 
import java.util.Iterator; 
import java.util.TreeSet; 

import net.sf.jabref.BibtexFields; 
import net.sf.jabref.GUIGlobals; 
import net.sf.jabref.Globals; 
import net.sf.jabref.JabRefFrame; 

import com.jgoodies.forms.builder.ButtonBarBuilder; 
import com.jgoodies.forms.builder.DefaultFormBuilder; 
import com.jgoodies.forms.layout.FormLayout; 


public  class  FieldWeightDialog  extends JDialog {
	

    JabRefFrame frame;

	
    

	
    JButton ok = new JButton(Globals.lang("OK")),
        cancel = new JButton(Globals.lang("Cancel"));

	

   public static void main(String[] args) {
        new FieldWeightDialog(null).setVisible(true);
    }


	

    public FieldWeightDialog(JabRefFrame frame) {
        this.frame = frame;
        JPanel main = buildMainPanel();
        main.setBorder(BorderFactory.createEmptyBorder(5,5,5,5));
        getContentPane().add(main, BorderLayout.CENTER);
        getContentPane().add(buildButtonPanel(), BorderLayout.SOUTH);
        pack();
    }


	

    public JPanel buildMainPanel() {
        FormLayout layout = new FormLayout
            ("right:pref, 4dlu, fill:pref, 8dlu, right:pref, 4dlu, fill:pref", 
             "");
        DefaultFormBuilder builder = new DefaultFormBuilder(layout);

        builder.appendSeparator(Globals.lang("Field sizes"));

        
        TreeSet<String> fields = new TreeSet<String>();
        
        sliders.clear();
        for (int i=0, len=BibtexFields.numberOfPublicFields(); i<len; i++)
        {
            fields.add(BibtexFields.getFieldName(i));
        }
        fields.remove("bibtexkey"); 
        

        

        for (Iterator<String> i=fields.iterator(); i.hasNext();) {
            String field = i.next();
            builder.append(field);
            int weight = (int)(100*BibtexFields.getFieldWeight(field)/GUIGlobals.MAX_FIELD_WEIGHT) ;
            
            JSlider slider = new JSlider(0, 100, weight);
            sliders.put(slider, new SliderInfo(field, weight));
            builder.append(slider);
        }
        builder.appendSeparator();

        return builder.getPanel();

    }


	

    public JPanel buildButtonPanel() {

        ok.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent actionEvent) {
                storeSettings();
                dispose();
            }
        });
        cancel.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent actionEvent) {
                dispose();
            }
        });

        ButtonBarBuilder builder = new ButtonBarBuilder();
        builder.addGlue();
        builder.addGridded(ok);
        builder.addGridded(cancel);
        builder.addGlue();
        return builder.getPanel();
    }


	

    public void storeSettings() {
        for (Iterator<JSlider> i=sliders.keySet().iterator(); i.hasNext();) {
            JSlider slider = i.next();
            SliderInfo sInfo = sliders.get(slider);
            
            if (sInfo.originalValue != slider.getValue()) {
                double weight = GUIGlobals.MAX_FIELD_WEIGHT*((double)slider.getValue())/100d;
                BibtexFields.setFieldWeight(sInfo.fieldName, weight);
            }
        }
        frame.removeCachedEntryEditors();
    }


	

    
    static  class  SliderInfo {
		
        String fieldName;

		
        int originalValue;

		
        public SliderInfo(String fieldName, int originalValue) {
            this.fieldName = fieldName;
            this.originalValue = originalValue;
        }



	}

	
    HashMap<JSlider, SliderInfo> sliders = new HashMap<JSlider, SliderInfo>();


}
