


import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.ListCellRenderer;
import java.awt.Component;

public class TextMessageRenderer extends JLabel implements ListCellRenderer {

		  public Component getListCellRendererComponent(JList list, Object value, int index,
		      boolean isSelected, boolean cellHasFocus) {
		    
			  if (value instanceof TextMessage) {
				  setText(((TextMessage)value).getSender() + ">"+((TextMessage)value).getContent());
			  }
			  else {
				  setText(value.toString());  
			  }
		 
		    return this;
		  }
	}