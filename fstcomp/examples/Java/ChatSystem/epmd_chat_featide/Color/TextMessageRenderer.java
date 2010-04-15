


import java.awt.Color;

public class TextMessageRenderer  {

 		public Component getListCellRendererComponent(JList list, Object value, int index,
		      boolean isSelected, boolean cellHasFocus) {
		    
			  if (value instanceof TextMessage) {
				
				  /*if[COLOR]*/
				  String col = ((TextMessage)value).getSetting(Utils.COLORKEY);
				  if (col != null)
					  setForeground(new Color(Integer.parseInt(col)));
				  else
					  setForeground(Color.BLACK);
				  /*end[COLOR]*/
			  }
			//return this;
		    return original(list, value, index, isSelected, cellHasFocus);
		  }

}