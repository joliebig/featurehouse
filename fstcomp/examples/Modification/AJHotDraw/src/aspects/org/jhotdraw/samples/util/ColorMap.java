
package org.jhotdraw.util; 
import java.awt.Color; 
 
class  ColorEntry {
		public String	fName;

		public Color	fColor;

		ColorEntry(String name, Color color) {	fColor = color;	fName = name;	}


} 
public  class  ColorMap {
		static ColorEntry fMap[] = {	new ColorEntry("Black", Color.black),	new ColorEntry("Blue", Color.blue),	new ColorEntry("Green", Color.green),	new ColorEntry("Red", Color.red),	new ColorEntry("Pink", Color.pink),	new ColorEntry("Magenta", Color.magenta),	new ColorEntry("Orange", Color.orange),	new ColorEntry("Yellow", Color.yellow),	new ColorEntry("New Tan", new Color(0xEBC79E)),	new ColorEntry("Aquamarine", new Color(0x70DB93)),	new ColorEntry("Sea Green", new Color(0x238E68)),	new ColorEntry("Dark Gray", Color.darkGray),	new ColorEntry("Light Gray", Color.lightGray),	new ColorEntry("White", Color.white),	new ColorEntry("None", new Color(0xFFC79E))	};

		public static int size() {	return fMap.length;	}

		public static Color color(int index) {	if (index < size() && index >= 0) {	return fMap[index].fColor;	}	throw new ArrayIndexOutOfBoundsException("Color index: " + index);	}

		public static Color color(String name) {	for (int i = 0; i < fMap.length; i++) {	if (fMap[i].fName.equals(name)) {	return fMap[i].fColor;	}	}	return Color.black;	}

		public static String name(int index) {	if (index < size() && index >= 0) {	return fMap[index].fName;	}	throw new ArrayIndexOutOfBoundsException("Color index: " + index);	}

		public static int colorIndex(Color color) {	for (int i=0; i<fMap.length; i++) {	if (fMap[i].fColor.equals(color)) {	return i;	}	}	return 0;	}

		public static boolean isTransparent(Color color) {	return color.equals(color("None"));	}


}
