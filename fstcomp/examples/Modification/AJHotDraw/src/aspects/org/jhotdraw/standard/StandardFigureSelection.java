
package org.jhotdraw.standard; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.util.*; 
import java.util.*; 
import java.io.*; 
public  class  StandardFigureSelection  implements FigureSelection, Serializable {
		private byte[] fData;

		public final static String TYPE = "org.jhotdraw.Figures";

		public StandardFigureSelection(FigureEnumeration fe, int figureCount) {	ByteArrayOutputStream output = new ByteArrayOutputStream(200);	StorableOutput writer = new StorableOutput(output);	writer.writeInt(figureCount);	while (fe.hasNextFigure()) {	writer.writeStorable(fe.nextFigure());	}	writer.close();	fData = output.toByteArray();	}

		public String getType() {	return TYPE;	}

		public Object getData(String type) {	if (type.equals(TYPE)) {	InputStream input = new ByteArrayInputStream(fData);	List result = CollectionsFactory.current().createList(10);	StorableInput reader = new StorableInput(input);	int numRead = 0;	try {	int count = reader.readInt();	while (numRead < count) {	Figure newFigure = (Figure) reader.readStorable();	result.add(newFigure);	numRead++;	}	}	catch (IOException e) {	System.err.println(e.toString());	}	catch (org.aspectj.lang.SoftException e) {	System.err.println(e.getWrappedThrowable().toString());	}	return new FigureEnumerator(result);	}	return null;	}

		public static FigureEnumeration duplicateFigures(FigureEnumeration toBeCloned, int figureCount) {	StandardFigureSelection duplicater = new StandardFigureSelection(toBeCloned, figureCount);	return (FigureEnumeration)duplicater.getData(duplicater.getType());	}


}
