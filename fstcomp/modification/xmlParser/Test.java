package modification.xmlParser;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;

import javax.xml.stream.XMLStreamException;

import cide.gparser.ParseException;

public class Test {

    /**
     * @param args
     * @throws XMLStreamException
     * @throws IOException
     * @throws ParseException
     */
    public static void main(String[] args) throws XMLStreamException,
	    IOException, ParseException {
	InputStream stream = new FileInputStream("d:/test.xml");
	XmlParser p = new XmlParser(stream);
	p.parse();

    }

}
