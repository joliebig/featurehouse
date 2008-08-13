package modification.xmlParser;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import javax.xml.validation.Validator;

import org.w3c.dom.Document;

import cide.gparser.ParseException;

public class Test {

    /**
     * @param args
     * @throws XMLStreamException
     * @throws IOException
     * @throws ParseException
     */
    public static void main(String[] args) throws Exception, IOException,
	    ParseException {

	DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
	DocumentBuilder builder = factory.newDocumentBuilder();
	Document document = builder.parse(new File("modification/xmlParser/test.xml"));
	System.out.println(document.getChildNodes());

	// DocumentBuilder parser = DocumentBuilderFactory.newInstance()
	// .newDocumentBuilder();
	// Document document = parser.parse(new
	// File("modification/xmlParser/test.xml"));
	//
	// SchemaFactory sfactory = SchemaFactory
	// .newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
	// Source schemaFile = new StreamSource(new File(
	// "modification/xmlParser/modCompilation.dtd"));
	// Schema schema = sfactory.newSchema(schemaFile);
	//
	// Validator validator = schema.newValidator();
	//
	// validator.validate(new DOMSource(document));

    }

}
