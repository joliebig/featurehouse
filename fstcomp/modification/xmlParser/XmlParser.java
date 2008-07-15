package modification.xmlParser;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;

import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

public class XmlParser {
    enum Elements {
	modification, modType, FSTNodeType, FSTTraversal, content, externInput
    }

    InputStream inputStream;

    public XmlParser(InputStream inputStream) throws FileNotFoundException,
	    XMLStreamException {
	this.inputStream = inputStream;
    }

    public Feature parse() throws XMLStreamException, IOException {
	XMLInputFactory factory = XMLInputFactory.newInstance();
	XMLStreamReader reader = factory.createXMLStreamReader(inputStream);
	Feature feat = new Feature();

	Modification mod = null;

	Elements currentElement = null;

	while (reader.hasNext()) {
	    switch (reader.getEventType()) {
	    case XMLStreamConstants.START_ELEMENT: {
		if (reader.getLocalName().equals(
			Elements.modification.toString())) {
		    mod = new Modification();
		} else if (reader.getLocalName().equals(
			Elements.modType.toString())) {
		    currentElement = Elements.modType;
		} else if (reader.getLocalName().equals(
			Elements.FSTNodeType.toString())) {
		    currentElement = Elements.FSTNodeType;
		} else if (reader.getLocalName().equals(
			Elements.FSTTraversal.toString())) {
		    currentElement = Elements.FSTTraversal;
		} else if (reader.getLocalName().equals(
			Elements.content.toString())) {
		    currentElement = Elements.content;
		} else if (reader.getLocalName().equals(
			Elements.externInput.toString())) {
		    currentElement = Elements.externInput;
		}
		break;
	    }
	    case XMLStreamConstants.CHARACTERS: {

		if (reader.isWhiteSpace())
		    break;
		System.out.println("<" + currentElement.toString() + ">");
		switch (currentElement) {
		case modType: {
		    mod.setModType(reader.getText());
		    System.out.println(reader.getText());
		    break;
		}
		case FSTTraversal:
		    System.out.println(reader.getText());
		    mod.setFstTraversal(reader.getText());
		    break;
		case FSTNodeType:
		    System.out.println(reader.getText());
		    mod.setFstNodeType(reader.getText());
		    break;
		case content:
		    System.out.println(reader.getText());
		    mod.setContent(reader.getText());
		    break;
		case externInput:
		    System.out.println(reader.getText());
		    FileReader f = new FileReader(reader.getText());
		    String s = "";
		    StringWriter sw = new StringWriter();
		    for (int c; (c = f.read()) != -1;) {
			System.out.print((char) c);
			sw.append((char) c);
		    }
		    System.out.println();
		    mod.setContent(sw.toString());
		    break;
		default:
		    break;
		}
		break;
	    }
	    case XMLStreamConstants.END_ELEMENT: {
		if (reader.getLocalName().equals(
			Elements.modification.toString())) {
		    feat.add(mod);
		    mod = null;
		}
		break;
	    }
	    default:
		break;
	    }
	    reader.next();
	}
	reader.close();
	for (Modification m : feat.getModList()) {
	    System.out.println(m.getModType());
	    System.out.println(m.getFstTraversal());
	    System.out.println(m.getFstNodeType());
	    System.out.println(m.getContent());
	}

	return feat;
    }
}
