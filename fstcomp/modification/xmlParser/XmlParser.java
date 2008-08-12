package modification.xmlParser;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;

import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

import cide.gparser.ParseException;

import modification.FSTGenerator;
import modification.Modification;
import modification.ModificationComposition;
import modification.ParsedTraversalFSTContent;
import modification.SuperimpositionModification;

public class XmlParser {
    enum Elements {
	modification, modType, FSTTraversal, parsedContent, plainText, customContent, plainTextType, text, externLink, contentFSTTraversal
    }

    InputStream inputStream;

    public XmlParser(InputStream inputStream) throws FileNotFoundException,
	    XMLStreamException {
	this.inputStream = inputStream;
    }

    public ModificationComposition parse() throws XMLStreamException,
	    IOException, ParseException {
	XMLInputFactory factory = XMLInputFactory.newInstance();
	XMLStreamReader reader = factory.createXMLStreamReader(inputStream);
	ModificationComposition feat = new ModificationComposition();

	String modType = "";
	String FSTTraversal = "";
	String plainTextType = "";
	String text = "";
	String externLink = "";
	String contentFSTTraversal = "";
	// boolean parsedContent = false;
	// boolean parsedContent = false;

	Elements currentElement = null;

	while (reader.hasNext()) {
	    switch (reader.getEventType()) {
	    case XMLStreamConstants.START_ELEMENT: {
		if (reader.getLocalName().equals(
			Elements.modification.toString())) {
		    currentElement = Elements.modification;
		} else if (reader.getLocalName().equals(
			Elements.modType.toString())) {
		    currentElement = Elements.modType;
		} else if (reader.getLocalName().equals(
			Elements.FSTTraversal.toString())) {
		    currentElement = Elements.FSTTraversal;
		} else if (reader.getLocalName().equals(
			Elements.parsedContent.toString())) {
		    currentElement = Elements.parsedContent;
		} else if (reader.getLocalName().equals(
			Elements.customContent.toString())) {
		    currentElement = Elements.customContent;
		} else if (reader.getLocalName().equals(
			Elements.plainTextType.toString())) {
		    currentElement = Elements.plainTextType;
		} else if (reader.getLocalName().equals(
			Elements.text.toString())) {
		    currentElement = Elements.text;
		} else if (reader.getLocalName().equals(
			Elements.externLink.toString())) {
		    currentElement = Elements.externLink;
		} else if (reader.getLocalName().equals(
			Elements.contentFSTTraversal.toString())) {
		    currentElement = Elements.contentFSTTraversal;
		}
		break;
	    }
	    case XMLStreamConstants.CHARACTERS: {

		if (reader.isWhiteSpace())
		    break;
		System.out.println("<" + currentElement.toString() + ">");
		switch (currentElement) {
		case modType:
		    modType = reader.getText();
		    System.out.println(reader.getText());
		    break;
		case FSTTraversal:
		    FSTTraversal = reader.getText();
		    System.out.println(reader.getText());
		    break;
		case plainTextType:
		    plainTextType = reader.getText();
		    System.out.println(reader.getText());
		    break;
		case text:
		    text = reader.getText();
		    System.out.println(reader.getText());
		    break;
		case externLink:
		    externLink = reader.getText();
		    System.out.println(reader.getText());
		    break;
		case contentFSTTraversal:
		    contentFSTTraversal = reader.getText();
		    System.out.println(reader.getText());
		    break;
		default:
		    break;
		}
		break;
	    }
	    case XMLStreamConstants.END_ELEMENT: {
		if (reader.getLocalName().equals(
			Elements.modification.toString())) {
		    if (modType.equals("superimposition")) {
			if (!plainTextType.equals("")) {
			    if (!contentFSTTraversal.equals("")) {
				feat.add(new SuperimpositionModification(
					FSTTraversal,
					new ParsedTraversalFSTContent(
						contentFSTTraversal,
						FSTGenerator
							.createFST(new File(
								externLink)))));
			    } else {
				// TODO add body
			    }
			} else if (!externLink.equals("")) {
			    if (!contentFSTTraversal.equals("")) {
				// TODO add body
			    } else {
				// TODO add body
			    }
			}
		    } else if (modType.equals("introduction")) {
			if (!plainTextType.equals("")) {
			    if (!contentFSTTraversal.equals("")) {

			    } else {

			    }
			} else if (!externLink.equals("")) {
			    if (!contentFSTTraversal.equals("")) {

			    } else {

			    }
			}
		    }
		}
		break;
	    }
	    default:
		break;
	    }
	    reader.next();
	}
	reader.close();
	return feat;
    }
}
