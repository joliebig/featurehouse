package modification.xmlParser;

import java.io.File;
import java.io.IOException;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;
import javax.xml.transform.Source;
import javax.xml.transform.TransformerException;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import javax.xml.validation.Validator;

import modification.IntroductionModification;
import modification.ModificationComposition;
import modification.SuperimpositionModification;
import modification.content.Content;
import modification.content.CustomFSTContent;
import modification.content.FSTGenerator;
import modification.content.InvalidFSTTraversalException;
import modification.content.ParsedTraversalFSTContent;
import modification.content.FSTParseables.FSTParseable;
import modification.content.FSTParseables.FileInput;
import modification.content.FSTParseables.StringInput;

import org.xml.sax.ErrorHandler;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

import cide.gparser.ParseException;

public class XmlParser {
    private static final String XML_SCHEMA = "modification/xmlParser/schema.xsd";

    File input;

    ModificationComposition mods = new ModificationComposition();

    enum Tags {
	modification, type, traversal, content, parsed, custom, plainText, tType, text, externLink, name, body, prefix, compositionMechanism, cTraversal, nodeType
    }

    enum ModClassification {
	superimposition, introduction
    }

    enum ContentType {
	parsed, custom
    }

    enum InputType {
	plainText, externLink
    }

    enum ContentTraversalFlag {
	traversal, noTraversal
    }

    public XmlParser(File file) throws XMLStreamException,
	    ParserConfigurationException, SAXException, IOException {
	this.input = file;
    }

    public ModificationComposition parse() throws XMLStreamException,
	    IOException, ParseException, TransformerException, SAXException,
	    ParserConfigurationException,
	    modification.traversalLanguageParser.ParseException,
	    InvalidFSTTraversalException {

	XMLInputFactory xmlif = XMLInputFactory.newInstance();

	Source source = new StreamSource(input);
	Validator schemaValidator = getValidator();

	XMLStreamReader reader = xmlif.createXMLStreamReader(source);

	schemaValidator.validate(source);

	ModClassification modclass = null;
	ContentType contentType = null;
	InputType inputType = null;
	ContentTraversalFlag contentTraversalFlag = ContentTraversalFlag.noTraversal;
	Tags currentElement = null;

	String[] tagContents = new String[Tags.values().length];
	ModificationComposition mods = new ModificationComposition();

	while (reader.hasNext()) {
	    switch (reader.getEventType()) {
	    case XMLStreamConstants.START_ELEMENT: {
		if (reader.getLocalName().equals(Tags.modification.toString())) {
		    currentElement = Tags.modification;
		} else if (reader.getLocalName().equals(Tags.type.toString())) {
		    currentElement = Tags.type;
		} else if (reader.getLocalName().equals(
			Tags.traversal.toString())) {
		    currentElement = Tags.traversal;
		} else if (reader.getLocalName()
			.equals(Tags.content.toString())) {
		    currentElement = Tags.content;
		} else if (reader.getLocalName().equals(Tags.parsed.toString())) {
		    currentElement = Tags.parsed;
		    contentType = ContentType.parsed;
		} else if (reader.getLocalName().equals(
			Tags.cTraversal.toString())) {
		    currentElement = Tags.cTraversal;
		    contentTraversalFlag = ContentTraversalFlag.traversal;
		} else if (reader.getLocalName().equals(
			Tags.plainText.toString())) {
		    currentElement = Tags.plainText;
		    inputType = InputType.plainText;
		} else if (reader.getLocalName().equals(Tags.text.toString())) {
		    currentElement = Tags.text;
		} else if (reader.getLocalName().equals(Tags.tType.toString())) {
		    currentElement = Tags.tType;
		} else if (reader.getLocalName().equals(
			Tags.externLink.toString())) {
		    currentElement = Tags.externLink;
		    inputType = InputType.externLink;
		} else if (reader.getLocalName().equals(Tags.custom.toString())) {
		    currentElement = Tags.custom;
		    contentType = ContentType.custom;
		} else if (reader.getLocalName().equals(
			Tags.nodeType.toString())) {
		    currentElement = Tags.nodeType;
		} else if (reader.getLocalName().equals(Tags.name.toString())) {
		    currentElement = Tags.name;
		} else if (reader.getLocalName().equals(Tags.body.toString())) {
		    currentElement = Tags.body;
		} else if (reader.getLocalName().equals(Tags.prefix.toString())) {
		    currentElement = Tags.prefix;
		} else if (reader.getLocalName().equals(
			Tags.compositionMechanism.toString())) {
		    currentElement = Tags.compositionMechanism;
		}
		break;
	    }

	    case XMLStreamConstants.CHARACTERS: {
		if (reader.isWhiteSpace())
		    break;

		String s = reader.getText();

		switch (currentElement) {
		case type:
		    if (s.equals(ModClassification.introduction.name()))
			modclass = ModClassification.introduction;
		    else if (s.equals(ModClassification.superimposition.name()))
			modclass = ModClassification.superimposition;
		    break;
		case traversal:
		    tagContents[Tags.traversal.ordinal()] = s;
		    break;
		case text:
		    tagContents[Tags.text.ordinal()] = s;
		    break;
		case tType:
		    tagContents[Tags.tType.ordinal()] = s;
		    break;
		case externLink:
		    tagContents[Tags.externLink.ordinal()] = s;
		    break;
		case cTraversal:
		    tagContents[Tags.cTraversal.ordinal()] = s;
		    break;
		case nodeType:
		    tagContents[Tags.nodeType.ordinal()] = s;
		    break;
		case name:
		    tagContents[Tags.name.ordinal()] = s;
		    break;
		case body:
		    tagContents[Tags.body.ordinal()] = s;
		    break;
		case prefix:
		    tagContents[Tags.prefix.ordinal()] = s;
		    break;
		case compositionMechanism:
		    tagContents[Tags.compositionMechanism.ordinal()] = s;
		    break;
		}
		break;
	    }

	    case XMLStreamConstants.END_ELEMENT: {
		if (!reader.getLocalName().equals(Tags.modification.toString())) {
		    break;
		}
		Content content = null;
		FSTParseable fstParseable = null;

		if (inputType != null)
		    switch (inputType) {
		    case externLink:
			fstParseable = FSTGenerator
				.createFSTParseable(new FileInput(new File(
					input.getParentFile().getPath()
						+ File.separator
						+ tagContents[Tags.externLink
							.ordinal()])));
			break;
		    case plainText:
			fstParseable = FSTGenerator
				.createFSTParseable(new StringInput(
					tagContents[Tags.text.ordinal()],
					tagContents[Tags.tType.ordinal()]));
			break;
		    }
		switch (contentType) {
		case parsed:
		    switch (contentTraversalFlag) {

		    case noTraversal:
			content = fstParseable;
			break;
		    case traversal:
			switch (inputType) {
			case plainText:
			    content = new ParsedTraversalFSTContent(
				    tagContents[Tags.cTraversal.ordinal()],
				    FSTGenerator.createFSTParseable(
					    new StringInput(
						    tagContents[Tags.text
							    .ordinal()],
						    tagContents[Tags.tType
							    .ordinal()]))
					    .getFST());
			    break;
			case externLink:
			    content = new ParsedTraversalFSTContent(
				    tagContents[Tags.cTraversal.ordinal()],
				    FSTGenerator.createFST(new File(input
					    .getParentFile().getPath()
					    + File.separator
					    + tagContents[Tags.externLink
						    .ordinal()])));
			    break;
			}
			break;
		    }
		    break;
		case custom:
		    if (tagContents[Tags.prefix.ordinal()] != null)
			content = new CustomFSTContent(
				tagContents[Tags.compositionMechanism.ordinal()],
				tagContents[Tags.body.ordinal()],
				tagContents[Tags.name.ordinal()],
				tagContents[Tags.prefix.ordinal()],
				tagContents[Tags.nodeType.ordinal()]);
		    else
			content = new CustomFSTContent(
				tagContents[Tags.compositionMechanism.ordinal()],
				tagContents[Tags.body.ordinal()],
				tagContents[Tags.name.ordinal()], "",
				tagContents[Tags.nodeType.ordinal()]);
		    break;
		}

		switch (modclass) {
		case introduction:
		    mods.add(new IntroductionModification(
			    tagContents[Tags.traversal.ordinal()], content));
		    break;
		case superimposition:
		    mods.add(new SuperimpositionModification(
			    tagContents[Tags.traversal.ordinal()], content));
		    break;
		}
		contentTraversalFlag = ContentTraversalFlag.noTraversal;
		break;
	    }
	    }
	    reader.next();
	}
	reader.close();
	return mods;
    }

    private Validator getValidator() throws SAXException {
	SchemaFactory schemaFactory = SchemaFactory
		.newInstance("http://www.w3.org/2001/XMLSchema");
	Schema schemaGrammar = schemaFactory.newSchema(new StreamSource(
		XML_SCHEMA));

	Validator schemaValidator = schemaGrammar.newValidator();
	schemaValidator.setErrorHandler(new ErrorHandler() {

	    @Override
	    public void warning(SAXParseException exception)
		    throws SAXException {
		throw exception;
	    }

	    @Override
	    public void fatalError(SAXParseException exception)
		    throws SAXException {
		throw exception;
	    }

	    @Override
	    public void error(SAXParseException exception) throws SAXException {
		throw exception;
	    }
	});
	return schemaValidator;
    }
}
