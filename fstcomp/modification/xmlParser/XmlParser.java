package modification.xmlParser;

import java.io.File;
import java.io.IOException;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;
import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import javax.xml.validation.Validator;

import modification.IntroductionModification;
import modification.JavaMethodBodyOverrideModification;
import modification.ModificationComposition;
import modification.SuperimpositionModification;
import modification.UpdateFeatureNameModification;
import modification.content.Content;
import modification.content.ContentGenerator;
import modification.content.CustomFSTContent;
import modification.content.TraversalFSTContent;
import modification.content.UnknownContentTypeParseException;
import modification.content.UnknownFileTypeParseException;
import modification.content.Parseables.java.JavaMethodBody;

import org.xml.sax.ErrorHandler;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

public class XmlParser {
    private static final String XML_SCHEMA = "modification/xmlParser/schema.xsd";

    File input;

    ModificationComposition mods = new ModificationComposition();

    enum Tags {
	modification, type, traversal, content, parsed, custom, plainText, tType, text, externLink, name, body, prefix, compositionMechanism, cTraversal, nodeType, modificationComposition
    }

    enum ModClassification {
	superimposition, introduction, javaMethodBodyOverriding
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
	    SAXException, IOException, UnknownFileTypeParseException,
	    UnknownContentTypeParseException {

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
	Tags lastElement = null;

	String text = "";

	String[] tagContents = new String[Tags.values().length];
	ModificationComposition mods = new ModificationComposition();

	while (reader.hasNext()) {
	    switch (reader.getEventType()) {
	    case XMLStreamConstants.START_ELEMENT: {
		if (reader.getLocalName().equals(
			Tags.modificationComposition.toString())) {
		    currentElement = Tags.modificationComposition;
		} else if (reader.getLocalName().equals(
			Tags.modification.toString())) {
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
		if (lastElement == null)
		    lastElement = currentElement;
		
		if (currentElement.name().equals(lastElement.name()))
		    text = text + reader.getText();
		else
		    text = reader.getText();

		lastElement = currentElement;

		switch (currentElement) {
		case type:
		    if (text.equals(ModClassification.introduction.name()))
			modclass = ModClassification.introduction;
		    else if (text.equals(ModClassification.superimposition
			    .name()))
			modclass = ModClassification.superimposition;
		    else if (text
			    .equals(ModClassification.javaMethodBodyOverriding
				    .name()))
			modclass = ModClassification.javaMethodBodyOverriding;
		    break;
		case traversal:
		    tagContents[Tags.traversal.ordinal()] = text;
		    break;
		case text:
		    tagContents[Tags.text.ordinal()] = text;
		    break;
		case tType:
		    tagContents[Tags.tType.ordinal()] = text;
		    break;
		case externLink:
		    tagContents[Tags.externLink.ordinal()] = text;
		    break;
		case cTraversal:
		    tagContents[Tags.cTraversal.ordinal()] = text;
		    break;
		case nodeType:
		    tagContents[Tags.nodeType.ordinal()] = text;
		    break;
		case name:
		    tagContents[Tags.name.ordinal()] = text;
		    break;
		case body:
		    tagContents[Tags.body.ordinal()] = text;
		    break;
		case prefix:
		    tagContents[Tags.prefix.ordinal()] = text;
		    break;
		case compositionMechanism:
		    tagContents[Tags.compositionMechanism.ordinal()] = text;
		    break;
		}
		break;
	    }

	    case XMLStreamConstants.END_ELEMENT: {
		if (reader.getLocalName().equals(
			Tags.modificationComposition.toString())) {
		    mods.addLast(new UpdateFeatureNameModification(input
			    .getParentFile().getParentFile().getName()));
		}
		if (!reader.getLocalName().equals(Tags.modification.toString())) {
		    break;
		}
		Content content = null;

		if (inputType != null)
		    switch (inputType) {
		    case externLink:
			content = ContentGenerator.createContent(new File(input
				.getParentFile().getPath()
				+ File.separator
				+ tagContents[Tags.externLink.ordinal()]));
			break;
		    case plainText:
			content = ContentGenerator.createContent(
				tagContents[Tags.tType.ordinal()],
				tagContents[Tags.text.ordinal()]);
			break;
		    }
		switch (contentType) {
		case parsed:
		    switch (contentTraversalFlag) {

		    case noTraversal:
			break;
		    case traversal:

			content = new TraversalFSTContent(
				tagContents[Tags.cTraversal.ordinal()], content);

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
		case javaMethodBodyOverriding:
		    mods.add(new JavaMethodBodyOverrideModification(
			    tagContents[Tags.traversal.ordinal()],
			    (JavaMethodBody) content));
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
