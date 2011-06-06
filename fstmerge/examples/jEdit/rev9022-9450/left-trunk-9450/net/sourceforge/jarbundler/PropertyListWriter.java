

package net.sourceforge.jarbundler;


import net.sourceforge.jarbundler.AppBundleProperties;


import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;


import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;


import java.lang.Boolean;
import java.lang.ClassCastException;
import java.lang.Double;
import java.lang.String;
import java.lang.System;


import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.util.FileUtils;


import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.ParserConfigurationException;


import org.w3c.dom.Document;
import org.w3c.dom.DOMImplementation;
import org.w3c.dom.Node;
import org.w3c.dom.Element;
import org.w3c.dom.Attr;



import org.apache.xml.serialize.OutputFormat;
import org.apache.xml.serialize.XMLSerializer;
import org.apache.xml.serialize.LineSeparator;






public class PropertyListWriter {


	
	private AppBundleProperties bundleProperties;

	private double version = 1.3;

	
	private Document document = null;


	private FileUtils fileUtils = FileUtils.getFileUtils();
	
	
	public PropertyListWriter(AppBundleProperties bundleProperties) {
		this.bundleProperties = bundleProperties;
		setJavaVersion(bundleProperties.getJVMVersion());
	}

	private void setJavaVersion(String version) {

		if (version == null)
			return;

		this.version = Double.valueOf(version.substring(0, 3)).doubleValue();
	}


	public void writeFile(File fileName) throws BuildException {

		Writer writer = null;

		try {

			this.document = createDOM();
			buildDOM();

			
			writer = new BufferedWriter(new OutputStreamWriter(
			                            new FileOutputStream(fileName), "UTF-8"));
			
			OutputFormat outputFormat = new OutputFormat();
			outputFormat.setMethod("xml");
			outputFormat.setIndenting(true);
			outputFormat.setIndent(2);
			outputFormat.setLineWidth(0);             
			
			
			XMLSerializer serializer = new XMLSerializer(writer, outputFormat);
			serializer.asDOMSerializer();
			serializer.serialize(this.document);

		} catch (ParserConfigurationException pce) {
			throw new BuildException(pce);
		} catch (IOException ex) {
			throw new BuildException("Unable to write  \"" + fileName + "\"");
		} finally {
			fileUtils.close(writer);
		}


	}

	private Document createDOM() throws ParserConfigurationException {
	
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		DocumentBuilder documentBuilder = factory.newDocumentBuilder();
		DOMImplementation domImpl = documentBuilder.getDOMImplementation();

		
		
		
		org.w3c.dom.DocumentType doctype = domImpl.createDocumentType(
		       "plist",
		       "-//Apple Computer//DTD PLIST 1.0//EN",
		       "http://www.apple.com/DTDs/PropertyList-1.0.dtd");

		return domImpl.createDocument(null, "plist", doctype);
	}


	private void buildDOM()  {

		Element plist = this.document.getDocumentElement();
		plist.setAttribute("version","1.0");		

		
		
		Node dict = createNode("dict", plist);

		
		writeKeyStringPair("CFBundleName", bundleProperties.getCFBundleName(), dict);

		
		writeKeyStringPair("CFBundleShortVersionString", bundleProperties.getCFBundleShortVersionString(), dict);
		
		
		writeKeyStringPair("CFBundleGetInfoString", bundleProperties.getCFBundleGetInfoString(), dict);
		
		
		writeKeyStringPair("CFBundleAllowMixedLocalizations", 
		     (bundleProperties.getCFBundleAllowMixedLocalizations() ? "true" : "false"),  
		     dict);

		
		writeKeyStringPair("CFBundleInfoDictionaryVersion", 
		     bundleProperties.getCFBundleInfoDictionaryVersion(), dict);

		
		writeKeyStringPair("CFBundleExecutable", bundleProperties.getCFBundleExecutable(), dict);

		
		writeKeyStringPair("CFBundleDevelopmentRegion", bundleProperties.getCFBundleDevelopmentRegion(), dict);

		
		writeKeyStringPair("CFBundlePackageType", bundleProperties.getCFBundlePackageType(), dict);

		
		writeKeyStringPair("CFBundleSignature", bundleProperties.getCFBundleSignature(), dict);

		
		if (bundleProperties.getCFBundleVersion() != null) 
			writeKeyStringPair("CFBundleVersion", bundleProperties.getCFBundleVersion(), dict);
		
		
		if (bundleProperties.getCFBundleIconFile() != null) 
			writeKeyStringPair("CFBundleIconFile", bundleProperties.getCFBundleIconFile(), dict);

		
		if (bundleProperties.getCFBundleIdentifier() != null) 
			writeKeyStringPair("CFBundleIdentifier", bundleProperties.getCFBundleIdentifier(), dict);

		
		if (bundleProperties.getCFBundleHelpBookFolder() != null) 
			writeKeyStringPair("CFBundleHelpBookFolder", bundleProperties.getCFBundleHelpBookFolder(), dict);

		
		if (bundleProperties.getCFBundleHelpBookName() != null) 
			writeKeyStringPair("CFBundleHelpBookName", bundleProperties.getCFBundleHelpBookName(), dict);

		
		List documentTypes = bundleProperties.getDocumentTypes();

		if (documentTypes.size() > 0) 
 			writeDocumentTypes(documentTypes, dict);

		
		writeKey("Java", dict);
		Node javaDict = createNode("dict", dict);

		
		writeKeyStringPair("MainClass", bundleProperties.getMainClass(), javaDict);

		
		if (bundleProperties.getJVMVersion() != null) 
			writeKeyStringPair("JVMVersion", bundleProperties.getJVMVersion(), javaDict);


		
		
		

		List classPath = bundleProperties.getClassPath();
		List extraClassPath = bundleProperties.getExtraClassPath();

		if ((classPath.size() > 0) || (extraClassPath.size() > 0)) 
			writeClasspath(classPath, extraClassPath, javaDict);
		

		
		if (bundleProperties.getVMOptions() != null) 
			writeKeyStringPair("VMOptions", bundleProperties.getVMOptions(), javaDict);

		
		if (bundleProperties.getWorkingDirectory() != null) 
			writeKeyStringPair("WorkingDirectory", bundleProperties.getWorkingDirectory(), javaDict);

		
		if (bundleProperties.getArguments() != null) 
			writeKeyStringPair("Arguments", bundleProperties.getArguments(), javaDict);

		
		Hashtable javaProperties = bundleProperties.getJavaProperties();

		if (javaProperties.isEmpty() == false) 
 			writeJavaProperties(javaProperties, javaDict);


		
		List services = bundleProperties.getServices();
		if (services.size() > 0) 
 			writeServices(services,dict);
		
	}


	private void writeDocumentTypes(List documentTypes, Node appendTo) {

		writeKey("CFBundleDocumentTypes", appendTo);
		
		Node array = createNode("array", appendTo);

		Iterator itor = documentTypes.iterator();

		while (itor.hasNext()) {

			DocumentType documentType = (DocumentType) itor.next();

			Node documentDict = createNode("dict", array);

			writeKeyStringPair("CFBundleTypeName", documentType.getName(), documentDict);
			writeKeyStringPair("CFBundleTypeRole", documentType.getRole(), documentDict);

			File iconFile = documentType.getIconFile();

			if (iconFile != null)
				writeKeyStringPair("CFBundleTypeIconFile", iconFile.getName(), documentDict);


			List extensions = documentType.getExtensions();

			if (extensions.isEmpty() == false) {
				writeKey("CFBundleTypeExtensions", documentDict);
				writeArray(extensions, documentDict);
			}

			List osTypes = documentType.getOSTypes();

			if (osTypes.isEmpty() == false) {
				writeKey("CFBundleTypeOSTypes", documentDict);
				writeArray(osTypes, documentDict);
			}

			
			List mimeTypes = documentType.getMimeTypes();

			if (mimeTypes.isEmpty() == false) {
				writeKey("CFBundleTypeMIMETypes", documentDict);
				writeArray(mimeTypes, documentDict);
			}

			
			if (documentType.isBundle()) 
				writeKeyStringPair("LSTypeIsPackage", "true", documentDict);
		}
	}
	
	private void writeServices(List services, Node appendTo) {
	
		writeKey("NSServices",appendTo);
		Node array = createNode("array",appendTo);
		Iterator itor = services.iterator();
		
		while (itor.hasNext()) {
			Service service = (Service)itor.next();
			Node serviceDict = createNode("dict",array);

			String portName = service.getPortName();
            if (portName == null)
            	portName = bundleProperties.getCFBundleName();
			
			writeKeyStringPair("NSPortName", portName, serviceDict);
			writeKeyStringPair("NSMessage",service.getMessage(),serviceDict);
			
			List sendTypes = service.getSendTypes();
			if (!sendTypes.isEmpty()) {
				writeKey("NSSendTypes",serviceDict);
				writeArray(sendTypes,serviceDict);
			}
			
			List returnTypes = service.getReturnTypes();
			if (!returnTypes.isEmpty()) {
				writeKey("NSReturnTypes",serviceDict);
				writeArray(returnTypes,serviceDict);
			}
			
			writeKey("NSMenuItem",serviceDict);
			Node menuItemDict = createNode("dict",serviceDict);
			writeKeyStringPair("default",service.getMenuItem(),menuItemDict);
			
			String keyEquivalent = service.getKeyEquivalent();
			if (null != keyEquivalent) {
				writeKey("NSKeyEquivalent",serviceDict);
				Node keyEquivalentDict = createNode("dict",serviceDict);
				writeKeyStringPair("default",keyEquivalent,keyEquivalentDict);
			}
			
			String userData = service.getUserData();
			if (null != userData)
				writeKeyStringPair("NSUserData", userData, serviceDict);
			
			String timeout = service.getTimeout();
			if (null != timeout)
				writeKeyStringPair("NSTimeout",timeout,serviceDict);
 		}
	}

	private void writeClasspath(List classpath, List extraClasspath, Node appendTo) {
		writeKey("ClassPath", appendTo);
		classpath.addAll(extraClasspath);
		writeArray(classpath, appendTo);
	}


	private void writeJavaProperties(Hashtable javaProperties, Node appendTo) {
	
		writeKey("Properties", appendTo);
		
		Node propertiesDict = createNode("dict", appendTo);

		for (Iterator i = javaProperties.keySet().iterator(); i.hasNext();) {
			String key = (String) i.next();

			if (key.startsWith("com.apple.") && (version >= 1.4)) {
				System.out.println("Deprecated as of 1.4: " + key);
				continue;
			}

			writeKeyStringPair(key, (String)javaProperties.get(key), propertiesDict);
		}
	}

	private Node createNode(String tag, Node appendTo) {
		Node node = this.document.createElement(tag);
		appendTo.appendChild(node);
		return node;
	}


	private void writeKeyStringPair(String key, String string, Node appendTo) {
	
		if (string == null)
			return;
	
		writeKey(key, appendTo);
		writeString(string, appendTo);
	}


	private void writeKey(String key, Node appendTo) {
		Element keyNode = this.document.createElement("key");
		appendTo.appendChild(keyNode);
		keyNode.appendChild(this.document.createTextNode(key));
	}


	private void writeString(String string, Node appendTo) {
		Element stringNode = this.document.createElement("string");
		stringNode.appendChild(this.document.createTextNode(string));
		appendTo.appendChild(stringNode);
	}

	private void writeArray(List stringList, Node appendTo) {
	
		Node arrayNode = createNode("array", appendTo);	

		for (Iterator it = stringList.iterator(); it.hasNext();) 
			writeString((String)it.next(), arrayNode);
		
	}

}
