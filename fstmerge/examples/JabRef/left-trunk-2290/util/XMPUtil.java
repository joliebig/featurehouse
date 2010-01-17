package net.sf.jabref.util;

import java.io.*;
import java.util.*;

import javax.xml.transform.TransformerException;

import net.sf.jabref.*;
import net.sf.jabref.imports.BibtexParser;
import net.sf.jabref.imports.ParserResult;

import org.jempbox.impl.DateConverter;
import org.jempbox.impl.XMLUtil;
import org.jempbox.xmp.XMPMetadata;
import org.jempbox.xmp.XMPSchema;
import org.jempbox.xmp.XMPSchemaDublinCore;
import org.pdfbox.cos.COSDictionary;
import org.pdfbox.cos.COSName;
import org.pdfbox.exceptions.COSVisitorException;
import org.pdfbox.pdmodel.PDDocument;
import org.pdfbox.pdmodel.PDDocumentCatalog;
import org.pdfbox.pdmodel.PDDocumentInformation;
import org.pdfbox.pdmodel.common.PDMetadata;


public class XMPUtil {

	
	public static List<BibtexEntry> readXMP(String filename) throws IOException {
		return readXMP(new File(filename));
	}

	
	public static void writeXMP(String filename, BibtexEntry entry,
			BibtexDatabase database) throws IOException, TransformerException {
		writeXMP(new File(filename), entry, database);
	}

	
	public static List<BibtexEntry> readXMP(File file) throws IOException {
		FileInputStream is = new FileInputStream(file);
		try {
			return readXMP(is);
		} finally {
			is.close();
		}
	}

	
	@SuppressWarnings("unchecked")
	public static List<BibtexEntry> readXMP(InputStream inputStream)
			throws IOException {

		List<BibtexEntry> result = new LinkedList<BibtexEntry>();

		PDDocument document = null;

		try {
			document = PDDocument.load(inputStream);
			if (document.isEncrypted()) {
				throw new EncryptionNotSupportedException(
						"Error: Cannot read metadata from encrypted document.");
			}

			XMPMetadata meta = getXMPMetadata(document);

			
			if (meta == null)
				return null;

			List<XMPSchema> schemas = meta
					.getSchemasByNamespaceURI(XMPSchemaBibtex.NAMESPACE);

			Iterator<XMPSchema> it = schemas.iterator();
			while (it.hasNext()) {
				XMPSchemaBibtex bib = (XMPSchemaBibtex) it.next();

				result.add(bib.getBibtexEntry());
			}

			
			if (result.size() == 0) {
				schemas = meta
						.getSchemasByNamespaceURI(XMPSchemaDublinCore.NAMESPACE);
				it = schemas.iterator();
				while (it.hasNext()) {
					XMPSchemaDublinCore dc = (XMPSchemaDublinCore) it.next();

					BibtexEntry entry = getBibtexEntryFromDublinCore(dc);

					if (entry != null)
						result.add(entry);
				}
			}

			if (result.size() == 0) {
				BibtexEntry entry = getBibtexEntryFromDocumentInformation(document
						.getDocumentInformation());

				if (entry != null)
					result.add(entry);
			}
		} finally {
			if (document != null)
				document.close();
		}
		return result;
	}

	
	@SuppressWarnings("unchecked")
	public static BibtexEntry getBibtexEntryFromDocumentInformation(
			PDDocumentInformation di) {

		BibtexEntry entry = new BibtexEntry();

		String s = di.getAuthor();
		if (s != null)
			entry.setField("author", s);

		s = di.getTitle();
		if (s != null)
			entry.setField("title", s);

		s = di.getKeywords();
		if (s != null)
			entry.setField("keywords", s);

		s = di.getSubject();
		if (s != null)
			entry.setField("abstract", s);

		COSDictionary dict = di.getDictionary();
		Iterator it = dict.keyList().iterator();
		while (it.hasNext()) {
			String key = ((COSName) it.next()).getName();
			if (key.startsWith("bibtex/")) {
				String value = dict.getString(key);
				key = key.substring("bibtex/".length());
				if (key.equals("entrytype")) {
					BibtexEntryType type = BibtexEntryType
							.getStandardType(value);
					if (type != null)
						entry.setType(type);
				} else
					entry.setField(key, value);
			}
		}

		
		return (entry.getAllFields().size() > 0 ? entry : null);
	}

	
	@SuppressWarnings("unchecked")
	public static BibtexEntry getBibtexEntryFromDublinCore(
			XMPSchemaDublinCore dcSchema) {

		BibtexEntry entry = new BibtexEntry();

		
		List contributors = dcSchema.getContributors();
		if (contributors != null) {
			Iterator it = contributors.iterator();
			StringBuffer sb = null;
			while (it.hasNext()) {
				if (sb != null) {
					sb.append(" and ");
				} else {
					sb = new StringBuffer();
				}
				sb.append(it.next());
			}
			if (sb != null)
				entry.setField("editor", sb.toString());
		}

		
		List creators = dcSchema.getCreators();
		if (creators != null) {
			Iterator it = creators.iterator();
			StringBuffer sb = null;
			while (it.hasNext()) {
				if (sb != null) {
					sb.append(" and ");
				} else {
					sb = new StringBuffer();
				}
				sb.append(it.next());
			}
			if (sb != null)
				entry.setField("author", sb.toString());
		}

		
		List dates = dcSchema.getSequenceList("dc:date");
		if (dates != null && dates.size() > 0) {
			String date = ((String) dates.get(0)).trim();
			Calendar c = null;
			try {
				c = DateConverter.toCalendar(date);
			} catch (Exception e) {

			}
			if (c != null) {
				entry.setField("year", String.valueOf(c.get(Calendar.YEAR)));
				if (date.length() > 4) {
					entry.setField("month", "#"
							+ Globals.MONTHS[c.get(Calendar.MONTH)] + "#");
				}
			}
		}

		
		String s = dcSchema.getDescription();
		if (s != null)
			entry.setField("abstract", s);

		
		s = dcSchema.getIdentifier();
		if (s != null)
			entry.setField("doi", s);

		
		List publishers = dcSchema.getPublishers();
		if (publishers != null) {
			Iterator it = dcSchema.getPublishers().iterator();
			StringBuffer sb = null;
			while (it.hasNext()) {
				if (sb != null) {
					sb.append(" and ");
				} else {
					sb = new StringBuffer();
				}
				sb.append(it.next());
			}
			if (sb != null)
				entry.setField("publishers", sb.toString());
		}

		
		List relationships = dcSchema.getRelationships();
		if (relationships != null) {
			Iterator it = relationships.iterator();
			while (it.hasNext()) {
				s = (String) it.next();
				if (s.startsWith("bibtex/")) {
					s = s.substring("bibtex/".length());
					int i = s.indexOf('/');
					if (i != -1) {
						entry.setField(s.substring(0, i), s.substring(i + 1));
					}
				}
			}
		}

		
		s = dcSchema.getRights();
		if (s != null)
			entry.setField("rights", s);

		
		s = dcSchema.getSource();
		if (s != null)
			entry.setField("source", s);

		
		List subjects = dcSchema.getSubjects();
		if (subjects != null) {
			Iterator it = subjects.iterator();
			StringBuffer sb = null;
			while (it.hasNext()) {
				if (sb != null) {
					sb.append(",");
				} else {
					sb = new StringBuffer();
				}
				sb.append(it.next());
			}
			if (sb != null)
				entry.setField("keywords", sb.toString());
		}

		
		s = dcSchema.getTitle();
		if (s != null)
			entry.setField("title", s);

		
		List l = dcSchema.getTypes();
		if (l != null && l.size() > 0) {
			s = (String) l.get(0);
			if (s != null) {
				BibtexEntryType type = BibtexEntryType.getStandardType(s);
				if (type != null)
					entry.setType(type);
			}
		}

		return (entry.getAllFields().size() > 0 ? entry : null);
	}

	
	public static void writeXMP(File file, BibtexEntry entry,
			BibtexDatabase database) throws IOException, TransformerException {
		List<BibtexEntry> l = new LinkedList<BibtexEntry>();
		l.add(entry);
		writeXMP(file, l, database, true);
	}

	
	public static void toXMP(Collection<BibtexEntry> bibtexEntries,
			BibtexDatabase database, OutputStream outputStream)
			throws IOException, TransformerException {

		if (database != null)
			bibtexEntries = database.resolveForStrings(bibtexEntries, true);

		XMPMetadata x = new XMPMetadata();

		Iterator<BibtexEntry> it = bibtexEntries.iterator();
		while (it.hasNext()) {
			BibtexEntry e = (BibtexEntry) it.next();
			XMPSchemaBibtex schema = new XMPSchemaBibtex(x);
			x.addSchema(schema);
			schema.setBibtexEntry(e);
		}

		x.save(outputStream);
	}

	
	public static String toXMP(Collection<BibtexEntry> bibtexEntries,
			BibtexDatabase database) throws TransformerException {
		try {
			ByteArrayOutputStream bs = new ByteArrayOutputStream();
			toXMP(bibtexEntries, database, bs);
			return bs.toString();
		} catch (IOException e) {
			throw new TransformerException(e);
		}
	}

	
	public static XMPMetadata readRawXMP(InputStream inputStream)
			throws IOException {
		PDDocument document = null;

		try {
			document = PDDocument.load(inputStream);
			if (document.isEncrypted()) {
				throw new EncryptionNotSupportedException(
						"Error: Cannot read metadata from encrypted document.");
			}

			return getXMPMetadata(document);

		} finally {
			if (document != null)
				document.close();
		}
	}

	static XMPMetadata getXMPMetadata(PDDocument document) throws IOException {
		PDDocumentCatalog catalog = document.getDocumentCatalog();
		PDMetadata metaRaw = catalog.getMetadata();

		if (metaRaw == null) {
			return null;
		}

		XMPMetadata meta = new XMPMetadata(XMLUtil.parse(metaRaw
				.createInputStream()));
		meta.addXMLNSMapping(XMPSchemaBibtex.NAMESPACE, XMPSchemaBibtex.class);
		return meta;
	}

	
	public static XMPMetadata readRawXMP(File file) throws IOException {
		FileInputStream is = new FileInputStream(file);
		try {
			return readRawXMP(is);
		} finally {
			is.close();
		}
	}

	static void writeToDCSchema(XMPSchemaDublinCore dcSchema,
			BibtexEntry entry, BibtexDatabase database) {

		if (database != null)
			entry = database.resolveForStrings(entry, false);

		
		
		for (String field : entry.getAllFields()){

			if (field.equals("editor")) {
				String o = entry.getField(field.toString()).toString();

				

				String authors = o.toString();
				AuthorList list = AuthorList.getAuthorList(authors);

				int n = list.size();
				for (int i = 0; i < n; i++) {
					dcSchema.addContributor(list.getAuthor(i).getFirstLast(
							false));
				}
				continue;
			}

			

			
			if (field.equals("author")) {
				String o = entry.getField(field.toString()).toString();
				String authors = o.toString();
				AuthorList list = AuthorList.getAuthorList(authors);

				int n = list.size();
				for (int i = 0; i < n; i++) {
					dcSchema.addCreator(list.getAuthor(i).getFirstLast(false));
				}
				continue;
			}

			if (field.equals("month")) {
				
				continue;
			}

			if (field.equals("year")) {

				
				String publicationDate = Util.getPublicationDate(entry);
				if (publicationDate != null) {
					dcSchema.addSequenceValue("dc:date", publicationDate);
				}
				continue;
			}
			
			if (field.equals("abstract")) {
				String o = entry.getField(field.toString()).toString();
				dcSchema.setDescription(o.toString());
				continue;
			}

			
			if (field.equals("doi")) {
				String o = entry.getField(field.toString()).toString();
				dcSchema.setIdentifier(o.toString());
				continue;
			}

			

			
			if (field.equals("publisher")) {
				String o = entry.getField(field.toString()).toString();
				dcSchema.addPublisher(o.toString());
				continue;
			}

			

			

			
			if (field.equals("keywords")) {
				String o = entry.getField(field.toString()).toString();
				String[] keywords = o.toString().split(",");
				for (int i = 0; i < keywords.length; i++) {
					dcSchema.addSubject(keywords[i].trim());
				}
				continue;
			}

			
			if (field.equals("title")) {
				String o = entry.getField(field.toString()).toString();
				dcSchema.setTitle(o.toString());
				continue;
			}

			
			
			String o = entry.getField(field.toString()).toString();
			dcSchema.addRelation("bibtex/" + field.toString() + "/" + o);
		}

		
		dcSchema.setFormat("application/pdf");

		
		Object o = entry.getType().getName();
		if (o != null)
			dcSchema.addType(o.toString());
	}

	
	public static void writeDublinCore(PDDocument document, BibtexEntry entry,
			BibtexDatabase database) throws IOException, TransformerException {

		List<BibtexEntry> entries = new ArrayList<BibtexEntry>();
		entries.add(entry);

		writeDublinCore(document, entries, database);
	}

	
	@SuppressWarnings("unchecked")
	public static void writeDublinCore(PDDocument document,
			Collection<BibtexEntry> entries, BibtexDatabase database)
			throws IOException, TransformerException {

		if (database != null)
			entries = database.resolveForStrings(entries, false);

		PDDocumentCatalog catalog = document.getDocumentCatalog();
		PDMetadata metaRaw = catalog.getMetadata();

		XMPMetadata meta;
		if (metaRaw != null) {
			meta = new XMPMetadata(XMLUtil.parse(metaRaw.createInputStream()));
		} else {
			meta = new XMPMetadata();
		}

		
		List schemas = meta
				.getSchemasByNamespaceURI(XMPSchemaDublinCore.NAMESPACE);
		Iterator it = schemas.iterator();
		while (it.hasNext()) {
			XMPSchema bib = (XMPSchema) it.next();
			bib.getElement().getParentNode().removeChild(bib.getElement());
		}

		for (BibtexEntry entry : entries) {
			XMPSchemaDublinCore dcSchema = new XMPSchemaDublinCore(meta);
			writeToDCSchema(dcSchema, entry, null);
			meta.addSchema(dcSchema);
		}

		
		ByteArrayOutputStream os = new ByteArrayOutputStream();
		meta.save(os);
		ByteArrayInputStream is = new ByteArrayInputStream(os.toByteArray());
		PDMetadata metadataStream = new PDMetadata(document, is, false);
		catalog.setMetadata(metadataStream);
	}

	
	public static void writeDocumentInformation(PDDocument document,
			BibtexEntry entry, BibtexDatabase database) {

		PDDocumentInformation di = document.getDocumentInformation();

		if (database != null)
			entry = database.resolveForStrings(entry, false);

		
		Set<String> fields = entry.getAllFields();

		for (String field : fields){
			if (field.equals("author")) {
				di.setAuthor(entry.getField("author").toString());
			} else if (field.equals("title")) {
				di.setTitle(entry.getField("title").toString());
			} else if (field.equals("keywords")) {
				di.setKeywords(entry.getField("keywords").toString());
			} else if (field.equals("abstract")) {
				di.setSubject(entry.getField("abstract").toString());
			} else {
				di.setCustomMetadataValue("bibtex/" + field.toString(),
						entry.getField(field.toString()).toString());
			}
		}
		di
				.setCustomMetadataValue("bibtex/entrytype", entry.getType()
						.getName());
	}

	
	@SuppressWarnings("unchecked")
	public static void writeXMP(File file,
			Collection<BibtexEntry> bibtexEntries, BibtexDatabase databasee,
			boolean writePDFInfo) throws IOException, TransformerException {

		if (databasee != null)
			bibtexEntries = databasee.resolveForStrings(bibtexEntries, false);

		PDDocument document = null;

		try {
			document = PDDocument.load(file.getAbsoluteFile());
			if (document.isEncrypted()) {
				throw new EncryptionNotSupportedException(
						"Error: Cannot add metadata to encrypted document.");
			}

			if (writePDFInfo && bibtexEntries.size() == 1) {
				writeDocumentInformation(document, (BibtexEntry) bibtexEntries
						.iterator().next(), null);
				writeDublinCore(document, bibtexEntries, null);
			}

			PDDocumentCatalog catalog = document.getDocumentCatalog();
			PDMetadata metaRaw = catalog.getMetadata();

			XMPMetadata meta;
			if (metaRaw != null) {
				meta = new XMPMetadata(XMLUtil.parse(metaRaw
						.createInputStream()));
			} else {
				meta = new XMPMetadata();
			}
			meta.addXMLNSMapping(XMPSchemaBibtex.NAMESPACE,
					XMPSchemaBibtex.class);

			
			List schemas = meta
					.getSchemasByNamespaceURI(XMPSchemaBibtex.NAMESPACE);
			Iterator it = schemas.iterator();
			while (it.hasNext()) {
				XMPSchemaBibtex bib = (XMPSchemaBibtex) it.next();
				bib.getElement().getParentNode().removeChild(bib.getElement());
			}

			it = bibtexEntries.iterator();
			while (it.hasNext()) {
				BibtexEntry e = (BibtexEntry) it.next();
				XMPSchemaBibtex bibtex = new XMPSchemaBibtex(meta);
				meta.addSchema(bibtex);
				bibtex.setBibtexEntry(e, null);
			}

			
			ByteArrayOutputStream os = new ByteArrayOutputStream();
			meta.save(os);
			ByteArrayInputStream is = new ByteArrayInputStream(os.toByteArray());
			PDMetadata metadataStream = new PDMetadata(document, is, false);
			catalog.setMetadata(metadataStream);

			
			try {
				document.save(file.getAbsolutePath());
			} catch (COSVisitorException e) {
				throw new TransformerException("Could not write XMP-metadata: "
						+ e.getLocalizedMessage());
			}

		} finally {
			if (document != null) {
				document.close();
			}
		}
	}

	
	protected static void usage() {
		System.out.println("Read or write XMP-metadata from or to pdf file.");
		System.out.println("");
		System.out.println("Usage:");
		System.out.println("Read from PDF and print as bibtex:");
		System.out.println("  xmpUtil <pdf>");
		System.out.println("Read from PDF and print raw XMP:");
		System.out.println("  xmpUtil -x <pdf>");
		System.out
				.println("Write the entry in <bib> given by <key> to the PDF:");
		System.out.println("  xmpUtil <key> <bib> <pdf>");
		System.out.println("Write all entries in <bib> to the PDF:");
		System.out.println("  xmpUtil <bib> <pdf>");
		System.out.println("");
		System.out
				.println("To report bugs visit http://jabref.sourceforge.net");
	}

	
	public static void main(String[] args) throws IOException,
			TransformerException {

		
		if (Globals.prefs == null) {
			Globals.prefs = JabRefPreferences.getInstance();
		}

		switch (args.length) {
		case 0:
			usage();
			break;
		case 1: {

			if (args[0].endsWith(".pdf")) {
				
				List<BibtexEntry> l = XMPUtil.readXMP(new File(args[0]));

				Iterator<BibtexEntry> it = l.iterator();
				while (it.hasNext()) {
					BibtexEntry e = (BibtexEntry) it.next();
					StringWriter sw = new StringWriter();
					e.write(sw, new net.sf.jabref.export.LatexFieldFormatter(),
							false);
					System.out.println(sw.getBuffer().toString());
				}

			} else if (args[0].endsWith(".bib")) {
				

				ParserResult result = BibtexParser
						.parse(new FileReader(args[0]));
				Collection<BibtexEntry> entries = result.getDatabase()
						.getEntries();

				if (entries.size() == 0) {
					System.err.println("Could not find BibtexEntry in "
							+ args[0]);
				} else {
					System.out.println(XMPUtil.toXMP(entries, result
							.getDatabase()));
				}

			} else {
				usage();
			}
			break;
		}
		case 2: {
			if (args[0].equals("-x") && args[1].endsWith(".pdf")) {
				
				XMPMetadata meta = XMPUtil.readRawXMP(new File(args[1]));

				if (meta == null) {
					System.err
							.println("The given pdf does not contain any XMP-metadata.");
				} else {
					XMLUtil.save(meta.getXMPDocument(), System.out, "UTF-8");
				}
				break;
			}

			if (args[0].endsWith(".bib") && args[1].endsWith(".pdf")) {
				ParserResult result = BibtexParser
						.parse(new FileReader(args[0]));

				Collection<BibtexEntry> entries = result.getDatabase()
						.getEntries();

				if (entries.size() == 0) {
					System.err.println("Could not find BibtexEntry in "
							+ args[0]);
				} else {
					XMPUtil.writeXMP(new File(args[1]), entries, result
							.getDatabase(), false);
					System.out.println("XMP written.");
				}
				break;
			}

			usage();
			break;
		}
		case 3: {
			if (!args[1].endsWith(".bib") && !args[2].endsWith(".pdf")) {
				usage();
				break;
			}

			ParserResult result = BibtexParser.parse(new FileReader(args[1]));

			BibtexEntry e = result.getDatabase().getEntryByKey(args[0]);

			if (e == null) {
				System.err.println("Could not find BibtexEntry " + args[0]
						+ " in " + args[0]);
			} else {
				XMPUtil.writeXMP(new File(args[2]), e, result.getDatabase());

				System.out.println("XMP written.");
			}
			break;
		}

		default:
			usage();
		}
	}

	
	public static boolean hasMetadata(InputStream is) {
		try {
			List<BibtexEntry> l = XMPUtil.readXMP(is);
			return l.size() > 0;
		} catch (Exception e) {
			return false;
		}
	}
}