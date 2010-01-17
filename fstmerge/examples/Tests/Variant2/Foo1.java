class Foo {
	static void writeToDCSchema(XMPSchemaDublinCore dcSchema,
			BibtexEntry entry, BibtexDatabase database) {

		if (database != null)
			entry = database.resolveForStrings(entry, false);

		// Set all the values including key and entryType
		Object[] fields = entry.getAllFields();

		for (int j = 0; j < fields.length; j++) {

			if (fields[j].equals("editor")) {
				String o = entry.getField(fields[j].toString()).toString();

				/**
				 * Editor -> Contributor
				 * 
				 * Field: dc:contributor
				 * 
				 * Type: bag ProperName
				 * 
				 * Category: External
				 * 
				 * Description: Contributors to the resource (other than the
				 * authors).
				 * 
				 * Bibtex-Fields used: editor
				 */

				String authors = o.toString();
				AuthorList list = AuthorList.getAuthorList(authors);

				int n = list.size();
				for (int i = 0; i < n; i++) {
					dcSchema.addContributor(list.getAuthor(i).getFirstLast(
							false));
				}
				continue;
			}

			/**
			 * ? -> Coverage
			 * 
			 * Unmapped
			 * 
			 * dc:coverage Text External The extent or scope of the resource.
			 */

			/**
			 * Author -> Creator
			 * 
			 * Field: dc:creator
			 * 
			 * Type: seq ProperName
			 * 
			 * Category: External
			 * 
			 * Description: The authors of the resource (listed in order of
			 * precedence, if significant).
			 * 
			 * Bibtex-Fields used: author
			 */
			if (fields[j].equals("author")) {
				String o = entry.getField(fields[j].toString()).toString();
				String authors = o.toString();
				AuthorList list = AuthorList.getAuthorList(authors);

				int n = list.size();
				for (int i = 0; i < n; i++) {
					dcSchema.addCreator(list.getAuthor(i).getFirstLast(false));
				}
				continue;
			}

			if (fields[j].equals("month")) {
				// Dealt with in year
				continue;
			}

			if (fields[j].equals("year")) {

				/**
				 * Year + Month -> Date
				 * 
				 * Field: dc:date
				 * 
				 * Type: seq Date
				 * 
				 * Category: External
				 * 
				 * Description: Date(s) that something interesting happened to
				 * the resource.
				 * 
				 * Bibtex-Fields used: year, month
				 */
				String publicationDate = Util.getPublicationDate(entry);
				if (publicationDate != null) {
					dcSchema.addSequenceValue("dc:date", publicationDate);
				}
				continue;
			}
			/**
			 * Abstract -> Description
			 * 
			 * Field: dc:description
			 * 
			 * Type: Lang Alt
			 * 
			 * Category: External
			 * 
			 * Description: A textual description of the content of the
			 * resource. Multiple values may be present for different languages.
			 * 
			 * Bibtex-Fields used: abstract
			 */
			if (fields[j].equals("abstract")) {
				String o = entry.getField(fields[j].toString()).toString();
				dcSchema.setDescription(o.toString());
				continue;
			}

			/**
			 * DOI -> identifier
			 * 
			 * Field: dc:identifier
			 * 
			 * Type: Text
			 * 
			 * Category: External
			 * 
			 * Description: Unique identifier of the resource.
			 * 
			 * Bibtex-Fields used: doi
			 */
			if (fields[j].equals("doi")) {
				String o = entry.getField(fields[j].toString()).toString();
				dcSchema.setIdentifier(o.toString());
				continue;
			}

			/**
			 * ? -> Language
			 * 
			 * Unmapped
			 * 
			 * dc:language bag Locale Internal An unordered array specifying the
			 * languages used in the resource.
			 */

			/**
			 * Publisher -> Publisher
			 * 
			 * Field: dc:publisher
			 * 
			 * Type: bag ProperName
			 * 
			 * Category: External
			 * 
			 * Description: Publishers.
			 * 
			 * Bibtex-Fields used: doi
			 */
			if (fields[j].equals("publisher")) {
				String o = entry.getField(fields[j].toString()).toString();
				dcSchema.addPublisher(o.toString());
				continue;
			}

			/**
			 * ? -> Rights
			 * 
			 * Unmapped
			 * 
			 * dc:rights Lang Alt External Informal rights statement, selected
			 * by language.
			 */

			/**
			 * ? -> Source
			 * 
			 * Unmapped
			 * 
			 * dc:source Text External Unique identifier of the work from which
			 * this resource was derived.
			 */

			/**
			 * Keywords -> Subject
			 * 
			 * Field: dc:subject
			 * 
			 * Type: bag Text
			 * 
			 * Category: External
			 * 
			 * Description: An unordered array of descriptive phrases or
			 * keywords that specify the topic of the content of the resource.
			 * 
			 * Bibtex-Fields used: doi
			 */
			if (fields[j].equals("keywords")) {
				String o = entry.getField(fields[j].toString()).toString();
				String[] keywords = o.toString().split(",");
				for (int i = 0; i < keywords.length; i++) {
					dcSchema.addSubject(keywords[i].trim());
				}
				continue;
			}

			/**
			 * Title -> Title
			 * 
			 * Field: dc:title
			 * 
			 * Type: Lang Alt
			 * 
			 * Category: External
			 * 
			 * Description: The title of the document, or the name given to the
			 * resource. Typically, it will be a name by which the resource is
			 * formally known.
			 * 
			 * Bibtex-Fields used: title
			 */
			if (fields[j].equals("title")) {
				String o = entry.getField(fields[j].toString()).toString();
				dcSchema.setTitle(o.toString());
				continue;
			}

			/**
			 * bibtextype -> relation
			 * 
			 * Field: dc:relation
			 * 
			 * Type: bag Text
			 * 
			 * Category: External
			 * 
			 * Description: Relationships to other documents.
			 * 
			 * Bibtex-Fields used: bibtextype
			 */
			/**
			 * All others (including the bibtex key) get packaged in the
			 * relation attribute
			 */
			String o = entry.getField(fields[j].toString()).toString();
			dcSchema.addRelation("bibtex/" + fields[j].toString() + "/" + o);
		}

		/**
		 * ? -> Format
		 * 
		 * Unmapped
		 * 
		 * dc:format MIMEType Internal The file format used when saving the
		 * resource. Tools and applications should set this property to the save
		 * format of the data. It may include appropriate qualifiers.
		 */
		dcSchema.setFormat("application/pdf");

		/**
		 * Type -> Type
		 * 
		 * Field: dc:type
		 * 
		 * Type: bag open Choice
		 * 
		 * Category: External
		 * 
		 * Description: A document type; for example, novel, poem, or working
		 * paper.
		 * 
		 * Bibtex-Fields used: title
		 */
		Object o = entry.getType().getName();
		if (o != null)
			dcSchema.addType(o.toString());
	}
}