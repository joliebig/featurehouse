class Foo {
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
}