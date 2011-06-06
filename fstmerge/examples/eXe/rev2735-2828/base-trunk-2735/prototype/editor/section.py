from coursemanager import CourseManager
from xmlreader import readConfig
from os.path import exists
from  shutil import rmtree, copyfile
from os import sep
class Section(CourseManager):
	def section_read_form( self, form ):
		self.read_section_dict()
		for item in self.section_dict:
			if item[-7:]<>"graphic" and item[-5:]<>"_file":
				try:
					self.section_dict[item]= form[item].value
				except:
					pass
			
	def edit_section( self, form ):
		self.get_course_detail( form["courseidentifier"].value)
		if form.has_key("topicidentifier"):
			self.get_topic_detail( form["topicidentifier"].value )
		else:
			print "Error, can't get topic detail of this section"
			return
		
		if form.has_key( "sectionidentifier" ):
			self.get_section_detail( form["sectionidentifier"].value )
			heading = ""
			content = self.xml_string( self.sectionForm, self.section_dict )
			crumb = "<H3><a href=%s?cmd=view_course&courseidentifier=%s>%s</a> -> <a href=%s?cmd=view_topic&courseidentifier=%s&topicidentifier=%s>%s</a> -> Edit %s</H3>\n"\
				 % ( self.startcgi, self.dict["courseidentifier"], self.dict["title"],\
					 self.startcgi, self.topic_dict["courseidentifier"], self.topic_dict["topicidentifier"], self.topic_dict["title"],\
						  self.section_dict["title"] )
			preview = self.previewcgi + "?courseidentifier=%s" % self.dict["courseidentifier"]
			outline = self.startcgi + "?cmd=outline_course&courseidentifier=%s" % self.dict["courseidentifier"]
			self.showexe( self.theme_template, heading, content, crumb, preview, outline )
		else:
			self.section_read_form( form )
			heading = ""
			content = self.xml_string( self.sectionForm, self.section_dict )
			crumb = "<H3><a href=%s?cmd=view_course&courseidentifier=%s>%s</a> ->\
			 <a href=%s?cmd=view_topic&courseidentifier=%s&topicidentifier=%s>%s</a>\
			  -> Add a new section</H3>\n"\
				 % ( self.startcgi, self.dict["courseidentifier"], self.dict["title"],\
					 self.startcgi, self.topic_dict["courseidentifier"], self.topic_dict["topicidentifier"], self.topic_dict["title"] )
			preview = self.previewcgi + "?courseidentifier=%s" % self.dict["courseidentifier"]
			outline = self.startcgi + "?cmd=outline_course&courseidentifier=%s" % self.dict["courseidentifier"]
			self.showexe( self.theme_template, heading, content, crumb, preview, outline )
	
		
				
	def save_new_section(self, form):
		self.get_course_detail( form["courseidentifier"].value)
		self.section_read_form( form )
		
		if self.section_dict["topicidentifier"]:
			self.get_topic_detail( self.section_dict["topicidentifier"] )
		else:
			print "Error, can't get topic detail of this section"
			return
			
		self.sectionxmlfilepath = self.doc_root + self.section_dict["courseidentifier"] + "/" + self.section_dict["topicidentifier"] + '/' + self.sectionxmlfile
		maxidentifier = self.max_identifier( self.sectionxmlfilepath, "sections", "sectionidentifier" ) + 1
		
		
		section_directory = self.doc_root + self.section_dict["courseidentifier"] + "/" + self.section_dict["topicidentifier"] + '/' + str( maxidentifier )
		while exists( section_directory ):
			maxidentifier = maxidentifier + 1
			section_directory = self.doc_root + self.section_dict["courseidentifier"] + "/" + self.section_dict["topicidentifier"] + '/' + str( maxidentifier )
		
		self.section_dict["sectionidentifier"] = str( maxidentifier )
		
		image_dir = section_directory + "/images/"
		file_dir  = section_directory + "/files/"
		if self.create_dir( section_directory ):
			self.section_dict["graphic"] = self.process_graphic( form, image_dir, "graphic", "graphic" )
			for item in self.section_dict:
				if item[-5:]=="_file":
					print "item:%s <p>\n" % item
					self.section_dict[item] = self.process_file( form, file_dir, item, item )
			
			self.save_section_file(action="add")
		else:
			print "Error while creating section directory"
			return
		self.view_topic( form )
	def save_section_file( self, action ):
	
		self.sectionxmlfilepath = self.doc_root + self.section_dict["courseidentifier"] + "/" + self.section_dict["topicidentifier"] + "/" + self.sectionxmlfile
		self.savexmlfile( action, "sections", self.section_dict, "sectionidentifier", self.sectionxmlfilepath, self.section_xml_template  )
				
	def update_section(self, form):
		self.get_course_detail( form["courseidentifier"].value)
		self.read_section_dict()
		self.section_read_form( form )
		
		if self.section_dict["topicidentifier"]:
			self.get_topic_detail( self.section_dict["topicidentifier"] )
		else:
			print "Error, can't get topic detail of this section"
			return
			
		target_dir = self.doc_root  + self.section_dict["courseidentifier"] + sep + self.section_dict["topicidentifier"] + sep + self.section_dict["sectionidentifier"]+ sep + "images" + sep
		file_dir   = self.doc_root  + self.section_dict["courseidentifier"] + sep + self.section_dict["topicidentifier"] + sep + self.section_dict["sectionidentifier"] + sep + "files" + sep
		if form.has_key("new_graphic"):
			self.section_dict["graphic"] = self.process_graphic( form, target_dir, "graphic", "new_graphic" )
		else:
			self.section_dict["graphic"] = self.process_graphic( form, target_dir, "graphic", "graphic" )	
		for item in self.topic_dict:
			if item[-5:]=="_file":
				if form.has_key( "new_%s" %item ):
					self.section_dict[item] = self.process_file( form, file_dir, item, "new_%s" % item )
				else:
					self.section_dict[item] = self.process_file( form, file_dir, item, item )
		self.save_section_file("update")
		self.view_section( form )
	def up_section( self, form ):
		"""move up the topic info from topics.xml
		"""
		self.read_section_dict()
		self.section_read_form(form)
		
		self.get_course_detail( self.section_dict["courseidentifier"] )
		self.get_topic_detail( self.section_dict["topicidentifier"] )	
		self.save_section_file( action="up" )
		self.view_topic( form )
	def down_section( self, form ):
		"""move down the topic info from topics.xml
		"""
		self.read_section_dict()
		self.section_read_form(form)
		
		self.get_course_detail( self.section_dict["courseidentifier"] )
		self.get_topic_detail( self.section_dict["topicidentifier"] )	
		self.save_section_file( action="down" )
		self.view_topic( form )
	
	def delete_section( self, form ):
		"""delete the course info from courses.xml and delete the course identifier
		"""
		self.read_section_dict()
		self.section_read_form(form)
		
		self.get_course_detail( self.section_dict["courseidentifier"] )
		self.get_topic_detail( self.section_dict["topicidentifier"] )	
		if self.section_dict["courseidentifier"] and self.section_dict["topicidentifier"] and self.section_dict["sectionidentifier"]:
				rm_identifier = self.doc_root + self.section_dict["courseidentifier"] + "/" + self.section_dict["topicidentifier"]+ "/" + self.section_dict["sectionidentifier"]
				rmtree( rm_identifier)
		self.save_section_file( action="delete" )
		self.view_topic( form )
