"""
A multichoice Idevice is one built up from question and options
"""
import logging
from exe.engine.persist   import Persistable
from exe.engine.idevice   import Idevice
from exe.engine.field     import ImageField
from exe.engine.translate import lateTranslate
from exe.engine.path      import toUnicode
from exe                  import globals as G
from exe.engine.field     import TextAreaField
import os
import re
log = logging.getLogger(__name__)
DEFAULT_IMAGE = 'empty.gif'
class Question(Persistable):
    """
    A Case iDevice is built up of question and options.  Each option can 
    be rendered as an XHTML element
    """
    persistenceVersion = 1
    def __init__(self, idevice):
        """
        Initialize 
        """
        self.questionTextArea = TextAreaField(u'', u'', u'')
        self.questionTextArea.idevice = idevice 
        self.feedbackTextArea = TextAreaField(u'', u'', u'')
        self.feedbackTextArea.idevice = idevice
    def setupImage(self, idevice):
        """
        Creates our image field: no longer needed for new content since 
        images are now embedded straight into the feedbackTextArea,
        but this routine is kept around for upgrade paths from old elps.
        """
        self.image = ImageField(x_(u"Image"),
                                x_(u"Choose an optional image to be shown to the student "
                                    "on completion of this question")) 
        self.image.idevice = idevice
        self.image.defaultImage = idevice.defaultImage
        self.image.isFeedback   = True
    def getResourcesField(self, this_resource):
        """
        implement the specific resource finding mechanism for this iDevice's
        Question object:
        """
        if hasattr(self, 'questionTextArea')\
        and hasattr(self.questionTextArea, 'images'):
            for this_image in self.questionTextArea.images:
                if hasattr(this_image, '_imageResource') \
                and this_resource == this_image._imageResource:
                    return self.questionTextArea
        if hasattr(self, 'feedbackTextArea')\
        and hasattr(self.feedbackTextArea, 'images'):
            for this_image in self.feedbackTextArea.images:
                if hasattr(this_image, '_imageResource') \
                and this_resource == this_image._imageResource:
                    return self.feedbackTextArea
        return None
    def getRichTextFields(self):
        """
        Like getResourcesField(), a general helper to allow nodes to search 
        through all of their fields without having to know the specifics of each
        iDevice type.  
        """
        fields_list = []
        if hasattr(self, 'questionTextArea'):
            fields_list.append(self.questionTextArea)
        if hasattr(self, 'feedbackTextArea'):
            fields_list.append(self.feedbackTextArea)
        return fields_list
    def upgradeToVersion1(self):
        """
        Upgrades to version 0.24
        """
        log.debug(u"Upgrading iDevice")
        self.image.isFeedback   = True
    def embedImageInFeedback(self):
        """
        Actually do the Converting of each question's
              CaseStudyIdevice's image -> embedded in its feedback field,
        now that its TextField can hold embeddded images.
        """
        new_content = ""
        if self.image is None or self.image.imageResource is None:
            return
        if not os.path.exists(self.image.imageResource.path) \
        or not os.path.isfile(self.image.imageResource.path):
            return
        if self.image.isDefaultImage:
            return
        new_content += "<img src=\"resources/" \
                + self.image.imageResource.storageName + "\" "
        if self.image.height: 
            new_content += "height=\"" + self.image.height + "\" "
        if self.image.width: 
            new_content += "width=\"" + self.image.width + "\" "
        new_content += "/> \n"
        new_content += "<BR>\n"
        new_content += self.feedbackTextArea.content_w_resourcePaths
        self.feedbackTextArea.content_w_resourcePaths = new_content
        self.feedbackTextArea.content = \
                self.feedbackTextArea.content_w_resourcePaths
        self.feedbackTextArea.content_wo_resourcePaths = \
                self.feedbackTextArea.MassageContentForRenderView( \
                   self.feedbackTextArea.content_w_resourcePaths)
        self.feedbackTextArea.setParentNode()
        from exe.engine.galleryidevice  import GalleryImage
        full_image_path = self.image.imageResource.path
        new_GalleryImage = GalleryImage(self.feedbackTextArea, \
                '',  full_image_path, mkThumbnail=False)
        self.image.setDefaultImage()
class CasestudyIdevice(Idevice):
    """
    A multichoice Idevice is one built up from question and options
    """
    persistenceVersion = 8
    def __init__(self, story="", defaultImage=None):
        """
        Initialize 
        """
        Idevice.__init__(self,
                         x_(u"Case Study"),
                         x_(u"University of Auckland"), 
                         x_(u"""A case study is a device that provides learners 
with a simulation that has an educational basis. It takes a situation, generally 
based in reality, and asks learners to demonstrate or describe what action they 
would take to complete a task or resolve a situation. The case study allows 
learners apply their own knowledge and experience to completing the tasks 
assigned. when designing a case study consider the following:<ul> 
<li>	What educational points are conveyed in the story</li>
<li>	What preparation will the learners need to do prior to working on the 
case study</li>
<li>	Where the case study fits into the rest of the course</li>
<li>	How the learners will interact with the materials and each other e.g.
if run in a classroom situation can teams be setup to work on different aspects
of the case and if so how are ideas feed back to the class</li></ul>"""), 
                         "",
                         u"casestudy")
        self.emphasis     = Idevice.SomeEmphasis
        self._storyInstruc = x_(u"""Create the case story. A good case is one 
that describes a controversy or sets the scene by describing the characters 
involved and the situation. It should also allow for some action to be taken 
in order to gain resolution of the situation.""")
        self.storyTextArea = TextAreaField(x_(u'Story:'), self._storyInstruc, story)
        self.storyTextArea.idevice = self
        self.questions    = []
        self._questionInstruc = x_(u"""Describe the activity tasks relevant 
to the case story provided. These could be in the form of questions or 
instructions for activity which may lead the learner to resolving a dilemma 
presented. """)
        self._feedbackInstruc = x_(u"""Provide relevant feedback on the 
situation.""")
        if defaultImage is None:
            defaultImage = G.application.config.webDir/'images'/DEFAULT_IMAGE
        self.defaultImage = toUnicode(defaultImage)
        self.addQuestion()
    storyInstruc    = lateTranslate('storyInstruc')
    questionInstruc = lateTranslate('questionInstruc')
    feedbackInstruc = lateTranslate('feedbackInstruc')
    storyInstruc    = lateTranslate('storyInstruc')
    questionInstruc = lateTranslate('questionInstruc')
    feedbackInstruc = lateTranslate('feedbackInstruc')
    def addQuestion(self):
        """
        Add a new question to this iDevice. 
        """
        self.questions.append(Question(self))
    def getResourcesField(self, this_resource):
        """
        implement the specific resource finding mechanism for this iDevice:
        """
        if hasattr(self, 'storyTextArea')\
        and hasattr(self.storyTextArea, 'images'):
            for this_image in self.storyTextArea.images:
                if hasattr(this_image, '_imageResource') \
                and this_resource == this_image._imageResource:
                    return self.storyTextArea
        for this_question in self.questions:
            this_field = this_question.getResourcesField(this_resource)
            if this_field is not None:
                return this_field
        return None
    def getRichTextFields(self):
        """
        Like getResourcesField(), a general helper to allow nodes to search 
        through all of their fields without having to know the specifics of each
        iDevice type.  
        """
        fields_list = []
        if hasattr(self, 'storyTextArea'):
            fields_list.append(self.storyTextArea)
        for this_question in self.questions:
            fields_list.extend(this_question.getRichTextFields())
        return fields_list
    def burstHTML(self, i):
        """
        takes a BeautifulSoup fragment (i) and bursts its contents to 
        import this idevice from a CommonCartridge export
        """
        title = i.find(name='span', attrs={'class' : 'iDeviceTitle' })
        self.title = title.renderContents().decode('utf-8')
        inner = i.find(name='div', attrs={'class' : 'iDevice_inner' })
        story = inner.find(name='div', 
                attrs={'class' : 'block' , 'id' : re.compile('^ta') })
        self.storyTextArea.content_wo_resourcePaths = \
                story.renderContents().decode('utf-8')
        self.storyTextArea.content_w_resourcePaths = \
                self.storyTextArea.MassageResourceDirsIntoContent( \
                    self.storyTextArea.content_wo_resourcePaths)
        self.storyTextArea.content = self.storyTextArea.content_w_resourcePaths
        case_questions = inner.findAll(name='div', attrs={'class' : 'question'})
        for question_num in range(len(case_questions)):
            if question_num > 0:
                self.addQuestion()
            question = case_questions[question_num]
            case_stories = question.findAll(name='div', 
                    attrs={'class' : 'block' , 
                        'id' : re.compile('^taquesQuestion') })
            if len(case_stories) == 1:
                inner_question = case_stories[0]
                self.questions[question_num].questionTextArea.content = \
                        inner_question.renderContents().decode('utf-8')
                self.questions[question_num].questionTextArea.content_w_resourcePaths \
                        = inner_question.renderContents().decode('utf-8')
                self.questions[question_num].questionTextArea.content_wo_resourcePaths \
                        = inner_question.renderContents().decode('utf-8')
                self.questions[question_num].questionTextArea.content_w_resourcePaths \
                        = self.questions[question_num].questionTextArea.MassageResourceDirsIntoContent( \
                            self.questions[question_num].questionTextArea.content_wo_resourcePaths)
                self.questions[question_num].questionTextArea.content = \
                        self.questions[question_num].questionTextArea.content_w_resourcePaths
            case_feedbacks = question.findAll(name='div', 
                    attrs={'class' : 'feedback' , 'id' : re.compile('^sq') })
            if len(case_feedbacks) == 1:
                inner_feedback = case_feedbacks[0]
                self.questions[question_num].feedbackTextArea.content_wo_resourcePaths \
                        = inner_feedback.renderContents().decode('utf-8')
                self.questions[question_num].feedbackTextArea.content_w_resourcePaths \
                        = self.questions[question_num].feedbackTextArea.MassageResourceDirsIntoContent( \
                            self.questions[question_num].feedbackTextArea.content_wo_resourcePaths)
                self.questions[question_num].feedbackTextArea.content = \
                        self.questions[question_num].feedbackTextArea.content_w_resourcePaths
            else:
                self.questions[question_num].feedbackTextArea.content = ""
                self.questions[question_num].feedbackTextArea.content_w_resourcePaths \
                        = ""
                self.questions[question_num].feedbackTextArea.content_wo_resourcePaths \
                        = ""
    def upgradeToVersion1(self):
        """
        Upgrades the node from version 0 to 1.
        Old packages will loose their icons, but they will load.
        """
        log.debug(u"Upgrading iDevice")
        self.icon = "casestudy"
    def upgradeToVersion2(self):
        """
        Upgrades the node from 1 (v0.5) to 2 (v0.6).
        Old packages will loose their icons, but they will load.
        """
        log.debug(u"Upgrading iDevice")
        self.emphasis = Idevice.SomeEmphasis
    def upgradeToVersion3(self):
        """
        Upgrades v0.6 to v0.7.
        """
        self.lastIdevice = False
    def upgradeToVersion4(self):
        """
        Upgrades to exe v0.10
        """
        self._upgradeIdeviceToVersion1()
        self._storyInstruc    = self.__dict__['storyInstruc']
        self._questionInstruc = self.__dict__['questionInstruc']
        self._feedbackInstruc = self.__dict__['feedbackInstruc']
    def upgradeToVersion5(self):
        """
        Upgrades to v0.12
        """
        self._upgradeIdeviceToVersion2()
    def upgradeToVersion6(self):
        """
        Upgrades for v0.18
        """
        self.defaultImage = toUnicode(G.application.config.webDir/'images'/DEFAULT_IMAGE)
        for question in self.questions:
            question.setupImage(self)
    def upgradeToVersion7(self):
        """
        Upgrades to somewhere before version 0.25 (post-v0.24)
        Taking the old unicode string fields, 
        and converting them into a image-enabled TextAreaFields:
        """
        self.storyTextArea = TextAreaField(x_(u'Story:'), 
                                 self._storyInstruc, self.story)
        self.storyTextArea.idevice = self
        for question in self.questions:
            question.questionTextArea = TextAreaField(u'', 
                                            u'', question.question)
            question.questionTextArea.idevice = self
            question.feedbackTextArea = TextAreaField(u'', 
                                            u'', question.feedback)
            question.feedbackTextArea.idevice = self
    def upgradeToVersion8(self):
        """
        Converting CaseStudyIdevice's image -> embedded image in its feedback
        field, a TextField than can now hold embedded images.
        BUT - due to the inconsistent loading of the objects via unpickling,
        since the resources aren't necessarily properly loaded and upgraded,
        NOR is the package necessarily, as it might not even have a list of
        resources yet, all of this conversion code must be done in an
        afterUpgradeHandler  
        (as perhaps should have been done for the previous upgradeToVersion7)
        """
        G.application.afterUpgradeHandlers.append(self.embedImagesInFeedback)
    def embedImagesInFeedback(self):
        """
        Loop through each question, to call their conversion:
              CaseStudyIdevice's image -> embedded in its feedback field,
        now that its TextField can hold embeddded images.
        """
        for question in self.questions:
            question.embedImageInFeedback()
