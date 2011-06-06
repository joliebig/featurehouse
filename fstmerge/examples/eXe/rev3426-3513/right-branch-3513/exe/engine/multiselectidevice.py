"""
A Multiple Select Idevice is one built up from Questions
"""
import logging
from exe.engine.persist   import Persistable
from exe.engine.idevice   import Idevice
from exe.engine.translate import lateTranslate
from exe.engine.field     import SelectQuestionField
import re
log = logging.getLogger(__name__)
class MultiSelectIdevice(Idevice):
    """
    A MultiSelect Idevice is one built up from question and options
    """
    def __init__(self):
        """
        Initialize 
        """
        Idevice.__init__(self,
                         x_(u"Multi-select"),
                         x_(u"University of Auckland"),
                         x_(u"""Although more often used in formal testing 
situations MCQs can be used as a testing tool to stimulate thought and  
discussion on topics students may feel a little reticent in responding to. 
When designing a MCQ test consider the following:
<ul>
<li> What learning outcomes are the questions testing</li>
<li>    What intellectual skills are being tested</li>
<li> What are the language skills of the audience</li>
<li> Gender and cultural issues</li>
<li> Avoid grammar language and question structures that might provide 
     clues</li>
</ul>
 """), x_(u"""When building an MCQ consider the following: <ul>
<li> Use phrases that learners are familiar with and have 
encountered in their study </li>
<li> Keep responses concise </li>
<li> There should be some consistency between the stem and the responses </li>
<li> Provide enough options to challenge learners to think about their response
</li>
<li> Try to make sure that correct responses are not more detailed than the 
distractors </li>
<li> Distractors should be incorrect but plausible </li>
</ul>
"""), u"question")
        self.emphasis   = Idevice.SomeEmphasis
        self.questions  = []
        self.addQuestion()
        self.systemResources += ["common.js"]
    def addQuestion(self):
        """
        Add a new question to this iDevice. 
        """
        question = SelectQuestionField(self, x_(u'Question'))
        question.addOption()
        self.questions.append(question)
    def getResourcesField(self, this_resource):
        """
        implement the specific resource finding mechanism for this iDevice:
        """
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
        ms_questions = inner.findAll(name='div', attrs={'class' : 'question'})
        if len(ms_questions) < 1:
            del self.questions[0]
        for question_num in range(len(ms_questions)):
            if question_num > 0:
                self.addQuestion()
            question = ms_questions[question_num]
            questions = question.findAll(name='div', attrs={'class' : 'block' , 'id' : re.compile('^taquestion') })
            if len(questions) == 1:
                inner_question = questions[0]
                self.questions[question_num].questionTextArea.content_wo_resourcePaths \
                        = inner_question.renderContents().decode('utf-8')
                self.questions[question_num].questionTextArea.content_w_resourcePaths \
                        = self.questions[question_num].questionTextArea.MassageResourceDirsIntoContent( \
                            self.questions[question_num].questionTextArea.content_wo_resourcePaths)
                self.questions[question_num].questionTextArea.content \
                        = self.questions[question_num].questionTextArea.content_w_resourcePaths
            options = question.findAll(name='div', attrs={'class' : 'block' , 
                    'id' : re.compile('^taans') })
            answers = question.findAll(name='input', 
                    attrs={'type' : 'checkbox'})
            feedbacks = question.findAll(name='div', 
                    attrs={'id' : re.compile('^tafeedback') })
            if len(options) < 1:
                del self.questions[question_num].options[0]
            for option_loop in range(0, len(options)):
                if option_loop >= 1:
                    self.questions[question_num].addOption()
                self.questions[question_num].options[option_loop].answerTextArea.content_wo_resourcePaths \
                        = options[option_loop].renderContents().decode('utf-8')
                self.questions[question_num].options[option_loop].answerTextArea.content_w_resourcePaths \
                        = self.questions[question_num].options[option_loop].answerTextArea.MassageResourceDirsIntoContent( \
                            self.questions[question_num].options[option_loop].answerTextArea.content_wo_resourcePaths)
                self.questions[question_num].options[option_loop].answerTextArea.content \
                        = self.questions[question_num].options[option_loop].answerTextArea.content_w_resourcePaths
                this_answer = answers[option_loop].attrMap['value']
                if this_answer == "True":
                    self.questions[question_num].options[option_loop].isCorrect\
                            = True
            if len(feedbacks) >= 1:
                inner_feedback = feedbacks[0]
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
