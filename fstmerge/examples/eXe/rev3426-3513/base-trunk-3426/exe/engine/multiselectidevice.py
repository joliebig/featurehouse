"""
A Multiple Select Idevice is one built up from Questions
"""
import logging
from exe.engine.persist   import Persistable
from exe.engine.idevice   import Idevice
from exe.engine.translate import lateTranslate
from exe.engine.field     import SelectQuestionField
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
