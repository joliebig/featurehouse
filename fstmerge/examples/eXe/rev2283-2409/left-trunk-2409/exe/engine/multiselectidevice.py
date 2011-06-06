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
                         x_(u"""Unlike the MCQ the SCORM quiz is used to test 
the learners knowledge on a topic without providing the learner with feedback 
to the correct answer. The quiz will often be given once the learner has had 
time to learn and practice using the information or skill.
 """), u"", "question")
        self.emphasis   = Idevice.SomeEmphasis
        self.questions  = []
        self.addQuestion()
        self.systemResources += ["common.js"]
    def addQuestion(self):
        """
        Add a new question to this iDevice. 
        """
        question = SelectQuestionField(x_(u'Question'))
        question.idevice = self
        question.addOption()
        self.questions.append(question)
