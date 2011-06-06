"""
A multichoice Idevice is one built up from question and options
"""
import logging
from twisted.spread       import jelly
from exe.engine.idevice   import Idevice
from exe.engine.field     import QuizQuestionField, QuizOptionField
from exe.engine.translate import lateTranslate
log = logging.getLogger(__name__)
class Option(jelly.Jellyable):
    """
    A Multichoice iDevice is built up of question and options.  Each option can
    be rendered as an XHTML element
    """
    def __init__(self, answer="", isCorrect=False, feedback=""):
        """
        Initialize 
        """
        self.answer    = answer
        self.isCorrect = isCorrect
        self.feedback  = feedback
class MultichoiceIdevice(Idevice):
    """
    A multichoice Idevice is one built up from question and options
    """
    persistenceVersion = 7
    def __init__(self, question=""):
        """
        Initialize 
        """
        Idevice.__init__(self,
                         x_(u"Multi-choice"),
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
        self.emphasis         = Idevice.SomeEmphasis
        self.questions        = []
        self.options          = []
        self.question         = ""
        self.hint             = ""
        self._hintInstruc     = x_(u"""Enter a hint here. If you
do not want to provide a hint, leave this field blank.""")
        self._questionInstruc      = x_(u"""Enter the question stem. 
The quest should be clear and unambiguous. Avoid negative premises 
as these can tend to be ambiguous.""")
        self._keyInstruc      = x_(u"""Select the correct option by clicking 
on the radio button.""")
        self._answerInstruc   = x_(u"""Enter the available choices here. 
You can add options by clicking the "Add Another Option" button. Delete options 
by clicking the red "X" next to the Option.""")
        self._feedbackInstruc = x_(u"""Type in the feedback that you want the 
student to see when selecting the particular option. If you don't complete this 
box, eXe will automatically provide default feedback as follows: "Correct 
answer" as indicated by the selection for the correct answer; or "Wrong answer"
for the other options.""")
        self.systemResources += ["common.js", "libot_drag.js",
                                 "panel-amusements.png", "stock-stop.png"]
        self.message          = ""
        self.addQuestion()
    hintInstruc     = lateTranslate('hintInstruc')
    questionInstruc = lateTranslate('questionInstruc')
    keyInstruc      = lateTranslate('keyInstruc')
    answerInstruc   = lateTranslate('answerInstruc')
    feedbackInstruc = lateTranslate('feedbackInstruc')
    def addQuestion(self):
        """
        Add a new question to this iDevice. 
        """
        question = QuizQuestionField(self, x_(u'Question'))
        question.addOption()
        self.questions.append(question)
    def upgradeToVersion1(self):
        """
        Called to upgrade from 0.4 release
        """
        self.hint  = ""
        self.icon  = "multichoice"
        self.__dict__['hintInstruc'] = \
                     x_(u"Enter a hint here. If you do not want to provide a "
                        u"hint, leave this field blank.")
    def upgradeToVersion2(self):
        """
        Upgrades the node from 1 (v0.5) to 2 (v0.6).
        Old packages will loose their icons, but they will load.
        """
        log.debug(u"Upgrading iDevice")
        self.emphasis = Idevice.SomeEmphasis
    def upgradeToVersion3(self):
        """
        Upgrades the node from 1 (v0.6) to 2 (v0.7).
        Change icon from 'multichoice' to 'question'
        """
        log.debug(u"Upgrading iDevice icon")
        self.icon = "question"
    def upgradeToVersion4(self):
        """
        Upgrades v0.6 to v0.7.
        """
        self.lastIdevice = False
    def upgradeToVersion5(self):
        """
        Upgrades to exe v0.10
        """
        self._upgradeIdeviceToVersion1()
        self._hintInstruc     = self.__dict__['hintInstruc']
        self._questionInstruc = self.__dict__['questionInstruc']
        self._keyInstruc      = self.__dict__['keyInstruc']
        self._answerInstruc   = self.__dict__['answerInstruc']
        self._feedbackInstruc = self.__dict__['feedbackInstruc']
    def upgradeToVersion6(self):
        """
        Upgrades to v0.12
        """
        self._upgradeIdeviceToVersion2()
        self.systemResources += ["common.js", "libot_drag.js",
                                 "panel-amusements.png", "stock-stop.png"]
    def upgradeToVersion7(self):
        """
        Upgrades to v0.19
        """
        self.questions = []
        length = len(self.options)
        if length >0:
            self.addQuestion()
            self.questions[0].hint = self.hint
            self.questions[0].question = self.question
            for i in range(1, length):
                self.questions[0].addOption()
                i += 1
            for i in range(0, length):
                self.questions[0].options[i].answer    = self.options[i].answer
                self.questions[0].options[i].feedback  = self.options[i].feedback
                self.questions[0].options[i].isCorrect = self.options[i].isCorrect
                self.questions[0].options[i].question  = self.questions[0]
                self.questions[0].options[i].idevice   = self
                i += 1
            self.question = ""
            self.options  = []
            self.hint     = ""
