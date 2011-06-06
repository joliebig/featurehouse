"""
TestQuestionElement is responsible for a block of question.  
Used by QuizTestBlock for SCORM Quiz
"""
import logging
from exe.webui.testoptionelement   import TestoptionElement
from exe.webui                     import common
from exe.webui.element   import TextAreaElement
log = logging.getLogger(__name__)
class TestquestionElement(object):
    """
    TestQuestionElement is responsible for a block of question.  
    Used by QuizTestBlock
    == SCORM Quiz Testquestion, 
    pretty much the same as MultiSelect's SelectquestionElement
    """
    def __init__(self, index, idevice, question):
        """
        Initialize
        """
        self.index      = index
        self.id         = unicode(index) + "b" + idevice.id        
        self.idevice    = idevice
        if question.questionTextArea.idevice is None: 
            question.questionTextArea.idevice = idevice
        self.questionElement = TextAreaElement(question.questionTextArea)
        self.question   = question
        self.questionId = "question"+self.id
        self.questionElement.id = self.questionId
        self.options    = []
        self.keyId      = "key" + self.id
        i = 0
        for option in question.options:
            self.options.append(TestoptionElement(i,
                                                  question, 
                                                  self.id, 
                                                  option,
                                                  idevice))
            i += 1
    def process(self, request):
        """
        Process the request arguments from the web server
        """
        log.info("process " + repr(request.args))
        if self.questionId in request.args: 
            self.questionElement.process(request)
        if ("addOption"+unicode(self.id)) in request.args: 
            self.question.addOption()
            self.idevice.edit = True
        if "action" in request.args and request.args["action"][0] == self.id:
            for q_field in self.question.getRichTextFields():
                 q_field.ReplaceAllInternalAnchorsLinks()  
                 q_field.RemoveAllInternalLinks()  
            self.idevice.questions.remove(self.question)
        for element in self.options:
            element.process(request)
    def renderEdit(self):
        """
        Returns an XHTML string with the form element for editing this element
        """
        html  = u"<div class=\"iDevice\">\n"
        html += common.submitImage(self.id, self.idevice.id,  
                "/images/stock-cancel.png", 
                _("Delete question")) 
        html += self.questionElement.renderEdit()
        html += u"<table width =\"100%%\">"
        html += u"<thead>"
        html += u"<tr>"
        html += u"<th>%s " % _("Options")
        html += common.elementInstruc(self.question.optionInstruc)
        html += u"</th>"
        html += u"</tr>"
        html += u"</thead>"
        html += u"<tbody>"
        for element in self.options:
            html += element.renderEdit() 
        html += u"</tbody>"
        html += u"</table>\n"
        value = _(u"Add another Option")    
        html += common.submitButton("addOption"+unicode(self.id), value)
        html += u"<br />"
        html += u"</div>\n"
        return html
    def renderPreview(self):
        """
        Returns an XHTML string for previewing this element
        """
        return self.renderView(preview=True)
    def renderView(self, preview=False):
        """
        Returns an XHTML string for viewing this element
        """
        html  = u""
        html += "<div class=\"question\">\n"
        if preview: 
            html += self.questionElement.renderPreview()
        else:
            html += self.questionElement.renderView()
        html += "<br/>\n"
        html += "<table>"
        for element in self.options:
            if preview: 
                html += element.renderPreview()      
            else:
                html += element.renderView()      
        html += "</table>"   
        html += "</div>\n"
        return html
    def getCorrectAns(self):
        """
        return the correct answer for the question
        """
        return self.question.correctAns
    def getNumOption(self):
        """
        return the number of options
        """
        return len(self.question.options)
