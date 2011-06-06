

package com.lowagie.text.rtf.direct;

import java.io.IOException;
import java.io.Reader;


public class RtfTokeniser {
    
    private static final int TOKENISER_STATE_READY = 0;
    
    private static final int TOKENISER_STATE_SLASH = 1;
    
    private static final int TOKENISER_STATE_IN_CTRL_WORD = 2;
    
    private static final int TOKENISER_STATE_IN_TEXT = 4;
    
    
    private int state = TOKENISER_STATE_READY;
    
    private int groupLevel = 0;
    
    private RtfParser rtfParser = null;

    
    public RtfTokeniser(RtfParser rtfParser, int startGroupLevel) {
        this.rtfParser = rtfParser;
        this.groupLevel = startGroupLevel;
    }
    
    
    public void tokenise(Reader reader) throws IOException {
        char[] nextChar = new char[1];
        StringBuffer temp = new StringBuffer();
        this.state = TOKENISER_STATE_READY;
        this.groupLevel = 0;
        while(reader.read(nextChar) != -1) {
            if(this.state == TOKENISER_STATE_READY) { 
                if(nextChar[0] == '{') { 
                    this.rtfParser.handleOpenGroup(this.groupLevel);
                    groupLevel++;
                } else if(nextChar[0] == '}') { 
                    this.rtfParser.handleCloseGroup(this.groupLevel);
                    groupLevel--;
                } else if(nextChar[0] == '\\') {
                    this.state = TOKENISER_STATE_SLASH;
                    temp = new StringBuffer();
                } else {
                    this.state = TOKENISER_STATE_IN_TEXT;
                    temp.append(nextChar[0]);
                }
            } else if((this.state & TOKENISER_STATE_SLASH) == TOKENISER_STATE_SLASH) { 
                if(nextChar[0] == '{') {
                    this.state = TOKENISER_STATE_IN_TEXT;
                    temp.append("\\{");
                } else if(nextChar[0] == '}') {
                    this.state = TOKENISER_STATE_IN_TEXT;
                    temp.append("\\}");
                } else if(nextChar[0] == '\\') {
                    this.state = TOKENISER_STATE_IN_TEXT;
                    temp.append("\\\\");
                } else {
                    if((this.state & TOKENISER_STATE_IN_TEXT) == TOKENISER_STATE_IN_TEXT) { 
                        this.rtfParser.handleText(temp.toString(), this.groupLevel);
                        temp = new StringBuffer();
                    }
                    if(nextChar[0] == '|') {
                        this.state = TOKENISER_STATE_READY;
                        this.rtfParser.handleCtrlCharacter("\\|", this.groupLevel);
                    } else if(nextChar[0] == '~') {
                        this.state = TOKENISER_STATE_READY;
                        this.rtfParser.handleCtrlCharacter("\\~", this.groupLevel);
                    } else if(nextChar[0] == '-') {
                        this.state = TOKENISER_STATE_READY;
                        this.rtfParser.handleCtrlCharacter("\\-", this.groupLevel);
                    } else if(nextChar[0] == '_') {
                        this.state = TOKENISER_STATE_READY;
                        this.rtfParser.handleCtrlCharacter("\\_", this.groupLevel);
                    } else if(nextChar[0] == ':') {
                        this.state = TOKENISER_STATE_READY;
                        this.rtfParser.handleCtrlCharacter("\\:", this.groupLevel);
                    } else if(nextChar[0] == '*') {
                        this.state = TOKENISER_STATE_READY;
                        this.rtfParser.handleCtrlCharacter("\\*", this.groupLevel);
                    } else {
                        this.state = TOKENISER_STATE_IN_CTRL_WORD;
                        temp = new StringBuffer("\\");
                        temp.append(nextChar[0]);
                    }
                }
            } else if(this.state == TOKENISER_STATE_IN_CTRL_WORD) { 
                if(nextChar[0] == '\n' || nextChar[0] == '\r') {
                    nextChar[0] = ' ';
                }
                if(nextChar[0] == '{') {
                    this.rtfParser.handleCtrlWord(temp.toString(), this.groupLevel);
                    this.rtfParser.handleOpenGroup(this.groupLevel);
                    groupLevel++;
                    this.state = TOKENISER_STATE_READY;
                    temp = new StringBuffer();
                } else if(nextChar[0] == '}') {
                    this.rtfParser.handleCtrlWord(temp.toString(), this.groupLevel);
                    this.rtfParser.handleCloseGroup(this.groupLevel);
                    groupLevel--;
                    this.state = TOKENISER_STATE_READY;
                    temp = new StringBuffer();
                } else if(nextChar[0] == '\\') {
                    this.rtfParser.handleCtrlWord(temp.toString(), this.groupLevel);
                    this.state = TOKENISER_STATE_SLASH;
                    temp = new StringBuffer();
                } else if(nextChar[0] == ' ') {
                    this.rtfParser.handleCtrlWord(temp.toString(), this.groupLevel);
                    this.rtfParser.handleText(" ", this.groupLevel);
                    this.state = TOKENISER_STATE_READY;
                    temp = new StringBuffer();
                } else if(nextChar[0] == ';') {
                    this.rtfParser.handleCtrlWord(temp.toString(), this.groupLevel);
                    this.rtfParser.handleText(";", this.groupLevel);
                    this.state = TOKENISER_STATE_READY;
                    temp = new StringBuffer();
                } else {
                    temp.append(nextChar[0]);
                }
            } else if(this.state == TOKENISER_STATE_IN_TEXT) { 
                if(nextChar[0] == '{') {
                    this.rtfParser.handleText(temp.toString(), this.groupLevel);
                    this.rtfParser.handleOpenGroup(this.groupLevel);
                    groupLevel++;
                    this.state = TOKENISER_STATE_READY;
                    temp = new StringBuffer();
                } else if(nextChar[0] == '}') {
                    this.rtfParser.handleText(temp.toString(), this.groupLevel);
                    this.rtfParser.handleCloseGroup(this.groupLevel);
                    groupLevel--;
                    this.state = TOKENISER_STATE_READY;
                    temp = new StringBuffer();
                } else if(nextChar[0] == '\\') {
                    this.state = TOKENISER_STATE_IN_TEXT | TOKENISER_STATE_SLASH;
                } else {
                    temp.append(nextChar[0]);
                }
            }
        }
        if((this.state & TOKENISER_STATE_IN_TEXT) == TOKENISER_STATE_IN_TEXT && !temp.toString().equals("")) { 
            this.rtfParser.handleText(temp.toString(), this.groupLevel);
        }
    }
}
