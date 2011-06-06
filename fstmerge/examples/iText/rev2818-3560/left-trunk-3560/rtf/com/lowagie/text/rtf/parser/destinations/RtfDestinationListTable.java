

package com.lowagie.text.rtf.parser.destinations;

import com.lowagie.text.Element;
import com.lowagie.text.rtf.list.RtfList;
import com.lowagie.text.rtf.list.RtfListLevel;
import com.lowagie.text.rtf.parser.RtfImportMgr;
import com.lowagie.text.rtf.parser.RtfParser;
import com.lowagie.text.rtf.parser.ctrlwords.RtfCtrlWordData;


public class RtfDestinationListTable extends RtfDestination {
    
    private RtfImportMgr importHeader = null;

    private RtfList newList = null;
    
    private int currentLevel = -1;
    private RtfListLevel currentListLevel = null;
    private int currentListMappingNumber = 0;
    private int currentSubGroupCount = 0;
    
    public RtfDestinationListTable() {
        super(null);
    }

    public RtfDestinationListTable(RtfParser parser) {
        super(parser);
        this.importHeader = parser.getImportManager();
    }

    public void setParser(RtfParser parser) {
        this.rtfParser = parser;
        this.importHeader = parser.getImportManager();
        this.setToDefaults();
    }

    
    public boolean handleOpeningSubGroup() {
        this.currentSubGroupCount++;
        return true;
    }

    
    public boolean closeDestination() {










        
        if (this.newList != null) {
            this.rtfParser.getRtfDocument().add(this.newList);
        }
        return true;
    }

    public boolean handleControlWord(RtfCtrlWordData ctrlWordData) {
        boolean result = true;
        boolean skipCtrlWord = false;

        if (this.rtfParser.isImport()) {
            skipCtrlWord = true;
            if (ctrlWordData.ctrlWord.equals("listtable")) {
                result = true;
                this.currentListMappingNumber = 0;
                
            } else
                
                if (ctrlWordData.ctrlWord.equals("listpicture")){
                    skipCtrlWord = true;
                    
                    result = true;
                } else
                    
                    if (ctrlWordData.ctrlWord.equals("list")) {
                        skipCtrlWord = true;
                        this.newList = new RtfList(this.rtfParser.getRtfDocument());
                        this.newList.setListType(RtfList.LIST_TYPE_NORMAL);    
                        this.currentLevel = -1;
                        this.currentListMappingNumber++;
                        this.currentSubGroupCount = 0;
                        result = true;
                    } else if (ctrlWordData.ctrlWord.equals("listtemplateid"))  {
                        
                        skipCtrlWord = true;
                        result = true;
                    } else if (ctrlWordData.ctrlWord.equals("listsimple"))  {
                        
                        if(ctrlWordData.hasParam && ctrlWordData.param == "1") {
                            this.newList.setListType(RtfList.LIST_TYPE_SIMPLE);
                        } else
                        {
                            this.newList.setListType(RtfList.LIST_TYPE_NORMAL);
                        }
                        skipCtrlWord = true;
                        result = true;
                        
                    } else if (ctrlWordData.ctrlWord.equals("listhybrid"))  {
                        this.newList.setListType(RtfList.LIST_TYPE_HYBRID);
                        skipCtrlWord = true;
                        result = true;
                        
                    } else if (ctrlWordData.ctrlWord.equals("listrestarthdn"))  {
                        skipCtrlWord = true;
                        result = true;
                    } else if (ctrlWordData.ctrlWord.equals("listid")) {    
                        
                        
                        skipCtrlWord = true;
                        result = true;
                    } else if (ctrlWordData.ctrlWord.equals("listname")) {
                        this.newList.setName(ctrlWordData.param);
                        skipCtrlWord = true;
                        result = true;
                    } else if (ctrlWordData.ctrlWord.equals("liststyleid")) {
                        skipCtrlWord = true;
                        result = true;
                    } else if (ctrlWordData.ctrlWord.equals("liststylename")) {
                        skipCtrlWord = true;
                        result = true;
                    } else
                        
                        if (ctrlWordData.ctrlWord.equals("listlevel")) {
                            this.currentLevel++;
                            this.currentListLevel = (RtfListLevel)this.newList.getListLevel(this.currentLevel);
                            this.currentListLevel.setTentative(false);
                            skipCtrlWord = true;
                            result = true;
                        } else if (ctrlWordData.ctrlWord.equals("leveljc")) { 
                            
                            if( this.currentListLevel.getAlignment()== RtfListLevel.LIST_TYPE_UNKNOWN) {
                                switch(ctrlWordData.intValue()) {
                                case 0:
                                    this.currentListLevel.setAlignment(Element.ALIGN_LEFT);
                                    break;
                                case 1:
                                    this.currentListLevel.setAlignment(Element.ALIGN_CENTER);
                                    break;
                                case 2:
                                    this.currentListLevel.setAlignment(Element.ALIGN_RIGHT);
                                    break;
                                }
                            }
                            skipCtrlWord = true;
                            result = true;
                        } else if (ctrlWordData.ctrlWord.equals("leveljcn")) { 
                            
                            
                            switch(ctrlWordData.intValue()) {
                            case 0:
                                this.currentListLevel.setAlignment(Element.ALIGN_LEFT);
                                break;
                            case 1:
                                this.currentListLevel.setAlignment(Element.ALIGN_CENTER);
                                break;
                            case 2:
                                this.currentListLevel.setAlignment(Element.ALIGN_RIGHT);
                                break;
                            }
                            skipCtrlWord = true;
                            result = true;
                        } else if (ctrlWordData.ctrlWord.equals("levelstartat")) {
                            this.currentListLevel.setListStartAt(ctrlWordData.intValue());
                            skipCtrlWord = true;
                            result = true;
                        } else if (ctrlWordData.ctrlWord.equals("lvltentative")) {
                            this.currentListLevel.setTentative(true);
                            skipCtrlWord = true;
                            result = true;
                        } else if (ctrlWordData.ctrlWord.equals("levelold")) {
                            
                            skipCtrlWord = true;
                            result = true;
                        } else if (ctrlWordData.ctrlWord.equals("levelprev")) {
                            
                            skipCtrlWord = true;
                            result = true;
                        } else if (ctrlWordData.ctrlWord.equals("levelprevspace")) {
                            
                            skipCtrlWord = true;
                            result = true;
                        } else if (ctrlWordData.ctrlWord.equals("levelspace")) {
                            skipCtrlWord = true;
                            result = true;
                        } else if (ctrlWordData.ctrlWord.equals("levelindent")) {
                            skipCtrlWord = true;
                            result = true;
                        } else if (ctrlWordData.ctrlWord.equals("leveltext")) {
                            skipCtrlWord = true;
                            result = true;
                        }  else if (ctrlWordData.ctrlWord.equals("levelfollow")) {
                            this.currentListLevel.setLevelFollowValue(ctrlWordData.intValue());
                            skipCtrlWord = true;
                            result = true;
                        } else if (ctrlWordData.ctrlWord.equals("levellegal")) {
                            this.currentListLevel.setLegal(ctrlWordData.param=="1"?true:false);
                            skipCtrlWord = true;
                            result = true;
                        } else if (ctrlWordData.ctrlWord.equals("levelnorestart")) {
                            skipCtrlWord = true;
                            result = true;
                        } else if (ctrlWordData.ctrlWord.equals("chrfmt")) {
                            
                            skipCtrlWord = true;
                            result = true;
                        } else if (ctrlWordData.ctrlWord.equals("levelpicture")) {
                            skipCtrlWord = true;
                            result = true;
                        } else if (ctrlWordData.ctrlWord.equals("li")) {
                            
                            skipCtrlWord = true;
                            result = true;
                        } else if (ctrlWordData.ctrlWord.equals("fi")) {
                            
                            skipCtrlWord = true;
                            result = true;
                        } else if (ctrlWordData.ctrlWord.equals("jclisttab")) {
                            
                            skipCtrlWord = true;
                            result = true;
                        } else if (ctrlWordData.ctrlWord.equals("tx")) {
                            
                            skipCtrlWord = true;
                            result = true;
                        } else
                            
                            if (ctrlWordData.ctrlWord.equals("levelnfc"))  {
                                if( this.currentListLevel.getListType()== RtfListLevel.LIST_TYPE_UNKNOWN) {
                                    this.currentListLevel.setListType(ctrlWordData.intValue()+RtfListLevel.LIST_TYPE_BASE);
                                }
                                skipCtrlWord = true;
                                result = true;
                            } else if (ctrlWordData.ctrlWord.equals("levelnfcn"))  {
                                this.currentListLevel.setListType(ctrlWordData.intValue()+RtfListLevel.LIST_TYPE_BASE);
                                skipCtrlWord = true;
                                result = true;
                            } else
                                    
                                    if (ctrlWordData.ctrlWord.equals("leveltemplateid")) {
                                        
                                        skipCtrlWord = true;
                                        result = true;
                                    } else
                                        
                                        if (ctrlWordData.ctrlWord.equals("levelnumbers")) {
                                            skipCtrlWord = true;
                                            result = true;
                                        }
        }

        if (this.rtfParser.isConvert()) {
            if (ctrlWordData.ctrlWord.equals("shppict")) {
                result = true;
            }
            if (ctrlWordData.ctrlWord.equals("nonshppict")) {
                skipCtrlWord = true;
                this.rtfParser.setTokeniserStateSkipGroup();
                result = true;
            }
        }
        if (!skipCtrlWord) {
            switch (this.rtfParser.getConversionType()) {
            case RtfParser.TYPE_IMPORT_FULL:
                
                
                result = true;
                break;
            case RtfParser.TYPE_IMPORT_FRAGMENT:
                
                
                result = true;
                break;
            case RtfParser.TYPE_CONVERT:
                result = true;
                break;
            default: 
                result = false;
            break;
            }
        }

        return result;
    }

    
    public boolean handleCloseGroup() {
        this.currentSubGroupCount--;
        if(this.newList != null && this.currentSubGroupCount == 0) {
            this.importHeader.importList(Integer.toString(this.currentListMappingNumber), 
                    Integer.toString(this.newList.getListNumber()));
            this.rtfParser.getRtfDocument().add(this.newList);
        }
        return true;
    }

    
    public boolean handleOpenGroup() {
        
        return true;
    }

    
    public boolean handleCharacter(int ch) {
        
        return true;
    }

    
    public void setToDefaults() {
        

    }

}
