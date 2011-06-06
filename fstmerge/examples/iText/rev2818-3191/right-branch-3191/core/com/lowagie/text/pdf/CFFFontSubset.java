
package com.lowagie.text.pdf;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;


public class CFFFontSubset extends CFFFont {
    
    
    static final String SubrsFunctions[] = {
            "RESERVED_0","hstem","RESERVED_2","vstem","vmoveto","rlineto","hlineto","vlineto",
            "rrcurveto","RESERVED_9","callsubr","return","escape","RESERVED_13",
            "endchar","RESERVED_15","RESERVED_16","RESERVED_17","hstemhm","hintmask",
            "cntrmask","rmoveto","hmoveto","vstemhm","rcurveline","rlinecurve","vvcurveto",
            "hhcurveto","shortint","callgsubr","vhcurveto","hvcurveto"
            };
    
    static final String SubrsEscapeFuncs[] = {
            "RESERVED_0","RESERVED_1","RESERVED_2","and","or","not","RESERVED_6",
            "RESERVED_7","RESERVED_8","abs","add","sub","div","RESERVED_13","neg",
            "eq","RESERVED_16","RESERVED_17","drop","RESERVED_19","put","get","ifelse",
            "random","mul","RESERVED_25","sqrt","dup","exch","index","roll","RESERVED_31",
            "RESERVED_32","RESERVED_33","hflex","flex","hflex1","flex1","RESERVED_REST"
    };
    
    
    HashMap<Integer, int[]> GlyphsUsed;
    
    ArrayList<Integer> glyphsInList;
    
    HashMap<Integer, Object> FDArrayUsed = new HashMap<Integer, Object>();
    
    HashMap<Integer, int[]>[] hSubrsUsed;
    
    ArrayList<Integer>[] lSubrsUsed;
    
    HashMap<Integer, int[]> hGSubrsUsed  = new HashMap<Integer, int[]>();
    
    ArrayList<Integer> lGSubrsUsed = new ArrayList<Integer>();
    
    HashMap<Integer, int[]> hSubrsUsedNonCID  = new HashMap<Integer, int[]>();
    
    ArrayList<Integer> lSubrsUsedNonCID = new ArrayList<Integer>();
    
    byte[][] NewLSubrsIndex;
    
    byte[] NewSubrsIndexNonCID;
    
    byte[] NewGSubrsIndex;
    
    byte[] NewCharStringsIndex;
    
    
    int GBias = 0;
    
    
    LinkedList<Item> OutputList;
    
    
    int NumOfHints=0;

    
    
    public CFFFontSubset(RandomAccessFileOrArray rf,HashMap<Integer, int[]> GlyphsUsed){
        
        super(rf);
        this.GlyphsUsed = GlyphsUsed;
        
        glyphsInList = new ArrayList<Integer>(GlyphsUsed.keySet());
        
        
        for (int i=0;i<fonts.length;++i)
        {
            
            seek(fonts[i].charstringsOffset);
            fonts[i].nglyphs = getCard16();
            
            
            seek(stringIndexOffset);
            fonts[i].nstrings = getCard16()+standardStrings.length;
            
            
            fonts[i].charstringsOffsets = getIndex(fonts[i].charstringsOffset);
            
            
            if (fonts[i].fdselectOffset>=0)
            {
                
                readFDSelect(i);
                
                BuildFDArrayUsed(i);
            }
            if (fonts[i].isCID)
                
                ReadFDArray(i);
            
            fonts[i].CharsetLength = CountCharset(fonts[i].charsetOffset,fonts[i].nglyphs);
        }
    }

    
    int CountCharset(int Offset,int NumofGlyphs){
        int format;
        int Length=0;
        seek(Offset);
        
        format = getCard8();
        
        switch (format){
            case 0:
                Length = 1+2*NumofGlyphs;
                break;
            case 1:
                Length = 1+3*CountRange(NumofGlyphs,1);
                break;
            case 2:
                Length = 1+4*CountRange(NumofGlyphs,2);
                break;
            default:
                break;
        }
        return Length;
    }
    
    
    int CountRange(int NumofGlyphs,int Type){
        int num=0;
        char Sid;
        int i=1,nLeft;
        while (i<NumofGlyphs){
            num++;
            Sid = getCard16();
            if (Type==1)
                nLeft = getCard8();
            else
                nLeft = getCard16();
            i += nLeft+1;
        }
        return num;
    }


    
    protected void readFDSelect(int Font)
    {
        
        int NumOfGlyphs = fonts[Font].nglyphs;
        int[] FDSelect = new int[NumOfGlyphs];
        
        seek(fonts[Font].fdselectOffset);
        
        fonts[Font].FDSelectFormat = getCard8();
        
        switch(fonts[Font].FDSelectFormat){
            
            
            case 0:
                for (int i=0;i<NumOfGlyphs;i++)
                {
                    FDSelect[i] = getCard8();
                }
                
                
                fonts[Font].FDSelectLength = fonts[Font].nglyphs+1;
                break;
            case 3:
                
                
                int nRanges = getCard16();
                int l=0;
                
                int first = getCard16();
                for (int i=0;i<nRanges;i++)
                {
                    
                    int fd = getCard8();
                    
                    int last = getCard16();
                    
                    int steps = last-first;
                    for (int k=0;k<steps;k++)
                    {
                        FDSelect[l] = fd;
                        l++;
                    }
                    
                    first = last;
                }
                
                fonts[Font].FDSelectLength = 1+2+nRanges*3+2;
                break;
            default:
                break;
        }
        
        fonts[Font].FDSelect = FDSelect; 
    }
    
    
    protected void BuildFDArrayUsed(int Font)
    {
        int[] FDSelect = fonts[Font].FDSelect;
        
        for (int i=0;i<glyphsInList.size();i++)
        {
            
            int glyph = glyphsInList.get(i).intValue();
            
            int FD = FDSelect[glyph];
            
            FDArrayUsed.put(new Integer(FD),null);
        }
    }

    
    protected void ReadFDArray(int Font)
    {
        seek(fonts[Font].fdarrayOffset);
        fonts[Font].FDArrayCount = getCard16();
        fonts[Font].FDArrayOffsize = getCard8();
        
        
        if (fonts[Font].FDArrayOffsize < 4)
            fonts[Font].FDArrayOffsize++;
        fonts[Font].FDArrayOffsets = getIndex(fonts[Font].fdarrayOffset);
    }

    
    
    public byte[] Process(String fontName)throws IOException{
        try
        {    
            
            buf.reOpen();
            
            int j;
            for (j=0; j<fonts.length; j++)
                if (fontName.equals(fonts[j].name)) break;
            if (j==fonts.length) return null;
            
            
            if (gsubrIndexOffset >= 0)
                GBias = CalcBias(gsubrIndexOffset,j);

            
            BuildNewCharString(j);
             
            BuildNewLGSubrs(j);
            
            byte[] Ret = BuildNewFile(j);
            return Ret;
        }
        finally {
            try {
                buf.close();
            }
            catch (Exception e) {
                
            }
        }
    }

    
    protected int CalcBias(int Offset,int Font)
    {
        seek(Offset);
        int nSubrs = getCard16();
        
        if (fonts[Font].CharstringType == 1)
            return 0;
        
        else if (nSubrs < 1240)
            return 107;
        else if (nSubrs < 33900)
            return 1131;
        else
            return 32768;
    }

    
    protected void BuildNewCharString(int FontIndex) throws IOException 
    {
        NewCharStringsIndex = BuildNewIndex(fonts[FontIndex].charstringsOffsets,GlyphsUsed);
    }
    
    
    protected void BuildNewLGSubrs(int Font)throws IOException
    {
        
        
        if(fonts[Font].isCID)
        {
            
            
            hSubrsUsed = new HashMap[fonts[Font].fdprivateOffsets.length];
            lSubrsUsed = new ArrayList[fonts[Font].fdprivateOffsets.length];
            
            NewLSubrsIndex = new byte[fonts[Font].fdprivateOffsets.length][];
            
            fonts[Font].PrivateSubrsOffset = new int[fonts[Font].fdprivateOffsets.length];
            
            fonts[Font].PrivateSubrsOffsetsArray = new int[fonts[Font].fdprivateOffsets.length][];
            
            
            ArrayList<Integer> FDInList = new ArrayList<Integer>(FDArrayUsed.keySet());
            
            for (int j=0;j<FDInList.size();j++)
            {
                
                int FD = FDInList.get(j).intValue();
                hSubrsUsed[FD] = new HashMap<Integer, int[]>();
                lSubrsUsed[FD] = new ArrayList<Integer>();
                
                
                BuildFDSubrsOffsets(Font,FD);
                
                if(fonts[Font].PrivateSubrsOffset[FD]>=0)
                {
                    
                    
                    BuildSubrUsed(Font,FD,fonts[Font].PrivateSubrsOffset[FD],fonts[Font].PrivateSubrsOffsetsArray[FD],hSubrsUsed[FD],lSubrsUsed[FD]);
                    
                    NewLSubrsIndex[FD] = BuildNewIndex(fonts[Font].PrivateSubrsOffsetsArray[FD],hSubrsUsed[FD]);
                }
            }
        }
        
        else if (fonts[Font].privateSubrs>=0)
        {
            
            fonts[Font].SubrsOffsets = getIndex(fonts[Font].privateSubrs);
            
            
            BuildSubrUsed(Font,-1,fonts[Font].privateSubrs,fonts[Font].SubrsOffsets,hSubrsUsedNonCID,lSubrsUsedNonCID);
        }
        
        
        BuildGSubrsUsed(Font);
        if (fonts[Font].privateSubrs>=0)
            
            NewSubrsIndexNonCID = BuildNewIndex(fonts[Font].SubrsOffsets,hSubrsUsedNonCID);
        
        NewGSubrsIndex = BuildNewIndex(gsubrOffsets,hGSubrsUsed);
    }

    
    protected void BuildFDSubrsOffsets(int Font,int FD)
    {
        
        fonts[Font].PrivateSubrsOffset[FD] = -1;
        
        seek(fonts[Font].fdprivateOffsets[FD]);
        
        while (getPosition() < fonts[Font].fdprivateOffsets[FD]+fonts[Font].fdprivateLengths[FD])
        {
            getDictItem();
            
            if (key=="Subrs")
                fonts[Font].PrivateSubrsOffset[FD] = ((Integer)args[0]).intValue()+fonts[Font].fdprivateOffsets[FD];
        }
        
        if (fonts[Font].PrivateSubrsOffset[FD] >= 0)
            fonts[Font].PrivateSubrsOffsetsArray[FD] = getIndex(fonts[Font].PrivateSubrsOffset[FD]); 
    }

    
    protected void BuildSubrUsed(int Font,int FD,int SubrOffset,int[] SubrsOffsets,HashMap<Integer, int[]> hSubr,ArrayList<Integer> lSubr)
    {

        
        int LBias = CalcBias(SubrOffset,Font);
        
        
        for (int i=0;i<glyphsInList.size();i++)
        {
            int glyph = glyphsInList.get(i).intValue();
            int Start = fonts[Font].charstringsOffsets[glyph];
            int End = fonts[Font].charstringsOffsets[glyph+1];
            
            
            if (FD >= 0)
            {
                EmptyStack();
                NumOfHints=0;
                
                int GlyphFD = fonts[Font].FDSelect[glyph];
                
                if (GlyphFD == FD)
                    
                    ReadASubr(Start,End,GBias,LBias,hSubr,lSubr,SubrsOffsets);
            }
            else
                
                
                ReadASubr(Start,End,GBias,LBias,hSubr,lSubr,SubrsOffsets);
        }
        
        for (int i=0;i<lSubr.size();i++)
        {
            
            int Subr = lSubr.get(i).intValue();
            
            if (Subr < SubrsOffsets.length-1 && Subr>=0)
            {
                
                int Start = SubrsOffsets[Subr];
                int End = SubrsOffsets[Subr+1];
                ReadASubr(Start,End,GBias,LBias,hSubr,lSubr,SubrsOffsets);
            }
        }
    }
    
    
    protected void BuildGSubrsUsed(int Font)
    {
        int LBias = 0;
        int SizeOfNonCIDSubrsUsed = 0;
        if (fonts[Font].privateSubrs>=0)
        {
            LBias = CalcBias(fonts[Font].privateSubrs,Font);
            SizeOfNonCIDSubrsUsed = lSubrsUsedNonCID.size();
        }
        
        
        for (int i=0;i<lGSubrsUsed.size();i++)
        {
            
            int Subr = lGSubrsUsed.get(i).intValue();
            if (Subr < gsubrOffsets.length-1 && Subr>=0)
            {
                
                int Start = gsubrOffsets[Subr];
                int End = gsubrOffsets[Subr+1];
                
                if (fonts[Font].isCID)
                    ReadASubr(Start,End,GBias,0,hGSubrsUsed,lGSubrsUsed,null);
                else
                {
                    ReadASubr(Start,End,GBias,LBias,hSubrsUsedNonCID,lSubrsUsedNonCID,fonts[Font].SubrsOffsets);
                    if (SizeOfNonCIDSubrsUsed < lSubrsUsedNonCID.size())
                    {
                        for (int j=SizeOfNonCIDSubrsUsed;j<lSubrsUsedNonCID.size();j++)
                        {
                            
                            int LSubr = lSubrsUsedNonCID.get(j).intValue();
                            if (LSubr < fonts[Font].SubrsOffsets.length-1 && LSubr>=0)
                            {
                                
                                int LStart = fonts[Font].SubrsOffsets[LSubr];
                                int LEnd = fonts[Font].SubrsOffsets[LSubr+1];
                                ReadASubr(LStart,LEnd,GBias,LBias,hSubrsUsedNonCID,lSubrsUsedNonCID,fonts[Font].SubrsOffsets);
                            }
                        }
                        SizeOfNonCIDSubrsUsed = lSubrsUsedNonCID.size();
                    }
                }
            }
        }
    }

    
    protected void ReadASubr(int begin,int end,int GBias,int LBias,HashMap<Integer, int[]> hSubr,ArrayList<Integer> lSubr,int[] LSubrsOffsets)
    {
        
        EmptyStack();
        NumOfHints = 0;
        
        seek(begin);
        while (getPosition() < end)
        {
            
            ReadCommand();
            int pos = getPosition();
            Object TopElement=null;
            if (arg_count > 0)
                TopElement = args[arg_count-1];
            int NumOfArgs = arg_count;
            
            HandelStack();
            
            if (key=="callsubr") 
            {
                
                if (NumOfArgs > 0)
                {
                    
                    int Subr = ((Integer)TopElement).intValue() + LBias;
                    
                    if (!hSubr.containsKey(new Integer (Subr)))
                    {
                        hSubr.put(new Integer(Subr),null);
                        lSubr.add(new Integer(Subr));
                    }
                    CalcHints(LSubrsOffsets[Subr],LSubrsOffsets[Subr+1],LBias,GBias,LSubrsOffsets);
                    seek(pos);
                }                
            }
            
            else if (key=="callgsubr")
            {
                
                if (NumOfArgs > 0)
                {
                    
                    int Subr = ((Integer)TopElement).intValue() + GBias;
                    
                    if (!hGSubrsUsed.containsKey(new Integer (Subr)))
                    {
                        hGSubrsUsed.put(new Integer(Subr),null);
                        lGSubrsUsed.add(new Integer(Subr));
                    }
                    CalcHints(gsubrOffsets[Subr],gsubrOffsets[Subr+1],LBias,GBias,LSubrsOffsets);
                    seek(pos);
                }
            }
            
            else if (key == "hstem" || key == "vstem" || key == "hstemhm" || key == "vstemhm")
                
                NumOfHints += NumOfArgs/2;
            
            else if (key == "hintmask" || key == "cntrmask")
            {
                
                int SizeOfMask = NumOfHints/8;
                if (NumOfHints%8 != 0 || SizeOfMask == 0)
                    SizeOfMask++;
                
                for (int i=0;i<SizeOfMask;i++)
                    getCard8();
            }
        }
    }

    
    protected void HandelStack()
    {
        
        int StackHandel = StackOpp();
        if (StackHandel < 2)
        {
            
            if (StackHandel==1)
                PushStack();
            
            else
            {
                
                StackHandel *= -1;
                for (int i=0;i<StackHandel;i++)
                    PopStack();
            }
            
        }
        
        else
            EmptyStack();        
    }
    
    
    protected int StackOpp()
    {
        if (key == "ifelse")
            return -3;
        if (key == "roll" || key == "put")
            return -2;
        if (key == "callsubr" || key == "callgsubr" || key == "add" || key == "sub" ||
            key == "div" || key == "mul" || key == "drop" || key == "and" || 
            key == "or" || key == "eq")
            return -1;
        if (key == "abs" || key == "neg" || key == "sqrt" || key == "exch" || 
            key == "index" || key == "get" || key == "not" || key == "return")
            return 0;
        if (key == "random" || key == "dup")
            return 1;
        return 2;
    }
    
    
    protected void EmptyStack()
    {
        
        for (int i=0; i<arg_count; i++) args[i]=null;
        arg_count = 0;        
    }
    
    
    protected void PopStack()
    {
        if (arg_count>0)
        {
            args[arg_count-1]=null;
            arg_count--;
        }
    }
    
    
    protected void PushStack()
    {
        arg_count++;
    }
    
    
    protected void ReadCommand()
    {
        key = null;
        boolean gotKey = false;
        
        while (!gotKey) {
            
            char b0 = getCard8();
            
            if (b0 == 28) 
            {
                int first = getCard8();
                int second = getCard8();
                args[arg_count] = new Integer(first<<8 | second);
                arg_count++;
                continue;
            }
            if (b0 >= 32 && b0 <= 246) 
            {
                args[arg_count] = new Integer(b0 - 139);
                arg_count++;
                continue;
            }
            if (b0 >= 247 && b0 <= 250) 
            {
                int w = getCard8();
                args[arg_count] = new Integer((b0-247)*256 + w + 108);
                arg_count++;
                continue;
            }
            if (b0 >= 251 && b0 <= 254)
            {
                int w = getCard8();
                args[arg_count] = new Integer(-(b0-251)*256 - w - 108);
                arg_count++;
                continue;
            }
            if (b0 == 255)
            {
                int first = getCard8();
                int second = getCard8();
                int third = getCard8();
                int fourth = getCard8();
                args[arg_count] = new Integer(first<<24 | second<<16 | third<<8 | fourth);
                arg_count++;
                continue;
            }
            if (b0<=31 && b0 != 28) 
            {
                gotKey=true;
                
                
                if (b0 == 12)
                {
                    int b1 = getCard8();
                    if (b1>SubrsEscapeFuncs.length-1)
                        b1 = SubrsEscapeFuncs.length-1;
                    key = SubrsEscapeFuncs[b1];
                }
                else
                    key = SubrsFunctions[b0];
                continue;
            }
        }        
    }
    
    
    protected int CalcHints(int begin,int end,int LBias,int GBias,int[] LSubrsOffsets)
    {
        
        seek(begin);
        while (getPosition() < end)
        {
            
            ReadCommand();
            int pos = getPosition();
            Object TopElement = null;
            if (arg_count>0)
                TopElement = args[arg_count-1];
            int NumOfArgs = arg_count;
            
            HandelStack();
            
            if (key=="callsubr") 
            {
                if (NumOfArgs>0)
                {
                    int Subr = ((Integer)TopElement).intValue() + LBias;
                    CalcHints(LSubrsOffsets[Subr],LSubrsOffsets[Subr+1],LBias,GBias,LSubrsOffsets);
                    seek(pos);                    
                }
            }
            
            else if (key=="callgsubr")
            {
                if (NumOfArgs>0)
                {
                    int Subr = ((Integer)TopElement).intValue() + GBias;
                    CalcHints(gsubrOffsets[Subr],gsubrOffsets[Subr+1],LBias,GBias,LSubrsOffsets);
                    seek(pos);                    
                }
            }
            
            else if (key == "hstem" || key == "vstem" || key == "hstemhm" || key == "vstemhm")
                
                NumOfHints += NumOfArgs/2;
            
            else if (key == "hintmask" || key == "cntrmask")
            {
                
                int SizeOfMask = NumOfHints/8;
                if (NumOfHints%8 != 0 || SizeOfMask == 0)
                    SizeOfMask++;
                
                for (int i=0;i<SizeOfMask;i++)
                    getCard8();
            }
        }
        return NumOfHints;
    }


    
    protected byte[] BuildNewIndex(int[] Offsets,HashMap<Integer, int[]> Used) throws IOException 
    {
        int Offset=0;
        int[] NewOffsets = new int[Offsets.length];
        
        for (int i=0;i<Offsets.length;++i)
        {
            NewOffsets[i] = Offset;
            
            
            if (Used.containsKey(new Integer(i)))
                Offset += Offsets[i+1] - Offsets[i];
                
        }
        
        byte[] NewObjects = new byte[Offset];
        
        for (int i=0;i<Offsets.length-1;++i)
        {
            int start = NewOffsets[i];
            int end = NewOffsets[i+1];
            
            
            if (start != end)
            {
                
                
                buf.seek(Offsets[i]);
                
                buf.readFully(NewObjects, start, end-start);
            }
        }
        
        return AssembleIndex(NewOffsets,NewObjects);
    }

    
    protected byte[] AssembleIndex(int[] NewOffsets,byte[] NewObjects)
    {
        
        char Count = (char)(NewOffsets.length-1);
        
        int Size = NewOffsets[NewOffsets.length-1];
        
        byte Offsize;
        if (Size <= 0xff) Offsize = 1;
        else if (Size <= 0xffff) Offsize = 2;
        else if (Size <= 0xffffff) Offsize = 3;
        else Offsize = 4;
        
        
        byte[] NewIndex = new byte[2+1+Offsize*(Count+1)+NewObjects.length];
        
        int Place = 0;
        
        NewIndex[Place++] = (byte) ((Count >>> 8) & 0xff);
        NewIndex[Place++] = (byte) ((Count >>> 0) & 0xff);
        
        NewIndex[Place++] = Offsize;
        
        for (int i=0;i<NewOffsets.length;i++)
        {
            
            int Num = NewOffsets[i]-NewOffsets[0]+1;
            
            switch (Offsize) {
                case 4:
                    NewIndex[Place++] = (byte) ((Num >>> 24) & 0xff);
                case 3:
                    NewIndex[Place++] = (byte) ((Num >>> 16) & 0xff);
                case 2:
                    NewIndex[Place++] = (byte) ((Num >>>  8) & 0xff);
                case 1:
                    NewIndex[Place++] = (byte) ((Num >>>  0) & 0xff);
            }                    
        }
        
        for (int i=0;i<NewObjects.length;i++)
        {
            NewIndex[Place++] = NewObjects[i];
        }
        
        return NewIndex;
    }
    
    
    protected byte[] BuildNewFile(int Font)
    {
        
        OutputList = new LinkedList<Item>();

        
        CopyHeader();
                
        
        BuildIndexHeader(1,1,1);
        OutputList.addLast(new UInt8Item((char)( 1+fonts[Font].name.length() )));
        OutputList.addLast(new StringItem(fonts[Font].name));
        
        
        BuildIndexHeader(1,2,1);
        OffsetItem topdictIndex1Ref = new IndexOffsetItem(2);
        OutputList.addLast(topdictIndex1Ref);
        IndexBaseItem topdictBase = new IndexBaseItem();
        OutputList.addLast(topdictBase);
                
        
        OffsetItem charsetRef     = new DictOffsetItem();
        OffsetItem charstringsRef = new DictOffsetItem();
        OffsetItem fdarrayRef     = new DictOffsetItem();
        OffsetItem fdselectRef    = new DictOffsetItem();
        OffsetItem privateRef     = new DictOffsetItem();
        
        
        if ( !fonts[Font].isCID ) {
            
            OutputList.addLast(new DictNumberItem(fonts[Font].nstrings));
            OutputList.addLast(new DictNumberItem(fonts[Font].nstrings+1));
            OutputList.addLast(new DictNumberItem(0));
            OutputList.addLast(new UInt8Item((char)12));
            OutputList.addLast(new UInt8Item((char)30));
            
            OutputList.addLast(new DictNumberItem(fonts[Font].nglyphs));
            OutputList.addLast(new UInt8Item((char)12));
            OutputList.addLast(new UInt8Item((char)34));
            
            
            
        }
        
        seek(topdictOffsets[Font]);
        
        while (getPosition() < topdictOffsets[Font+1]) {
            int p1 = getPosition();
            getDictItem();
            int p2 = getPosition();
            
            if (key=="Encoding"
            
            || key=="Private" 
            || key=="FDSelect"
            || key=="FDArray" 
            || key=="charset" 
            || key=="CharStrings"
            ) {
            }else {
            
                OutputList.add(new RangeItem(buf,p1,p2-p1));
            }
        }
        
        CreateKeys(fdarrayRef,fdselectRef,charsetRef,charstringsRef);
        
        
        OutputList.addLast(new IndexMarkerItem(topdictIndex1Ref,topdictBase));
        
        

        if (fonts[Font].isCID) 
            OutputList.addLast(getEntireIndexRange(stringIndexOffset));
        
        
        
        else
            CreateNewStringIndex(Font);
        
        
        OutputList.addLast(new RangeItem(new RandomAccessFileOrArray(NewGSubrsIndex),0,NewGSubrsIndex.length));
        
        
        
        if (fonts[Font].isCID) {
            
       
            
            
            OutputList.addLast(new MarkerItem(fdselectRef));
            
            if (fonts[Font].fdselectOffset>=0)
                OutputList.addLast(new RangeItem(buf,fonts[Font].fdselectOffset,fonts[Font].FDSelectLength));
            
            else
                CreateFDSelect(fdselectRef,fonts[Font].nglyphs);
                           
              
            
            OutputList.addLast(new MarkerItem(charsetRef));
            OutputList.addLast(new RangeItem(buf,fonts[Font].charsetOffset,fonts[Font].CharsetLength));
            
            
            
            if (fonts[Font].fdarrayOffset>=0)
            {
                
                OutputList.addLast(new MarkerItem(fdarrayRef));
                
                Reconstruct(Font);
            }
            else
                
                CreateFDArray(fdarrayRef,privateRef,Font);
           
        }
        
        else 
        {
            
            CreateFDSelect(fdselectRef,fonts[Font].nglyphs);            
            
            CreateCharset(charsetRef,fonts[Font].nglyphs);            
            
            CreateFDArray(fdarrayRef,privateRef,Font);            
        }
        
        
        if (fonts[Font].privateOffset>=0)
        {
            
            IndexBaseItem PrivateBase = new IndexBaseItem();
            OutputList.addLast(PrivateBase);
            OutputList.addLast(new MarkerItem(privateRef));

            OffsetItem Subr = new DictOffsetItem();
            
            CreateNonCIDPrivate(Font,Subr);
            
            CreateNonCIDSubrs(Font,PrivateBase,Subr);
        }
        
        
        OutputList.addLast(new MarkerItem(charstringsRef));

        
        OutputList.addLast(new RangeItem(new RandomAccessFileOrArray(NewCharStringsIndex),0,NewCharStringsIndex.length));
        
        
        int[] currentOffset = new int[1];
        currentOffset[0] = 0;
        

        for (Item item: OutputList) {
            item.increment(currentOffset);
        }
        
        for (Item item: OutputList) {
            item.xref();
        }
        
        int size = currentOffset[0];
        byte[] b = new byte[size];
        
        
        for (Item item: OutputList) {
            item.emit(b);
        }
        
        return b;
    }

    
    protected void CopyHeader()
    {
        seek(0);
        int major = getCard8();
        int minor = getCard8();
        int hdrSize = getCard8();
        int offSize = getCard8();
        nextIndexOffset = hdrSize;
        OutputList.addLast(new RangeItem(buf,0,hdrSize));
    }

    
    protected void BuildIndexHeader(int Count,int Offsize,int First)
    {
        
        OutputList.addLast(new UInt16Item((char)Count)); 
        
        OutputList.addLast(new UInt8Item((char)Offsize)); 
        
        switch(Offsize){
            case 1:
                OutputList.addLast(new UInt8Item((char)First)); 
                break;
            case 2:
                OutputList.addLast(new UInt16Item((char)First)); 
                break;
            case 3:
                OutputList.addLast(new UInt24Item((char)First)); 
                break;
            case 4:
                OutputList.addLast(new UInt32Item((char)First)); 
                break;
            default:
                break;    
        }
    }
    
    
    protected void CreateKeys(OffsetItem fdarrayRef,OffsetItem fdselectRef,OffsetItem charsetRef,OffsetItem charstringsRef)
    {
        
        OutputList.addLast(fdarrayRef);
        OutputList.addLast(new UInt8Item((char)12));
        OutputList.addLast(new UInt8Item((char)36));
        
        OutputList.addLast(fdselectRef);
        OutputList.addLast(new UInt8Item((char)12));
        OutputList.addLast(new UInt8Item((char)37));
        
        OutputList.addLast(charsetRef);
        OutputList.addLast(new UInt8Item((char)15));
        
        OutputList.addLast(charstringsRef);
        OutputList.addLast(new UInt8Item((char)17));
    }
    
    
    protected void CreateNewStringIndex(int Font)
    {
        String fdFontName = fonts[Font].name+"-OneRange";
        if (fdFontName.length() > 127)
            fdFontName = fdFontName.substring(0,127);
        String extraStrings = "Adobe"+"Identity"+fdFontName;
        
        int origStringsLen = stringOffsets[stringOffsets.length-1]
        - stringOffsets[0];
        int stringsBaseOffset = stringOffsets[0]-1;
        
        byte stringsIndexOffSize;
        if (origStringsLen+extraStrings.length() <= 0xff) stringsIndexOffSize = 1;
        else if (origStringsLen+extraStrings.length() <= 0xffff) stringsIndexOffSize = 2;
        else if (origStringsLen+extraStrings.length() <= 0xffffff) stringsIndexOffSize = 3;
        else stringsIndexOffSize = 4;
        
        OutputList.addLast(new UInt16Item((char)((stringOffsets.length-1)+3))); 
        OutputList.addLast(new UInt8Item((char)stringsIndexOffSize)); 
        for (int i=0; i<stringOffsets.length; i++)
            OutputList.addLast(new IndexOffsetItem(stringsIndexOffSize,
            stringOffsets[i]-stringsBaseOffset));
        int currentStringsOffset = stringOffsets[stringOffsets.length-1]
        - stringsBaseOffset;
        
        currentStringsOffset += ("Adobe").length();
        OutputList.addLast(new IndexOffsetItem(stringsIndexOffSize,currentStringsOffset));
        currentStringsOffset += ("Identity").length();
        OutputList.addLast(new IndexOffsetItem(stringsIndexOffSize,currentStringsOffset));
        currentStringsOffset += fdFontName.length();
        OutputList.addLast(new IndexOffsetItem(stringsIndexOffSize,currentStringsOffset));
        
        OutputList.addLast(new RangeItem(buf,stringOffsets[0],origStringsLen));
        OutputList.addLast(new StringItem(extraStrings));
    }
     
    
    protected void CreateFDSelect(OffsetItem fdselectRef,int nglyphs)
    {
        OutputList.addLast(new MarkerItem(fdselectRef));
        OutputList.addLast(new UInt8Item((char)3)); 
        OutputList.addLast(new UInt16Item((char)1)); 
        
        OutputList.addLast(new UInt16Item((char)0)); 
        OutputList.addLast(new UInt8Item((char)0)); 
        
        OutputList.addLast(new UInt16Item((char)nglyphs)); 
    }

    
    protected void CreateCharset(OffsetItem charsetRef,int nglyphs)
    {
        OutputList.addLast(new MarkerItem(charsetRef));
        OutputList.addLast(new UInt8Item((char)2)); 
        OutputList.addLast(new UInt16Item((char)1)); 
        OutputList.addLast(new UInt16Item((char)(nglyphs-1))); 
    }
    
    
    protected void CreateFDArray(OffsetItem fdarrayRef,OffsetItem privateRef,int Font)
    {
        OutputList.addLast(new MarkerItem(fdarrayRef));
        
        BuildIndexHeader(1,1,1);
        
        
        OffsetItem privateIndex1Ref = new IndexOffsetItem(1);
        OutputList.addLast(privateIndex1Ref);
        IndexBaseItem privateBase = new IndexBaseItem();
        
        OutputList.addLast(privateBase);
        
        
        int NewSize = fonts[Font].privateLength;
        
        int OrgSubrsOffsetSize = CalcSubrOffsetSize(fonts[Font].privateOffset,fonts[Font].privateLength);
        
        if (OrgSubrsOffsetSize != 0)
            NewSize += 5-OrgSubrsOffsetSize;
        OutputList.addLast(new DictNumberItem(NewSize));
        OutputList.addLast(privateRef);
        OutputList.addLast(new UInt8Item((char)18)); 
        
        OutputList.addLast(new IndexMarkerItem(privateIndex1Ref,privateBase));
    }
    
    
    void Reconstruct(int Font)
    {
        
        OffsetItem[] fdPrivate = new DictOffsetItem[fonts[Font].FDArrayOffsets.length-1];
        IndexBaseItem[] fdPrivateBase = new IndexBaseItem[fonts[Font].fdprivateOffsets.length]; 
        OffsetItem[] fdSubrs = new DictOffsetItem[fonts[Font].fdprivateOffsets.length];
        
        ReconstructFDArray(Font,fdPrivate);
        ReconstructPrivateDict(Font,fdPrivate,fdPrivateBase,fdSubrs);
        ReconstructPrivateSubrs(Font,fdPrivateBase,fdSubrs);
    }

    
    void ReconstructFDArray(int Font,OffsetItem[] fdPrivate)
    {
        
        BuildIndexHeader(fonts[Font].FDArrayCount,fonts[Font].FDArrayOffsize,1);

        
        OffsetItem[] fdOffsets = new IndexOffsetItem[fonts[Font].FDArrayOffsets.length-1];
        for (int i=0;i<fonts[Font].FDArrayOffsets.length-1;i++)
        {
            fdOffsets[i] = new IndexOffsetItem(fonts[Font].FDArrayOffsize);
            OutputList.addLast(fdOffsets[i]);
        }
        
        
        IndexBaseItem fdArrayBase = new IndexBaseItem();
        OutputList.addLast(fdArrayBase);
        
        
        
        
        
        for (int k=0; k<fonts[Font].FDArrayOffsets.length-1; k++) {
            if (FDArrayUsed.containsKey(new Integer (k)))
            {
                
                seek(fonts[Font].FDArrayOffsets[k]);
                while (getPosition() < fonts[Font].FDArrayOffsets[k+1])
                {
                    int p1 = getPosition();
                    getDictItem();
                    int p2 = getPosition();
                    
                    
                    if (key=="Private") {
                        
                        int NewSize = ((Integer)args[0]).intValue();
                        
                        int OrgSubrsOffsetSize = CalcSubrOffsetSize(fonts[Font].fdprivateOffsets[k],fonts[Font].fdprivateLengths[k]);
                        
                        if (OrgSubrsOffsetSize != 0)
                            NewSize += 5-OrgSubrsOffsetSize;
                        
                        OutputList.addLast(new DictNumberItem(NewSize));
                        fdPrivate[k] = new DictOffsetItem();
                        OutputList.addLast(fdPrivate[k]);
                        OutputList.addLast(new UInt8Item((char)18)); 
                        
                        seek(p2);
                    }
                    
                    else  
                        OutputList.addLast(new RangeItem(buf,p1,p2-p1));
                }
            }
            
            OutputList.addLast(new IndexMarkerItem(fdOffsets[k],fdArrayBase));
        }
    }
    
    void ReconstructPrivateDict(int Font,OffsetItem[] fdPrivate,IndexBaseItem[] fdPrivateBase,
            OffsetItem[] fdSubrs)
    {
        
        
        
        
        for (int i=0;i<fonts[Font].fdprivateOffsets.length;i++)
        {
            if (FDArrayUsed.containsKey(new Integer (i)))
            {
                
                OutputList.addLast(new MarkerItem(fdPrivate[i]));
                fdPrivateBase[i] = new IndexBaseItem();
                OutputList.addLast(fdPrivateBase[i]);
                
                seek(fonts[Font].fdprivateOffsets[i]);
                while (getPosition() < fonts[Font].fdprivateOffsets[i]+fonts[Font].fdprivateLengths[i])
                {
                    int p1 = getPosition();
                    getDictItem();
                    int p2 = getPosition();
                    
                    
                    if (key=="Subrs") {
                        fdSubrs[i] = new DictOffsetItem();
                        OutputList.addLast(fdSubrs[i]);
                        OutputList.addLast(new UInt8Item((char)19)); 
                    }
                    
                    else
                        OutputList.addLast(new RangeItem(buf,p1,p2-p1));
                }
            }
        }
    }
    
    
    
    void ReconstructPrivateSubrs(int Font,IndexBaseItem[] fdPrivateBase,
            OffsetItem[] fdSubrs)
    {
        
        for (int i=0;i<fonts[Font].fdprivateLengths.length;i++)
        {
            
            
            if (fdSubrs[i]!= null && fonts[Font].PrivateSubrsOffset[i] >= 0)
            {                
                OutputList.addLast(new SubrMarkerItem(fdSubrs[i],fdPrivateBase[i]));
                OutputList.addLast(new RangeItem(new RandomAccessFileOrArray(NewLSubrsIndex[i]),0,NewLSubrsIndex[i].length));
            }
        }
    }

    
    int CalcSubrOffsetSize(int Offset,int Size)
    {
        
        int OffsetSize = 0;
        
        seek(Offset);
        
        while (getPosition() < Offset+Size)
        {
            int p1 = getPosition();
            getDictItem();
            int p2 = getPosition();
            
            if (key=="Subrs") {
                
                OffsetSize = p2-p1-1;
            }
            
        }
        
        return OffsetSize;
    }
    
    
    protected int countEntireIndexRange(int indexOffset) 
    {
        
        seek(indexOffset);
        
        int count = getCard16();
        
        if (count==0) 
            return 2;
        else 
        {
            
            int indexOffSize = getCard8();
            
            seek(indexOffset+2+1+count*indexOffSize);
            
            int size = getOffset(indexOffSize)-1;
            
            return 2+1+(count+1)*indexOffSize+size;
        }
    }
    
    
    void CreateNonCIDPrivate(int Font,OffsetItem Subr)
    {
        
        seek(fonts[Font].privateOffset);
        while (getPosition() < fonts[Font].privateOffset+fonts[Font].privateLength)
        {
            int p1 = getPosition();
            getDictItem();
            int p2 = getPosition();
            
            
            if (key=="Subrs") {
                OutputList.addLast(Subr);
                OutputList.addLast(new UInt8Item((char)19)); 
            }
            
            else
                OutputList.addLast(new RangeItem(buf,p1,p2-p1));
        }
    }
    
    
    void CreateNonCIDSubrs(int Font,IndexBaseItem PrivateBase,OffsetItem Subrs)
    {
        
        OutputList.addLast(new SubrMarkerItem(Subrs,PrivateBase));
        
        OutputList.addLast(new RangeItem(new RandomAccessFileOrArray(NewSubrsIndexNonCID),0,NewSubrsIndexNonCID.length));
    }    
}