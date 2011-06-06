

package com.lowagie.text.pdf;

import java.util.ArrayList;

import com.lowagie.text.Chunk;


public class BidiLine {
    
    protected int runDirection;
    protected int pieceSize = 2048;
    protected char text[] = new char[pieceSize];
    protected PdfChunk detailChunks[] = new PdfChunk[pieceSize];
    protected int totalTextLength = 0;
    
    protected byte orderLevels[] = new byte[pieceSize];
    protected int indexChars[] = new int[pieceSize];
    
    protected ArrayList chunks = new ArrayList();
    protected int indexChunk = 0;
    protected int indexChunkChar = 0;
    protected int currentChar = 0;
    
    protected int storedRunDirection;
    protected char storedText[] = new char[0];
    protected PdfChunk storedDetailChunks[] = new PdfChunk[0];
    protected int storedTotalTextLength = 0;
    
    protected byte storedOrderLevels[] = new byte[0];
    protected int storedIndexChars[] = new int[0];
    
    protected int storedIndexChunk = 0;
    protected int storedIndexChunkChar = 0;
    protected int storedCurrentChar = 0;
    
    protected boolean shortStore;

    protected static final IntHashtable mirrorChars = new IntHashtable();
    protected int arabicOptions;

    
    public BidiLine() {
    }
    
    public BidiLine(BidiLine org) {
        runDirection = org.runDirection;
        pieceSize = org.pieceSize;
        text = (char[])org.text.clone();
        detailChunks = (PdfChunk[])org.detailChunks.clone();
        totalTextLength = org.totalTextLength;

        orderLevels = (byte[])org.orderLevels.clone();
        indexChars = (int[])org.indexChars.clone();

        chunks = new ArrayList(org.chunks);
        indexChunk = org.indexChunk;
        indexChunkChar = org.indexChunkChar;
        currentChar = org.currentChar;

        storedRunDirection = org.storedRunDirection;
        storedText = (char[])org.storedText.clone();
        storedDetailChunks = (PdfChunk[])org.storedDetailChunks.clone();
        storedTotalTextLength = org.storedTotalTextLength;

        storedOrderLevels = (byte[])org.storedOrderLevels.clone();
        storedIndexChars = (int[])org.storedIndexChars.clone();

        storedIndexChunk = org.storedIndexChunk;
        storedIndexChunkChar = org.storedIndexChunkChar;
        storedCurrentChar = org.storedCurrentChar;

        shortStore = org.shortStore;
        arabicOptions = org.arabicOptions;
    }
    
    public boolean isEmpty() {
        return (currentChar >= totalTextLength && indexChunk >= chunks.size());
    }
    
    public void clearChunks() {
        chunks.clear();
        totalTextLength = 0;
        currentChar = 0;
    }

    public boolean getParagraph(int runDirection) {
        this.runDirection = runDirection;
        currentChar = 0;
        totalTextLength = 0;
        boolean hasText = false;
        char c;
        char uniC;
        BaseFont bf;
        for (; indexChunk < chunks.size(); ++indexChunk) {
            PdfChunk ck = (PdfChunk)chunks.get(indexChunk);
            bf = ck.font().getFont();
            String s = ck.toString();
            int len = s.length();
            for (; indexChunkChar < len; ++indexChunkChar) {
                c = s.charAt(indexChunkChar);
                uniC = bf.getUnicodeEquivalent(c);
                if (uniC == '\r' || uniC == '\n') {
                    
                    if (uniC == '\r' && indexChunkChar + 1 < len && s.charAt(indexChunkChar + 1) == '\n')
                        ++indexChunkChar;
                    ++indexChunkChar;
                    if (indexChunkChar >= len) {
                        indexChunkChar = 0;
                        ++indexChunk;
                    }
                    hasText = true;
                    if (totalTextLength == 0)
                        detailChunks[0] = ck;
                    break;
                }
                addPiece(c, ck);
            }
            if (hasText)
                break;
            indexChunkChar = 0;
        }
        if (totalTextLength == 0)
            return hasText;

        
        totalTextLength = trimRight(0, totalTextLength - 1) + 1;
        if (totalTextLength == 0)
            return true;
        
        if (runDirection == PdfWriter.RUN_DIRECTION_LTR || runDirection == PdfWriter.RUN_DIRECTION_RTL) {
            if (orderLevels.length < totalTextLength) {
                orderLevels = new byte[pieceSize];
                indexChars = new int[pieceSize];
            }
            ArabicLigaturizer.processNumbers(text, 0, totalTextLength, arabicOptions);
            BidiOrder order = new BidiOrder(text, 0, totalTextLength, (byte)(runDirection == PdfWriter.RUN_DIRECTION_RTL ? 1 : 0));
            byte od[] = order.getLevels();
            for (int k = 0; k < totalTextLength; ++k) {
                orderLevels[k] = od[k];
                indexChars[k] = k;
            }
            doArabicShapping();
            mirrorGlyphs();
        }
        totalTextLength = trimRightEx(0, totalTextLength - 1) + 1;
        return true;
    }
    
    public void addChunk(PdfChunk chunk) {
        chunks.add(chunk);
    }
    
    public void addChunks(ArrayList chunks) {
        this.chunks.addAll(chunks);
    }
    
    public void addPiece(char c, PdfChunk chunk) {
        if (totalTextLength >= pieceSize) {
            char tempText[] = text;
            PdfChunk tempDetailChunks[] = detailChunks;
            pieceSize *= 2;
            text = new char[pieceSize];
            detailChunks = new PdfChunk[pieceSize];
            System.arraycopy(tempText, 0, text, 0, totalTextLength);
            System.arraycopy(tempDetailChunks, 0, detailChunks, 0, totalTextLength);
        }
        text[totalTextLength] = c;
        detailChunks[totalTextLength++] = chunk;
    }
    
    public void save() {
        if (indexChunk > 0) {
            if (indexChunk >= chunks.size())
                chunks.clear();
            else {
                for (--indexChunk; indexChunk >= 0; --indexChunk)
                    chunks.remove(indexChunk);
            }
            indexChunk = 0;
        }
        storedRunDirection = runDirection;
        storedTotalTextLength = totalTextLength;
        storedIndexChunk = indexChunk;
        storedIndexChunkChar = indexChunkChar;
        storedCurrentChar = currentChar;
        shortStore = (currentChar < totalTextLength);
        if (!shortStore) {
            
            if (storedText.length < totalTextLength) {
                storedText = new char[totalTextLength];
                storedDetailChunks = new PdfChunk[totalTextLength];
            }
            System.arraycopy(text, 0, storedText, 0, totalTextLength);
            System.arraycopy(detailChunks, 0, storedDetailChunks, 0, totalTextLength);
        }
        if (runDirection == PdfWriter.RUN_DIRECTION_LTR || runDirection == PdfWriter.RUN_DIRECTION_RTL) {
            if (storedOrderLevels.length < totalTextLength) {
                storedOrderLevels = new byte[totalTextLength];
                storedIndexChars = new int[totalTextLength];
            }
            System.arraycopy(orderLevels, currentChar, storedOrderLevels, currentChar, totalTextLength - currentChar);
            System.arraycopy(indexChars, currentChar, storedIndexChars, currentChar, totalTextLength - currentChar);
        }
    }
    
    public void restore() {
        runDirection = storedRunDirection;
        totalTextLength = storedTotalTextLength;
        indexChunk = storedIndexChunk;
        indexChunkChar = storedIndexChunkChar;
        currentChar = storedCurrentChar;
        if (!shortStore) {
            
            System.arraycopy(storedText, 0, text, 0, totalTextLength);
            System.arraycopy(storedDetailChunks, 0, detailChunks, 0, totalTextLength);
        }
        if (runDirection == PdfWriter.RUN_DIRECTION_LTR || runDirection == PdfWriter.RUN_DIRECTION_RTL) {
            System.arraycopy(storedOrderLevels, currentChar, orderLevels, currentChar, totalTextLength - currentChar);
            System.arraycopy(storedIndexChars, currentChar, indexChars, currentChar, totalTextLength - currentChar);
        }
    }
    
    public void mirrorGlyphs() {
        for (int k = 0; k < totalTextLength; ++k) {
            if ((orderLevels[k] & 1) == 1) {
                int mirror = mirrorChars.get(text[k]);
                if (mirror != 0)
                    text[k] = (char)mirror;
            }
        }
    }
    
    public void doArabicShapping() {
        int src = 0;
        int dest = 0;
        for (;;) {
            while (src < totalTextLength) {
                char c = text[src];
                if (c >= 0x0600 && c <= 0x06ff)
                    break;
                if (src != dest) {
                    text[dest] = text[src];
                    detailChunks[dest] = detailChunks[src];
                    orderLevels[dest] = orderLevels[src];
                }
                ++src;
                ++dest;
            }
            if (src >= totalTextLength) {
                totalTextLength = dest;
                return;
            }
            int startArabicIdx = src;
            ++src;
            while (src < totalTextLength) {
                char c = text[src];
                if (c < 0x0600 || c > 0x06ff)
                    break;
                ++src;
            }
            int arabicWordSize = src - startArabicIdx;
            int size = ArabicLigaturizer.arabic_shape(text, startArabicIdx, arabicWordSize, text, dest, arabicWordSize, arabicOptions );
            if (startArabicIdx != dest) {
                for (int k = 0; k < size; ++k) {
                    detailChunks[dest] = detailChunks[startArabicIdx];
                    orderLevels[dest++] = orderLevels[startArabicIdx++];
                }
            }
            else
                dest += size;
        }
    }
       
    public PdfLine processLine(float width, int alignment, int runDirection, int arabicOptions) {
        this.arabicOptions = arabicOptions;
        save();
        boolean isRTL = (runDirection == PdfWriter.RUN_DIRECTION_RTL);
        if (currentChar >= totalTextLength) {
            boolean hasText = getParagraph(runDirection);
            if (!hasText)
                return null;
            if (totalTextLength == 0) {
                ArrayList ar = new ArrayList();
                PdfChunk ck = new PdfChunk("", detailChunks[0]);
                ar.add(ck);
                return new PdfLine(0, 0, alignment, true, ar, isRTL);
            }
        }
        float originalWidth = width;
        int lastSplit = -1;
        if (currentChar != 0)
            currentChar = trimLeftEx(currentChar, totalTextLength - 1);
        int oldCurrentChar = currentChar;
        char c = 0;
        char uniC = 0;
        PdfChunk ck = null;
        float charWidth = 0;
        PdfChunk lastValidChunk = null;
        for (; currentChar < totalTextLength; ++currentChar) {
            c = text[currentChar];
            ck = detailChunks[currentChar];
            uniC = ck.getUnicodeEquivalent(c);
            if (PdfChunk.noPrint(uniC))
                continue;
            charWidth = ck.getCharWidth(c);
            if (ck.isExtSplitCharacter(oldCurrentChar, currentChar, totalTextLength, text, detailChunks))
                lastSplit = currentChar;
            if (width - charWidth < 0)
                break;
            width -= charWidth;
            lastValidChunk = ck;
        }
        if (lastValidChunk == null) {
            
            ++currentChar;
            return new PdfLine(0, 0, alignment, false, createArrayOfPdfChunks(currentChar - 1, currentChar - 1), isRTL);
        }
        if (currentChar >= totalTextLength) {
            
            return new PdfLine(0, width, alignment, true, createArrayOfPdfChunks(oldCurrentChar, totalTextLength - 1), isRTL);
        }
        int newCurrentChar = trimRightEx(oldCurrentChar, currentChar - 1);
        if (newCurrentChar < oldCurrentChar) {
            
            return new PdfLine(0, width, alignment, false, createArrayOfPdfChunks(oldCurrentChar, currentChar - 1), isRTL);
        }
        if (newCurrentChar == currentChar - 1) { 
            HyphenationEvent he = (HyphenationEvent)lastValidChunk.getAttribute(Chunk.HYPHENATION);
            if (he != null) {
                int word[] = getWord(oldCurrentChar, newCurrentChar);
                if (word != null) {
                    float testWidth = width + getWidth(word[0], currentChar - 1);
                    String pre = he.getHyphenatedWordPre(new String(text, word[0], word[1] - word[0]), lastValidChunk.font().getFont(), lastValidChunk.font().size(), testWidth);
                    String post = he.getHyphenatedWordPost();
                    if (pre.length() > 0) {
                        PdfChunk extra = new PdfChunk(pre, lastValidChunk);
                        currentChar = word[1] - post.length();
                        return new PdfLine(0, testWidth - lastValidChunk.font().width(pre), alignment, false, createArrayOfPdfChunks(oldCurrentChar, word[0] - 1, extra), isRTL);
                    }
                }
            }
        }
        if (lastSplit == -1 || lastSplit >= newCurrentChar) {
            
            return new PdfLine(0, width + getWidth(newCurrentChar + 1, currentChar - 1), alignment, false, createArrayOfPdfChunks(oldCurrentChar, newCurrentChar), isRTL);
        }
        
        currentChar = lastSplit + 1;
        newCurrentChar = trimRightEx(oldCurrentChar, lastSplit);
        if (newCurrentChar < oldCurrentChar) {
            
            newCurrentChar = currentChar - 1;
        }
        return new PdfLine(0, originalWidth - getWidth(oldCurrentChar, newCurrentChar), alignment, false, createArrayOfPdfChunks(oldCurrentChar, newCurrentChar), isRTL);
    }
    
        
    public float getWidth(int startIdx, int lastIdx) {
        char c = 0;
        char uniC;
        PdfChunk ck = null;
        float width = 0;
        for (; startIdx <= lastIdx; ++startIdx) {
            c = text[startIdx];
            ck = detailChunks[startIdx];
            uniC = ck.getUnicodeEquivalent(c);
            if (PdfChunk.noPrint(uniC))
                continue;
            width += detailChunks[startIdx].getCharWidth(c);
        }
        return width;
    }
    
    public ArrayList createArrayOfPdfChunks(int startIdx, int endIdx) {
        return createArrayOfPdfChunks(startIdx, endIdx, null);
    }
    
    public ArrayList createArrayOfPdfChunks(int startIdx, int endIdx, PdfChunk extraPdfChunk) {
        boolean bidi = (runDirection == PdfWriter.RUN_DIRECTION_LTR || runDirection == PdfWriter.RUN_DIRECTION_RTL);
        if (bidi)
            reorder(startIdx, endIdx);
        ArrayList ar = new ArrayList();
        PdfChunk refCk = detailChunks[startIdx];
        PdfChunk ck = null;
        StringBuffer buf = new StringBuffer();
        char c;
        int idx = 0;
        for (; startIdx <= endIdx; ++startIdx) {
            idx = bidi ? indexChars[startIdx] : startIdx;
            c = text[idx];
            ck = detailChunks[idx];
            if (PdfChunk.noPrint(ck.getUnicodeEquivalent(c)))
                continue;
            if (ck.isImage()) {
                if (buf.length() > 0) {
                    ar.add(new PdfChunk(buf.toString(), refCk));
                    buf = new StringBuffer();
                }
                ar.add(ck);
            }
            else if (ck == refCk) {
                buf.append(c);
            }
            else {
                if (buf.length() > 0) {
                    ar.add(new PdfChunk(buf.toString(), refCk));
                    buf = new StringBuffer();
                }
                if (!ck.isImage())
                    buf.append(c);
                refCk = ck;
            }
        }
        if (buf.length() > 0) {
            ar.add(new PdfChunk(buf.toString(), refCk));
        }
        if (extraPdfChunk != null)
            ar.add(extraPdfChunk);
        return ar;
    }
    
    public int[] getWord(int startIdx, int idx) {
        int last = idx;
        int first = idx;
        
        for (; last < totalTextLength; ++last) {
            if (!Character.isLetter(text[last]))
                break;            
        }
        if (last == idx)
            return null;
        
        for (; first >= startIdx; --first) {
            if (!Character.isLetter(text[first]))
                break;            
        }
        ++first;
        return new int[]{first, last};
    }
    
    public int trimRight(int startIdx, int endIdx) {
        int idx = endIdx;
        char c;
        for (; idx >= startIdx; --idx) {
            c = detailChunks[idx].getUnicodeEquivalent(text[idx]);
            if (!isWS(c))
                break;
        }
        return idx;
    }
    
    public int trimLeft(int startIdx, int endIdx) {
        int idx = startIdx;
        char c;
        for (; idx <= endIdx; ++idx) {
            c = detailChunks[idx].getUnicodeEquivalent(text[idx]);
            if (!isWS(c))
                break;
        }
        return idx;
    }
    
    public int trimRightEx(int startIdx, int endIdx) {
        int idx = endIdx;
        char c = 0;
        for (; idx >= startIdx; --idx) {
            c = detailChunks[idx].getUnicodeEquivalent(text[idx]);
            if (!isWS(c) && !PdfChunk.noPrint(c))
                break;
        }
        return idx;
    }
    
    public int trimLeftEx(int startIdx, int endIdx) {
        int idx = startIdx;
        char c = 0;
        for (; idx <= endIdx; ++idx) {
            c = detailChunks[idx].getUnicodeEquivalent(text[idx]);
            if (!isWS(c) && !PdfChunk.noPrint(c))
                break;
        }
        return idx;
    }
    
    public void reorder(int start, int end) {
        byte maxLevel = orderLevels[start];
        byte minLevel = maxLevel;
        byte onlyOddLevels = maxLevel;
        byte onlyEvenLevels = maxLevel;
        for (int k = start + 1; k <= end; ++k) {
            byte b = orderLevels[k];
            if (b > maxLevel)
                maxLevel = b;
            else if (b < minLevel)
                minLevel = b;
            onlyOddLevels &= b;
            onlyEvenLevels |= b;
        }
        if ((onlyEvenLevels & 1) == 0) 
            return;
        if ((onlyOddLevels & 1) == 1) { 
            flip(start, end + 1);
            return;
        }
        minLevel |= 1;
        for (; maxLevel >= minLevel; --maxLevel) {
            int pstart = start;
            for (;;) {
                for (;pstart <= end; ++pstart) {
                    if (orderLevels[pstart] >= maxLevel)
                        break;
                }
                if (pstart > end)
                    break;
                int pend = pstart + 1;
                for (; pend <= end; ++pend) {
                    if (orderLevels[pend] < maxLevel)
                        break;
                }
                flip(pstart, pend);
                pstart = pend + 1;
            }
        }
    }
    
    public void flip(int start, int end) {
        int mid = (start + end) / 2;
        --end;
        for (; start < mid; ++start, --end) {
            int temp = indexChars[start];
            indexChars[start] = indexChars[end];
            indexChars[end] = temp;
        }
    }
    
    public static boolean isWS(char c) {
        return (c <= ' ');
    }

    static {
        mirrorChars.put(0x0028, 0x0029); 
        mirrorChars.put(0x0029, 0x0028); 
        mirrorChars.put(0x003C, 0x003E); 
        mirrorChars.put(0x003E, 0x003C); 
        mirrorChars.put(0x005B, 0x005D); 
        mirrorChars.put(0x005D, 0x005B); 
        mirrorChars.put(0x007B, 0x007D); 
        mirrorChars.put(0x007D, 0x007B); 
        mirrorChars.put(0x00AB, 0x00BB); 
        mirrorChars.put(0x00BB, 0x00AB); 
        mirrorChars.put(0x2039, 0x203A); 
        mirrorChars.put(0x203A, 0x2039); 
        mirrorChars.put(0x2045, 0x2046); 
        mirrorChars.put(0x2046, 0x2045); 
        mirrorChars.put(0x207D, 0x207E); 
        mirrorChars.put(0x207E, 0x207D); 
        mirrorChars.put(0x208D, 0x208E); 
        mirrorChars.put(0x208E, 0x208D); 
        mirrorChars.put(0x2208, 0x220B); 
        mirrorChars.put(0x2209, 0x220C); 
        mirrorChars.put(0x220A, 0x220D); 
        mirrorChars.put(0x220B, 0x2208); 
        mirrorChars.put(0x220C, 0x2209); 
        mirrorChars.put(0x220D, 0x220A); 
        mirrorChars.put(0x2215, 0x29F5); 
        mirrorChars.put(0x223C, 0x223D); 
        mirrorChars.put(0x223D, 0x223C); 
        mirrorChars.put(0x2243, 0x22CD); 
        mirrorChars.put(0x2252, 0x2253); 
        mirrorChars.put(0x2253, 0x2252); 
        mirrorChars.put(0x2254, 0x2255); 
        mirrorChars.put(0x2255, 0x2254); 
        mirrorChars.put(0x2264, 0x2265); 
        mirrorChars.put(0x2265, 0x2264); 
        mirrorChars.put(0x2266, 0x2267); 
        mirrorChars.put(0x2267, 0x2266); 
        mirrorChars.put(0x2268, 0x2269); 
        mirrorChars.put(0x2269, 0x2268); 
        mirrorChars.put(0x226A, 0x226B); 
        mirrorChars.put(0x226B, 0x226A); 
        mirrorChars.put(0x226E, 0x226F); 
        mirrorChars.put(0x226F, 0x226E); 
        mirrorChars.put(0x2270, 0x2271); 
        mirrorChars.put(0x2271, 0x2270); 
        mirrorChars.put(0x2272, 0x2273); 
        mirrorChars.put(0x2273, 0x2272); 
        mirrorChars.put(0x2274, 0x2275); 
        mirrorChars.put(0x2275, 0x2274); 
        mirrorChars.put(0x2276, 0x2277); 
        mirrorChars.put(0x2277, 0x2276); 
        mirrorChars.put(0x2278, 0x2279); 
        mirrorChars.put(0x2279, 0x2278); 
        mirrorChars.put(0x227A, 0x227B); 
        mirrorChars.put(0x227B, 0x227A); 
        mirrorChars.put(0x227C, 0x227D); 
        mirrorChars.put(0x227D, 0x227C); 
        mirrorChars.put(0x227E, 0x227F); 
        mirrorChars.put(0x227F, 0x227E); 
        mirrorChars.put(0x2280, 0x2281); 
        mirrorChars.put(0x2281, 0x2280); 
        mirrorChars.put(0x2282, 0x2283); 
        mirrorChars.put(0x2283, 0x2282); 
        mirrorChars.put(0x2284, 0x2285); 
        mirrorChars.put(0x2285, 0x2284); 
        mirrorChars.put(0x2286, 0x2287); 
        mirrorChars.put(0x2287, 0x2286); 
        mirrorChars.put(0x2288, 0x2289); 
        mirrorChars.put(0x2289, 0x2288); 
        mirrorChars.put(0x228A, 0x228B); 
        mirrorChars.put(0x228B, 0x228A); 
        mirrorChars.put(0x228F, 0x2290); 
        mirrorChars.put(0x2290, 0x228F); 
        mirrorChars.put(0x2291, 0x2292); 
        mirrorChars.put(0x2292, 0x2291); 
        mirrorChars.put(0x2298, 0x29B8); 
        mirrorChars.put(0x22A2, 0x22A3); 
        mirrorChars.put(0x22A3, 0x22A2); 
        mirrorChars.put(0x22A6, 0x2ADE); 
        mirrorChars.put(0x22A8, 0x2AE4); 
        mirrorChars.put(0x22A9, 0x2AE3); 
        mirrorChars.put(0x22AB, 0x2AE5); 
        mirrorChars.put(0x22B0, 0x22B1); 
        mirrorChars.put(0x22B1, 0x22B0); 
        mirrorChars.put(0x22B2, 0x22B3); 
        mirrorChars.put(0x22B3, 0x22B2); 
        mirrorChars.put(0x22B4, 0x22B5); 
        mirrorChars.put(0x22B5, 0x22B4); 
        mirrorChars.put(0x22B6, 0x22B7); 
        mirrorChars.put(0x22B7, 0x22B6); 
        mirrorChars.put(0x22C9, 0x22CA); 
        mirrorChars.put(0x22CA, 0x22C9); 
        mirrorChars.put(0x22CB, 0x22CC); 
        mirrorChars.put(0x22CC, 0x22CB); 
        mirrorChars.put(0x22CD, 0x2243); 
        mirrorChars.put(0x22D0, 0x22D1); 
        mirrorChars.put(0x22D1, 0x22D0); 
        mirrorChars.put(0x22D6, 0x22D7); 
        mirrorChars.put(0x22D7, 0x22D6); 
        mirrorChars.put(0x22D8, 0x22D9); 
        mirrorChars.put(0x22D9, 0x22D8); 
        mirrorChars.put(0x22DA, 0x22DB); 
        mirrorChars.put(0x22DB, 0x22DA); 
        mirrorChars.put(0x22DC, 0x22DD); 
        mirrorChars.put(0x22DD, 0x22DC); 
        mirrorChars.put(0x22DE, 0x22DF); 
        mirrorChars.put(0x22DF, 0x22DE); 
        mirrorChars.put(0x22E0, 0x22E1); 
        mirrorChars.put(0x22E1, 0x22E0); 
        mirrorChars.put(0x22E2, 0x22E3); 
        mirrorChars.put(0x22E3, 0x22E2); 
        mirrorChars.put(0x22E4, 0x22E5); 
        mirrorChars.put(0x22E5, 0x22E4); 
        mirrorChars.put(0x22E6, 0x22E7); 
        mirrorChars.put(0x22E7, 0x22E6); 
        mirrorChars.put(0x22E8, 0x22E9); 
        mirrorChars.put(0x22E9, 0x22E8); 
        mirrorChars.put(0x22EA, 0x22EB); 
        mirrorChars.put(0x22EB, 0x22EA); 
        mirrorChars.put(0x22EC, 0x22ED); 
        mirrorChars.put(0x22ED, 0x22EC); 
        mirrorChars.put(0x22F0, 0x22F1); 
        mirrorChars.put(0x22F1, 0x22F0); 
        mirrorChars.put(0x22F2, 0x22FA); 
        mirrorChars.put(0x22F3, 0x22FB); 
        mirrorChars.put(0x22F4, 0x22FC); 
        mirrorChars.put(0x22F6, 0x22FD); 
        mirrorChars.put(0x22F7, 0x22FE); 
        mirrorChars.put(0x22FA, 0x22F2); 
        mirrorChars.put(0x22FB, 0x22F3); 
        mirrorChars.put(0x22FC, 0x22F4); 
        mirrorChars.put(0x22FD, 0x22F6); 
        mirrorChars.put(0x22FE, 0x22F7); 
        mirrorChars.put(0x2308, 0x2309); 
        mirrorChars.put(0x2309, 0x2308); 
        mirrorChars.put(0x230A, 0x230B); 
        mirrorChars.put(0x230B, 0x230A); 
        mirrorChars.put(0x2329, 0x232A); 
        mirrorChars.put(0x232A, 0x2329); 
        mirrorChars.put(0x2768, 0x2769); 
        mirrorChars.put(0x2769, 0x2768); 
        mirrorChars.put(0x276A, 0x276B); 
        mirrorChars.put(0x276B, 0x276A); 
        mirrorChars.put(0x276C, 0x276D); 
        mirrorChars.put(0x276D, 0x276C); 
        mirrorChars.put(0x276E, 0x276F); 
        mirrorChars.put(0x276F, 0x276E); 
        mirrorChars.put(0x2770, 0x2771); 
        mirrorChars.put(0x2771, 0x2770); 
        mirrorChars.put(0x2772, 0x2773); 
        mirrorChars.put(0x2773, 0x2772); 
        mirrorChars.put(0x2774, 0x2775); 
        mirrorChars.put(0x2775, 0x2774); 
        mirrorChars.put(0x27D5, 0x27D6); 
        mirrorChars.put(0x27D6, 0x27D5); 
        mirrorChars.put(0x27DD, 0x27DE); 
        mirrorChars.put(0x27DE, 0x27DD); 
        mirrorChars.put(0x27E2, 0x27E3); 
        mirrorChars.put(0x27E3, 0x27E2); 
        mirrorChars.put(0x27E4, 0x27E5); 
        mirrorChars.put(0x27E5, 0x27E4); 
        mirrorChars.put(0x27E6, 0x27E7); 
        mirrorChars.put(0x27E7, 0x27E6); 
        mirrorChars.put(0x27E8, 0x27E9); 
        mirrorChars.put(0x27E9, 0x27E8); 
        mirrorChars.put(0x27EA, 0x27EB); 
        mirrorChars.put(0x27EB, 0x27EA); 
        mirrorChars.put(0x2983, 0x2984); 
        mirrorChars.put(0x2984, 0x2983); 
        mirrorChars.put(0x2985, 0x2986); 
        mirrorChars.put(0x2986, 0x2985); 
        mirrorChars.put(0x2987, 0x2988); 
        mirrorChars.put(0x2988, 0x2987); 
        mirrorChars.put(0x2989, 0x298A); 
        mirrorChars.put(0x298A, 0x2989); 
        mirrorChars.put(0x298B, 0x298C); 
        mirrorChars.put(0x298C, 0x298B); 
        mirrorChars.put(0x298D, 0x2990); 
        mirrorChars.put(0x298E, 0x298F); 
        mirrorChars.put(0x298F, 0x298E); 
        mirrorChars.put(0x2990, 0x298D); 
        mirrorChars.put(0x2991, 0x2992); 
        mirrorChars.put(0x2992, 0x2991); 
        mirrorChars.put(0x2993, 0x2994); 
        mirrorChars.put(0x2994, 0x2993); 
        mirrorChars.put(0x2995, 0x2996); 
        mirrorChars.put(0x2996, 0x2995); 
        mirrorChars.put(0x2997, 0x2998); 
        mirrorChars.put(0x2998, 0x2997); 
        mirrorChars.put(0x29B8, 0x2298); 
        mirrorChars.put(0x29C0, 0x29C1); 
        mirrorChars.put(0x29C1, 0x29C0); 
        mirrorChars.put(0x29C4, 0x29C5); 
        mirrorChars.put(0x29C5, 0x29C4); 
        mirrorChars.put(0x29CF, 0x29D0); 
        mirrorChars.put(0x29D0, 0x29CF); 
        mirrorChars.put(0x29D1, 0x29D2); 
        mirrorChars.put(0x29D2, 0x29D1); 
        mirrorChars.put(0x29D4, 0x29D5); 
        mirrorChars.put(0x29D5, 0x29D4); 
        mirrorChars.put(0x29D8, 0x29D9); 
        mirrorChars.put(0x29D9, 0x29D8); 
        mirrorChars.put(0x29DA, 0x29DB); 
        mirrorChars.put(0x29DB, 0x29DA); 
        mirrorChars.put(0x29F5, 0x2215); 
        mirrorChars.put(0x29F8, 0x29F9); 
        mirrorChars.put(0x29F9, 0x29F8); 
        mirrorChars.put(0x29FC, 0x29FD); 
        mirrorChars.put(0x29FD, 0x29FC); 
        mirrorChars.put(0x2A2B, 0x2A2C); 
        mirrorChars.put(0x2A2C, 0x2A2B); 
        mirrorChars.put(0x2A2D, 0x2A2C); 
        mirrorChars.put(0x2A2E, 0x2A2D); 
        mirrorChars.put(0x2A34, 0x2A35); 
        mirrorChars.put(0x2A35, 0x2A34); 
        mirrorChars.put(0x2A3C, 0x2A3D); 
        mirrorChars.put(0x2A3D, 0x2A3C); 
        mirrorChars.put(0x2A64, 0x2A65); 
        mirrorChars.put(0x2A65, 0x2A64); 
        mirrorChars.put(0x2A79, 0x2A7A); 
        mirrorChars.put(0x2A7A, 0x2A79); 
        mirrorChars.put(0x2A7D, 0x2A7E); 
        mirrorChars.put(0x2A7E, 0x2A7D); 
        mirrorChars.put(0x2A7F, 0x2A80); 
        mirrorChars.put(0x2A80, 0x2A7F); 
        mirrorChars.put(0x2A81, 0x2A82); 
        mirrorChars.put(0x2A82, 0x2A81); 
        mirrorChars.put(0x2A83, 0x2A84); 
        mirrorChars.put(0x2A84, 0x2A83); 
        mirrorChars.put(0x2A8B, 0x2A8C); 
        mirrorChars.put(0x2A8C, 0x2A8B); 
        mirrorChars.put(0x2A91, 0x2A92); 
        mirrorChars.put(0x2A92, 0x2A91); 
        mirrorChars.put(0x2A93, 0x2A94); 
        mirrorChars.put(0x2A94, 0x2A93); 
        mirrorChars.put(0x2A95, 0x2A96); 
        mirrorChars.put(0x2A96, 0x2A95); 
        mirrorChars.put(0x2A97, 0x2A98); 
        mirrorChars.put(0x2A98, 0x2A97); 
        mirrorChars.put(0x2A99, 0x2A9A); 
        mirrorChars.put(0x2A9A, 0x2A99); 
        mirrorChars.put(0x2A9B, 0x2A9C); 
        mirrorChars.put(0x2A9C, 0x2A9B); 
        mirrorChars.put(0x2AA1, 0x2AA2); 
        mirrorChars.put(0x2AA2, 0x2AA1); 
        mirrorChars.put(0x2AA6, 0x2AA7); 
        mirrorChars.put(0x2AA7, 0x2AA6); 
        mirrorChars.put(0x2AA8, 0x2AA9); 
        mirrorChars.put(0x2AA9, 0x2AA8); 
        mirrorChars.put(0x2AAA, 0x2AAB); 
        mirrorChars.put(0x2AAB, 0x2AAA); 
        mirrorChars.put(0x2AAC, 0x2AAD); 
        mirrorChars.put(0x2AAD, 0x2AAC); 
        mirrorChars.put(0x2AAF, 0x2AB0); 
        mirrorChars.put(0x2AB0, 0x2AAF); 
        mirrorChars.put(0x2AB3, 0x2AB4); 
        mirrorChars.put(0x2AB4, 0x2AB3); 
        mirrorChars.put(0x2ABB, 0x2ABC); 
        mirrorChars.put(0x2ABC, 0x2ABB); 
        mirrorChars.put(0x2ABD, 0x2ABE); 
        mirrorChars.put(0x2ABE, 0x2ABD); 
        mirrorChars.put(0x2ABF, 0x2AC0); 
        mirrorChars.put(0x2AC0, 0x2ABF); 
        mirrorChars.put(0x2AC1, 0x2AC2); 
        mirrorChars.put(0x2AC2, 0x2AC1); 
        mirrorChars.put(0x2AC3, 0x2AC4); 
        mirrorChars.put(0x2AC4, 0x2AC3); 
        mirrorChars.put(0x2AC5, 0x2AC6); 
        mirrorChars.put(0x2AC6, 0x2AC5); 
        mirrorChars.put(0x2ACD, 0x2ACE); 
        mirrorChars.put(0x2ACE, 0x2ACD); 
        mirrorChars.put(0x2ACF, 0x2AD0); 
        mirrorChars.put(0x2AD0, 0x2ACF); 
        mirrorChars.put(0x2AD1, 0x2AD2); 
        mirrorChars.put(0x2AD2, 0x2AD1); 
        mirrorChars.put(0x2AD3, 0x2AD4); 
        mirrorChars.put(0x2AD4, 0x2AD3); 
        mirrorChars.put(0x2AD5, 0x2AD6); 
        mirrorChars.put(0x2AD6, 0x2AD5); 
        mirrorChars.put(0x2ADE, 0x22A6); 
        mirrorChars.put(0x2AE3, 0x22A9); 
        mirrorChars.put(0x2AE4, 0x22A8); 
        mirrorChars.put(0x2AE5, 0x22AB); 
        mirrorChars.put(0x2AEC, 0x2AED); 
        mirrorChars.put(0x2AED, 0x2AEC); 
        mirrorChars.put(0x2AF7, 0x2AF8); 
        mirrorChars.put(0x2AF8, 0x2AF7); 
        mirrorChars.put(0x2AF9, 0x2AFA); 
        mirrorChars.put(0x2AFA, 0x2AF9); 
        mirrorChars.put(0x3008, 0x3009); 
        mirrorChars.put(0x3009, 0x3008); 
        mirrorChars.put(0x300A, 0x300B); 
        mirrorChars.put(0x300B, 0x300A); 
        mirrorChars.put(0x300C, 0x300D); 
        mirrorChars.put(0x300D, 0x300C); 
        mirrorChars.put(0x300E, 0x300F); 
        mirrorChars.put(0x300F, 0x300E); 
        mirrorChars.put(0x3010, 0x3011); 
        mirrorChars.put(0x3011, 0x3010); 
        mirrorChars.put(0x3014, 0x3015); 
        mirrorChars.put(0x3015, 0x3014); 
        mirrorChars.put(0x3016, 0x3017); 
        mirrorChars.put(0x3017, 0x3016); 
        mirrorChars.put(0x3018, 0x3019); 
        mirrorChars.put(0x3019, 0x3018); 
        mirrorChars.put(0x301A, 0x301B); 
        mirrorChars.put(0x301B, 0x301A); 
        mirrorChars.put(0xFF08, 0xFF09); 
        mirrorChars.put(0xFF09, 0xFF08); 
        mirrorChars.put(0xFF1C, 0xFF1E); 
        mirrorChars.put(0xFF1E, 0xFF1C); 
        mirrorChars.put(0xFF3B, 0xFF3D); 
        mirrorChars.put(0xFF3D, 0xFF3B); 
        mirrorChars.put(0xFF5B, 0xFF5D); 
        mirrorChars.put(0xFF5D, 0xFF5B); 
        mirrorChars.put(0xFF5F, 0xFF60); 
        mirrorChars.put(0xFF60, 0xFF5F); 
        mirrorChars.put(0xFF62, 0xFF63); 
        mirrorChars.put(0xFF63, 0xFF62); 
    }
}