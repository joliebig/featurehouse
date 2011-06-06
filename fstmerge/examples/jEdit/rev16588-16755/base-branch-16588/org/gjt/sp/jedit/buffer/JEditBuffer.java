

package org.gjt.sp.jedit.buffer;


import org.gjt.sp.jedit.Debug;
import org.gjt.sp.jedit.Mode;
import org.gjt.sp.jedit.TextUtilities;
import org.gjt.sp.jedit.indent.IndentAction;
import org.gjt.sp.jedit.indent.IndentRule;
import org.gjt.sp.jedit.syntax.*;
import org.gjt.sp.jedit.textarea.TextArea;
import org.gjt.sp.util.IntegerArray;
import org.gjt.sp.util.Log;
import org.gjt.sp.util.StandardUtilities;

import javax.swing.text.Position;
import javax.swing.text.Segment;
import java.awt.*;
import java.util.*;
import java.util.List;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.regex.Pattern;



public class JEditBuffer
{
	
	public static final String LINESEP = "lineSeparator";

	
	public static final String ENCODING = "encoding";

	
	public JEditBuffer(Map props)
	{
		bufferListeners = new Vector<Listener>();
		undoListeners = new Vector<BufferUndoListener>();
		lock = new ReentrantReadWriteLock();
		contentMgr = new ContentManager();
		lineMgr = new LineManager();
		positionMgr = new PositionManager(this);
		undoMgr = new UndoManager(this);
		integerArray = new IntegerArray();
		propertyLock = new Object();
		properties = new HashMap<Object, PropValue>();

		
		Set<Map.Entry> set = props.entrySet();
		for (Map.Entry entry : set)
		{
			properties.put(entry.getKey(),new PropValue(entry.getValue(),false));
		} 

		
		
		if(getProperty(ENCODING) == null)
			properties.put(ENCODING,new PropValue(System.getProperty("file.encoding"),false));
		if(getProperty(LINESEP) == null)
			properties.put(LINESEP,new PropValue(System.getProperty("line.separator"),false));
	}

	
	public JEditBuffer()
	{
		bufferListeners = new Vector<Listener>();
		undoListeners = new Vector<BufferUndoListener>();
		lock = new ReentrantReadWriteLock();
		contentMgr = new ContentManager();
		lineMgr = new LineManager();
		positionMgr = new PositionManager(this);
		undoMgr = new UndoManager(this);
		integerArray = new IntegerArray();
		propertyLock = new Object();
		properties = new HashMap<Object, PropValue>();

		properties.put("wrap",new PropValue("none",false));
		properties.put("folding",new PropValue("none",false));
		tokenMarker = new TokenMarker();
		tokenMarker.addRuleSet(new ParserRuleSet("text","MAIN"));
		setTokenMarker(tokenMarker);

		loadText(null,null);
		
		if(getProperty(ENCODING) == null)
			properties.put(ENCODING,new PropValue(System.getProperty("file.encoding"),false));
		if(getProperty(LINESEP) == null)
			properties.put(LINESEP,new PropValue(System.getProperty("line.separator"),false));

		setFoldHandler(new DummyFoldHandler());
	} 

	

	
	
	public boolean isDirty()
	{
		return dirty;
	} 

	
	public boolean isLoading()
	{
		return loading;
	} 

	
	public void setLoading(boolean loading)
	{
		this.loading = loading;
	} 

	
	
	public boolean isPerformingIO()
	{
		return isLoading() || io;
	} 

	
	
	public void setPerformingIO(boolean io)
	{
		this.io = io;
	} 

	
	
	public boolean isEditable()
	{
		return !(isReadOnly() || isPerformingIO());
	} 

	
	
	public boolean isReadOnly()
	{
		return readOnly || readOnlyOverride;
	} 

	
	
	public void setReadOnly(boolean readOnly)
	{
		readOnlyOverride = readOnly;
	} 

	
	
	public void setDirty(boolean d)
	{
		boolean editable = isEditable();

		if(d)
		{
			if(editable)
				dirty = true;
		}
		else
		{
			dirty = false;

			
			
			if(!isUndoInProgress())
			{
				
				
				undoMgr.resetClearDirty();
			}
		}
	} 

	

	

	
	
	public void readLock()
	{
		lock.readLock().lock();
	} 

	
	
	public void readUnlock()
	{
		lock.readLock().unlock();
	} 

	
	
	public void writeLock()
	{
		lock.writeLock().lock();
	} 

	
	
	public void writeUnlock()
	{
		lock.writeLock().unlock();
	} 

	

	

	
	
	public int getLength()
	{
		
		return contentMgr.getLength();
	} 

	
	
	public int getLineCount()
	{
		
		return lineMgr.getLineCount();
	} 

	
	
	public int getLineOfOffset(int offset)
	{
		try
		{
			readLock();

			if(offset < 0 || offset > getLength())
				throw new ArrayIndexOutOfBoundsException(offset);

			return lineMgr.getLineOfOffset(offset);
		}
		finally
		{
			readUnlock();
		}
	} 

	
	
	public int getLineStartOffset(int line)
	{
		try
		{
			readLock();

			if(line < 0 || line >= lineMgr.getLineCount())
				throw new ArrayIndexOutOfBoundsException(line);
			else if(line == 0)
				return 0;

			return lineMgr.getLineEndOffset(line - 1);
		}
		finally
		{
			readUnlock();
		}
	} 

	
	
	public int getLineEndOffset(int line)
	{
		try
		{
			readLock();

			if(line < 0 || line >= lineMgr.getLineCount())
				throw new ArrayIndexOutOfBoundsException(line);

			return lineMgr.getLineEndOffset(line);
		}
		finally
		{
			readUnlock();
		}
	} 

	
	
	public int getLineLength(int line)
	{
		try
		{
			readLock();

			return getLineEndOffset(line)
				- getLineStartOffset(line) - 1;
		}
		finally
		{
			readUnlock();
		}
	} 

	
	
	public int getPriorNonEmptyLine(int lineIndex)
	{
		int returnValue = -1;

		if (!mode.getIgnoreWhitespace())
		{
			return lineIndex - 1;
		}

		for(int i = lineIndex - 1; i >= 0; i--)
		{
			Segment seg = new Segment();
			getLineText(i,seg);
			if(seg.count != 0)
				returnValue = i;
			for(int j = 0; j < seg.count; j++)
			{
				char ch = seg.array[seg.offset + j];
				if(!Character.isWhitespace(ch))
					return i;
			}
		}

		
		
		return returnValue;
	} 

	

	

	
	
	public String getLineText(int line)
	{
		if(line < 0 || line >= lineMgr.getLineCount())
			throw new ArrayIndexOutOfBoundsException(line);

		try
		{
			readLock();

			int start = line == 0 ? 0 : lineMgr.getLineEndOffset(line - 1);
			int end = lineMgr.getLineEndOffset(line);

			return getText(start,end - start - 1);
		}
		finally
		{
			readUnlock();
		}
	} 

	
	public void getLineText(int line, Segment segment)
	{
		if(line < 0 || line >= lineMgr.getLineCount())
			throw new ArrayIndexOutOfBoundsException(line);

		try
		{
			readLock();

			int start = line == 0 ? 0 : lineMgr.getLineEndOffset(line - 1);
			int end = lineMgr.getLineEndOffset(line);

			getText(start,end - start - 1,segment);
		}
		finally
		{
			readUnlock();
		}
	} 

	
	
	public CharSequence getLineSegment(int line)
	{
		if(line < 0 || line >= lineMgr.getLineCount())
			throw new ArrayIndexOutOfBoundsException(line);

		try
		{
			readLock();

			int start = line == 0 ? 0 : lineMgr.getLineEndOffset(line - 1);
			int end = lineMgr.getLineEndOffset(line);

			return getSegment(start,end - start - 1);
		}
		finally
		{
			readUnlock();
		}
	} 

	
	
	public String getText(int start, int length)
	{
		try
		{
			readLock();

			if(start < 0 || length < 0
				|| start + length > contentMgr.getLength())
				throw new ArrayIndexOutOfBoundsException(start + ":" + length);

			return contentMgr.getText(start,length);
		}
		finally
		{
			readUnlock();
		}
	}

	
	public void getText(int start, int length, Segment seg)
	{
		try
		{
			readLock();

			if(start < 0 || length < 0
				|| start + length > contentMgr.getLength())
				throw new ArrayIndexOutOfBoundsException(start + ":" + length);

			contentMgr.getText(start,length,seg);
		}
		finally
		{
			readUnlock();
		}
	} 

	
	
	public CharSequence getSegment(int start, int length)
	{
		try
		{
			readLock();

			if(start < 0 || length < 0
				|| start + length > contentMgr.getLength())
				throw new ArrayIndexOutOfBoundsException(start + ":" + length);

			return contentMgr.getSegment(start,length);
		}
		finally
		{
			readUnlock();
		}
	} 

	
	
	public void insert(int offset, String str)
	{
		if(str == null)
			return;

		int len = str.length();

		if(len == 0)
			return;

		if(isReadOnly())
			throw new RuntimeException("buffer read-only");

		try
		{
			writeLock();

			if(offset < 0 || offset > contentMgr.getLength())
				throw new ArrayIndexOutOfBoundsException(offset);

			contentMgr.insert(offset,str);

			integerArray.clear();

			for(int i = 0; i < len; i++)
			{
				if(str.charAt(i) == '\n')
					integerArray.add(i + 1);
			}

			if(!undoInProgress)
			{
				undoMgr.contentInserted(offset,len,str,!dirty);
			}

			contentInserted(offset,len,integerArray);
		}
		finally
		{
			writeUnlock();
		}
	}

	
	public void insert(int offset, Segment seg)
	{
		if(seg.count == 0)
			return;

		if(isReadOnly())
			throw new RuntimeException("buffer read-only");

		try
		{
			writeLock();

			if(offset < 0 || offset > contentMgr.getLength())
				throw new ArrayIndexOutOfBoundsException(offset);

			contentMgr.insert(offset,seg);

			integerArray.clear();

			for(int i = 0; i < seg.count; i++)
			{
				if(seg.array[seg.offset + i] == '\n')
					integerArray.add(i + 1);
			}

			if(!undoInProgress)
			{
				undoMgr.contentInserted(offset,seg.count,
					seg.toString(),!dirty);
			}

			contentInserted(offset,seg.count,integerArray);
		}
		finally
		{
			writeUnlock();
		}
	} 

	
	
	public void remove(int offset, int length)
	{
		if(length == 0)
			return;

		if(isReadOnly())
			throw new RuntimeException("buffer read-only");

		try
		{
			transaction = true;

			writeLock();

			if(offset < 0 || length < 0
				|| offset + length > contentMgr.getLength())
				throw new ArrayIndexOutOfBoundsException(offset + ":" + length);

			int startLine = lineMgr.getLineOfOffset(offset);
			int endLine = lineMgr.getLineOfOffset(offset + length);

			int numLines = endLine - startLine;

			if(!undoInProgress && !loading)
			{
				undoMgr.contentRemoved(offset,length,
					getText(offset,length),
					!dirty);
			}

			firePreContentRemoved(startLine,offset,numLines,length);

			contentMgr.remove(offset,length);
			lineMgr.contentRemoved(startLine,offset,numLines,length);
			positionMgr.contentRemoved(offset,length);

			fireContentRemoved(startLine,offset,numLines,length);

			
			if(!undoInProgress && !insideCompoundEdit())
				fireTransactionComplete();

			setDirty(true);
		}
		finally
		{
			transaction = false;

			writeUnlock();
		}
	} 

	

	

	
	
	public void removeTrailingWhiteSpace(int[] lines)
	{
		try
		{
			beginCompoundEdit();

			for(int i = 0; i < lines.length; i++)
			{
				int pos, lineStart, lineEnd, tail;
				Segment seg = new Segment();
				getLineText(lines[i],seg);

				
				if (seg.count == 0) continue;

				lineStart = seg.offset;
				lineEnd = seg.offset + seg.count - 1;

				for (pos = lineEnd; pos >= lineStart; pos--)
				{
					if (!Character.isWhitespace(seg.array[pos]))
						break;
				}

				tail = lineEnd - pos;

				
				if (tail == 0) continue;

				remove(getLineEndOffset(lines[i]) - 1 - tail,tail);
			}
		}
		finally
		{
			endCompoundEdit();
		}
	} 

	
	
	public void shiftIndentLeft(int[] lines)
	{
		int tabSize = getTabSize();
		int indentSize = getIndentSize();
		boolean noTabs = getBooleanProperty("noTabs");

		try
		{
			beginCompoundEdit();

			for(int i = 0; i < lines.length; i++)
			{
				int lineStart = getLineStartOffset(lines[i]);
				CharSequence line = getLineSegment(lines[i]);
				int whiteSpace = StandardUtilities
					.getLeadingWhiteSpace(line);
				if(whiteSpace == 0)
					continue;
				int whiteSpaceWidth = Math.max(0,StandardUtilities
					.getLeadingWhiteSpaceWidth(line,tabSize)
					- indentSize);

				insert(lineStart + whiteSpace,StandardUtilities
					.createWhiteSpace(whiteSpaceWidth,
					noTabs ? 0 : tabSize));
				remove(lineStart,whiteSpace);
			}

		}
		finally
		{
			endCompoundEdit();
		}
	} 

	
	
	public void shiftIndentRight(int[] lines)
	{
		try
		{
			beginCompoundEdit();

			int tabSize = getTabSize();
			int indentSize = getIndentSize();
			boolean noTabs = getBooleanProperty("noTabs");
			for(int i = 0; i < lines.length; i++)
			{
				int lineStart = getLineStartOffset(lines[i]);
				CharSequence line = getLineSegment(lines[i]);
				int whiteSpace = StandardUtilities
					.getLeadingWhiteSpace(line);

				
				
				

				int whiteSpaceWidth = StandardUtilities
					.getLeadingWhiteSpaceWidth(
					line,tabSize) + indentSize;
				insert(lineStart + whiteSpace,StandardUtilities
					.createWhiteSpace(whiteSpaceWidth,
					noTabs ? 0 : tabSize));
				remove(lineStart,whiteSpace);
			}
		}
		finally
		{
			endCompoundEdit();
		}
	} 

	
	
	public void indentLines(int start, int end)
	{
		try
		{
			beginCompoundEdit();
			for(int i = start; i <= end; i++)
				indentLine(i,true);
		}
		finally
		{
			endCompoundEdit();
		}
	}

	
	public void indentLines(int[] lines)
	{
		try
		{
			beginCompoundEdit();
			for(int i = 0; i < lines.length; i++)
				indentLine(lines[i],true);
		}
		finally
		{
			endCompoundEdit();
		}
	} 

	
	
	 @Deprecated
	 public boolean indentLine(int lineIndex, boolean canIncreaseIndent,
		boolean canDecreaseIndent)
	{
		return indentLine(lineIndex,canDecreaseIndent);
	}

	
	public boolean indentLine(int lineIndex, boolean canDecreaseIndent)
	{
		int[] whitespaceChars = new int[1];
		int currentIndent = getCurrentIndentForLine(lineIndex,
			whitespaceChars);
		int prevLineIndex = getPriorNonEmptyLine(lineIndex);
		int prevLineIndent = (prevLineIndex == -1) ? 0 :
			StandardUtilities.getLeadingWhiteSpaceWidth(getLineSegment(
				prevLineIndex), getTabSize());
		int idealIndent = getIdealIndentForLine(lineIndex, prevLineIndex,
			prevLineIndent);

		if (idealIndent == -1 || idealIndent == currentIndent ||
			(!canDecreaseIndent && idealIndent < currentIndent))
			return false;

		
		try
		{
			beginCompoundEdit();

			int start = getLineStartOffset(lineIndex);

			remove(start,whitespaceChars[0]);
			String prevIndentString = (prevLineIndex >= 0) ?
				StandardUtilities.getIndentString(getLineText(
					prevLineIndex)) : null;
			String indentString;
			if (prevIndentString == null)
			{
				indentString = StandardUtilities.createWhiteSpace(
					idealIndent,
					getBooleanProperty("noTabs") ? 0 : getTabSize());
			}
			else if (idealIndent == prevLineIndent)
				indentString = prevIndentString;
			else if (idealIndent < prevLineIndent)
				indentString = StandardUtilities.truncateWhiteSpace(
					idealIndent, getTabSize(), prevIndentString);
			else
				indentString = prevIndentString +
					StandardUtilities.createWhiteSpace(
						idealIndent - prevLineIndent,
						getBooleanProperty("noTabs") ? 0 : getTabSize(),
						prevLineIndent);
			insert(start, indentString);
		}
		finally
		{
			endCompoundEdit();
		}

		return true;
	} 

	
	
	public int getCurrentIndentForLine(int lineIndex, int[] whitespaceChars)
	{
		Segment seg = new Segment();
		getLineText(lineIndex,seg);

		int tabSize = getTabSize();

		int currentIndent = 0;
loop:		for(int i = 0; i < seg.count; i++)
		{
			char c = seg.array[seg.offset + i];
			switch(c)
			{
			case ' ':
				currentIndent++;
				if(whitespaceChars != null)
					whitespaceChars[0]++;
				break;
			case '\t':
				currentIndent += tabSize - (currentIndent
					% tabSize);
				if(whitespaceChars != null)
					whitespaceChars[0]++;
				break;
			default:
				break loop;
			}
		}

		return currentIndent;
	} 

	
	
	public int getIdealIndentForLine(int lineIndex)
	{
		int prevLineIndex = getPriorNonEmptyLine(lineIndex);
		int oldIndent = prevLineIndex == -1 ? 0 :
			StandardUtilities.getLeadingWhiteSpaceWidth(
			getLineSegment(prevLineIndex),
			getTabSize());
		return getIdealIndentForLine(lineIndex, prevLineIndex,
			oldIndent);
	} 

	
	
	private int getIdealIndentForLine(int lineIndex, int prevLineIndex,
		int oldIndent)
	{
		int prevPrevLineIndex = prevLineIndex < 0 ? -1
			: getPriorNonEmptyLine(prevLineIndex);
		int newIndent = oldIndent;

		List<IndentRule> indentRules = getIndentRules(lineIndex);
		List<IndentAction> actions = new LinkedList<IndentAction>();
		for (int i = 0;i<indentRules.size();i++)
		{
			IndentRule rule = indentRules.get(i);
			rule.apply(this,lineIndex,prevLineIndex,
				prevPrevLineIndex,actions);
		}


		for (IndentAction action : actions)
		{
			newIndent = action.calculateIndent(this, lineIndex,
					oldIndent, newIndent);
			if (!action.keepChecking())
				break;
		}
		if (newIndent < 0)
			newIndent = 0;

		return newIndent;
	} 

	
	
	public int getVirtualWidth(int line, int column)
	{
		try
		{
			readLock();

			int start = getLineStartOffset(line);
			Segment seg = new Segment();
			getText(start,column,seg);

			return StandardUtilities.getVirtualWidth(seg,getTabSize());
		}
		finally
		{
			readUnlock();
		}
	} 

	
	
	public int getOffsetOfVirtualColumn(int line, int column,
		int[] totalVirtualWidth)
	{
		try
		{
			readLock();

			Segment seg = new Segment();
			getLineText(line,seg);

			return StandardUtilities.getOffsetOfVirtualColumn(seg,
				getTabSize(),column,totalVirtualWidth);
		}
		finally
		{
			readUnlock();
		}
	} 

	
	
	public void insertAtColumn(int line, int col, String str)
	{
		try
		{
			writeLock();

			int[] total = new int[1];
			int offset = getOffsetOfVirtualColumn(line,col,total);
			if(offset == -1)
			{
				offset = getLineEndOffset(line) - 1;
				str = StandardUtilities.createWhiteSpace(col - total[0],0) + str;
			}
			else
				offset += getLineStartOffset(line);

			insert(offset,str);
		}
		finally
		{
			writeUnlock();
		}
	} 

	
	
	public int insertIndented(int offset, String text)
	{
		try
		{
			beginCompoundEdit();

			
			int firstLine = getLineOfOffset(offset);
			CharSequence lineText = getLineSegment(firstLine);
			int leadingIndent
				= StandardUtilities.getLeadingWhiteSpaceWidth(
				lineText,getTabSize());

			String whiteSpace = StandardUtilities.createWhiteSpace(
				leadingIndent,getBooleanProperty("noTabs")
				? 0 : getTabSize());

			insert(offset,text);

			int lastLine = getLineOfOffset(offset + text.length());

			
			
			for(int i = firstLine + 1; i <= lastLine; i++)
			{
				insert(getLineStartOffset(i),whiteSpace);
			}

			return whiteSpace.length();
		}
		finally
		{
			endCompoundEdit();
		}
	} 

	
	
	public boolean isElectricKey(char ch)
	{
		return mode.isElectricKey(ch);
	}

	
	public boolean isElectricKey(char ch, int line)
	{
		TokenMarker.LineContext ctx = lineMgr.getLineContext(line);
		Mode mode = ModeProvider.instance.getMode(ctx.rules.getModeName());

		
		if (mode == null)
			return false;
		return mode.isElectricKey(ch);
	} 

	

	

	
	
	public void markTokens(int lineIndex, TokenHandler tokenHandler)
	{
		Segment seg = new Segment();

		if(lineIndex < 0 || lineIndex >= lineMgr.getLineCount())
			throw new ArrayIndexOutOfBoundsException(lineIndex);

		int firstInvalidLineContext = lineMgr.getFirstInvalidLineContext();
		int start;
		if(textMode || firstInvalidLineContext == -1)
		{
			start = lineIndex;
		}
		else
		{
			start = Math.min(firstInvalidLineContext,
				lineIndex);
		}

		if(Debug.TOKEN_MARKER_DEBUG)
			Log.log(Log.DEBUG,this,"tokenize from " + start + " to " + lineIndex);
		TokenMarker.LineContext oldContext = null;
		TokenMarker.LineContext context = null;
		for(int i = start; i <= lineIndex; i++)
		{
			getLineText(i,seg);

			oldContext = lineMgr.getLineContext(i);

			TokenMarker.LineContext prevContext = (
				(i == 0 || textMode) ? null
				: lineMgr.getLineContext(i - 1)
			);

			context = tokenMarker.markTokens(prevContext,
				(i == lineIndex ? tokenHandler
				: DummyTokenHandler.INSTANCE), seg);
			lineMgr.setLineContext(i,context);
		}

		int lineCount = lineMgr.getLineCount();
		if(lineCount - 1 == lineIndex)
			lineMgr.setFirstInvalidLineContext(-1);
		else if(oldContext != context)
			lineMgr.setFirstInvalidLineContext(lineIndex + 1);
		else if(firstInvalidLineContext == -1)
			;
		else
		{
			lineMgr.setFirstInvalidLineContext(Math.max(
				firstInvalidLineContext,lineIndex + 1));
		}
	} 

	
	public TokenMarker getTokenMarker()
	{
		return tokenMarker;
	} 

	
	public void setTokenMarker(TokenMarker tokenMarker)
	{
		TokenMarker oldTokenMarker = this.tokenMarker;

		this.tokenMarker = tokenMarker;

		
		if(oldTokenMarker != null && tokenMarker != oldTokenMarker)
		{
			lineMgr.setFirstInvalidLineContext(0);
		}
	} 

	
	
	public Position createPosition(int offset)
	{
		try
		{
			readLock();

			if(offset < 0 || offset > contentMgr.getLength())
				throw new ArrayIndexOutOfBoundsException(offset);

			return positionMgr.createPosition(offset);
		}
		finally
		{
			readUnlock();
		}
	} 

	

	

	
	
	public void propertiesChanged()
	{
		String folding = getStringProperty("folding");
		FoldHandler handler = FoldHandler.getFoldHandler(folding);

		if(handler != null)
		{
			setFoldHandler(handler);
		}
		else
		{
			if (folding != null)
				Log.log(Log.WARNING, this, "invalid 'folding' property: " + folding);
			setFoldHandler(new DummyFoldHandler());
		}
	} 

	
	
	public int getTabSize()
	{
		int tabSize = getIntegerProperty("tabSize",8);
		if(tabSize <= 0)
			return 8;
		else
			return tabSize;
	} 

	
	
	public int getIndentSize()
	{
		int indentSize = getIntegerProperty("indentSize",8);
		if(indentSize <= 0)
			return 8;
		else
			return indentSize;
	} 

	
	
	public Object getProperty(Object name)
	{
		synchronized(propertyLock)
		{
			
			PropValue o = properties.get(name);
			if(o != null)
				return o.value;

			
			if(!(name instanceof String))
				return null;

			Object retVal = getDefaultProperty((String)name);

			if(retVal == null)
				return null;
			else
			{
				properties.put(name,new PropValue(retVal,true));
				return retVal;
			}
		}
	} 

	
	public Object getDefaultProperty(String key)
	{
		return null;
	} 

	
	
	public void setProperty(String name, Object value)
	{
		if(value == null)
			properties.remove(name);
		else
		{
			PropValue test = properties.get(name);
			if(test == null)
				properties.put(name,new PropValue(value,false));
			else if(test.value.equals(value))
			{
				
			}
			else
			{
				test.value = value;
				test.defaultValue = false;
			}
		}
	} 

	
	public void setDefaultProperty(String name, Object value)
	{
		properties.put(name,new PropValue(value,true));
	} 

	
	
	public void unsetProperty(String name)
	{
		properties.remove(name);
	} 

	
	public void resetCachedProperties()
	{
		
		
		Iterator<PropValue> iter = properties.values().iterator();
		while(iter.hasNext())
		{
			PropValue value = iter.next();
			if(value.defaultValue)
				iter.remove();
		}
	} 

	
	
	public String getStringProperty(String name)
	{
		Object obj = getProperty(name);
		if(obj != null)
			return obj.toString();
		else
			return null;
	} 

	
	
	public void setStringProperty(String name, String value)
	{
		setProperty(name,value);
	} 

	
	
	public boolean getBooleanProperty(String name)
	{
		return getBooleanProperty(name, false);
	}

	
	public boolean getBooleanProperty(String name, boolean def)
	{
		Object obj = getProperty(name);
		return StandardUtilities.getBoolean(obj, def);
	} 

	
	
	public void setBooleanProperty(String name, boolean value)
	{
		setProperty(name,value ? Boolean.TRUE : Boolean.FALSE);
	} 

	
	
	public int getIntegerProperty(String name, int defaultValue)
	{
		boolean defaultValueFlag;
		Object obj;
		PropValue value = properties.get(name);
		if(value != null)
		{
			obj = value.value;
			defaultValueFlag = value.defaultValue;
		}
		else
		{
			obj = getProperty(name);
			
			defaultValueFlag = true;
		}

		if(obj == null)
			return defaultValue;
		else if(obj instanceof Number)
			return ((Number)obj).intValue();
		else
		{
			try
			{
				int returnValue = Integer.parseInt(
					obj.toString().trim());
				properties.put(name,new PropValue(
					returnValue,
					defaultValueFlag));
				return returnValue;
			}
			catch(Exception e)
			{
				return defaultValue;
			}
		}
	} 

	
	
	public void setIntegerProperty(String name, int value)
	{
		setProperty(name,value);
	} 

	
	
	public Pattern getPatternProperty(String name, int flags)
	{
		synchronized(propertyLock)
		{
			boolean defaultValueFlag;
			Object obj;
			PropValue value = properties.get(name);
			if(value != null)
			{
				obj = value.value;
				defaultValueFlag = value.defaultValue;
			}
			else
			{
				obj = getProperty(name);
				
				defaultValueFlag = true;
			}

			if(obj == null)
				return null;
			else if (obj instanceof Pattern)
				return (Pattern) obj;
			else
			{
				Pattern re = Pattern.compile(obj.toString(),flags);
				properties.put(name,new PropValue(re,
					defaultValueFlag));
				return re;
			}
		}
	} 

	
	
	public ParserRuleSet getRuleSetAtOffset(int offset)
	{
		int line = getLineOfOffset(offset);
		offset -= getLineStartOffset(line);
		if(offset != 0)
			offset--;

		DefaultTokenHandler tokens = new DefaultTokenHandler();
		markTokens(line,tokens);
		Token token = TextUtilities.getTokenAtOffset(tokens.getTokens(),offset);
		return token.rules;
	} 

	
	
	public KeywordMap getKeywordMapAtOffset(int offset)
	{
		return getRuleSetAtOffset(offset).getKeywords();
	} 

	
	
	public String getContextSensitiveProperty(int offset, String name)
	{
		ParserRuleSet rules = getRuleSetAtOffset(offset);

		Object value = null;

		Map<String, String> rulesetProps = rules.getProperties();
		if(rulesetProps != null)
			value = rulesetProps.get(name);

		if(value == null)
			return null;
		else
			return String.valueOf(value);
	} 

	
	
	public Mode getMode()
	{
		return mode;
	} 

	
	
	public void setMode(String mode)
	{
		setMode(ModeProvider.instance.getMode(mode));
	}

	
	public void setMode(Mode mode)
	{
		
		if(mode == null)
			throw new NullPointerException("Mode must be non-null");

		this.mode = mode;

		textMode = "text".equals(mode.getName());

		setTokenMarker(mode.getTokenMarker());

		resetCachedProperties();
		propertiesChanged();
	} 

	

	

	
	
	public boolean isFoldStart(int line)
	{
		return line != getLineCount() - 1
			&& getFoldLevel(line) < getFoldLevel(line + 1);
	} 

	
	
	public boolean isFoldEnd(int line)
	{
		return line != getLineCount() - 1
			&& getFoldLevel(line) > getFoldLevel(line + 1);
	} 

	
	
	public void invalidateCachedFoldLevels()
	{
		lineMgr.setFirstInvalidFoldLevel(0);
		fireFoldLevelChanged(0,getLineCount());
	} 

	
	
	public int getFoldLevel(int line)
	{
		if(line < 0 || line >= lineMgr.getLineCount())
			throw new ArrayIndexOutOfBoundsException(line);

		if(foldHandler instanceof DummyFoldHandler)
			return 0;

		int firstInvalidFoldLevel = lineMgr.getFirstInvalidFoldLevel();
		if(firstInvalidFoldLevel == -1 || line < firstInvalidFoldLevel)
		{
			return lineMgr.getFoldLevel(line);
		}
		else
		{
			if(Debug.FOLD_DEBUG)
				Log.log(Log.DEBUG,this,"Invalid fold levels from " + firstInvalidFoldLevel + " to " + line);

			int newFoldLevel = 0;
			boolean changed = false;
			int firstUpdatedFoldLevel = firstInvalidFoldLevel;

			for(int i = firstInvalidFoldLevel; i <= line; i++)
			{
				Segment seg = new Segment();
				newFoldLevel = foldHandler.getFoldLevel(this,i,seg);
				if(newFoldLevel != lineMgr.getFoldLevel(i))
				{
					if(Debug.FOLD_DEBUG)
						Log.log(Log.DEBUG,this,i + " fold level changed");
					changed = true;
					
					if (i == firstInvalidFoldLevel)
					{
						List<Integer> precedingFoldLevels =
							foldHandler.getPrecedingFoldLevels(
								this,i,seg,newFoldLevel);
						if (precedingFoldLevels != null)
						{
							int j = i;
							for (Integer foldLevel: precedingFoldLevels)
							{
								j--;
								lineMgr.setFoldLevel(j,foldLevel.intValue());
							}
							if (j < firstUpdatedFoldLevel)
								firstUpdatedFoldLevel = j;
						}
					}
				}
				lineMgr.setFoldLevel(i,newFoldLevel);
			}

			if(line == lineMgr.getLineCount() - 1)
				lineMgr.setFirstInvalidFoldLevel(-1);
			else
				lineMgr.setFirstInvalidFoldLevel(line + 1);

			if(changed)
			{
				if(Debug.FOLD_DEBUG)
					Log.log(Log.DEBUG,this,"fold level changed: " + firstUpdatedFoldLevel + ',' + line);
				fireFoldLevelChanged(firstUpdatedFoldLevel,line);
			}

			return newFoldLevel;
		}
	} 

	
	
	public int[] getFoldAtLine(int line)
	{
		int start, end;

		if(isFoldStart(line))
		{
			start = line;
			int foldLevel = getFoldLevel(line);

			line++;

			while(getFoldLevel(line) > foldLevel)
			{
				line++;

				if(line == getLineCount())
					break;
			}

			end = line - 1;
		}
		else
		{
			start = line;
			int foldLevel = getFoldLevel(line);
			while(getFoldLevel(start) >= foldLevel)
			{
				if(start == 0)
					break;
				else
					start--;
			}

			end = line;
			while(getFoldLevel(end) >= foldLevel)
			{
				end++;

				if(end == getLineCount())
					break;
			}

			end--;
		}

		while(getLineLength(end) == 0 && end > start)
			end--;

		return new int[] { start, end };
	} 

	
	
	public FoldHandler getFoldHandler()
	{
		return foldHandler;
	} 

	
	
	public void setFoldHandler(FoldHandler foldHandler)
	{
		FoldHandler oldFoldHandler = this.foldHandler;

		if(foldHandler.equals(oldFoldHandler))
			return;

		this.foldHandler = foldHandler;

		lineMgr.setFirstInvalidFoldLevel(0);

		fireFoldHandlerChanged();
	} 

	

	

	
	
	public void undo(TextArea textArea)
	{
		if(undoMgr == null)
			return;

		if(!isEditable())
		{
			textArea.getToolkit().beep();
			return;
		}

		try
		{
			writeLock();

			undoInProgress = true;
			fireBeginUndo();
			int caret = undoMgr.undo();
			if(caret == -1)
				textArea.getToolkit().beep();
			else
				textArea.setCaretPosition(caret);

			fireEndUndo();
			fireTransactionComplete();
		}
		finally
		{
			undoInProgress = false;

			writeUnlock();
		}
	} 

	
	
	public void redo(TextArea textArea)
	{
		if(undoMgr == null)
			return;

		if(!isEditable())
		{
			Toolkit.getDefaultToolkit().beep();
			return;
		}

		try
		{
			writeLock();

			undoInProgress = true;
			fireBeginRedo();
			int caret = undoMgr.redo();
			if(caret == -1)
				textArea.getToolkit().beep();
			else
				textArea.setCaretPosition(caret);

			fireEndRedo();
			fireTransactionComplete();
		}
		finally
		{
			undoInProgress = false;

			writeUnlock();
		}
	} 

	
	
	public boolean isTransactionInProgress()
	{
		return transaction || undoInProgress || insideCompoundEdit();
	} 

	
	
	public void beginCompoundEdit()
	{
		try
		{
			writeLock();

			undoMgr.beginCompoundEdit();
		}
		finally
		{
			writeUnlock();
		}
	} 

	
	
	public void endCompoundEdit()
	{
		try
		{
			writeLock();

			undoMgr.endCompoundEdit();

			if(!insideCompoundEdit())
				fireTransactionComplete();
		}
		finally
		{
			writeUnlock();
		}
	}

	
	
	public boolean insideCompoundEdit()
	{
		return undoMgr.insideCompoundEdit();
	} 

	
	
	public boolean isUndoInProgress()
	{
		return undoInProgress;
	} 

	
	
	public Object getUndoId()
	{
		return undoMgr.getUndoId();
	} 

	

	
	public static final int NORMAL_PRIORITY = 0;
	public static final int HIGH_PRIORITY = 1;

	static class Listener
	{
		BufferListener listener;
		int priority;

		Listener(BufferListener listener, int priority)
		{
			this.listener = listener;
			this.priority = priority;
		}
	}

	
	
	public void addBufferListener(BufferListener listener,
		int priority)
	{
		Listener l = new Listener(listener,priority);
		for(int i = 0; i < bufferListeners.size(); i++)
		{
			Listener _l = bufferListeners.get(i);
			if(_l.priority < priority)
			{
				bufferListeners.add(i,l);
				return;
			}
		}
		bufferListeners.add(l);
	}

	
	public void addBufferListener(BufferListener listener)
	{
		addBufferListener(listener,NORMAL_PRIORITY);
	} 

	
	
	public void removeBufferListener(BufferListener listener)
	{
		for(int i = 0; i < bufferListeners.size(); i++)
		{
			if(bufferListeners.get(i).listener == listener)
			{
				bufferListeners.remove(i);
				return;
			}
		}
	} 

	
	
	public BufferListener[] getBufferListeners()
	{
		BufferListener[] returnValue
			= new BufferListener[
			bufferListeners.size()];
		for(int i = 0; i < returnValue.length; i++)
		{
			returnValue[i] = bufferListeners.get(i).listener;
		}
		return returnValue;
	} 

	
	
	public void addBufferUndoListener(BufferUndoListener listener)
	{
		undoListeners.add(listener);
	} 

	
	
	public void removeBufferUndoListener(BufferUndoListener listener)
	{
		undoListeners.remove(listener);
	} 

	
	
	public void setUndoLimit(int limit)
	{
		if (undoMgr != null)
			undoMgr.setLimit(limit);
	} 

	
	
	public boolean canUndo()
	{
		if (undoMgr == null)
			return false;
		return undoMgr.canUndo();
	} 

	
	
	public boolean canRedo()
	{
		if (undoMgr == null)
			return false;
		return undoMgr.canRedo();
	} 

	

	

	protected Mode mode;
	protected boolean textMode;
	protected UndoManager undoMgr;

	

	
	protected void fireFoldLevelChanged(int start, int end)
	{
		for(int i = 0; i < bufferListeners.size(); i++)
		{
			BufferListener listener = getListener(i);
			try
			{
				listener.foldLevelChanged(this,start,end);
			}
			catch(Throwable t)
			{
				Log.log(Log.ERROR,this,"Exception while sending buffer event to "+ listener +" :");
				Log.log(Log.ERROR,this,t);
			}
		}
	} 

	
	protected void fireContentInserted(int startLine, int offset,
		int numLines, int length)
	{
		for(int i = 0; i < bufferListeners.size(); i++)
		{
			BufferListener listener = getListener(i);
			try
			{
				listener.contentInserted(this,startLine,
					offset,numLines,length);
			}
			catch(Throwable t)
			{
				Log.log(Log.ERROR,this,"Exception while sending buffer event to "+ listener +" :");
				Log.log(Log.ERROR,this,t);
			}
		}
	} 

	
	protected void fireContentRemoved(int startLine, int offset,
		int numLines, int length)
	{
		for(int i = 0; i < bufferListeners.size(); i++)
		{
			BufferListener listener = getListener(i);
			try
			{
				listener.contentRemoved(this,startLine,
					offset,numLines,length);
			}
			catch(Throwable t)
			{
				Log.log(Log.ERROR,this,"Exception while sending buffer event to "+ listener +" :");
				Log.log(Log.ERROR,this,t);
			}
		}
	} 

	
	protected void firePreContentInserted(int startLine, int offset,
		int numLines, int length)
	{
		for(int i = 0; i < bufferListeners.size(); i++)
		{
			BufferListener listener = getListener(i);
			try
			{
				listener.preContentInserted(this,startLine,
					offset,numLines,length);
			}
			catch(Throwable t)
			{
				Log.log(Log.ERROR,this,"Exception while sending buffer event to "+ listener +" :");
				Log.log(Log.ERROR,this,t);
			}
		}
	} 

	
	protected void firePreContentRemoved(int startLine, int offset,
		int numLines, int length)
	{
		for(int i = 0; i < bufferListeners.size(); i++)
		{
			BufferListener listener = getListener(i);
			try
			{
				listener.preContentRemoved(this,startLine,
					offset,numLines,length);
			}
			catch(Throwable t)
			{
				Log.log(Log.ERROR,this,"Exception while sending buffer event to "+ listener +" :");
				Log.log(Log.ERROR,this,t);
			}
		}
	} 

	
	protected void fireBeginUndo()
	{
		for (BufferUndoListener listener: undoListeners)
		{
			try
			{
				listener.beginUndo(this);
			}
			catch(Throwable t)
			{
				Log.log(Log.ERROR,this,"Exception while sending buffer undo event to "+ listener +" :");
				Log.log(Log.ERROR,this,t);
			}
		}
	} 

	
	protected void fireEndUndo()
	{
		for (BufferUndoListener listener: undoListeners)
		{
			try
			{
				listener.endUndo(this);
			}
			catch(Throwable t)
			{
				Log.log(Log.ERROR,this,"Exception while sending buffer undo event to "+ listener +" :");
				Log.log(Log.ERROR,this,t);
			}
		}
	} 

	
	protected void fireBeginRedo()
	{
		for (BufferUndoListener listener: undoListeners)
		{
			try
			{
				listener.beginRedo(this);
			}
			catch(Throwable t)
			{
				Log.log(Log.ERROR,this,"Exception while sending buffer begin redo event to "+ listener +" :");
				Log.log(Log.ERROR,this,t);
			}
		}
	} 
	
	
	protected void fireEndRedo()
	{
		for (BufferUndoListener listener: undoListeners)
		{
			try
			{
				listener.endRedo(this);
			}
			catch(Throwable t)
			{
				Log.log(Log.ERROR,this,"Exception while sending buffer end redo event to "+ listener +" :");
				Log.log(Log.ERROR,this,t);
			}
		}
	} 
	
	
	protected void fireTransactionComplete()
	{
		for(int i = 0; i < bufferListeners.size(); i++)
		{
			BufferListener listener = getListener(i);
			try
			{
				listener.transactionComplete(this);
			}
			catch(Throwable t)
			{
				Log.log(Log.ERROR,this,"Exception while sending buffer event to "+ listener +" :");
				Log.log(Log.ERROR,this,t);
			}
		}
	} 

	
	protected void fireFoldHandlerChanged()
	{
		for(int i = 0; i < bufferListeners.size(); i++)
		{
			BufferListener listener = getListener(i);
			try
			{
				listener.foldHandlerChanged(this);
			}
			catch(Throwable t)
			{
				Log.log(Log.ERROR,this,"Exception while sending buffer event to "+ listener +" :");
				Log.log(Log.ERROR,this,t);
			}
		}
	} 

	
	protected void fireBufferLoaded()
	{
		for(int i = 0; i < bufferListeners.size(); i++)
		{
			BufferListener listener = getListener(i);
			try
			{
				listener.bufferLoaded(this);
			}
			catch(Throwable t)
			{
				Log.log(Log.ERROR,this,"Exception while sending buffer event to "+ listener +" :");
				Log.log(Log.ERROR,this,t);
			}
		}
	} 

	

	
	protected boolean isFileReadOnly()
	{
		return readOnly;
	} 

	
	protected void setFileReadOnly(boolean readOnly)
	{
		this.readOnly = readOnly;
	} 

	
	protected void loadText(Segment seg, IntegerArray endOffsets)
	{
		if(seg == null)
			seg = new Segment(new char[1024],0,0);

		if(endOffsets == null)
		{
			endOffsets = new IntegerArray();
			endOffsets.add(1);
		}

		try
		{
			writeLock();

			
			
			int length = getLength();

			firePreContentRemoved(0,0,getLineCount()
				- 1,length);

			contentMgr.remove(0,length);
			lineMgr.contentRemoved(0,0,getLineCount()
				- 1,length);
			positionMgr.contentRemoved(0,length);
			fireContentRemoved(0,0,getLineCount()
				- 1,length);

			firePreContentInserted(0, 0, endOffsets.getSize() - 1, seg.count - 1);
			
			
			
			contentMgr._setContent(seg.array,seg.count);

			lineMgr._contentInserted(endOffsets);
			positionMgr.contentInserted(0,seg.count);

			fireContentInserted(0,0,
				endOffsets.getSize() - 1,
				seg.count - 1);
		}
		finally
		{
			writeUnlock();
		}
	} 

	
	protected void invalidateFoldLevels()
	{
		lineMgr.setFirstInvalidFoldLevel(0);
	} 

	
	protected void parseBufferLocalProperties()
	{
		int lastLine = Math.min(9,getLineCount() - 1);
		parseBufferLocalProperties(getSegment(0,getLineEndOffset(lastLine) - 1));

		
		
		int firstLine = Math.max(lastLine + 1, getLineCount() - 10);
		if(firstLine < getLineCount())
		{
			int length = getLineEndOffset(getLineCount() - 1)
				- (getLineStartOffset(firstLine) + 1);
			parseBufferLocalProperties(getSegment(getLineStartOffset(firstLine),length));
		}
	} 

	
	protected static class PropValue
	{
		PropValue(Object value, boolean defaultValue)
		{
			if(value == null)
				throw new NullPointerException();
			this.value = value;
			this.defaultValue = defaultValue;
		}

		Object value;

		
		boolean defaultValue;

		
		public String toString()
		{
			return value.toString();
		}
	} 

	

	
	private List<Listener> bufferListeners;
	private List<BufferUndoListener> undoListeners;
	private final ReentrantReadWriteLock lock;
	private ContentManager contentMgr;
	private LineManager lineMgr;
	private PositionManager positionMgr;
	private FoldHandler foldHandler;
	private IntegerArray integerArray;
	private TokenMarker tokenMarker;
	private boolean undoInProgress;
	private boolean dirty;
	private boolean readOnly;
	private boolean readOnlyOverride;
	private boolean transaction;
	private boolean loading;
	private boolean io;
	private final Map<Object, PropValue> properties;
	private final Object propertyLock;

	
	private BufferListener getListener(int index)
	{
		return bufferListeners.get(index).listener;
	} 

	
	private void contentInserted(int offset, int length,
		IntegerArray endOffsets)
	{
		try
		{
			transaction = true;

			int startLine = lineMgr.getLineOfOffset(offset);
			int numLines = endOffsets.getSize();

			if (!loading)
			{
				firePreContentInserted(startLine, offset, numLines, length);
			}

			lineMgr.contentInserted(startLine,offset,numLines,length,
				endOffsets);
			positionMgr.contentInserted(offset,length);

			setDirty(true);

			if(!loading)
			{
				fireContentInserted(startLine,offset,numLines,length);

				if(!undoInProgress && !insideCompoundEdit())
					fireTransactionComplete();
			}

		}
		finally
		{
			transaction = false;
		}
	} 

	
	private void parseBufferLocalProperties(CharSequence prop)
	{
		StringBuilder buf = new StringBuilder();
		String name = null;
		boolean escape = false;
		for(int i = 0; i < prop.length(); i++)
		{
			char c = prop.charAt(i);
			switch(c)
			{
			case ':':
				if(escape)
				{
					escape = false;
					buf.append(':');
					break;
				}
				if(name != null)
				{
					
					
					
					
					
					properties.put(name,new PropValue(buf.toString(),false));
					name = null;
				}
				buf.setLength(0);
				break;
			case '=':
				if(escape)
				{
					escape = false;
					buf.append('=');
					break;
				}
				name = buf.toString();
				buf.setLength(0);
				break;
			case '\\':
				if(escape)
					buf.append('\\');
				escape = !escape;
				break;
			case 'n':
				if(escape)
				{	buf.append('\n');
					escape = false;
					break;
				}
			case 'r':
				if(escape)
				{	buf.append('\r');
					escape = false;
					break;
				}
			case 't':
				if(escape)
				{
					buf.append('\t');
					escape = false;
					break;
				}
			default:
				buf.append(c);
				break;
			}
		}
	} 

	
	private List<IndentRule> getIndentRules(int line)
	{
		String modeName = null;
		TokenMarker.LineContext ctx = lineMgr.getLineContext(line);
		if (ctx != null && ctx.rules != null)
			modeName = ctx.rules.getModeName();
		if (modeName == null)
			modeName = tokenMarker.getMainRuleSet().getModeName();
		return ModeProvider.instance.getMode(modeName).getIndentRules();
	} 

	
}
