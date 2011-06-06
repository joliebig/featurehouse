

package org.gjt.sp.jedit.buffer;


import java.util.ArrayList;
import org.gjt.sp.jedit.textarea.JEditTextArea;
import org.gjt.sp.jedit.Buffer;
import org.gjt.sp.util.Log;



public class UndoManager
{
	
	public UndoManager(Buffer buffer)
	{
		this.buffer = buffer;
		undos = new ArrayList(100);
	} 

	
	public void setLimit(int limit)
	{
		this.limit = limit;
	} 

	
	public void clear()
	{
		undos.clear();
		undoPos = undoCount = 0;
	} 

	
	public boolean undo(JEditTextArea textArea)
	{
		if(insideCompoundEdit())
			throw new InternalError("Unbalanced begin/endCompoundEdit()");

		if(undoPos == 0)
			return false;
		else
		{
			boolean dirty = buffer.isDirty();
			Edit edit = (Edit)undos.get(--undoPos);
			int caret = edit.undo();
			if(caret != -1)
				textArea.setCaretPosition(caret);
			return true;
		}
	} 

	
	public boolean redo(JEditTextArea textArea)
	{
		if(insideCompoundEdit())
			throw new InternalError("Unbalanced begin/endCompoundEdit()");

		if(undoPos == undoCount)
			return false;
		else
		{
			Edit edit = (Edit)undos.get(undoPos++);
			int caret = edit.redo();
			if(caret != -1)
				textArea.setCaretPosition(caret);
			return true;
		}
	} 

	
	public void beginCompoundEdit()
	{
		if(compoundEditCount == 0)
			compoundEdit = new CompoundEdit();

		compoundEditCount++;
	} 

	
	public void endCompoundEdit()
	{
		if(compoundEditCount == 0)
		{
			Log.log(Log.WARNING,this,new Exception("Unbalanced begin/endCompoundEdit()"));
			return;
		}
		else if(compoundEditCount == 1)
		{
			switch(compoundEdit.undos.size())
			{
			case 0:
				;
				break;
			case 1:
				addEdit((Edit)compoundEdit.undos.get(0));
				break;
			default:
				addEdit(compoundEdit);
			}

			compoundEdit = null;
		}

		compoundEditCount--;
	} 

	
	public boolean insideCompoundEdit()
	{
		return compoundEditCount != 0;
	} 

	
	public void contentInserted(int offset, int length, String text, boolean clearDirty)
	{
		Edit toMerge = getLastEdit();

		if(!clearDirty && toMerge instanceof Insert)
		{
			Insert ins = (Insert)toMerge;
			if(ins.offset == offset)
			{
				ins.str = text.concat(ins.str);
				ins.length += length;
				return;
			}
			else if(ins.offset + ins.length == offset)
			{
				ins.str = ins.str.concat(text);
				ins.length += length;
				return;
			}
		}

		Insert ins = new Insert(offset,length,text,clearDirty);
		if(clearDirty)
		{
			if(clearDirtyEdit != null)
				clearDirtyEdit.clearDirty = false;
			clearDirtyEdit = ins;
		}

		if(compoundEdit != null)
			compoundEdit.undos.add(ins);
		else
			addEdit(ins);
	} 

	
	public void contentRemoved(int offset, int length, String text, boolean clearDirty)
	{
		Edit toMerge = getLastEdit();

		if(!clearDirty && toMerge instanceof Remove)
		{
			Remove rem = (Remove)toMerge;
			if(rem.offset == offset)
			{
				rem.str = rem.str.concat(text);
				rem.length += length;
				return;
			}
			else if(offset + length == rem.offset)
			{
				rem.str = text.concat(rem.str);
				rem.length += length;
				rem.offset = offset;
				return;
			}
		}

		Remove rem = new Remove(offset,length,text,clearDirty);
		if(clearDirty)
		{
			if(clearDirtyEdit != null)
				clearDirtyEdit.clearDirty = false;
			clearDirtyEdit = rem;
		}

		if(compoundEdit != null)
			compoundEdit.undos.add(rem);
		else
			addEdit(rem);
	} 

	
	public void bufferSaved()
	{
		if(clearDirtyEdit != null)
		{
			clearDirtyEdit.clearDirty = false;
			clearDirtyEdit = null;
		}
	} 

	

	
	private Buffer buffer;
	private ArrayList undos;
	private int limit;
	private int undoPos;
	private int undoCount;
	private int compoundEditCount;
	private CompoundEdit compoundEdit;
	private Edit clearDirtyEdit;
	

	
	private void addEdit(Edit edit)
	{
		
		undos.add(undoPos++,edit);

		for(int i = undoCount - 1; i >= undoPos; i--)
		{
			
			undos.remove(i);
		}

		if(undoPos > limit)
		{
			
			undos.remove(0);
			undoPos--;
		}

		undoCount = undoPos;
	} 

	
	private Edit getLastEdit()
	{
		if(compoundEdit != null)
		{
			int size = compoundEdit.undos.size();
			if(size != 0)
				return (Edit)compoundEdit.undos.get(size - 1);
			else
				return null;
		}
		else if(undoCount != 0 && undoPos != 0)
		{
			Edit e = (Edit)undos.get(undoPos - 1);
			if(e instanceof CompoundEdit)
			{
				CompoundEdit c = (CompoundEdit)e;
				return (Edit)c.undos.get(c.undos.size() - 1);
			}
		}

		if(undoPos != 0)
			return (Edit)undos.get(undoPos - 1);
		else
			return null;
	} 

	

	

	
	abstract class Edit
	{
		
		abstract int undo();
		

		
		abstract int redo();
		

		boolean clearDirty;
	} 

	
	class Insert extends Edit
	{
		
		Insert(int offset, int length, String str, boolean clearDirty)
		{
			this.offset = offset;
			this.length = length;
			this.str = str;
			this.clearDirty = clearDirty;
		} 

		
		int undo()
		{
			buffer.remove(offset,length);
			if(clearDirty)
				buffer.setDirty(false);
			return offset;
		} 

		
		int redo()
		{
			buffer.insert(offset,str);
			if(clearDirty)
				buffer.setDirty(false);
			return offset + length;
		} 

		int offset;
		int length;
		String str;
	} 

	
	class Remove extends Edit
	{
		
		Remove(int offset, int length, String str, boolean clearDirty)
		{
			this.offset = offset;
			this.length = length;
			this.str = str;
			this.clearDirty = clearDirty;
		} 

		
		int undo()
		{
			buffer.insert(offset,str);
			if(clearDirty)
				buffer.setDirty(false);
			return offset + length;
		} 

		
		int redo()
		{
			buffer.remove(offset,length);
			return offset;
		} 

		int offset;
		int length;
		String str;
	} 

	
	class CompoundEdit extends Edit
	{
		
		public int undo()
		{
			int retVal = -1;
			for(int i = undos.size() - 1; i >= 0; i--)
			{
				retVal = ((Edit)undos.get(i)).undo();
			}
			return retVal;
		} 

		
		public int redo()
		{
			int retVal = -1;
			for(int i = 0; i < undos.size(); i++)
			{
				retVal = ((Edit)undos.get(i)).redo();
			}
			return retVal;
		} 

		ArrayList undos = new ArrayList();
	} 

	
}
