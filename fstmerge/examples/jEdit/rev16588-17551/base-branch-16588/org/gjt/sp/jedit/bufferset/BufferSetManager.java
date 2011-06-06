
package org.gjt.sp.jedit.bufferset;


import org.gjt.sp.jedit.*;
import org.gjt.sp.jedit.msg.ViewUpdate;
import org.gjt.sp.jedit.msg.EditPaneUpdate;
import org.gjt.sp.jedit.msg.PropertiesChanged;
import org.gjt.sp.util.Log;

import java.util.*;



public class BufferSetManager implements EBComponent
{
	
	public enum NewBufferSetAction
	{
		empty, copy, currentbuffer;

		public static NewBufferSetAction fromString(String s)
		{
			NewBufferSetAction[] newBufferSetActions = values();
			for (NewBufferSetAction newBufferSetAction : newBufferSetActions)
			{
				if (newBufferSetAction.getName().equals(s))
					return newBufferSetAction;
			}

			return currentbuffer;
		}

		public String getName()
		{
			return super.toString();
		}

		@Override
		public String toString()
		{
			return jEdit.getProperty("options.editpane.bufferset.newbufferset." + getName());
		}
	} 

	
	public BufferSetManager()
	{
		EditBus.addToBus(this);
	} 

	
	public void handleMessage(EBMessage message)
	{
		if (message instanceof ViewUpdate)
		{
			ViewUpdate viewUpdate = (ViewUpdate) message;
			if (viewUpdate.getWhat() == ViewUpdate.CLOSED)
			{
				View view = viewUpdate.getView();
				
				BufferSet viewBufferSet = view.getLocalBufferSet();
				viewBufferSet.getAllBuffers(new BufferSetClosed(viewBufferSet));
			}
		}
		else if (message instanceof EditPaneUpdate)
		{
			EditPaneUpdate editPaneUpdate = (EditPaneUpdate) message;
			if (editPaneUpdate.getWhat() == EditPaneUpdate.DESTROYED)
			{
				EditPane editPane = editPaneUpdate.getEditPane();
				
				if (editPane.getBufferSetScope() == BufferSet.Scope.editpane)
				{
					BufferSet editPaneBufferSet = editPane.getBufferSet();
					editPaneBufferSet.getAllBuffers(new BufferSetClosed(editPaneBufferSet));
				}
			}
		}
		else if (message instanceof PropertiesChanged)
		{
			
			
			visit(new BufferSetVisitor()
			{
				public void visit(BufferSet bufferSet)
				{
					bufferSet.handleMessage();
				}
			});
		}

	} 

	
	
	public void mergeBufferSet(BufferSet target, BufferSet source)
	{
		Buffer[] buffers = source.getAllBuffers();
		for (Buffer buffer : buffers)
		{
			addBuffer(target, buffer);
		}
	} 

	
	
	public int countBufferSets(Buffer buffer)
	{
		return getOwners(buffer).size();
	} 

	
	
	public void addBuffer(View view, Buffer buffer)
	{
		EditPane editPane = view == null ? null : view.getEditPane();
		addBuffer(editPane, buffer);
	}

	
	public void addBuffer(EditPane editPane, Buffer buffer)
	{
		if (editPane == null)
		{
			addBuffer(jEdit.getGlobalBufferSet(), buffer);
		}
		else
		{
			BufferSet bufferSet = editPane.getBufferSet();
			addBuffer(bufferSet, buffer);
		}
	}

	
	public void addBuffer(BufferSet bufferSet, Buffer buffer)
	{
		bufferSet.addBuffer(buffer);
	} 

	
	
	public void addAllBuffers(BufferSet bufferSet)
	{
		Buffer[] buffers = jEdit.getBuffers();
		for (Buffer buffer : buffers)
		{
			if (!buffer.isClosed())
				addBuffer(bufferSet, buffer);
		}
	} 

	
	
	public void moveBuffer(EditPane editPane,
		int oldPosition, int newPosition)
	{
		editPane.getBufferSet().moveBuffer(oldPosition, newPosition);
	} 

	
	
	public void removeBuffer(EditPane editPane, Buffer buffer)
	{
		BufferSet bufferSet = editPane.getBufferSet();
		removeBuffer(bufferSet, buffer);
	}

	
	void removeBuffer(BufferSet bufferSet, Buffer buffer)
	{
		Log.log(Log.DEBUG, this, "removeBuffer("+bufferSet+','+buffer+')');
		Set<BufferSet> owners = getOwners(buffer);
		owners.remove(bufferSet);
		bufferSet.removeBuffer(buffer);

		if (owners.isEmpty())
		{
			Log.log(Log.DEBUG, this, "Buffer:"+buffer+" is in no bufferSet anymore, closing it");
			jEdit._closeBuffer(null, buffer);
		}
		if (bufferSet.size() == 0 && bufferSet.hasListeners())
		{
			int untitledCount = jEdit.getNextUntitledBufferId();
			Buffer newEmptyBuffer = jEdit.openTemporary(jEdit.getActiveView(), null,
								    "Untitled-" + untitledCount,true, null);
			jEdit.commitTemporary(newEmptyBuffer);
			jEdit.getBufferSetManager().addBuffer(bufferSet, newEmptyBuffer);
		}
	} 

	
	
	public void removeBuffer(Buffer buffer)
	{
		for (BufferSet bufferSet : getOwners(buffer))
		{
			bufferSet.removeBuffer(buffer);
			if (bufferSet.size() == 0 && bufferSet.hasListeners())
			{
				int untitledCount = jEdit.getNextUntitledBufferId();
				Buffer newEmptyBuffer = jEdit.openTemporary(jEdit.getActiveView(), null,
									    "Untitled-" + untitledCount,true, null);
				jEdit.commitTemporary(newEmptyBuffer);
				jEdit.getBufferSetManager().addBuffer(bufferSet, newEmptyBuffer);
			}
		}

	} 

	
	
	public void visit(BufferSetVisitor visitor)
	{
		BufferSet global = jEdit.getGlobalBufferSet();
		visitor.visit(jEdit.getGlobalBufferSet());
		for (View view: jEdit.getViews())
		{
			BufferSet viewLocal = view.getLocalBufferSet();
			if (viewLocal != null)
			{
				visitor.visit(viewLocal);
			}
			for (EditPane editPane: view.getEditPanes())
			{
				BufferSet used = editPane.getBufferSet();
				if (used != global && used != viewLocal)
				{
					visitor.visit(used);
				}
			}
		}
	} 

	

	
	
	private Set<BufferSet> getOwners(Buffer buffer)
	{
		final Set<BufferSet> candidates = new HashSet<BufferSet>();
		
		visit(new BufferSetVisitor()
		{
			public void visit(BufferSet bufferSet)
			{
				candidates.add(bufferSet);
			}
		});
		
		Iterator<BufferSet> i = candidates.iterator();
		while (i.hasNext())
		{
			if (i.next().indexOf(buffer) == -1)
			{
				i.remove();
			}
		}
		
		return candidates;
	} 

	
	public static interface BufferSetVisitor
	{
		void visit(BufferSet bufferSet);
	} 

	
	private class BufferSetClosed extends BufferSetAdapter
	{
		
		private final BufferSet closedBufferSet;

		
		private BufferSetClosed(BufferSet closedBufferSet)
		{
			this.closedBufferSet = closedBufferSet;
		} 

		
		@Override
		public void bufferAdded(Buffer buffer, int index)
		{
			Set<BufferSet> owners = getOwners(buffer);
			owners.remove(closedBufferSet);
			if (owners.isEmpty())
			{
				Log.log(Log.MESSAGE, this, "The buffer " +
					buffer + " was removed from a BufferSet, closing it");
				jEdit._closeBuffer(null, buffer);
			}
		} 

	} 

	
}
