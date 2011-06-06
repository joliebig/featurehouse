
package org.gjt.sp.jedit.bufferset;


import org.gjt.sp.jedit.*;
import org.gjt.sp.jedit.msg.EditPaneUpdate;
import org.gjt.sp.jedit.msg.PropertiesChanged;
import org.gjt.sp.jedit.visitors.JEditVisitorAdapter;
import org.gjt.sp.util.Log;

import java.util.*;



public class BufferSetManager implements EBComponent
{
	
	public BufferSetManager()
	{
		EditBus.addToBus(this);
		scope = BufferSet.Scope.fromString(jEdit.getProperty("bufferset.scope", "global"));
	} 

	
	public void handleMessage(EBMessage message)
	{
		if (message instanceof EditPaneUpdate)
		{
			EditPaneUpdate editPaneUpdate = (EditPaneUpdate) message;
			if (editPaneUpdate.getWhat() == EditPaneUpdate.DESTROYED)
			{
				EditPane editPane = editPaneUpdate.getEditPane();
				BufferSet bufferSet = editPane.getBufferSet();
				Buffer[] allBuffers = bufferSet.getAllBuffers();
				for (Buffer buffer : allBuffers)
				{
					removeBuffer(bufferSet, buffer);
				}
			}
		}
		else if (message instanceof PropertiesChanged)
		{
			
			
			jEdit.visit(new JEditVisitorAdapter()
			{
				@Override
				public void visit(EditPane editPane)
				{
					editPane.getBufferSet().propertiesChanged();
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
			editPane = jEdit.getActiveView().getEditPane();
		}
		BufferSet bufferSet = editPane.getBufferSet();
		addBuffer(bufferSet, buffer);
	}

	
	public void addBuffer(BufferSet bufferSet, final Buffer buffer)
	{
		switch (scope)
		{
			case editpane:
				bufferSet.addBuffer(buffer);
				break;
			case view:
				EditPane owner = bufferSet.getOwner();
				EditPane[] editPanes = owner.getView().getEditPanes();
				for (EditPane editPane:editPanes)
				{
					if (editPane == null)
						continue;
					BufferSet bfs = editPane.getBufferSet();
					bfs.addBuffer(buffer);
				}
				break;
			case global:
				jEdit.visit(new JEditVisitorAdapter()
				{
					@Override
					public void visit(EditPane editPane)
					{
						BufferSet bfs = editPane.getBufferSet();
						bfs.addBuffer(buffer);
					}
				});
		}
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
		switch (scope)
		{
			case editpane:
				BufferSet bufferSet = editPane.getBufferSet();
				removeBuffer(bufferSet, buffer);
				break;
			case view:
				EditPane[] editPanes = editPane.getView().getEditPanes();
				for (EditPane pane : editPanes)
				{
					removeBuffer(pane.getBufferSet(), buffer);
				}
				break;
			case global:
				jEdit._closeBuffer(null, buffer);
				break;
		}
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
			Buffer newEmptyBuffer = createUntitledBuffer();
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
				Buffer newEmptyBuffer = createUntitledBuffer();
				jEdit.getBufferSetManager().addBuffer(bufferSet, newEmptyBuffer);
			}
		}

	} 

	public static Buffer createUntitledBuffer()
	{
		int untitledCount = jEdit.getNextUntitledBufferId();
		Buffer newEmptyBuffer = jEdit.openTemporary(jEdit.getActiveView(), null,
							    "Untitled-" + untitledCount,true, null);
		jEdit.commitTemporary(newEmptyBuffer);
		return newEmptyBuffer;
	}

	

	
	
	public Set<BufferSet> getOwners(Buffer buffer)
	{
		final Set<BufferSet> candidates = new HashSet<BufferSet>();
		
		jEdit.visit(new JEditVisitorAdapter()
		{
			@Override
			public void visit(EditPane editPane)
			{
				candidates.add(editPane.getBufferSet());
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

	public void setScope(BufferSet.Scope scope)
	{
		if (scope == this.scope)
			return;
		if (scope.compareTo(this.scope) > 0)
		{
			
			if (scope == BufferSet.Scope.global)
			{
				final Buffer[] buffers = jEdit.getBuffers();
				jEdit.visit(new JEditVisitorAdapter()
				{
					@Override
					public void visit(EditPane editPane)
					{
						BufferSet bufferSet = editPane.getBufferSet();
						for (Buffer buffer : buffers)
						{
							bufferSet.addBuffer(buffer);
						}
					}
				});
			}
			else
			{
				final Map<View,Set<Buffer>> buffersMap = new HashMap<View, Set<Buffer>>();
				jEdit.visit(new JEditVisitorAdapter()
				{
					@Override
					public void visit(EditPane editPane)
					{
						BufferSet bufferSet = editPane.getBufferSet();
						Buffer[] buffers = bufferSet.getAllBuffers();
						Set<Buffer> set = buffersMap.get(editPane.getView());
						if (set == null)
						{
							set = new HashSet<Buffer>();
							buffersMap.put(editPane.getView(), set);
						}
						set.addAll(Arrays.asList(buffers));
					}
				});
				jEdit.visit(new JEditVisitorAdapter()
				{
					@Override
					public void visit(EditPane editPane)
					{
						BufferSet bufferSet = editPane.getBufferSet();
						Set<Buffer> set = buffersMap.get(editPane.getView());
						while (set.iterator().hasNext())
						{
							Buffer buffer = set.iterator().next();
							bufferSet.addBuffer(buffer);
						}
					}
				});
			}
		}
		this.scope = scope;
		EditBus.send(new EditPaneUpdate(null, EditPaneUpdate.BUFFERSET_CHANGED));
	}

	public BufferSet.Scope getScope()
	{
		return scope;
	}

	
	private BufferSet.Scope scope;
}
