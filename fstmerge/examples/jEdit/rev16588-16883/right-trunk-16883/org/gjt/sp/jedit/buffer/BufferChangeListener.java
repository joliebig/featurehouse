

package org.gjt.sp.jedit.buffer;

import org.gjt.sp.jedit.Buffer;


public interface BufferChangeListener
{
	
	
	void foldLevelChanged(Buffer buffer, int startLine, int endLine);
	

	
	
	void contentInserted(Buffer buffer, int startLine, int offset,
		int numLines, int length);
	

	
	
	void contentRemoved(Buffer buffer, int startLine, int offset,
		int numLines, int length);
	

	
	
	public void preContentRemoved(Buffer buffer, int startLine, int offset,
		int numLines, int length);
	

	
	
	void transactionComplete(Buffer buffer);
	

	
	
	void foldHandlerChanged(Buffer buffer);
	

	
	
	void bufferLoaded(Buffer buffer);
	
	
	
	public class Adapter implements BufferListener
	{
		private BufferChangeListener delegate;

		
		public Adapter(BufferChangeListener delegate)
		{
			this.delegate = delegate;
		} 
	
		
		public BufferChangeListener getDelegate()
		{
			return delegate;
		} 

		
		
		public void foldLevelChanged(JEditBuffer buffer, int startLine, int endLine)
		{
			delegate.foldLevelChanged((Buffer)buffer,startLine,endLine);
		} 
	
		
		
		public void contentInserted(JEditBuffer buffer, int startLine, int offset,
			int numLines, int length)
		{
			delegate.contentInserted((Buffer)buffer,startLine,offset,numLines,length);
		} 
	
		
		
		public void contentRemoved(JEditBuffer buffer, int startLine, int offset,
			int numLines, int length)
		{
			delegate.contentRemoved((Buffer)buffer,startLine,offset,numLines,length);
		} 

		
		public void preContentInserted(JEditBuffer buffer, int startLine, int offset, int numLines, int length)
		{
		}

		
		
		public void preContentRemoved(JEditBuffer buffer, int startLine, int offset,
			int numLines, int length)
		{
			delegate.preContentRemoved((Buffer)buffer,startLine,offset,numLines,length);
		} 
	
		
		
		public void transactionComplete(JEditBuffer buffer)
		{
			delegate.transactionComplete((Buffer)buffer);
		} 
	
		
		
		public void foldHandlerChanged(JEditBuffer buffer)
		{
			delegate.foldHandlerChanged((Buffer)buffer);
		} 
	
		
		
		public void bufferLoaded(JEditBuffer buffer)
		{
			delegate.bufferLoaded((Buffer)buffer);
		} 
	} 
}
