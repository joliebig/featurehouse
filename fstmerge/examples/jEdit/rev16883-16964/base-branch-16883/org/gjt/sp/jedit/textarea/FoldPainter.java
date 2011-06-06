

package org.gjt.sp.jedit.textarea;

import java.awt.Graphics2D;

import org.gjt.sp.jedit.buffer.JEditBuffer;


public interface FoldPainter {
	
	
	void paintFoldStart(Gutter gutter, Graphics2D gfx, int screenLine,
		int physicalLine, boolean nextLineVisible, int y, int lineHeight,
		JEditBuffer buffer);
	
	
	void paintFoldEnd(Gutter gutter, Graphics2D gfx, int screenLine,
		int physicalLine, int y, int lineHeight, JEditBuffer buffer);
	
	
	void paintFoldMiddle(Gutter gutter, Graphics2D gfx, int screenLine,
		int physicalLine, int y, int lineHeight, JEditBuffer buffer);
}
