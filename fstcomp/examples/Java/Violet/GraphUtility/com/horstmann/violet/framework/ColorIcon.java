package com.horstmann.violet.framework;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.beans.PropertyEditorSupport;
import java.util.Arrays;
import java.util.Comparator;

import javax.swing.Icon;
import javax.swing.JComboBox;

class ColorIcon implements Icon
   {
      public ColorIcon(Color color) { this.color = color; }
      public Color getColor() { return color; }
      public int getIconWidth() { return WIDTH; }
      public int getIconHeight() { return HEIGHT; }
      public void paintIcon(Component c, Graphics g, int x, int y)
      {
         Rectangle r = new Rectangle(x, y, WIDTH - 1, HEIGHT - 1);
         Graphics2D g2 = (Graphics2D) g;
         Color oldColor = g2.getColor();
         g2.setColor(color);
         g2.fill(r);
         g2.setColor(Color.BLACK);
         g2.draw(r);
         g2.setColor(oldColor);
      }
      private Color color;
      private static final int WIDTH = 40;
      private static final int HEIGHT = 15;
   }