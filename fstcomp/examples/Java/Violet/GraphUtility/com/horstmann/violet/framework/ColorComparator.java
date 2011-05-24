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

class ColorComparator implements Comparator  {
      public int compare(Object obj1, Object obj2)
      {
         Color c1 = ((ColorIcon) obj1).getColor();
         Color c2 = ((ColorIcon) obj2).getColor();
         Color.RGBtoHSB(c1.getRed(), c1.getGreen(), c1.getBlue(), hsb);
         float hue1 = hsb[0];
         float sat1 = hsb[1];
         float bri1 = hsb[2];
         Color.RGBtoHSB(c2.getRed(), c2.getGreen(), c2.getBlue(), hsb);
         float hue2 = hsb[0];
         float sat2 = hsb[1];
         float bri2 = hsb[2];
         if (hue1 < hue2) return 1;
         if (hue1 > hue2) return -1;
         if (sat1 < sat2) return 1;
         if (sat1 > sat2) return -1;
         if (bri1 < bri2) return 1;
         if (bri1 > bri2) return -1;
         return 0;
      }
      private static float[] hsb = new float[3];
}