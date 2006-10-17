package edu.mit.csail.sdg.alloy4util;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Insets;
import javax.swing.border.Border;

/**
 * This class allows us to add borders to the top, to the bottom, or to both the top and bottom of a Component.
 *
 * <p/><b>Thread Safety:</b> Can be called only by the AWT thread.
 *
 * @author Felix Chang
 */

public final class OurBorder implements Border {

    /** True if we want to have a border on top. */
    private final boolean drawTop;

    /** True if we want to have a border on bottom. */
    private final boolean drawBottom;

    /** Construct a Border object that draws a light gray line on top, bottom, or both sides of the components. */
    public OurBorder(boolean drawTop, boolean drawBottom) {
        this.drawTop=drawTop; this.drawBottom=drawBottom;
    }

    /** This method is called by the Java library to actually draw the borders. */
    public void paintBorder(Component c, Graphics g, int x, int y, int width, int height) {
        if (width<1 || height<1) return;
        Color oldColor = g.getColor();
        g.setColor(Color.LIGHT_GRAY);
        if (drawTop) g.drawLine(x, y, x+width-1, y);
        if (drawBottom) g.drawLine(x, y+height-1, x+width-1, y+height-1);
        g.setColor(oldColor);
    }

    /** This method is called by the Java library to retrieve the dimension of the border. */
    public Insets getBorderInsets(Component c) {
        return new Insets(drawTop?1:0, 0, drawBottom?1:0, 0);
    }

    /** This method is called by the Java library to find out if the Border object is opaque or not. */
    public boolean isBorderOpaque() { return true; }
}
