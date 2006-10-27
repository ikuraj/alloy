package edu.mit.csail.sdg.alloy4;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Insets;
import javax.swing.border.Border;

/**
 * Graphical border on zero, one, two, three, or all four sides of a component.
 *
 * <p/><b>Thread Safety:</b> Can be called only by the AWT thread.
 *
 * @author Felix Chang
 */

public final class OurBorder implements Border {

    /** True if we want to draw a border line above the component. */
    private final boolean top;

    /** True if we want to draw a border line to the left of the component. */
    private final boolean left;

    /** True if we want to draw a border line below the component. */
    private final boolean bottom;

    /** True if we want to draw a border line to the right of the component. */
    private final boolean right;

    /**
     * Construct a Border object that draws a light gray line on 0, 1, 2, 3, or all 4 sides of the component.
     * @param top    - true if we want to draw a border line above the component
     * @param left   - true if we want to draw a border line to the left of the component
     * @param bottom - true if we want to draw a border line below the component
     * @param right  - true if we want to draw a border line to the right of the component
     */
    public OurBorder(boolean top,boolean left,boolean bottom,boolean right) {
        this.top=top;
        this.left=left;
        this.bottom=bottom;
        this.right=right;
    }

    /** This method is called by Swing to actually draw the borders. */
    public void paintBorder(Component c, Graphics g, int x, int y, int width, int height) {
        if (width<1 || height<1) return;
        Color oldColor = g.getColor();
        g.setColor(Color.LIGHT_GRAY);
        if (top) g.drawLine(x, y, x+width-1, y);
        if (left) g.drawLine(x, y, x, y+height-1);
        if (bottom) g.drawLine(x, y+height-1, x+width-1, y+height-1);
        if (right) g.drawLine(x+width-1, y, x+width-1, y+height-1);
        g.setColor(oldColor);
    }

    /** This method is called by Swing to retrieve the dimension of the border. */
    public Insets getBorderInsets(Component c) { return new Insets(top?1:0, left?1:0, bottom?1:0, right?1:0); }

    /** This method is called by Swing to find out whether this border object needs to fill in its own background. */
    public boolean isBorderOpaque() { return true; }
}
