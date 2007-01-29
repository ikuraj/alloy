package edu.mit.csail.sdg.alloy4;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Insets;
import javax.swing.border.Border;

/**
 * Graphical border on zero, one, two, three, or all four sides of a component.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT thread.
 */

public final class OurBorder implements Border {

    /** non-null if we want to draw a border line above the component. */
    private final Color top;

    /** True if we want to draw a border line to the left of the component. */
    private final Color left;

    /** True if we want to draw a border line below the component. */
    private final Color bottom;

    /** True if we want to draw a border line to the right of the component. */
    private final Color right;

    /**
     * Construct a Border object that draws a light gray line on 0, 1, 2, 3, or all 4 sides of the component.
     * Note: it paints the borders top, bottom, left, then right.
     * @param top    - nonnull if we want to draw a border line (with that color) above the component
     * @param left   - nonnull if we want to draw a border line (with that color) to the left of the component
     * @param bottom - nonnull if we want to draw a border line (with that color) below the component
     * @param right  - nonnull if we want to draw a border line (with that color) to the right of the component
     */
    public OurBorder(Color top, Color left, Color bottom, Color right) {
        this.top=top;
        this.left=left;
        this.bottom=bottom;
        this.right=right;
    }

    /**
     * Construct a Border object that draws a light gray line on 0, 1, 2, 3, or all 4 sides of the component.
     * Note: it paints the borders top, bottom, left, then right.
     * @param top    - true if we want to draw a LIGHT_GRAY border line above the component
     * @param left   - true if we want to draw a LIGHT_GRAY border line to the left of the component
     * @param bottom - true if we want to draw a LIGHT_GRAY border line below the component
     * @param right  - true if we want to draw a LIGHT_GRAY border line to the right of the component
     */
    public OurBorder(boolean top, boolean left, boolean bottom, boolean right) {
        this.top = top?Color.LIGHT_GRAY:null;
        this.left = left?Color.LIGHT_GRAY:null;
        this.bottom = bottom?Color.LIGHT_GRAY:null;
        this.right = right?Color.LIGHT_GRAY:null;
    }

    /** This method is called by Swing to actually draw the borders. */
    public void paintBorder(Component component, Graphics graphics, int x, int y, int width, int height) {
        if (width<1 || height<1) return;
        Color oldColor = graphics.getColor();
        if (top!=null) { graphics.setColor(top); graphics.drawLine(x, y, x+width-1, y); }
        if (bottom!=null) { graphics.setColor(bottom); graphics.drawLine(x, y+height-1, x+width-1, y+height-1); }
        if (left!=null) { graphics.setColor(left); graphics.drawLine(x, y, x, y+height-1); }
        if (right!=null) { graphics.setColor(right); graphics.drawLine(x+width-1, y, x+width-1, y+height-1); }
        graphics.setColor(oldColor);
    }

    /** This method is called by Swing to retrieve the dimension of the border. */
    public Insets getBorderInsets(Component c) {return new Insets(top!=null?1:0, left!=null?1:0, bottom!=null?1:0, right!=null?1:0);}

    /** This method is called by Swing to find out whether this border object needs to fill in its own background. */
    public boolean isBorderOpaque() { return true; }
}
