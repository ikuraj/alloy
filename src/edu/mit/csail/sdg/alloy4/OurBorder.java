package edu.mit.csail.sdg.alloy4;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Insets;
import javax.swing.border.Border;

/**
 * Graphical border above or below a component.
 *
 * <p/><b>Thread Safety:</b> Can be called only by the AWT thread.
 *
 * @author Felix Chang
 */

public final class OurBorder implements Border {

    /** True if we want to a border above the component; false if we want the border below the component. */
    private final boolean above;

    /**
     * Construct a Border object that draws a light gray line above or below the component.
     * @param above - if true, the border is above the component; if false, the border is below the component.
     */
    public OurBorder(boolean above) { this.above=above; }

    /** This method is called by Swing to actually draw the borders. */
    public void paintBorder(Component c, Graphics g, int x, int y, int width, int height) {
        if (width<1 || height<1) return;
        Color oldColor = g.getColor();
        g.setColor(Color.LIGHT_GRAY);
        if (above) g.drawLine(x, y, x+width-1, y); else g.drawLine(x, y+height-1, x+width-1, y+height-1);
        g.setColor(oldColor);
    }

    /** This method is called by Swing to retrieve the dimension of the border. */
    public Insets getBorderInsets(Component c) { return new Insets(above?1:0, 0, above?0:1, 0); }

    /** This method is called by Swing to find out whether this border object needs to fill in its own background. */
    public boolean isBorderOpaque() { return true; }
}
