package edu.mit.csail.sdg.alloy4util;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Insets;
import javax.swing.JSplitPane;
import javax.swing.border.Border;
import javax.swing.plaf.basic.BasicSplitPaneUI;

/**
 * This wrapper around JSplitPane provides additional convenience methods and looks better than the default.
 *
 * <p/><b>Thread Safety:</b> Can be called only by the AWT thread.
 *
 * @author Code adapted from Alloy3
 */

public final class OurSplitPane extends JSplitPane {

    /** This silences javac's warning about missing serialVersionUID. */
    private static final long serialVersionUID = 1L;

    /**
     * Constructs a new SplitPane containing the two components given as arguments
     * @param orientation - the orientation (HORIZONTAL_SPLIT or VERTICAL_SPLIT)
     * @param leftComp - the left component (if horizontal) or top component (if vertical)
     * @param rightComp - the right component (if horizontal) or bottom component (if vertical)
     * @param initialDividerLocation - the initial divider location (in pixels)
     */
    public OurSplitPane(int orientation, Component leftComp, Component rightComp, int initialDividerLocation) {
        super(orientation, leftComp, rightComp);
        setBorder(null);
        setContinuousLayout(true);
        setDividerLocation(initialDividerLocation);
        setOneTouchExpandable(false);
        setResizeWeight(0.5);
        fixDivider();
    }

    /**
     * Changes the orientation, then calls fixDivider() to make sure we're using a nice looking border on Mac.
     * @param orientation - either HORIZONTAL_SPLIT or VERTICAL_SPLIT
     */
    @Override public void setOrientation(int orientation) {
        super.setOrientation(orientation);
        fixDivider();
    }

    /** Private helper method that fixes the border to use a nicer looking border on Mac. */
    private void fixDivider() {
        if (Util.onMac())
            ((BasicSplitPaneUI)getUI()).getDivider().setBorder(new NiceSplitPaneDividerBorder(getOrientation()));
    }

    /** Private helper class that provides a nicer looking border for the divider. */
    private static class NiceSplitPaneDividerBorder implements Border {
        private final Insets insets;
        public NiceSplitPaneDividerBorder(int orientation) {
            insets = (orientation == JSplitPane.HORIZONTAL_SPLIT) ? new Insets(0,1,0,1) : new Insets(1,0,1,0);
        }
        public void paintBorder(Component c, Graphics g, int x, int y, int width, int height) {
            if (height==0 || width==0) return;
            Color oldcolor=g.getColor();
            g.setColor(Color.lightGray);
            g.fillRect(0,       0,        width + insets.left + insets.right,   insets.top);
            g.fillRect(0,       height-1, width + insets.left + insets.right,   insets.bottom);
            g.fillRect(0,       0,        insets.left,                          height + insets.top + insets.bottom);
            g.fillRect(width-1, 0,        insets.right,                         height + insets.top + insets.bottom);
            g.setColor(oldcolor);
        }
        public Insets getBorderInsets(Component c) { return insets; }
        public boolean isBorderOpaque() { return true; }
    }
}
