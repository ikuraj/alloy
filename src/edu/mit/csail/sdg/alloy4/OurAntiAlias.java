package edu.mit.csail.sdg.alloy4;

import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.util.Map;
import java.util.WeakHashMap;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JTextPane;

/**
 * Graphical convenience methods for managing and constructing antialias-capable components.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
 */

public final class OurAntiAlias {

    /** This constructor is private, since this utility class never needs to be instantiated. */
    private OurAntiAlias() { }

    /** Use anti-alias or not. */
    private static boolean antiAlias = Util.onMac() || Util.onWindows();

    /** Stores a weak-reference set of all objects that need to be redrawn when anti-alias setting changes. */
    private static WeakHashMap<JComponent, Boolean> map = new WeakHashMap<JComponent, Boolean>();

    /** Changes whether anti-aliasing should be done or not (when this changes, we will automatically repaint all affected components). */
    public static void enableAntiAlias(boolean enableAntiAlias) {
        if (Util.onMac() || Util.onWindows()) enableAntiAlias = true; // On Mac and Windows they are already antialiased
        if (antiAlias == enableAntiAlias) return;
        antiAlias = enableAntiAlias;
        for(Map.Entry<JComponent,Boolean> x: map.entrySet()) { JComponent y=x.getKey(); if (y!=null) {y.invalidate(); y.repaint(); y.validate();} }
    }

    /** Returns true if anti-alias is currently enabled. */
    static boolean antiAlias() { return antiAlias; }

    /** Registers a component to be included in the "automatic repaint" list. */
    static void register(JComponent component) { map.put(component, Boolean.TRUE); }

    /** Constructs an antialias-capable JLabel */
    public static JLabel label(String label) {
        JLabel ans = new JLabel(label) {
            private static final long serialVersionUID = 1L;
            @Override public void paint(Graphics gr) {
                if (antiAlias && gr instanceof Graphics2D) ((Graphics2D)gr).setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
                super.paint(gr);
            }
        };
        map.put(ans, Boolean.TRUE);
        return ans;
    }

    /** Constructs an antialias-capable JTextPane */
    public static JTextPane pane() {
        JTextPane ans = new JTextPane() {
            private static final long serialVersionUID = 1L;
            @Override public void paint(Graphics gr) {
                if (antiAlias && gr instanceof Graphics2D) ((Graphics2D)gr).setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
                super.paint(gr);
            }
        };
        map.put(ans, Boolean.TRUE);
        return ans;
    }
}
