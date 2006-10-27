package edu.mit.csail.sdg.alloy4;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.image.BufferedImage;
import java.lang.reflect.InvocationTargetException;
import java.net.URL;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;

/**
 * Graphical convenience methods.
 *
 * <p/><b>Thread Safety:</b> Can be called only by the AWT thread.
 *
 * @author Felix Chang
 */

public final class OurUtil {

    /** This constructor is private, since this utility class never needs to be instantiated. */
    private OurUtil() { }

    /**
     * Make a graphical button
     * @param label - the text to show beneath the button
     * @param tip - the tooltip to show when the mouse hovers over the button
     * @param iconname - the filename of the icon to show (it will be loaded from an accompanying jar file)
     * @param func - the function to call when the button is pressed (null if we don't want to call any function)
     */
    public static JButton button (Color color, String label, String tip, String iconname, final OurFunc0 func) {
        JButton button = new JButton(label,loadIcon(iconname));
        if (func!=null) button.addActionListener(new ActionListener() {
            public final void actionPerformed(ActionEvent e) { func.run(); }
        });
        button.setVerticalTextPosition(JButton.BOTTOM);
        button.setHorizontalTextPosition(JButton.CENTER);
        button.setBorderPainted(false);
        button.setFocusable(false);
        if (!Util.onMac()) button.setBackground(color);
        button.setFont(button.getFont().deriveFont(10.0f));
        button.setToolTipText(tip);
        return button;
    }

    /** Returns the recommended font to use in a textbox, based on the OS and the given font size. */
    public static Font getFont(int fontSize) { return new Font(getFontName(), Font.PLAIN, fontSize); }

    /** Returns the recommended font name to use in a textbox, based on the OS. */
    public static String getFontName() { return "LucidaGrande"; }

    /** Returns the screen height (in pixels). */
    public static int getScreenHeight() { return Toolkit.getDefaultToolkit().getScreenSize().height; }

    /** Returns the screen width (in pixels). */
    public static int getScreenWidth() { return Toolkit.getDefaultToolkit().getScreenSize().width; }

    /** Returns the recommended font to use in the visualizer, based on the OS. */
    public static Font getVizFont() {
        if (Util.onMac()) return new Font("LucidaGrande", Font.PLAIN, 11);
        return new Font("Dialog", Font.BOLD, 12);
    }

    /** Run f.run() using the AWT thread; if it's not the AWT thread, use SwingUtilities.invokeAndWait() on it. */
    public static void invokeAndWait(final OurFunc0 f) {
        if (SwingUtilities.isEventDispatchThread()) { f.run(); return; }
        invokeAndWait(new Runnable() { public final void run() { f.run(); } });
    }

    /** Run f.run(arg) using the AWT thread; if it's not the AWT thread, use SwingUtilities.invokeAndWait() on it. */
    public static void invokeAndWait(final OurFunc1 f, final String arg) {
        if (SwingUtilities.isEventDispatchThread()) { f.run(arg); return; }
        invokeAndWait(new Runnable() { public final void run() { f.run(arg); } });
    }

    /** Run r.run() using the AWT thread; if it's not the AWT thread, use SwingUtilities.invokeAndWait() on it. */
    public static void invokeAndWait(Runnable r) {
        if (SwingUtilities.isEventDispatchThread()) { r.run(); return; }
        try { SwingUtilities.invokeAndWait(r); }
        catch (InterruptedException e) { Util.harmless("invokeAndWait",e); }
        catch (InvocationTargetException e) { Util.harmless("invokeAndWait",e); }
    }

    /**
     * Make a JLabel with the given color;
     * if x>=0, it is the preferred width;
     * if y>=0, it is the preferred height.
     */
    public static JLabel label(Color color, int x, int y, String label) {
        JLabel answer = new JLabel(label);
        if (color!=null) answer.setForeground(color);
        Dimension d=answer.getPreferredSize();
        if (x>=0) d.width=x;
        if (y>=0) d.height=y;
        answer.setPreferredSize(d);
        answer.setMaximumSize(d);
        return answer;
    }

    /** Make a JLabel with the given color. */
    public static JLabel label(Color color, String label) {
        JLabel answer = new JLabel(label);
        if (color!=null) answer.setForeground(color);
        answer.setFont(getVizFont());
        return answer;
    }

    /** Make a JLabel with the given font. */
    public static JLabel label(Font font, String label) {
        JLabel answer = new JLabel(label);
        answer.setFont(font);
        return answer;
    }

    /** Load the given image file from an accompanying JAR file, and return it as an Icon object. */
    public static Icon loadIcon(String pathname) {
        URL url = OurUtil.class.getClassLoader().getResource(pathname);
        if (url!=null) return new ImageIcon(Toolkit.getDefaultToolkit().createImage(url));
        return new ImageIcon(new BufferedImage(5, 5, BufferedImage.TYPE_INT_RGB));
    }

    /** Make a JPanel with BoxLayout in the given orientation; xy must be BoxLayout.X_AXIS or BoxLayout.Y_AXIS. */
    private static JPanel makeBox(int xy) {
        JPanel ans = new JPanel();
        ans.setLayout(new BoxLayout(ans, xy));
        ans.setAlignmentX(JPanel.LEFT_ALIGNMENT);
        ans.setAlignmentY(JPanel.TOP_ALIGNMENT);
        return ans;
    }

    /**
     * Make a JPanel with the given dimension using BoxLayout, and add the components to it.
     * <br/> It will have the X_AXIS layout (or Y_AXIS if h>w).
     * <br/> If a component is null, we will insert a horizontal (or vertical) glue instead.
     * <br/> If a component is integer, we will insert an "n*1" rigid area (or "1*n" rigid area) instead.
     * <br/> Each component will be center-aligned.
     * <br/> Note: if the first component is a Color, we will set everything's background color to that color.
     */
    public static JPanel makeBox(int w, int h, Object... a) {
        JPanel ans=makeBox(w>=h ? BoxLayout.X_AXIS : BoxLayout.Y_AXIS);
        ans.setPreferredSize(new Dimension(w,h));
        ans.setMaximumSize(new Dimension(w,h));
        Color color=null;
        for(int i=0; i<a.length; i++) {
            Component c;
            if (a[i] instanceof Color) { color=(Color)(a[i]); ans.setBackground(color); continue; }
            if (a[i] instanceof Component) c=(Component)(a[i]);
            else if (a[i] instanceof Integer) {
                if (w>=h) c=Box.createRigidArea(new Dimension((Integer)a[i], 1));
                else c=Box.createRigidArea(new Dimension(1, (Integer)a[i]));
            } else if (a[i]==null) {
                if (w>=h) c=Box.createHorizontalGlue();
                else c=Box.createVerticalGlue();
            } else continue;
            if (color!=null) c.setBackground(color);
            if (c instanceof JComponent) {
                ((JComponent)c).setAlignmentX(0.5f);
                ((JComponent)c).setAlignmentY(0.5f);
            }
            ans.add(c);
        }
        return ans;
    }

    /**
     * Make a JPanel using horizontal BoxLayout, and add the components to it.
     * <br/> If a component is null, we will insert a horizontal glue instead.
     * <br/> If a component is integer, we will insert an "n*1" rigid area instead.
     * <br/> Each component will be center-aligned.
     * <br/> Note: if the first component is a Color, we will set everything's background color to that color.
     */
    public static JPanel makeH(Object... a) {
        JPanel ans=makeBox(BoxLayout.X_AXIS);
        Color color=null;
        for(int i=0; i<a.length; i++) {
            Component c;
            if (a[i] instanceof Color) { color=(Color)(a[i]); ans.setBackground(color); continue; }
            if (a[i] instanceof Component) c=(Component)(a[i]);
            else if (a[i] instanceof Integer) {
                c=Box.createRigidArea(new Dimension((Integer)a[i], 1));
            } else if (a[i]==null) {
                c=Box.createHorizontalGlue();
            } else continue;
            if (color!=null) c.setBackground(color);
            if (c instanceof JComponent) {
                ((JComponent)c).setAlignmentX(0.5f);
                ((JComponent)c).setAlignmentY(0.5f);
            }
            ans.add(c);
        }
        return ans;
    }

    /**
     * Make a JPanel using vertical BoxLayout, and add the components to it.
     * <br/> If a component is null, we will insert a vertical glue instead.
     * <br/> If a component is integer, we will insert an "1*n" rigid area instead.
     * <br/> Each component will be center-aligned.
     * <br/> Note: if the first component is a Color, we will set everything's background color to that color.
     */
    public static JPanel makeV(Object... a) {
        JPanel ans=makeBox(BoxLayout.Y_AXIS);
        Color color=null;
        for(int i=0; i<a.length; i++) {
            Component c;
            if (a[i] instanceof Color) { color=(Color)(a[i]); ans.setBackground(color); continue; }
            if (a[i] instanceof Component) c=(Component)(a[i]);
            else if (a[i] instanceof Integer) {
                c=Box.createRigidArea(new Dimension(1,(Integer)a[i]));
            } else if (a[i]==null) {
                c=Box.createVerticalGlue();
            } else continue;
            if (color!=null) c.setBackground(color);
            if (c instanceof JComponent) {
                ((JComponent)c).setAlignmentX(0.5f);
                ((JComponent)c).setAlignmentY(0.5f);
            }
            ans.add(c);
        }
        return ans;
    }

    /**
     * Make a JPanel using vertical BoxLayout, and add the components to it.
     * <br/> If a component is null, we will insert a vertical glue instead.
     * <br/> If a component is integer, we will insert an "1*n" rigid area instead.
     * <br/> Each component will be right-aligned.
     * <br/> Note: if the first component is a Color, we will set everything's background color to that color.
     */
    public static JPanel makeVR(Object... a) {
        JPanel ans=makeBox(BoxLayout.Y_AXIS);
        Color color=null;
        for(int i=0; i<a.length; i++) {
            Component c;
            if (a[i] instanceof Color) { color=(Color)(a[i]); ans.setBackground(color); continue; }
            if (a[i] instanceof Component) c=(Component)(a[i]);
            else if (a[i] instanceof Integer) {
                c=Box.createRigidArea(new Dimension(1,(Integer)a[i]));
            } else if (a[i]==null) {
                c=Box.createVerticalGlue();
            } else continue;
            if (color!=null) c.setBackground(color);
            if (c instanceof JComponent) {
                ((JComponent)c).setAlignmentX(1f);
                ((JComponent)c).setAlignmentY(0.5f);
            }
            ans.add(c);
        }
        return ans;
    }

    /** Make an empty JScrollPane; scrollbars will only show as needed. */
    public static JScrollPane scrollpane() {
        JScrollPane ans = new JScrollPane(
                JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        ans.setMinimumSize(new Dimension(50, 50));
        return ans;
    }

    /** Make a JScrollPane containing the component; scrollbars will only show up as needed. */
    public static JScrollPane scrollpane(Component component) {
        JScrollPane ans = new JScrollPane(component,
                JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        ans.setMinimumSize(new Dimension(50, 50));
        return ans;
    }
}
