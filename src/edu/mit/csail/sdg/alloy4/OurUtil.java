package edu.mit.csail.sdg.alloy4;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Toolkit;
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
import javax.swing.JSplitPane;
import javax.swing.SwingUtilities;
import javax.swing.border.EmptyBorder;
import javax.swing.plaf.basic.BasicSplitPaneUI;

import edu.mit.csail.sdg.alloy4.MultiRunner.MultiRunnable;

/**
 * Graphical convenience methods.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
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
     * @param key - the parameter to pass to func() when the button is pressed
     */
    public static JButton button(String label, String tip,String iconname, MultiRunnable func, int key) {
        JButton button = new JButton(label,loadIcon(iconname));
        if (func!=null) button.addActionListener(new MultiRunner(func,key));
        button.setVerticalTextPosition(JButton.BOTTOM);
        button.setHorizontalTextPosition(JButton.CENTER);
        button.setBorderPainted(false);
        button.setFocusable(false);
        if (!Util.onMac()) button.setBackground(new Color(0.9f, 0.9f, 0.9f));
        button.setFont(button.getFont().deriveFont(10.0f));
        button.setToolTipText(tip);
        return button;
    }

    /** Returns the recommended font to use in the visualizer, based on the OS. */
    public static Font getVizFont() {
        return Util.onMac() ? new Font("Lucida Grande",Font.PLAIN,11) : new Font("Dialog",Font.PLAIN,12);
    }

    /** Returns the screen height (in pixels). */
    public static int getScreenHeight() { return Toolkit.getDefaultToolkit().getScreenSize().height; }

    /** Returns the screen width (in pixels). */
    public static int getScreenWidth() { return Toolkit.getDefaultToolkit().getScreenSize().width; }

    /** Run r.run() using the AWT event thread; if it's not the AWT event thread, use SwingUtilities.invokeAndWait() on it. */
    public static void invokeAndWait(Runnable r) {
        if (SwingUtilities.isEventDispatchThread()) { r.run(); return; }
        try { SwingUtilities.invokeAndWait(r); }
        catch (InterruptedException e) { }
        catch (InvocationTargetException e) { }
    }

    /** Make a JLabel with the given color. */
    public static JLabel label(Color color, String label) {
        JLabel answer = new JLabel(label);
        answer.setForeground(color==null ? Color.BLACK : color);
        answer.setFont(getVizFont());
        return answer;
    }

    /** Make a JLabel with the given font. */
    public static JLabel label(Font font, String label) {
        JLabel answer = new JLabel(label);
        if (font!=null) answer.setFont(font);
        return answer;
    }

    /** Load the given image file from an accompanying JAR file, and return it as an Icon object. */
    public static Icon loadIcon(String pathname) {
        URL url = OurUtil.class.getClassLoader().getResource(pathname);
        if (url!=null) return new ImageIcon(Toolkit.getDefaultToolkit().createImage(url));
        return new ImageIcon(new BufferedImage(2, 2, BufferedImage.TYPE_INT_RGB));
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
     * <br> It will have the X_AXIS layout (or Y_AXIS if h>w).
     * <br> If a component is null, we will insert a horizontal (or vertical) glue instead.
     * <br> If a component is integer, we will insert an "n*1" rigid area (or "1*n" rigid area) instead.
     * <br> Each component will be center-aligned.
     * <br> Note: if the first component is a Color, we will set everything's background color to that color.
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
            else if (a[i] instanceof String) c=label(Color.BLACK, (String)(a[i]));
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
     * <br> If a component is null, we will insert a horizontal glue instead.
     * <br> If a component is integer, we will insert an "n*1" rigid area instead.
     * <br> Each component will be center-aligned.
     * <br> Note: if the first component is a Color, we will set everything's background color to that color.
     */
    public static JPanel makeH(Object... a) {
        JPanel ans=makeBox(BoxLayout.X_AXIS);
        Color color=null;
        for(int i=0; i<a.length; i++) {
            Component c;
            if (a[i] instanceof Color) { color=(Color)(a[i]); ans.setBackground(color); continue; }
            if (a[i] instanceof Component) c=(Component)(a[i]);
            else if (a[i] instanceof String) c=label(Color.BLACK, (String)(a[i]));
            else if (a[i] instanceof Integer) c=Box.createRigidArea(new Dimension((Integer)a[i], 1));
            else if (a[i]==null) c=Box.createHorizontalGlue();
            else continue;
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
     * <br> If a component is null, we will insert a horizontal glue instead.
     * <br> If a component is integer, we will insert an "n*1" rigid area instead.
     * <br> Each component will be top-aligned.
     * <br> Note: if the first component is a Color, we will set everything's background color to that color.
     */
    public static JPanel makeHT(Object... a) {
        JPanel ans=makeBox(BoxLayout.X_AXIS);
        Color color=null;
        for(int i=0; i<a.length; i++) {
            Component c;
            if (a[i] instanceof Color) { color=(Color)(a[i]); ans.setBackground(color); continue; }
            if (a[i] instanceof Component) c=(Component)(a[i]);
            else if (a[i] instanceof String) c=label(Color.BLACK, (String)(a[i]));
            else if (a[i] instanceof Integer) c=Box.createRigidArea(new Dimension((Integer)a[i], 1));
            else if (a[i]==null) c=Box.createHorizontalGlue();
            else continue;
            if (color!=null) c.setBackground(color);
            if (c instanceof JComponent) {
                ((JComponent)c).setAlignmentX(0.5f);
                ((JComponent)c).setAlignmentY(0.0f);
            }
            ans.add(c);
        }
        return ans;
    }

    /**
     * Make a JPanel using horizontal BoxLayout, and add the components to it.
     * <br> If a component is null, we will insert a horizontal glue instead.
     * <br> If a component is integer, we will insert an "n*1" rigid area instead.
     * <br> Each component will be bottom-aligned.
     * <br> Note: if the first component is a Color, we will set everything's background color to that color.
     */
    public static JPanel makeHB(Object... a) {
        JPanel ans=makeBox(BoxLayout.X_AXIS);
        Color color=null;
        for(int i=0; i<a.length; i++) {
            Component c;
            if (a[i] instanceof Color) { color=(Color)(a[i]); ans.setBackground(color); continue; }
            if (a[i] instanceof Component) c=(Component)(a[i]);
            else if (a[i] instanceof String) c=label(Color.BLACK, (String)(a[i]));
            else if (a[i] instanceof Integer) c=Box.createRigidArea(new Dimension((Integer)a[i], 1));
            else if (a[i]==null) c=Box.createHorizontalGlue();
            else continue;
            if (color!=null) c.setBackground(color);
            if (c instanceof JComponent) {
                ((JComponent)c).setAlignmentX(0.5f);
                ((JComponent)c).setAlignmentY(1.0f);
            }
            ans.add(c);
        }
        return ans;
    }

    /**
     * Make a JPanel using vertical BoxLayout, and add the components to it.
     * <br> If a component is null, we will insert a vertical glue instead.
     * <br> If a component is integer, we will insert an "1*n" rigid area instead.
     * <br> Each component will be center-aligned.
     * <br> Note: if the first component is a Color, we will set everything's background color to that color.
     */
    public static JPanel makeV(Object... a) {
        JPanel ans=makeBox(BoxLayout.Y_AXIS);
        Color color=null;
        for(int i=0; i<a.length; i++) {
            Component c;
            if (a[i] instanceof Color) { color=(Color)(a[i]); ans.setBackground(color); continue; }
            if (a[i] instanceof Component) c=(Component)(a[i]);
            else if (a[i] instanceof String) c=label(Color.BLACK, (String)(a[i]));
            else if (a[i] instanceof Integer) c=Box.createRigidArea(new Dimension(1,(Integer)a[i]));
            else if (a[i]==null) c=Box.createVerticalGlue();
            else continue;
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
     * <br> If a component is null, we will insert a vertical glue instead.
     * <br> If a component is integer, we will insert an "1*n" rigid area instead.
     * <br> Each component will be left-aligned.
     * <br> Note: if the first component is a Color, we will set everything's background color to that color.
     */
    public static JPanel makeVL(Object... a) {
        JPanel ans=makeBox(BoxLayout.Y_AXIS);
        Color color=null;
        for(int i=0; i<a.length; i++) {
            Component c;
            if (a[i] instanceof Color) { color=(Color)(a[i]); ans.setBackground(color); continue; }
            if (a[i] instanceof Component) c=(Component)(a[i]);
            else if (a[i] instanceof String) c=label(Color.BLACK, (String)(a[i]));
            else if (a[i] instanceof Integer) c=Box.createRigidArea(new Dimension(1,(Integer)a[i]));
            else if (a[i]==null) c=Box.createVerticalGlue();
            else continue;
            if (color!=null) c.setBackground(color);
            if (c instanceof JComponent) {
                ((JComponent)c).setAlignmentX(0.0f);
                ((JComponent)c).setAlignmentY(0.5f);
            }
            ans.add(c);
        }
        return ans;
    }

    /**
     * Make a JPanel using vertical BoxLayout, and add the components to it.
     * <br> If a component is null, we will insert a vertical glue instead.
     * <br> If a component is integer, we will insert an "1*n" rigid area instead.
     * <br> Each component will be right-aligned.
     * <br> Note: if the first component is a Color, we will set everything's background color to that color.
     */
    public static JPanel makeVR(Object... a) {
        JPanel ans=makeBox(BoxLayout.Y_AXIS);
        Color color=null;
        for(int i=0; i<a.length; i++) {
            Component c;
            if (a[i] instanceof Color) { color=(Color)(a[i]); ans.setBackground(color); continue; }
            if (a[i] instanceof Component) c=(Component)(a[i]);
            else if (a[i] instanceof String) c=label(Color.BLACK, (String)(a[i]));
            else if (a[i] instanceof Integer) c=Box.createRigidArea(new Dimension(1,(Integer)a[i]));
            else if (a[i]==null) c=Box.createVerticalGlue();
            else continue;
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
        ans.setBorder(new EmptyBorder(0,0,0,0));
        return ans;
    }

    /** Make a JScrollPane containing the component; scrollbars will only show up as needed. */
    public static JScrollPane scrollpane(Component component) {
        JScrollPane ans = new JScrollPane(component,
                JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        ans.setMinimumSize(new Dimension(50, 50));
        ans.setBorder(new EmptyBorder(0,0,0,0));
        return ans;
    }

    /**
     * Constructs a new SplitPane containing the two components given as arguments
     * @param orientation - the orientation (HORIZONTAL_SPLIT or VERTICAL_SPLIT)
     * @param leftComp - the left component (if horizontal) or top component (if vertical)
     * @param rightComp - the right component (if horizontal) or bottom component (if vertical)
     * @param initialDividerLocation - the initial divider location (in pixels)
     */
    public static JSplitPane splitpane
    (int orientation, Component leftComp, Component rightComp, int initialDividerLocation) {
        JSplitPane x = new JSplitPane(orientation, leftComp, rightComp);
        x.setBorder(null);
        x.setContinuousLayout(true);
        x.setDividerLocation(initialDividerLocation);
        x.setOneTouchExpandable(false);
        x.setResizeWeight(0.5);
        if (Util.onMac()) {
            // This makes the border look nicer on Mac OS X
            boolean h = (orientation == JSplitPane.HORIZONTAL_SPLIT);
            ((BasicSplitPaneUI)(x.getUI())).getDivider().setBorder(new OurBorder(!h, h, !h, h));
        }
        return x;
    }
}
