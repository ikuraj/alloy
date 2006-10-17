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
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;

/**
 * Convenience utility class that provides many functions useful for GUI.
 *
 * <p/><b>Thread Safety:</b> Can be called only by the AWT thread.
 *
 * @author Felix Chang
 */

public final class OurUtil {

    /** This constructor is private, since this utility class never needs to be instantiated. */
    private OurUtil() { }

    /** Returns the screen width (in pixels). */
    public static int getScreenWidth() { return Toolkit.getDefaultToolkit().getScreenSize().width; }

    /** Returns the screen height (in pixels). */
    public static int getScreenHeight() { return Toolkit.getDefaultToolkit().getScreenSize().height; }

    /** Run r using the AWT thread; if this is not the AWT thread, then call SwingUtilities.invokeAndWait() for it. */
    public static void invokeAndWait(Runnable r) {
        if (SwingUtilities.isEventDispatchThread()) { r.run(); return; }
        try { SwingUtilities.invokeAndWait(r); }
        catch (InterruptedException e) { Util.harmless("invokeAndWait",e); }
        catch (InvocationTargetException e) { Util.harmless("invokeAndWait",e); }
    }

    /** Run f using the AWT thread; if this is not the AWT thread, then call SwingUtilities.invokeAndWait() for it. */
    public static void invokeAndWait(final OurFunc0 func) {
        if (SwingUtilities.isEventDispatchThread()) { func.run(); return; }
        try { SwingUtilities.invokeAndWait(new Runnable() { public final void run() { func.run(); }}); }
        catch (InterruptedException e) { Util.harmless("invokeAndWait",e); }
        catch (InvocationTargetException e) { Util.harmless("invokeAndWait",e); }
    }

    /** Run f using the AWT thread; if this is not the AWT thread, then call SwingUtilities.invokeAndWait() for it. */
    public static void invokeAndWait(final OurFunc1 func, final String arg) {
        if (SwingUtilities.isEventDispatchThread()) { func.run(arg); return; }
        try { SwingUtilities.invokeAndWait(new Runnable() { public final void run() { func.run(arg); }}); }
        catch (InterruptedException e) { Util.harmless("invokeAndWait",e); }
        catch (InvocationTargetException e) { Util.harmless("invokeAndWait",e); }
    }

    /** Make a JLabel with the given font. */
    public static JLabel makeJLabel(String label, Font font) {
        JLabel ans = new JLabel(label);
        ans.setFont(font);
        return ans;
    }

    /** Make a JScrollPane containing the component; scrollbars will only show as needed. */
    public static JScrollPane makeJScrollPane(Component component) {
        JScrollPane ans = new JScrollPane(component,
                JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        ans.setMinimumSize(new Dimension(50, 50));
        return ans;
    }

    /** Make an empty JScrollPane; scrollbars will only show as needed. */
    public static JScrollPane makeJScrollPane() {
        JScrollPane ans = new JScrollPane(
                JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        ans.setMinimumSize(new Dimension(50, 50));
        return ans;
    }

    /**
     * Make a graphical button
     * @param label - the text to show beneath the button
     * @param tip - the tooltip to show when the mouse hovers over the button
     * @param iconname - the filename of the icon to show (it will be loaded from the accompanying jar file)
     * @param func - the function to call when the button is pressed (null if we don't want to call any function)
     */
    public static JButton makeJButton (Color color, String label, String tip, String iconname, final OurFunc0 func) {
        ImageIcon icon = loadIcon(iconname);
        JButton button = new JButton(label,icon);
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

    /**
     * Make a JLabel with the given color and label;
     * <br/> if x>=0, it is set as the preferred width;
     * <br/> if y>=0, it is set as the preferred height.
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

    /** Make a JLabel with the given color and label. */
    public static JLabel label(Color color, String label) {
        JLabel answer = new JLabel(label);
        if (color!=null) answer.setForeground(color);
        return answer;
    }

    /** Returns the recommended font name to use in a textbox, based on the OS. */
    public static String getFontName() { if (Util.onMac()) return "LucidaGrande"; else return "LucidaGrande";}//Monospaced"; }

    /** Returns the recommended font to use in a textbox, based on the OS and the given font size. */
    public static Font getFont(int fontSize) { return new Font(getFontName(), Font.PLAIN, fontSize); }

    /** Returns the recommended font to use in the visualizer, based on the OS. */
    public static Font getVizFont() {
        if (Util.onMac()) return new Font("LucidaGrande", Font.PLAIN, 11);
        else return new Font("Dialog", Font.BOLD, 12);
    }

    /** Load the given image file (from the JAR file) and return it as an ImageIcon object. */
    public static ImageIcon loadIcon(String pathname) {
        URL url = OurUtil.class.getClassLoader().getResource(pathname);
        if (url!=null) return new ImageIcon(Toolkit.getDefaultToolkit().createImage(url));
        return new ImageIcon(new BufferedImage(5, 5, BufferedImage.TYPE_INT_RGB));
    }

    /** Make a JPanel with BoxLayout in the given orientation; xy must be either X_AXIS or Y_AXIS. */
    private static JPanel makeBox(int xy) {
        JPanel ans = new JPanel();
        ans.setLayout(new BoxLayout(ans, xy));
        ans.setAlignmentX(JPanel.LEFT_ALIGNMENT);
        ans.setAlignmentY(JPanel.TOP_ALIGNMENT);
        return ans;
    }

    /**
     * Make a JPanel with the given dimension using BoxLayout, and add the components to it;
     * <br/> if w>=h, it will have X_AXIS layout, otherwise it will have Y_AXIS layout;
     * <br/> if a component is null, we will insert a horizontal (or vertical) glue instead;
     * <br/> if a component is integer, we will insert a n*0 rigid area (or 0*n rigid area) instead.
     */
    public static JPanel makeBox(int w, int h, Object... a) {
        JPanel ans=makeBox(w>=h ? BoxLayout.X_AXIS : BoxLayout.Y_AXIS);
        ans.setPreferredSize(new Dimension(w,h));
        ans.setMaximumSize(new Dimension(w,h));
        for(int i=0; i<a.length; i++) {
            Component c;
            if (a[i] instanceof Component) c=(Component)(a[i]);
            else if (a[i] instanceof Integer) {
                if (w>=h) c=Box.createRigidArea(new Dimension((Integer)a[i], 0));
                else c=Box.createRigidArea(new Dimension(0, (Integer)a[i]));
            } else if (a[i]==null) {
                if (w>=h) c=Box.createHorizontalGlue();
                else c=Box.createVerticalGlue();
            } else continue;
            if (c instanceof JComponent) {
                if (w>=h) ((JComponent)c).setAlignmentX(0.5f);
                else ((JComponent)c).setAlignmentY(0.5f);
            }
            ans.add(c);
        }
        return ans;
    }

    /**
     * Make a JPanel using horizontal BoxLayout, and add the components to it;
     * <br/> if a component is null, we will insert a horizontal glue instead;
     * <br/> if a component is integer, we will insert a n*0 rigid area instead.
     */
    public static JPanel makeH(Object... a) {
        JPanel ans=makeBox(BoxLayout.X_AXIS);
        for(int i=0; i<a.length; i++) {
            Component c;
            if (a[i] instanceof Component) c=(Component)(a[i]);
            else if (a[i] instanceof Integer) {
                c=Box.createRigidArea(new Dimension((Integer)a[i], 0));
            } else if (a[i]==null) {
                c=Box.createHorizontalGlue();
            } else continue;
            if (c instanceof JComponent) ((JComponent)c).setAlignmentX(0.5f);
            ans.add(c);
        }
        return ans;
    }

    /**
     * Make a JPanel using vertical BoxLayout, and add the components to it;
     * <br/> if a component is null, we will insert a vertical glue instead.
     * <br/> if a component is integer, we will insert a 0*n rigid area instead.
     */
    public static JPanel makeV(Object... a) {
        JPanel ans=makeBox(BoxLayout.Y_AXIS);
        for(int i=0; i<a.length; i++) {
            Component c;
            if (a[i] instanceof Component) c=(Component)(a[i]);
            else if (a[i] instanceof Integer) {
                c=Box.createRigidArea(new Dimension(0,(Integer)a[i]));
            } else if (a[i]==null) {
                c=Box.createVerticalGlue();
            } else continue;
            if (c instanceof JComponent) ((JComponent)c).setAlignmentY(0.5f);
            ans.add(c);
        }
        return ans;
    }
}
