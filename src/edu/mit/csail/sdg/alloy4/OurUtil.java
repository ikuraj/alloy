/*
 * Alloy Analyzer 4 -- Copyright (c) 2006-2008, Felix Chang
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

package edu.mit.csail.sdg.alloy4;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Toolkit;
import java.awt.event.ActionListener;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.image.BufferedImage;
import java.lang.reflect.InvocationTargetException;
import java.net.URL;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import static javax.swing.JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED;
import static javax.swing.JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED;
import javax.swing.JSplitPane;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.border.Border;
import javax.swing.border.EmptyBorder;
import javax.swing.event.MenuEvent;
import javax.swing.event.MenuListener;
import javax.swing.plaf.basic.BasicSplitPaneUI;

/**
 * Graphical convenience methods.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
 */

public final class OurUtil {

    /** An empty border. */
    public static final EmptyBorder empty = new EmptyBorder(0, 0, 0, 0);

    /** This constructor is private, since this utility class never needs to be instantiated. */
    private OurUtil() { }

    /**
     * Assign the given attributes to the given JComponent, then return the JComponent again.
     * <p> If Font x is given in the list, we call obj.setFont(x0
     * <p> If String x is given in the list, we call obj.setToolTipText(x)
     * <p> If Border x is given in the list, we call obj.setBorder(x)
     * <p> If Dimension x is given in the list, we call obj.setPreferredSize(x)
     * <p> If Color x is given in the list, and it's the first color, we call obj.setForeground(x)
     * <p> If Color x is given in the list, and it's not the first color, we call obj.setBackground(x) then obj.setOpaque(true)
     * <p> (If no Font is given, then after all these changes have been applied, we will call obj.setFont() will a default font)
     */
    public static<X extends JComponent> X make(X obj, Object... attributes) {
        boolean hasFont = false;
        boolean hasForeground = false;
        if (attributes!=null) for(int i=0; i<attributes.length; i++) {
            Object at = attributes[i];
            if (at instanceof Color && !hasForeground) { obj.setForeground((Color)at); hasForeground=true; continue; }
            if (at instanceof Color) { obj.setBackground((Color)at); obj.setOpaque(true); }
            if (at instanceof Font) { obj.setFont((Font)at); hasFont=true; }
            if (at instanceof String) { obj.setToolTipText((String)at); }
            if (at instanceof Border) { obj.setBorder((Border)at); }
            if (at instanceof Dimension) { obj.setPreferredSize((Dimension)at); }
        }
        if (!hasFont) obj.setFont(getVizFont());
        return obj;
    }

    /** Make a JLabel, then call Util.make() to apply a set of attributes to it. */
    public static JLabel label (String label, Object... attributes)  { return make(new JLabel(label), attributes); }

    /** Make a JTextField, then call Util.make() to apply a set of attributes to it. */
    public static JTextField textfield (String text, int columns, Object... attributes)  { return make(new JTextField(text, columns), attributes); }

    /** Make a JTextArea, then call Util.make() to apply a set of attributes to it. */
    public static JTextArea textarea (String text, int rows, int columns, boolean editable, boolean wrap, Object... attributes) {
        JTextArea ans = new JTextArea(text, rows, columns);
        ans.setForeground(Color.BLACK);
        ans.setBackground(Color.WHITE);
        ans.setEditable(editable);
        ans.setLineWrap(wrap);
        ans.setWrapStyleWord(wrap);
        ans.setBorder(null);
        return make(ans, attributes);
    }

    /** Make a JScrollPane containing the given component (which can be null), then call Util.make() to apply a set of attributes to it. */
    public static JScrollPane scrollpane (Component component, Object... attributes) {
        JScrollPane ans = new JScrollPane(VERTICAL_SCROLLBAR_AS_NEEDED, HORIZONTAL_SCROLLBAR_AS_NEEDED);
        if (component!=null) ans.setViewportView(component);
        ans.setMinimumSize(new Dimension(50, 50));
        ans.setBorder(null);
        return make(ans, attributes);
    }

    /** Returns the recommended font to use in the visualizer, based on the OS. */
    public static Font getVizFont()  { return Util.onMac() ? new Font("Lucida Grande", Font.PLAIN, 11) : new Font("Dialog", Font.PLAIN, 12); }

    /** Returns the screen height (in pixels). */
    public static int getScreenHeight()  { return Toolkit.getDefaultToolkit().getScreenSize().height; }

    /** Returns the screen width (in pixels). */
    public static int getScreenWidth()  { return Toolkit.getDefaultToolkit().getScreenSize().width; }

    /** Run r.run() using the AWT event thread; if it's not the AWT event thread, use SwingUtilities.invokeAndWait() on it. */
    public static void invokeAndWait(Runnable r) {
        if (SwingUtilities.isEventDispatchThread()) { r.run(); return; }
        try {
            SwingUtilities.invokeAndWait(r);
        } catch (InterruptedException e) {
            // Nothing we can do about it
        } catch (InvocationTargetException e) {
            // Nothing we can do about it
        }
    }

    /**
     * Make a textual button
     * @param label - the text to show beneath the button
     * @param func - the function to call when the button is pressed (null if we don't want to call any function)
     */
    public static JButton button (String label, ActionListener func) {
        JButton button = new JButton(label);
        if (func!=null) button.addActionListener(func);
        return button;
    }

    /**
     * Make a graphical button
     * @param label - the text to show beneath the button
     * @param tip - the tooltip to show when the mouse hovers over the button
     * @param iconname - the filename of the icon to show (it will be loaded from an accompanying jar file)
     * @param func - the function to call when the button is pressed (null if we don't want to call any function)
     */
    public static JButton button (String label, String tip, String iconname, ActionListener func) {
        JButton button = new JButton(label, (iconname!=null && iconname.length()>0) ? loadIcon(iconname) : null);
        if (func!=null) button.addActionListener(func);
        button.setVerticalTextPosition(JButton.BOTTOM);
        button.setHorizontalTextPosition(JButton.CENTER);
        button.setBorderPainted(false);
        button.setFocusable(false);
        if (!Util.onMac()) button.setBackground(new Color(0.9f, 0.9f, 0.9f));
        button.setFont(button.getFont().deriveFont(10.0f));
        if (tip!=null && tip.length()>0) button.setToolTipText(tip);
        return button;
    }

    /** Load the given image file from an accompanying JAR file, and return it as an Icon object. */
    public static Icon loadIcon(String pathname) {
        URL url = OurUtil.class.getClassLoader().getResource(pathname);
        if (url!=null) return new ImageIcon(Toolkit.getDefaultToolkit().createImage(url));
        return new ImageIcon(new BufferedImage(2, 2, BufferedImage.TYPE_INT_RGB));
    }

    /**
     * Make a JPanel using horizontal or vertical BoxLayout, and add the components to it.
     * <br> If a component is null, we will insert a horizontal (or vertical) glue instead.
     * <br> If a component is Integer, we will insert an "n*1" (or "1*n") rigid area instead.
     * <br> If a component is String, we will insert a JLabel with it as the label.
     * <br> Each component will be aligned by xAlign and yAlign.
     * <br> Note: if a component is a Color, we will set it as the background of every component after it (until we encounter another Color object).
     */
    private static JPanel makeBox(boolean horizontal, float xAlign, float yAlign, Object[] a) {
        JPanel ans = new JPanel();
        ans.setLayout(new BoxLayout(ans, horizontal ? BoxLayout.X_AXIS : BoxLayout.Y_AXIS));
        ans.setAlignmentX(0.0f);
        ans.setAlignmentY(0.0f);
        Color color = null;
        for(int i=0; i<a.length; i++) {
            Component c;
            if (a[i] instanceof Color) {
                color=(Color)(a[i]);
                ans.setBackground(color);
                continue;
            }
            if (a[i] instanceof Component) {
                c=(Component)(a[i]);
            } else if (a[i] instanceof String) {
                c=label((String)(a[i]), Color.BLACK);
            } else if (a[i] instanceof Integer) {
                Dimension d = horizontal ?  new Dimension((Integer)a[i], 1) : new Dimension(1, (Integer)a[i]);
                c = Box.createRigidArea(d);
            } else if (a[i]==null) {
                c = horizontal ? Box.createHorizontalGlue() : Box.createVerticalGlue();
            } else {
                continue;
            }
            if (color!=null) c.setBackground(color);
            if (c instanceof JComponent) { ((JComponent)c).setAlignmentX(xAlign); ((JComponent)c).setAlignmentY(yAlign); }
            ans.add(c);
        }
        return ans;
    }

    /**
     * Make a JPanel with the given dimension using BoxLayout, and add the components to it.
     * <br> It will have the X_AXIS layout (or Y_AXIS if h>w).
     * <br> If a component is null, we will insert a horizontal (or vertical) glue instead.
     * <br> If a component is Integer, we will insert an "n*1" rigid area (or "1*n" rigid area) instead.
     * <br> If a component is String, we will insert a JLabel with it as the label.
     * <br> Each component will be center-aligned.
     * <br> Note: if a component is a Color, we will set it as the background of every component after it (until we encounter another Color object).
     */
    public static JPanel makeBox(int w, int h, Object... a) {
        JPanel ans = makeBox(w>=h, 0.5f, 0.5f, a);
        ans.setPreferredSize(new Dimension(w, h));
        ans.setMaximumSize(new Dimension(w, h));
        return ans;
    }

    /**
     * Make a JPanel using horizontal BoxLayout, and add the components to it.
     * <br> If a component is null, we will insert a horizontal glue instead.
     * <br> If a component is Integer, we will insert an "n*1" rigid area instead.
     * <br> If a component is String, we will insert a JLabel with it as the label.
     * <br> Each component will be center-aligned.
     * <br> Note: if a component is a Color, we will set it as the background of every component after it (until we encounter another Color object).
     */
    public static JPanel makeH(Object... a) { return makeBox(true, 0.5f, 0.5f, a); }

    /**
     * Make a JPanel using horizontal BoxLayout, and add the components to it.
     * <br> If a component is null, we will insert a horizontal glue instead.
     * <br> If a component is Integer, we will insert an "n*1" rigid area instead.
     * <br> If a component is String, we will insert a JLabel with it as the label.
     * <br> Each component will be top-aligned.
     * <br> Note: if a component is a Color, we will set it as the background of every component after it (until we encounter another Color object).
     */
    public static JPanel makeHT(Object... a) { return makeBox(true, 0.5f, 0.0f, a); }

    /**
     * Make a JPanel using horizontal BoxLayout, and add the components to it.
     * <br> If a component is null, we will insert a horizontal glue instead.
     * <br> If a component is Integer, we will insert an "n*1" rigid area instead.
     * <br> If a component is String, we will insert a JLabel with it as the label.
     * <br> Each component will be bottom-aligned.
     * <br> Note: if a component is a Color, we will set it as the background of every component after it (until we encounter another Color object).
     */
    public static JPanel makeHB(Object... a) { return makeBox(true, 0.5f, 1.0f, a); }

    /**
     * Make a JPanel using vertical BoxLayout, and add the components to it.
     * <br> If a component is null, we will insert a vertical glue instead.
     * <br> If a component is Integer, we will insert an "1*n" rigid area instead.
     * <br> If a component is String, we will insert a JLabel with it as the label.
     * <br> Each component will be center-aligned.
     * <br> Note: if a component is a Color, we will set it as the background of every component after it (until we encounter another Color object).
     */
    public static JPanel makeV(Object... a) { return makeBox(false, 0.5f, 0.5f, a); }

    /**
     * Make a JPanel using vertical BoxLayout, and add the components to it.
     * <br> If a component is null, we will insert a vertical glue instead.
     * <br> If a component is Integer, we will insert an "1*n" rigid area instead.
     * <br> If a component is String, we will insert a JLabel with it as the label.
     * <br> Each component will be left-aligned.
     * <br> Note: if a component is a Color, we will set it as the background of every component after it (until we encounter another Color object).
     */
    public static JPanel makeVL(Object... a) { return makeBox(false, 0.0f, 0.5f, a); }

    /**
     * Make a JPanel using vertical BoxLayout, and add the components to it.
     * <br> If a component is null, we will insert a vertical glue instead.
     * <br> If a component is Integer, we will insert an "1*n" rigid area instead.
     * <br> If a component is String, we will insert a JLabel with it as the label.
     * <br> Each component will be right-aligned.
     * <br> Note: if a component is a Color, we will set it as the background of every component after it (until we encounter another Color object).
     */
    public static JPanel makeVR(Object... a) { return makeBox(false, 1.0f, 0.5f, a); }

    /**
     * Constructs a new SplitPane containing the two components given as arguments
     * @param orientation - the orientation (HORIZONTAL_SPLIT or VERTICAL_SPLIT)
     * @param leftComp - the left component (if horizontal) or top component (if vertical)
     * @param rightComp - the right component (if horizontal) or bottom component (if vertical)
     * @param initialDividerLocation - the initial divider location (in pixels)
     */
    public static JSplitPane splitpane (int orientation, Component leftComp, Component rightComp, int initialDividerLocation) {
        JSplitPane x = new JSplitPane(orientation, leftComp, rightComp);
        x.setBorder(null);
        x.setContinuousLayout(true);
        x.setDividerLocation(initialDividerLocation);
        x.setOneTouchExpandable(false);
        x.setResizeWeight(0.5);
        if (Util.onMac() && (x.getUI() instanceof BasicSplitPaneUI)) {
            // This makes the border look nicer on Mac OS X
            boolean h = (orientation != JSplitPane.HORIZONTAL_SPLIT);
            ((BasicSplitPaneUI)(x.getUI())).getDivider().setBorder(new OurBorder(h,h,h,h));
        }
        return x;
    }

    /**
     * Convenience method that recursively enables every JMenu and JMenuItem inside "menu".
     * @param menu - the menu to start the recursive search
     */
    public static void enableAll (JMenu menu) {
        for(int i=0; i<menu.getMenuComponentCount(); i++) {
            Component obj=menu.getMenuComponent(i);
            if (obj instanceof JMenuItem) {
                ((JMenuItem)obj).setEnabled(true);
            } else if (obj instanceof JMenu) {
                enableAll((JMenu)obj);
            }
        }
    }

    /**
     * Construct a new JMenu and add it to an existing JMenuBar.
     *
     * <p> Note: every time the user expands then collapses this JMenu,
     * it will automatically enable all JMenu and JMenuItem objects inside it.
     *
     * @param parent - the JMenuBar to add this Menu into (or null if we don't want to add it to a JMenuBar yet)
     * @param label - the label to show on screen
     * @param mnemonic - the mnemonic (eg. KeyEvent.VK_F), or -1 if you don't want mnemonic
     * @param func - the function to call if the user expands this menu (or null if there is no function to call)
     */
    public static JMenu makeMenu (JMenuBar parent, String label, int mnemonic, final Runnable func) {
        final JMenu x = new JMenu(label, false);
        if (mnemonic!=-1 && !Util.onMac()) x.setMnemonic(mnemonic);
        x.addMenuListener(new MenuListener() {
            public final void menuSelected (MenuEvent e) { if (func!=null) func.run(); }
            public final void menuDeselected (MenuEvent e) { OurUtil.enableAll(x); }
            public final void menuCanceled (MenuEvent e) { OurUtil.enableAll(x); }
        });
        if (parent!=null) parent.add(x);
        return x;
    }

    /**
     * Construct a new JMenuItem then add it to an existing JMenu.
     * @param parent - the JMenu to add this JMenuItem into (or null if you don't want to add it to any JMenu yet)
     * @param label - the text to show on the menu
     * @param attrs - a list of attributes to apply onto the new JMenuItem
     * <p> If one nonnegative integer a is supplied, we call setMnemonic(a)
     * <p> If two nonnegative integers a and b are supplied, and a!=VK_ALT, and a!=VK_SHIFT, we call setMnemoic(a) and setAccelerator(b)
     * <p> If two nonnegative integers a and b are supplied, and a==VK_ALT or a==VK_SHIFT, we call setAccelerator(a | b)
     * <p> If an Icon is supplied, we call setIcon()
     * <p> If an ActionListener is supplied, we call addActionListener()
     * <p> If true is supplied, we call setEnabled(true)
     * <p> If false is supplied, we call setEnabled(false)
     */
    public static JMenuItem makeMenuItem (JMenu parent, String label, Object... attrs) {
        JMenuItem x = new JMenuItem(label, null);
        int accelMask = Toolkit.getDefaultToolkit().getMenuShortcutKeyMask();
        boolean hasMnemonic = false;
        for(int i=0; i<attrs.length; i++) {
            Object at = attrs[i];
            if (at instanceof Integer) {
                int k = (Integer)at;
                if (k<0) continue;
                if (k==KeyEvent.VK_ALT)   { hasMnemonic=true; accelMask = accelMask | InputEvent.ALT_MASK;   continue; }
                if (k==KeyEvent.VK_SHIFT) { hasMnemonic=true; accelMask = accelMask | InputEvent.SHIFT_MASK; continue; }
                if (!hasMnemonic) x.setMnemonic(k); else x.setAccelerator(KeyStroke.getKeyStroke(k, accelMask));
                hasMnemonic=true;
            }
            if (at instanceof ActionListener) x.addActionListener((ActionListener)at);
            if (at instanceof Icon) x.setIcon((Icon)at);
            if (Boolean.TRUE.equals(at)) x.setEnabled(true);
            if (Boolean.FALSE.equals(at)) x.setEnabled(false);
        }
        if (parent!=null) parent.add(x);
        return x;
    }

    /** This method minimizes the window. */
    public static void minimize(JFrame frame) { frame.setExtendedState(JFrame.ICONIFIED); }

    /** This method alternatingly maximizes or restores the window. */
    public static void zoom(JFrame frame) {
        int goal = JFrame.MAXIMIZED_BOTH;
        if ((frame.getExtendedState() & goal)==goal) goal=JFrame.NORMAL;
        frame.setExtendedState(goal);
    }

    /** Make the frame visible, non-iconized, and focused. */
    public static void show(JFrame frame) {
        frame.setVisible(true);
        frame.setExtendedState(frame.getExtendedState() & ~JFrame.ICONIFIED);
        frame.requestFocus();
        frame.toFront();
    }
}
