/*
 * Alloy Analyzer
 * Copyright (c) 2007 Massachusetts Institute of Technology
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA,
 * 02110-1301, USA
 */

package edu.mit.csail.sdg.alloy4;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.InputEvent;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.net.URL;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JEditorPane;
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
import javax.swing.border.EmptyBorder;
import javax.swing.event.MenuEvent;
import javax.swing.event.MenuListener;
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

    /** Returns the recommended font to use in the visualizer, based on the OS. */
    public static Font getVizFont() {
        return Util.onMac() ? new Font("Lucida Grande",Font.PLAIN,11) : new Font("Dialog",Font.PLAIN,12);
    }

    /** Returns the screen height (in pixels). */
    public static int getScreenHeight() {
        return Toolkit.getDefaultToolkit().getScreenSize().height;
    }

    /** Returns the screen width (in pixels). */
    public static int getScreenWidth() {
        return Toolkit.getDefaultToolkit().getScreenSize().width;
    }

    /** Run r.run() using the AWT event thread; if it's not the AWT event thread, use SwingUtilities.invokeAndWait() on it. */
    public static void invokeAndWait(Runnable r) {
        if (SwingUtilities.isEventDispatchThread()) {
            r.run();
            return;
        }
        try {
            SwingUtilities.invokeAndWait(r);
        } catch (InterruptedException e) {
            // Nothing we can do about it
        } catch (InvocationTargetException e) {
            // Nothing we can do about it
        }
    }

    /**
     * Make a graphical button
     * @param label - the text to show beneath the button
     * @param tip - the tooltip to show when the mouse hovers over the button
     * @param iconname - the filename of the icon to show (it will be loaded from an accompanying jar file)
     * @param func - the function to call when the button is pressed (null if we don't want to call any function)
     * @param key - the parameter to pass to func() when the button is pressed
     */
    public static JButton button(String label, String tip, String iconname, MultiRunnable func, int key) {
        JButton button = new JButton(label, (iconname!=null && iconname.length()>0) ? loadIcon(iconname) : null);
        if (func!=null) button.addActionListener(new MultiRunner(func,key));
        button.setVerticalTextPosition(JButton.BOTTOM);
        button.setHorizontalTextPosition(JButton.CENTER);
        button.setBorderPainted(false);
        button.setFocusable(false);
        if (!Util.onMac()) button.setBackground(new Color(0.9f, 0.9f, 0.9f));
        button.setFont(button.getFont().deriveFont(10.0f));
        if (tip!=null && tip.length()>0) button.setToolTipText(tip);
        return button;
    }

    /** Make a JTextField. */
    public static JTextField textfield(String text, int columns) {
        JTextField answer = new JTextField(text, columns);
        return answer;
    }

    /** Make a JEditorPane. */
    public static JEditorPane editorPane(String type, String text) {
        return new JEditorPane(type, text);
    }

    /** Make a JEditorPane. */
    public static JEditorPane editorPane(URL url) throws IOException {
        return new JEditorPane(url);
    }

    /** Make a JTextArea. */
    public static JTextArea textarea(String text, int rows, int columns) {
        JTextArea answer = new JTextArea(text,rows,columns);
        return answer;
    }

    /** Make a JLabel with the given font. */
    public static JLabel label(Font font, String label) {
        JLabel answer = new JLabel(label);
        answer.setFont(font);
        return answer;
    }

    /** Make a JLabel with the given color. */
    public static JLabel label(Color color, String label) {
        JLabel answer = label(getVizFont(), label);
        answer.setForeground(color);
        return answer;
    }

    /** Load the given image file from an accompanying JAR file, and return it as an Icon object. */
    public static Icon loadIcon(String pathname) {
        URL url = OurUtil.class.getClassLoader().getResource(pathname);
        if (url!=null) {
            return new ImageIcon(Toolkit.getDefaultToolkit().createImage(url));
        }
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
     * <br> If a component is Integer, we will insert an "n*1" rigid area (or "1*n" rigid area) instead.
     * <br> If a component is String, we will insert a JLabel with it as the label.
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
            if (a[i] instanceof Color) {
                color=(Color)(a[i]);
                ans.setBackground(color);
                continue;
            }
            if (a[i] instanceof Component) {
                c = (Component)(a[i]);
            } else if (a[i] instanceof String) {
                c = label(Color.BLACK, (String)(a[i]));
            } else if (a[i] instanceof Integer) {
                Dimension d = (w>=h) ? new Dimension((Integer)a[i], 1) : new Dimension(1, (Integer)a[i]);
                c = Box.createRigidArea(d);
            } else if (a[i]==null) {
                c = (w>=h) ? Box.createHorizontalGlue() : Box.createVerticalGlue();
            } else {
                continue;
            }
            if (color!=null) {
                c.setBackground(color);
            }
            if (c instanceof JComponent) {
                ((JComponent)c).setAlignmentX(0.5f);
                ((JComponent)c).setAlignmentY(0.5f);
            }
            ans.add(c);
        }
        return ans;
    }

    /**
     * Make a JPanel using BoxLayout, and add the components to it.
     * <br> If a component is null, we will insert a horizontal (or vertical) glue instead.
     * <br> If a component is Integer, we will insert an "n*1" (or "1*n") rigid area instead.
     * <br> If a component is String, we will insert a JLabel with it as the label.
     * <br> Each component will be aligned by xAlign and yAlign.
     * <br> Note: if the first component is a Color, we will set everything's background color to that color.
     */
    private static JPanel makeBox(boolean horizontal, float xAlign, float yAlign, Object[] a) {
        JPanel ans=makeBox(horizontal ? BoxLayout.X_AXIS : BoxLayout.Y_AXIS);
        Color color=null;
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
                c=label(Color.BLACK, (String)(a[i]));
            } else if (a[i] instanceof Integer) {
                Dimension d = horizontal ?  new Dimension((Integer)a[i], 1) : new Dimension(1, (Integer)a[i]);
                c = Box.createRigidArea(d);
            } else if (a[i]==null) {
                c = horizontal ? Box.createHorizontalGlue() : Box.createVerticalGlue();
            } else {
                continue;
            }
            if (color!=null) {
                c.setBackground(color);
            }
            if (c instanceof JComponent) {
                ((JComponent)c).setAlignmentX(xAlign);
                ((JComponent)c).setAlignmentY(yAlign);
            }
            ans.add(c);
        }
        return ans;
    }

    /**
     * Make a JPanel using horizontal BoxLayout, and add the components to it.
     * <br> If a component is null, we will insert a horizontal glue instead.
     * <br> If a component is Integer, we will insert an "n*1" rigid area instead.
     * <br> If a component is String, we will insert a JLabel with it as the label.
     * <br> Each component will be center-aligned.
     * <br> Note: if the first component is a Color, we will set everything's background color to that color.
     */
    public static JPanel makeH(Object... a) { return makeBox(true, 0.5f, 0.5f, a); }

    /**
     * Make a JPanel using horizontal BoxLayout, and add the components to it.
     * <br> If a component is null, we will insert a horizontal glue instead.
     * <br> If a component is Integer, we will insert an "n*1" rigid area instead.
     * <br> If a component is String, we will insert a JLabel with it as the label.
     * <br> Each component will be top-aligned.
     * <br> Note: if the first component is a Color, we will set everything's background color to that color.
     */
    public static JPanel makeHT(Object... a) { return makeBox(true, 0.5f, 0.0f, a); }

    /**
     * Make a JPanel using horizontal BoxLayout, and add the components to it.
     * <br> If a component is null, we will insert a horizontal glue instead.
     * <br> If a component is Integer, we will insert an "n*1" rigid area instead.
     * <br> If a component is String, we will insert a JLabel with it as the label.
     * <br> Each component will be bottom-aligned.
     * <br> Note: if the first component is a Color, we will set everything's background color to that color.
     */
    public static JPanel makeHB(Object... a) { return makeBox(true, 0.5f, 1.0f, a); }

    /**
     * Make a JPanel using vertical BoxLayout, and add the components to it.
     * <br> If a component is null, we will insert a vertical glue instead.
     * <br> If a component is Integer, we will insert an "1*n" rigid area instead.
     * <br> If a component is String, we will insert a JLabel with it as the label.
     * <br> Each component will be center-aligned.
     * <br> Note: if the first component is a Color, we will set everything's background color to that color.
     */
    public static JPanel makeV(Object... a) { return makeBox(false, 0.5f, 0.5f, a); }

    /**
     * Make a JPanel using vertical BoxLayout, and add the components to it.
     * <br> If a component is null, we will insert a vertical glue instead.
     * <br> If a component is Integer, we will insert an "1*n" rigid area instead.
     * <br> If a component is String, we will insert a JLabel with it as the label.
     * <br> Each component will be left-aligned.
     * <br> Note: if the first component is a Color, we will set everything's background color to that color.
     */
    public static JPanel makeVL(Object... a) { return makeBox(false, 0.0f, 0.5f, a); }

    /**
     * Make a JPanel using vertical BoxLayout, and add the components to it.
     * <br> If a component is null, we will insert a vertical glue instead.
     * <br> If a component is Integer, we will insert an "1*n" rigid area instead.
     * <br> If a component is String, we will insert a JLabel with it as the label.
     * <br> Each component will be right-aligned.
     * <br> Note: if the first component is a Color, we will set everything's background color to that color.
     */
    public static JPanel makeVR(Object... a) { return makeBox(false, 1.0f, 0.5f, a); }

    /** Make an empty JScrollPane; scrollbars will only show up as needed. */
    public static JScrollPane scrollpane() {
        JScrollPane ans = new JScrollPane(VERTICAL_SCROLLBAR_AS_NEEDED, HORIZONTAL_SCROLLBAR_AS_NEEDED);
        ans.setMinimumSize(new Dimension(50, 50));
        ans.setBorder(new EmptyBorder(0,0,0,0));
        return ans;
    }

    /** Make a JScrollPane containing the component; scrollbars will only show up as needed. */
    public static JScrollPane scrollpane(Component component) {
        JScrollPane ans = new JScrollPane(component, VERTICAL_SCROLLBAR_AS_NEEDED, HORIZONTAL_SCROLLBAR_AS_NEEDED);
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

    /** Constructs a JMenuBar */
    public static JMenuBar makeMenuBar() {
        return new JMenuBar();
    }

    /** Constructs a JMenu */
    public static JMenu makeMenu(String label) {
        return new JMenu(label, false);
    }

    /** Constructs a JMenuItem */
    public static JMenuItem makeMenuItem(String label, Icon icon) {
        return new JMenuItem(label, icon);
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
     * @param key - the argument to pass to func() when the user expands this menu
     */
    public static JMenu makeMenu(JMenuBar parent, String label, int mnemonic, final MultiRunnable func, final int key) {
        final JMenu x = makeMenu(label);
        if (mnemonic!=-1 && !Util.onMac()) {
            x.setMnemonic(mnemonic);
        }
        x.addMenuListener(new MenuListener() {
            public final void menuSelected (MenuEvent e) {
                if (func!=null) {
                    func.run(key);
                }
            }
            public final void menuDeselected (MenuEvent e) {
                OurUtil.enableAll(x);
            }
            public final void menuCanceled (MenuEvent e) {
                OurUtil.enableAll(x);
            }
        });
        if (parent!=null) {
            parent.add(x);
        }
        return x;
    }

    /**
     * Construct a new JMenuItem then add it to an existing JMenu.
     * @param parent - the JMenu to add this JMenuItem into (or null if you don't want to add it to any JMenu yet)
     * @param label - the text to show on the menu
     * @param mnemonic - the mnemonic (eg. KeyEvent.VK_F), or -1 if you don't want a mnemonic
     * @param accel - the accelerator (eg. KeyEvent.VK_F), or -1 if you don't want an accelerator
     * @param func - the runnable to run if the user clicks this item (or null if there is no runnable to run)
     */
    public static JMenuItem makeMenuItem(JMenu parent, String label, int mnemonic, int accel, final Runnable func) {
        JMenuItem x = makeMenuItem(label,null);
        if (mnemonic!=-1) {
            x.setMnemonic(mnemonic);
        }
        if (accel!=-1) {
            int accelMask = Toolkit.getDefaultToolkit().getMenuShortcutKeyMask();
            x.setAccelerator(KeyStroke.getKeyStroke(accel, accelMask));
        }
        if (func!=null) {
            x.addActionListener(new ActionListener() {
                public final void actionPerformed(ActionEvent e) {
                    func.run();
                }
            });
        }
        if (parent!=null) {
            parent.add(x);
        }
        return x;
    }

    /**
     * Construct a new JMenuItem then add it to an existing JMenu.
     * @param parent - the JMenu to add this JMenuItem into (or null if you don't want to add it to any JMenu yet)
     * @param label - the text to show on the menu
     * @param enabled - true if this JMenuItem should be enabled initially; false if it should be disabled initially
     * @param mnemonic - the mnemonic (eg. KeyEvent.VK_F), or -1 if you don't want a mnemonic
     * @param accel - the accelerator (eg. KeyEvent.VK_F), or -1 if you don't want an accelerator
     * @param func - the runnable to run if the user clicks this item (or null if there is no runnable to run)
     * @param key - the argument to pass to func() when the user clicks this item
     */
    public static JMenuItem makeMenuItem(JMenu parent, String label, boolean enabled, int mnemonic, int accel, final MultiRunnable func, final int key) {
        JMenuItem x = makeMenuItem(label,null);
        if (mnemonic!=-1) {
            x.setMnemonic(mnemonic);
        }
        if (accel!=-1) {
            int accelMask = Toolkit.getDefaultToolkit().getMenuShortcutKeyMask();
            x.setAccelerator(KeyStroke.getKeyStroke(accel, accelMask));
        }
        if (func!=null) {
            x.addActionListener(new ActionListener() {
                public final void actionPerformed(ActionEvent e) {
                    func.run(key);
                }
            });
        }
        if (parent!=null) {
            parent.add(x);
        }
        if (!enabled) {
            x.setEnabled(false);
        }
        return x;
    }

    /**
     * Construct a new JMenuItem then add it to an existing JMenu with SHIFT+accelerator.
     * @param parent - the JMenu to add this JMenuItem into (or null if you don't want to add it to any JMenu yet)
     * @param label - the text to show on the menu
     * @param accel - the accelerator (eg. KeyEvent.VK_F); we will add the "SHIFT" mask on top of it
     * @param func - the action listener to call if the user clicks this item (or null if there is no action to do)
     */
    public static JMenuItem makeMenuItemWithShift(JMenu parent, String label, int accel, ActionListener func) {
        JMenuItem x = makeMenuItem(label,null);
        int accelMask = Toolkit.getDefaultToolkit().getMenuShortcutKeyMask();
        x.setAccelerator(KeyStroke.getKeyStroke(accel, accelMask | InputEvent.SHIFT_MASK));
        if (func!=null) {
            x.addActionListener(func);
        }
        if (parent!=null) {
            parent.add(x);
        }
        return x;
    }
}
