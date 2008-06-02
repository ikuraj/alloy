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

import java.awt.Dimension;
import java.awt.Component;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.util.List;
import java.util.ArrayList;
import java.util.Vector;
import javax.swing.Icon;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.ListCellRenderer;
import javax.swing.border.EmptyBorder;

/**
 * Graphical combobox.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
 */

public class OurCombobox extends JComboBox {

    /** This silences javac's warning about missing serialVersionUID. */
    private static final long serialVersionUID = 1L;

    /**
     * This renderer draws the combobox value using the text and icon given by the GetterSetter.
     *
     * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
     */
    private final class OurComboboxRenderer extends JLabel implements ListCellRenderer {
        /** This silences javac's warning about missing serialVersionUID. */
        private static final long serialVersionUID = 1L;
        /** This configures the JLabel with the appropriate icon and text, then return it to be displayed. */
        public Component getListCellRendererComponent(JList list, Object value, int i, boolean selected, boolean focused) {
            setOpaque(true);
            setText(do_getText(value));
            setIcon(do_getIcon(value));
            setBorder(new EmptyBorder(0, 2, 0, 0));
            setBackground(selected ? list.getSelectionBackground() : list.getBackground());
            setForeground(selected ? list.getSelectionForeground() : list.getForeground());
            return this;
        }
    }

    /** Subclass can override this method to provide the custom text for any given value (or "" if no text is needed) */
    public String do_getText(Object value) { return String.valueOf(value); }

    /** Subclass can override this method to provide the custom icon for any given value (or null if no icon is needed) */
    public Icon do_getIcon(Object value) { return null; }

    /** Subclass can override this method to react upon selection change. */
    public void do_changed(Object newValue) { }

    /** This helper method makes a copy of the list, and then optionally prepend null at the beginning of the list. */
    private static<T> Vector<T> copy (List<T> list, boolean addNull) {
        Vector<T> answer = new Vector<T>(list.size() + (addNull ? 1 : 0));
        if (addNull) answer.add(null);
        answer.addAll(list);
        return answer;
    }

    /**
     * Constructs a new OurCombobox object.
     * @param list - the list of allowed values
     */
    public OurCombobox (Vector<Object> list)  { this(false, new ArrayList<Object>(list), 0, 0, null); }

    /**
     * Constructs a new OurCombobox object.
     * @param list - the list of allowed values
     */
    public OurCombobox (Object[] list)  { this(false, Util.asList(list), 0, 0, null); }

    /**
     * Constructs a new OurCombobox object.
     * @param addNull - whether we should prepend null onto the beginning of the list of allowed values
     * @param list - the list of allowed values
     * @param width - the width to use (if width==0 && height==0, then we ignore this parameter)
     * @param height - the height to use (if width==0 && height==0, then we ignore this parameter)
     * @param initialValue - the initial value to choose in this combo box
     */
    public OurCombobox (boolean addNull, List<?> list, int width, int height, Object initialValue) {
        super(copy(list, addNull));
        setFont(OurUtil.getVizFont());
        setRenderer(new OurComboboxRenderer());
        if (width>0 && height>0) {
            if (Util.onWindows() && height>25) height=25; // Otherwise, the height is too high on Windows
            setPreferredSize(new Dimension(width, height));
            setMaximumSize(new Dimension(width, height));
        }
        if (!Util.onWindows() && !Util.onMac() && width>0 && height>0) {
            setBorder(new EmptyBorder(4, 3, 4, 0));
        }
        if (initialValue!=null) setSelectedItem(initialValue);
        addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) { OurCombobox.this.do_changed(getSelectedItem()); }
        });
    }
}
