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

import java.awt.Dimension;
import java.awt.Component;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.util.List;
import java.util.ArrayList;
import java.util.Vector;
import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.ListCellRenderer;

/**
 * Graphical combobox.
 *
 * <p> To construct a OurCombobox object, you need to give it a GetterSetter object and a key.
 * <br> When the combobox is first created, it calls gs.get(key) to find out the initial value.
 * <br> When the combobox value is changed by the user, it immediately calls gs.set(key,value) to notify the change.
 *
 * <p> This combobox does remember its current selection; so if the underlying data is altered by other code,
 * then this combobox and the underlying data will become out-of-sync.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
 */

public final class OurCombobox extends JComboBox {

    /** This silences javac's warning about missing serialVersionUID. */
    private static final long serialVersionUID = 1L;

    /** The GetterSetter associated with this combobox. */
    private final ComboGetterSetter gs;

    /** The key associated with this combobox. */
    private final Object key;

    /**
     * This defines getIcon/getText/getValue/setValue methods.
     *
     * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
     */
    public interface ComboGetterSetter {
        /** Given a key and a value, this returns a suitable icon to display; returns null if no icon is needed. */
        public Icon getIcon(Object key, Object value);
        /** Given a key and a value, this returns a suitable text to display. */
        public String getText(Object key, Object value);
        /** This retrieves the current value for that key. */
        public Object getValue(Object key);
        /** This sets the current value for that key. */
        public void setValue(Object key, Object value);
    }

    /**
     * This renderer draws the combobox value using the text and icon given by the GetterSetter.
     *
     * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
     */
    private final class OurComboboxRenderer extends JLabel implements ListCellRenderer {
        /** This silences javac's warning about missing serialVersionUID. */
        private static final long serialVersionUID = 1L;
        /** This configures the JLabel with the appropriate icon and text, then return it to be displayed. */
        public Component getListCellRendererComponent(JList list,Object value,int i,boolean selected,boolean focused) {
            setOpaque(true);
            setText(gs!=null ? gs.getText(key,value) : String.valueOf(value));
            setIcon(gs!=null ? gs.getIcon(key,value) : null);
            setBorder(BorderFactory.createEmptyBorder(0, 2, 0, 0));
            setBackground(selected ? list.getSelectionBackground() : list.getBackground());
            setForeground(selected ? list.getSelectionForeground() : list.getForeground());
            return this;
        }
    }

    /** This helper method makes a copy of the list, and then optionally prepend null at the beginning of the list. */
    private static Vector<Object> copy (List<Object> list, boolean addNull) {
        Vector<Object> answer = new Vector<Object>(list.size() + (addNull?1:0));
        if (addNull) { answer.add(null); }
        answer.addAll(list);
        return answer;
    }

    /**
     * Constructs a new OurCombobox object.
     * @param list - the list of allowed values
     */
    public OurCombobox (Vector<Object> list) {
        this(null, false, new ArrayList<Object>(list), 0, 0, null);
    }

    /**
     * Constructs a new OurCombobox object.
     * @param list - the list of allowed values
     */
    public OurCombobox (Object[] list) {
        this(null, false, Util.asList(list), 0, 0, null);
    }

    /**
     * Constructs a new OurCombobox object.
     * @param gs - the GetterSetter associated with this combobox (null if there is no need to call getter/setter)
     * @param addNull - whether we should prepend null onto the beginning of the list of allowed values
     * @param list - the list of allowed values
     * @param width - the width to use (if width==0 && height==0, then we ignore this parameter)
     * @param height - the height to use (if width==0 && height==0, then we ignore this parameter)
     * @param key - the key associated with this combobox
     */
    public OurCombobox
    (final ComboGetterSetter gs, boolean addNull, List<Object> list, int width, int height, final Object key) {
        super(copy(list, addNull));
        this.gs = gs;
        this.key = key;
        setFont(OurUtil.getVizFont());
        setRenderer(new OurComboboxRenderer());
        if (width>0 && height>0) {
            if (Util.onWindows() && height>25) height=25; // Otherwise, the height is too high on Windows
            setPreferredSize(new Dimension(width,height));
            setMaximumSize(new Dimension(width,height));
        }
        if (!Util.onWindows() && !Util.onMac() && width>0 && height>0) {
            setBorder(BorderFactory.createEmptyBorder(4, 3, 4, 0));
        }
        // To avoid useless or harmful synchronization between this GUI and the underlying data,
        // we make sure we set the initial value before adding the ActionListener.
        if (gs!=null) {
            setSelectedItem(gs.getValue(key));
            addActionListener(new ActionListener() {
                public final void actionPerformed(ActionEvent e) {
                    gs.setValue(key, getSelectedItem());
                }
            });
        }
    }
}
