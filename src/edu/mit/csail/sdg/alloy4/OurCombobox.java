package edu.mit.csail.sdg.alloy4;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
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
 * <p/> To construct a OurCombobox object, you need to give it a GetterSetter object and a key.
 * <br/> When the combobox is first created, it calls gs.get(key) to find out the initial value.
 * <br/> When the combobox value is changed by the user, it immediately calls gs.set(key,value) to notify the change.
 *
 * <p/> This combobox does remember its current selection; so if the underlying data is altered by other code,
 * then this combobox and the underlying data will become out-of-sync.
 *
 * <p/><b>Thread Safety:</b> Can be called only by the AWT thread.
 *
 * @author Felix Chang
 */

public final class OurCombobox extends JComboBox {

    /** This silences javac's warning about missing serialVersionUID. */
    private static final long serialVersionUID = 1L;

    /** The GetterSetter associated with this combobox. */
    private final ComboGetterSetter gs;

    /** The key associated with this combobox. */
    private final Object key;

    /**
     * This interface defines the getIcon, getText, getValue, and setValue methods.
     *
     * <p/><b>Thread Safety:</b> Can be called only by the AWT thread.
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
     * <p/><b>Thread Safety:</b> Can be called only by the AWT thread.
     */
    private final class OurComboboxRenderer extends JLabel implements ListCellRenderer {
        /** This silences javac's warning about missing serialVersionUID. */
        private static final long serialVersionUID = 1L;
        /** This configures the JLabel with the appropriate icon and text, then return it to be displayed. */
        public Component getListCellRendererComponent(JList list,Object value,int i,boolean selected,boolean focused) {
            setOpaque(true);
            setText(gs.getText(key,value));
            setIcon(gs.getIcon(key,value));
            setBorder(BorderFactory.createEmptyBorder(0, 2, 0, 0));
            setBackground(selected ? list.getSelectionBackground() : list.getBackground());
            setForeground(selected ? list.getSelectionForeground() : list.getForeground());
            return this;
        }
    }

    /** This helper method makes a copy of the list, and then optionally prepend null at the beginning of the list. */
    private static Vector<Object> addNull(Vector<Object> list, boolean addNull) {
        Vector<Object> answer=new Vector<Object>();
        if (addNull) answer.add(null);
        answer.addAll(list);
        return answer;
    }

    /**
     * Constructs a new OurCombobox object.
     * @param gs - the GetterSetter associated with this combobox
     * @param addNull - whether we should prepend null onto the beginning of the list of allowed values
     * @param list - the list of allowed values
     * @param width - the maximum width to use
     * @param height - the maximum height to use
     * @param key - the key associated with this combobox
     */
    public OurCombobox(
            final ComboGetterSetter gs, boolean addNull, Vector<Object> list,
            int width, int height, final Object key) {
        super(addNull(list,addNull));
        this.gs = gs;
        this.key = key;
        setRenderer(new OurComboboxRenderer());
        if (Util.onWindows()) { if (height>25) height=25; setPreferredSize(new Dimension(width,height)); }
        setMaximumSize(new Dimension(width,height));
        if (!Util.onWindows()) setBorder(BorderFactory.createEmptyBorder(4, 3, 4, 0));
        // To avoid useless or harmful synchronizing between this GUI and the underlying data,
        // we make sure we set the initial value before adding the ActionListener.
        setSelectedItem(gs.getValue(key));
        addActionListener(new ActionListener() {
            public final void actionPerformed(ActionEvent e) { gs.setValue(key, getSelectedItem()); }
        });
    }
}
