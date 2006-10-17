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

public final class OurCombobox extends JComboBox {

    private static final long serialVersionUID = 1L;

    private final ComboGetterSetter gs;
    private final Object field;

    public interface ComboGetterSetter {
        public Icon getIcon(Object key, Object value);
        public String getText(Object key, Object value);
        public Object getValue(Object key);
        public void setValue(Object key, Object value);
    }

    private final class DotPropertyRenderer extends JLabel implements ListCellRenderer {
        private static final long serialVersionUID = 1L;
        public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected, boolean cellHasFocus) {
            setOpaque(true);
            setText(gs.getText(field,value));
            setIcon(gs.getIcon(field,value));
            setBorder(BorderFactory.createEmptyBorder(0, 2, 0, 0));
            if (isSelected) {
                setBackground(list.getSelectionBackground());
                setForeground(list.getSelectionForeground());
            } else {
                setBackground(list.getBackground());
                setForeground(list.getForeground());
            }
            return this;
        }
    }

    private static Vector<Object> addNull(Vector<Object> list, boolean flag) {
        Vector<Object> answer=new Vector<Object>();
        if (flag) answer.add(null);
        answer.addAll(list);
        return answer;
    }

    public OurCombobox(final ComboGetterSetter gs, boolean addNull, Vector<Object> list, int width, int height, final Object field) {
        super(addNull(list,addNull));
        this.gs=gs;
        this.field = field;
        setRenderer(new DotPropertyRenderer());
        if (Util.onWindows()) { if (height>25) height=25; setPreferredSize(new Dimension(width,height)); }
        setMaximumSize(new Dimension(width,height));
        if (!Util.onWindows()) setBorder(BorderFactory.createEmptyBorder(4, 3, 4, 0));
        // To avoid useless or harmful synchronizing between Model and View,
        // we make sure we set the initial value before adding the ActionListener.
        setSelectedItem(gs.getValue(field));
        addActionListener(new ActionListener() {
            public final void actionPerformed(ActionEvent e) { gs.setValue(field, getSelectedItem()); }
        });
    }
}
