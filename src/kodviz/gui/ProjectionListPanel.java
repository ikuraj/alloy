package kodviz.gui;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Iterator;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ListCellRenderer;
import javax.swing.ListSelectionModel;
import javax.swing.UIManager;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import kodviz.util.Dbg;

import kodviz.alloyviz.AlloyType;


/**
 * ProjectionListPanel is a JPanel with two lists. The first list
 * contains all types specified that are not projected. The second
 * list contains the types being projected. The panel allows types to
 * switch between the two lists. Multiple selections of items in the
 * lists are allowed.
 */
@SuppressWarnings("serial")
public class ProjectionListPanel extends JPanel {
    /* The types that are not being projected.*/
    private JList nonProjectedTypesList;
    //private List nonProjected;
    private DefaultListModel nonProjectedTypesModel;

    /* The types that are being projectd*/
    private JList projectedTypesList;
    //private List projected;
    private DefaultListModel projectedTypesModel;
    /**
     * Invariant: Both lists should always have something selected if
     * the lists are not empty.
     */

    /* The button to project based the type
    selected from nonProjectedTypes*/
    private JButton projectButton;

    /* The button to project based on the type selected from*/
    /* projectedTypesList*/
    private JButton unProjectButton;

    private ModulePanel _parent;

    /*Listener for buttons*/
    private final ActionListener buttonListener = new ActionListener() {
        public void actionPerformed(ActionEvent evt) {
            Object source = evt.getSource();
            if (source == projectButton) {
                // something should be selected in the left list
                // otherwise, this button should be disabled
                int[] selections = nonProjectedTypesList.getSelectedIndices();
                for (int i = 0; i < selections.length; i++) {
                    Object value = nonProjectedTypesModel.get(selections[i] - i);
                    // remove from left list
                    _parent.projectOn((AlloyType)value);
                    nonProjectedTypesModel.remove(selections[i] - i);
                    // add to right list
                    projectedTypesModel.addElement(value);
                }

                // set selection for right list to first element
                projectedTypesList.setSelectedIndex(0);

                // if the left list is empty, disable project button
                // else set selection to first element
                if (nonProjectedTypesModel.isEmpty()) {
                    projectButton.setEnabled(false);
                } else {
                    nonProjectedTypesList.setSelectedIndex(0);
                }
            } else if (source == unProjectButton) {
                // something should be selected or this button
                // should be disabled
                int[] selections = projectedTypesList.getSelectedIndices();
                for (int i = 0; i < selections.length; i++) {
                    Object value = projectedTypesModel.get(selections[i] - i);
                    // remove from right list
                    _parent.deproject((AlloyType)value);
                    projectedTypesModel.remove(selections[i] - i);
                    // add to left list
                    nonProjectedTypesModel.addElement(value);
                }
                // set the selection for left list to first element
                nonProjectedTypesList.setSelectedIndex(0);

                // if the right list is empty, disable unproject button
                // else set selection to first element
                if (projectedTypesModel.isEmpty()) {
                    unProjectButton.setEnabled(false);
                } else {
                    projectedTypesList.setSelectedIndex(0);
                }
            } else {
                Dbg.fail("This should never happen ... Project Lists");
            }
        }
    };

    /* Listener for two lists. Used mainly for enabling/disabling
    buttons when lists are empty/nonempty*/
    private final ListSelectionListener listListener = new ListSelectionListener() {
        public void valueChanged(ListSelectionEvent evt) {
            if (evt.getValueIsAdjusting()) {
                return;
            }
            Object source = evt.getSource();
            if (source == nonProjectedTypesList) {
                if (!projectButton.isEnabled()) {
                    //enable button with selection in left list
                    projectButton.setEnabled(true);
                }
            }
            if (source == projectedTypesList) {
                if (!unProjectButton.isEnabled()) {
                    //enable button with selection in right list
                    unProjectButton.setEnabled(true);
                }
            }
        }
    };

    /**
     * Creates a ProjectionListPanel with specified list of types
     * that are/are not projected in the view.
     * One should pass in lists of AlloyTypes
     */
    public ProjectionListPanel(ModulePanel parent_, List nonProjectedTypes, List projectedTypes) {
        this.setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
        this.setMaximumSize(new Dimension(Short.MAX_VALUE,
        Short.MAX_VALUE));

        _parent = parent_;

        JPanel leftPanel = createLeftPanel(nonProjectedTypes);
        JPanel rightPanel = createRightPanel(projectedTypes);

        //leftPanel.setMaximumSize(new Dimension(600, Short.MAX_VALUE));
        //rightPanel.setMaximumSize(new Dimension(600, Short.MAX_VALUE));

        this.add(leftPanel);
        this.add(Box.createRigidArea(new Dimension(20, 0)));
        this.add(rightPanel);
    }

    /**
     * Sets the list of unprojected types to types
     */
    private void setNonProjectedList(List types) {
        // populate list model
        nonProjectedTypesModel.clear();
        Iterator typesIterator = types.iterator();
        while (typesIterator.hasNext()) {
            nonProjectedTypesModel.addElement(typesIterator.next());
        }

        // set selection to first element if list is not empty
        if (!nonProjectedTypesModel.isEmpty()) {
            nonProjectedTypesList.setSelectedIndex(0);
        }
        // else disable project button
        else {
            projectButton.setEnabled(false);
        }
    }

    /**
     * Sets the list of projected types to types
     */
    private void setProjectedList(List types) {
        //populate list model
        projectedTypesModel.clear();
        Iterator typesIterator = types.iterator();
        while (typesIterator.hasNext()) {
            projectedTypesModel.addElement(typesIterator.next());
        }

        if (!projectedTypesModel.isEmpty()) {
            projectedTypesList.setSelectedIndex(0);
        } else {
            unProjectButton.setEnabled(false);
        }
    }

    private JPanel createLeftPanel(List types) {
        JPanel pane = new JPanel();
        //pane.setPreferredSize(new Dimension(150, 150));
        pane.setLayout(new BorderLayout());
        pane.setBorder(BorderFactory.createTitledBorder("Unprojected Types"));
        
        // use the default UI font, because on a Mac the titled border fault evidently
        // looks out of place
        Font titleFont = UIManager.getFont("Label.font");        
        pane.setFont(titleFont);
        
        nonProjectedTypesModel = new DefaultListModel();
        nonProjectedTypesList = new JList(nonProjectedTypesModel);
        //nonProjectedTypesList.setPreferredSize(new Dimension(125, 150));
        //nonProjectedTypesList.setMinimumSize(new Dimension(225, 0));
        ListCellRenderer renderer = new TypeListCellRenderer();
        nonProjectedTypesList.setCellRenderer(renderer);
        nonProjectedTypesList.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
        nonProjectedTypesList.addListSelectionListener(listListener);

        projectButton = new JButton("Project");
        projectButton.addActionListener(buttonListener);

        setNonProjectedList(types);

        JScrollPane sp = new JScrollPane(nonProjectedTypesList);

        //pane.add(nonProjectedTypesList, BorderLayout.CENTER);
        pane.add(sp, BorderLayout.CENTER);
        pane.add(projectButton, BorderLayout.SOUTH);

        return pane;
    }

    private JPanel createRightPanel(List types) {
        JPanel pane = new JPanel();
        //pane.setPreferredSize(new Dimension(150, 150));
        pane.setLayout(new BorderLayout());
        pane.setBorder(BorderFactory.createTitledBorder("Projected Types"));

        // use the default UI font, because on a Mac the titled border fault evidently
        // looks out of place
        Font titleFont = UIManager.getFont("Label.font");        
        pane.setFont(titleFont);
        
        projectedTypesModel = new DefaultListModel();
        projectedTypesList = new JList(projectedTypesModel);
        //projectedTypesList.setPreferredSize(new Dimension(125, 150));
        ListCellRenderer renderer = new TypeListCellRenderer();
        projectedTypesList.setCellRenderer(renderer);
        projectedTypesList.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
        projectedTypesList.addListSelectionListener(listListener);

        unProjectButton = new JButton("Unproject");
        unProjectButton.addActionListener(buttonListener);

        setProjectedList(types);

        JScrollPane sp = new JScrollPane(projectedTypesList);

        //pane.add(projectedTypesList, BorderLayout.CENTER);
        pane.add(sp, BorderLayout.CENTER);
        pane.add(unProjectButton, BorderLayout.SOUTH);

        return pane;
    }

    /**
     * accessors to get the types on each list
     */
    /** maybe i won't need them
    List getProjectedTypes() {
    return projected;
    }

    List getNonProjectedTypes() {
    return nonProjected;
    }
    */
}

@SuppressWarnings("serial")
class TypeListCellRenderer extends JLabel implements ListCellRenderer {

    TypeListCellRenderer() {
        super();
        setOpaque(true);
    }

    public Component getListCellRendererComponent(
        JList list,
        Object value,
        int index,
        boolean isSelected,
        boolean cellHasFocus) {

        this.setText(((AlloyType)value).getName());
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
