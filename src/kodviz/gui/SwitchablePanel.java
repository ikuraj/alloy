/*
 * Alloy Analyzer
 * Copyright (c) 2002 Massachusetts Institute of Technology
 *
 * The Alloy Analyzer is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * The Alloy Analyzer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with the Alloy Analyzer; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

package kodviz.gui;

import java.awt.Dimension;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.Border;

/**
 * SwitchablePanel is used to easily interact with the two checkboxes that
 * hide the visual options and the unselected elements
 */
@SuppressWarnings("unchecked")
public class SwitchablePanel extends JPanel {

	private static final long serialVersionUID = 1L;

	private Border WIDGET_BORDER = BorderFactory.createEmptyBorder(5, 5, 5, 5);

    private boolean basVizOptionsOn;
    private boolean advVizOptionsOn;
    private boolean showUnselectedOn;

    private List checkBoxes, headers, vizOptions, elements;

    public SwitchablePanel(String label_) {
        this(label_, true, true, true);
    }
    public SwitchablePanel(
        String label_,
        boolean showUnselect_,
        boolean showBas_,
        boolean showAdv_) {
        super();
        basVizOptionsOn = true;
        advVizOptionsOn = true;
        showUnselectedOn = true;
        setBorder(WIDGET_BORDER);
        //BorderFactory.createCompoundBorder(WIDGET_BORDER,BorderFactory.createTitledBorder(label_)));
        setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));

        checkBoxes = new ArrayList();
        headers = new ArrayList();
        vizOptions = new ArrayList();
        elements = new ArrayList();

        showUnselectedOn = showUnselect_;
        basVizOptionsOn = showBas_;
        advVizOptionsOn = showAdv_;

    }

    /**
     * take a checkbox (with element name), a "header" panel for visibility
     * radio buttons and a viz options panel, and the element itself.
     * Adds to the appropriate lists and also puts the swing components
     * in this panel.
     */
    public void addEntry(JCheckBox cb, JPanel header, JPanel viz, Object elt) {

        checkBoxes.add(cb);
        headers.add(header);
        vizOptions.add(viz);
        elements.add(elt);
        header.setAlignmentX(RIGHT_ALIGNMENT);
        JPanel temp = new JPanel();
        temp.setLayout(new BoxLayout(temp, BoxLayout.X_AXIS));
        temp.setAlignmentX(LEFT_ALIGNMENT);
        // temp.add(cb); jbaek disabled TODO
        	temp.add(new JLabel(cb.getText()));
        //temp.add(Box.createHorizontalGlue());
        temp.add(Box.createHorizontalStrut(15));
        temp.add(header);
        //temp.setMaximumSize(new Dimension(550,60));
        this.add(temp);
        this.add(viz);

        this.revalidate();
        this.repaint();
    }

    public void hideBasicOptions() {
        basVizOptionsOn = false;
        for (int i = 0; i < vizOptions.size(); i++) {
            //((JComponent)vizOptions.get(i)).setVisible(false);
             ((OptionsPanel)vizOptions.get(i)).showBasic(false);
            //((JComponent)headers.get(i)).setVisible(false);
        }
        this.setMaximumSize(
            new Dimension(this.getMaximumSize().width, this.getPreferredSize().height + 5));
    }

    public void hideAdvancedOptions() {
        advVizOptionsOn = false;
        for (int i = 0; i < vizOptions.size(); i++) {
            //((JComponent)vizOptions.get(i)).setVisible(false);
             ((OptionsPanel)vizOptions.get(i)).showAdvanced(false);
            //((JComponent)headers.get(i)).setVisible(false);
        }
        this.setMaximumSize(
            new Dimension(this.getMaximumSize().width, this.getPreferredSize().height + 5));
    }

    public void showBasicOptions() {
        basVizOptionsOn = true;
        if (!showUnselectedOn) {
            for (int i = 0; i < checkBoxes.size(); i++) {
                JCheckBox temp = (JCheckBox)checkBoxes.get(i);
                if (temp.isSelected()) {
                    // if we're hiding unselected, then just show
                    // the selected ones' viz options (and headers)
                     ((JComponent)vizOptions.get(i)).setVisible(true);
                    ((OptionsPanel)vizOptions.get(i)).showBasic(true);
                    ((JComponent)headers.get(i)).setVisible(true);
                }
            }
        } else {
            for (int i = 0; i < vizOptions.size(); i++) {
                ((JComponent)vizOptions.get(i)).setVisible(true);
                ((OptionsPanel)vizOptions.get(i)).showBasic(true);
                ((JComponent)headers.get(i)).setVisible(true);
            }
        }
        this.setMaximumSize(
            new Dimension(this.getMaximumSize().width, this.getPreferredSize().height + 5));
    }

    public void showAdvancedOptions() {
        advVizOptionsOn = true;
        if (!showUnselectedOn) {
            for (int i = 0; i < checkBoxes.size(); i++) {
                JCheckBox temp = (JCheckBox)checkBoxes.get(i);
                if (temp.isSelected()) {
                    // if we're hiding unselected, then just show
                    // the selected ones' viz options (and headers)
                     ((JComponent)vizOptions.get(i)).setVisible(true);
                    ((OptionsPanel)vizOptions.get(i)).showAdvanced(true);
                    ((JComponent)headers.get(i)).setVisible(true);
                }
            }
        } else {
            for (int i = 0; i < vizOptions.size(); i++) {
                ((JComponent)vizOptions.get(i)).setVisible(true);
                ((OptionsPanel)vizOptions.get(i)).showAdvanced(true);
                ((JComponent)headers.get(i)).setVisible(true);
            }
        }
        this.setMaximumSize(
            new Dimension(this.getMaximumSize().width, this.getPreferredSize().height + 5));
    }

    public void hideUnselected() {
        showUnselectedOn = false;
        int numberUnselected = 0;
        for (int i = 0; i < checkBoxes.size(); i++) {
            JCheckBox temp = (JCheckBox)checkBoxes.get(i);
            if (!temp.isSelected()) {
                // if it's not selected, hide everything
                 ((JComponent)vizOptions.get(i)).setVisible(false);
                ((JComponent)headers.get(i)).setVisible(false);
                temp.setVisible(false);
                numberUnselected++;
            } else {
                // if it IS selected, just grayout the checkbox
                temp.setEnabled(false);
            }
        }
        if (numberUnselected == checkBoxes.size()) {
            // all were unselected, hide the whole panel
            this.setVisible(false);
        }
        this.setMaximumSize(
            new Dimension(this.getMaximumSize().width, this.getPreferredSize().height + 5));
    }

    public void showUnselected() {
        showUnselectedOn = true;

        this.setVisible(true); // in case the whole thing was hidden
        if (basVizOptionsOn || advVizOptionsOn) {
            for (int i = 0; i < checkBoxes.size(); i++) {
                JCheckBox temp = (JCheckBox)checkBoxes.get(i);
                temp.setVisible(true);
                temp.setEnabled(true);
                if (!temp.isSelected()) {
                    // if it wasn't selected, and viz options is on, show the
                    // options and header
                     ((JComponent)vizOptions.get(i)).setVisible(true);
                    if (basVizOptionsOn) {
                        ((OptionsPanel)vizOptions.get(i)).showBasic(true);
                    }
                    if (advVizOptionsOn) {
                        ((OptionsPanel)vizOptions.get(i)).showAdvanced(true);
                    }
                    ((JComponent)headers.get(i)).setVisible(true);
                }
            }
        } else {
            // if viz options are off, just turn on all the checkboxes.
            for (int i = 0; i < checkBoxes.size(); i++) {
                JCheckBox temp = (JCheckBox)checkBoxes.get(i);
                temp.setVisible(true);
                temp.setEnabled(true);
                ((JComponent)headers.get(i)).setVisible(true);
            }
        }
        this.setMaximumSize(
            new Dimension(this.getMaximumSize().width, this.getPreferredSize().height + 5));
    }

    public void doSelections(boolean showUnsel, boolean showBas, boolean showAdv) {

        
        showUnselectedOn = showUnsel;
        basVizOptionsOn = showBas;
        advVizOptionsOn = showAdv;
        

        for (int i = 0; i < checkBoxes.size(); i++) {

            JCheckBox cb = (JCheckBox)checkBoxes.get(i);
            JPanel viz = (JPanel)vizOptions.get(i);
            JComponent header = (JComponent)headers.get(i);
            ((OptionsPanel)viz).showBasic(basVizOptionsOn);
            ((OptionsPanel)viz).showAdvanced(advVizOptionsOn);

            if (showUnselectedOn) {
                cb.setEnabled(true);
                cb.setVisible(true);
                header.setVisible(true);
                viz.setVisible(true);
            } else if (cb.isSelected()) {
                cb.setVisible(true);
                cb.setEnabled(false);
                header.setVisible(true);
                viz.setVisible(true);
            } else {
                cb.setVisible(false);
                header.setVisible(false);
                viz.setVisible(false);
            }

            viz.validate();

            viz.revalidate();
            viz.repaint();
        }


		this.setMaximumSize(
					new Dimension(this.getMaximumSize().width, this.getPreferredSize().height + 5));
    }

    public List getCheckBoxes() {
        return Collections.unmodifiableList(checkBoxes);
    }

    public List getVizOptions() {
        return Collections.unmodifiableList(vizOptions);
    }

    public List getElements() {
        return Collections.unmodifiableList(elements);
    }

    public List getHeaders() {
        return Collections.unmodifiableList(headers);
    }

}
