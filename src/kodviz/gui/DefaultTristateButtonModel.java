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

import javax.swing.ButtonModel;
import javax.swing.JToggleButton;

/**
 * DefaultTristateButtonModel is a subclass of DefaultButtonModel to implement TristateButtonModel.
 *
 * original author: btsang from princeton.edu
 */
public class DefaultTristateButtonModel extends JToggleButton.ToggleButtonModel
                                        implements TristateButtonModel
{
	private static final long serialVersionUID = 1L;

	private boolean tristate;

    /**
     * Constructs a DefaultTristateButtonModel.
     */
    public DefaultTristateButtonModel()
    {
        super();

        tristate = false;
    }

    /**
     * Constructs a DefaultTristateButtonModel.
     */
    public DefaultTristateButtonModel(ButtonModel model)
    {
        super();

        stateMask = 
            (model.isArmed()    ? ARMED    : 0) |
            (model.isEnabled()  ? ENABLED  : 0) |
            (model.isPressed()  ? PRESSED  : 0) |
            (model.isRollover() ? ROLLOVER : 0) |
            (model.isSelected() ? SELECTED : 0);
        actionCommand = model.getActionCommand();
        mnemonic = model.getMnemonic();

        tristate = false;
    }

    /**
     * Returns true if the button is in tristate.  If this returns true, isSelected() should also
     * return true.
     *
     * @return True iff the button is in tristate.
     */
    public boolean isTristate()
    {
        return tristate;
    }

    /**
     * Sets whether or not the button is in tristate.  If true is passed, setSelected(true)
     * would also be called.
     *
     * @param tristate Whether or not the button should be in tristate.
     */
    public void setTristate(boolean tristate)
    {
        if (tristate == this.tristate)
            return;

        this.tristate = tristate;

        
        // changing this will change the toggle pattern from tristate to normal
        // (i.e. tristate->checked or tristate->not checked)
        if (tristate && isSelected())
        {
            super.setSelected(false);
        }
        else {
            fireStateChanged();
        }
    }
        
    /**
     * Selects or deselects the button.  This method is overridden to take the button out of
     * tristate.
     * 
     * DO NOT USE THIS METHOD AS YOU WOULD FOR A JCHECKBOX -- USE setState()
     */
    public void setSelected(boolean b)
    {
        /*if (!tristate && isSelected() == b)
            return;

        tristate = false;

        if (isSelected() != b)
        {
            super.setSelected(b);
        }
        else {
            fireStateChanged();
        }*/
        
        if (tristate) {
        	// if it is in tristate, we take it out of tristate
        	
        	tristate = false;
        	
        	if (isSelected() != b) {
        		super.setSelected(b);
        	}
        	else {
        		fireStateChanged();
        	}
        	
        }
        else {
        	// if it wasn't in tristate, we might need to turn it into
        	// tristate, depending on whether or not this was selected
        	
        	if (isSelected()==b) {
        		return;
        	}
        	
        	if (isSelected()) {
        		// was selected, simply go to unselected        	
        	    super.setSelected(b);
        	}
        	else {
        		// wasn't selected, go to tristate
        		super.setSelected(b);
        		setTristate(true);        		        		        	
        		
        	}        	        	
        }
    }
}
