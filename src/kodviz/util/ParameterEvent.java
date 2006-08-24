/*
 * Alloy Analyzer
 * Copyright (c) 2004 Massachusetts Institute of Technology
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

package kodviz.util;

import java.util.EventObject;

public class ParameterEvent extends EventObject {

	private static final long serialVersionUID = 1L;
	private String _oldValue;
    private String _newValue;

    public ParameterEvent(Parameter source, 
			  String oldValue,
			  String newValue) {
	super(source);
	_oldValue = oldValue;
	_newValue = newValue;
    }

    public Parameter getParameter() {
	return (Parameter)getSource();
    }

    public String getOldValue() {
	return _oldValue;
    }

    public String getNewValue() {
	return _newValue;
    }
}
