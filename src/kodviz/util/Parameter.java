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

package kodviz.util;

import java.awt.Font;
import java.awt.FontFormatException;
import java.util.Iterator;

/** SAT solver parameter -- not to be confused with {@link alloy.inter.Parameter}! */
public class Parameter {
    public final static int BOOLEAN = 0;

    public final static int INT = 1;

    public final static int ENUM = 2;

    public final static int DOUBLE = 3;

    public final static int STRING = 4;

    public final static int PATH = 5;

    public final static int FONT = 6;

    private EnumParameter enumx = null;

    private String name;

    private int type;

    private String defval;

    private String message;

    private WeakSet listenerWeakSet;

    public Parameter(String sname, String stype, String sdefault,
            String smessage) {
        name = sname;
        if (stype.equals("double"))
            type = DOUBLE;
        else if (stype.equals("bool"))
            type = BOOLEAN;
        else if (stype.equals("enum"))
            type = ENUM;
        else if (stype.equals("int"))
            type = INT;
        else if (stype.equals("string"))
            type = STRING;
        else if (stype.equals("path"))
            type = PATH;
        else if (stype.equals("font"))
            type = FONT;
        else
            Dbg.fail("Bad param type: " + stype);

        defval = sdefault;
        message = smessage;
    }

    public String getName() {
        return name;
    }

    public int getType() {
        return type;
    }

    public String getValue() {
        return defval;
    }

    public String getMessage() {
        return message;
    }

    public EnumParameter getEnum() {
        return enumx;
    }

    public void setValue(String newval) {
        String oldval = defval;
        defval = newval;
        fireValueChanged(oldval, newval);
    }

    public String toString() {
        String desc = "{" + name + "," + type + "," + defval + "," + message;

        if (type == ENUM)
            desc += " /" + enumx + "/";

        return desc + "}";
    }

    public String typeString() {
        switch (type) {
        case ENUM:
            return "enum";
        case DOUBLE:
            return "double";
        case INT:
            return "int";
        case BOOLEAN:
            return "bool";
        case STRING:
            return "string";
        case PATH:
            return "path";
        case FONT:
            return "font";
        default:
            return null;
        }
    }

    public void setEnum(EnumParameter e) {
        enumx = e;
    }

    // A parameter of type FONT is represented as a String in the form
    // "<Face>,<Size>" where <Face> is the name of the font face
    // and <Size> is an integer-valued font size

    public static Font stringToFont(String s) throws FontFormatException {
        if (s.equals("default"))
            return defaultFont();

        int commaIndex = s.indexOf(',');
        try {
            if (commaIndex > -1) {
                return new Font(s.substring(0, commaIndex), Font.PLAIN, Integer
                        .parseInt(s.substring(commaIndex + 1)));
            } else {
                throw new FontFormatException("Comma missing");
            }
        } catch (NumberFormatException e) {
            throw new FontFormatException("Invalid font size");
        }
    }

    private static Font defaultFont;

    public static Font defaultFont() {
        if (defaultFont == null)
            defaultFont = new Font(javax.swing.UIManager.getFont(
                    "TextField.font").getPSName(), Font.PLAIN,
                    kodviz.gui.AlloySwingUtilities.onMac() ? 11 : 12);
        return defaultFont;
    }

    public static String fontToString(Font f) {
        return f.getName() + "," + f.getSize();
    }

    public void addParameterListener(ParameterListener l) {
        if (listenerWeakSet == null) {
            listenerWeakSet = new WeakSet();
        }
        listenerWeakSet.add(l);
    }

    public void removeParameterListener(ParameterListener l) {
        if (listenerWeakSet != null) {
            listenerWeakSet.remove(l);
        }
    }

    private void fireValueChanged(String oldValue, String newValue) {
        if (listenerWeakSet != null) {
            ParameterEvent event = new ParameterEvent(this, oldValue, newValue);
            for (Iterator iter = listenerWeakSet.iterator(); iter.hasNext(); ) {
                ((ParameterListener) iter.next()).valueChanged(event);
            }
        }
    }
}

