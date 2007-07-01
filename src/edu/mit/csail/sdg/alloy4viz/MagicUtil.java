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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
 */

package edu.mit.csail.sdg.alloy4viz;


public final class MagicUtil {

    /**
     * Constructor.
     */
    private MagicUtil() {}

    
    static void trimLabelBeforeLastSlash(final VizState vizState, final AlloyElement x) {
        vizState.label(x, trimBeforeLastSlash(vizState.label(x)));
    }

    static String trimBeforeLastSlash(final String label) {
        final int lastSlash = label.lastIndexOf('/');
        if (lastSlash >= 0) {
            return label.substring(lastSlash+1);
        } else {
            return label;
        }
    }

    
    /**
     * Determines whether a type is actually visible -- ie, if it has an inherited value,
     * looks up the hierarchy until that is resolved.
     * @param t
     * @return true if this type will be shown to the user, false if this type will be hidden from the user
     */
    static boolean isActuallyVisible(final VizState vizState, final AlloyType t) {
        final Boolean V = vizState.nodeVisible(t);
        if (V != null) return V.booleanValue();

        // inherited value, find out the real deal
        final AlloyModel model = vizState.getCurrentModel();
        AlloyType parent = model.getSuperType(t);
        while (parent != null) {
            final Boolean pV = vizState.nodeVisible(parent);
            if (pV != null) {
                // found a real setting
                break;
            }
            parent = model.getSuperType(parent);
        }
        if (parent == null) {
            // made it to univ without finding a real setting
            return true;
        } else {
            // found a concrete setting, use it
            return vizState.nodeVisible(parent).booleanValue();
        }
    }

    static boolean isActuallyVisible(final VizState vizState, final AlloySet s) {
        final Boolean V = vizState.nodeVisible(s);
        if (V != null) return V.booleanValue();

        return isActuallyVisible(vizState, s.getType());
    }

 }
