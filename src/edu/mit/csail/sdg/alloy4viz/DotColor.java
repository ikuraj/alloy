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

package edu.mit.csail.sdg.alloy4viz;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import javax.swing.Icon;
import edu.mit.csail.sdg.alloy4.OurUtil;

/**
 * Immutable; this defines the set of colors that dot can produce.
 *
 * <p><b>Thread Safety:</b>  Safe (since everything is constructed statically)
 */

public final class DotColor extends DotAttribute {

    /** The list of values that the user can select from a combobox. */
    static final List<DotColor> values;

    public static final DotColor WHITE = new DotColor("White", "white");
    public static final DotColor GRAY = new DotColor("Gray", "lightgray");
    public static final DotColor BLACK = new DotColor("Black", "black");
    public static final DotColor RED = new DotColor("Red", "palevioletred", "red", "salmon", "magenta");
    public static final DotColor GREEN = new DotColor("Green", "limegreen", "green2","darkolivegreen2","chartreuse2");
    public static final DotColor BLUE = new DotColor("Blue", "cornflowerblue", "blue", "cadetblue", "cyan");
    public static final DotColor YELLOW = new DotColor("Yellow", "gold", "yellow", "lightgoldenrod", "yellow");

    static {
        List<DotColor> list = new ArrayList<DotColor>();
        list.add(YELLOW);
        list.add(GREEN);
        list.add(BLUE);
        list.add(RED);
        list.add(GRAY);
        list.add(WHITE);
        list.add(BLACK);
        values=Collections.unmodifiableList(list);
    }

    /**
     * The list of colors to use, corresponding to the current palette;
     * if there are more palette choices than colors.size(), then the extra palettes would all use the first color.
     */
    private final List<String> colors = new ArrayList<String>();

    /**
     * The list of icons to show, corresponding to the current palette;
     * if there are more palette choices than icons.size(), then the extra palettes would all use the first icon.
     */
    private final List<Icon> icons = new ArrayList<Icon>();

    /**
     * Construct a new DotColor.
     * @param displayedText - the text to show when the user is selecting a color from a combobox
     * @param colors - the list of actual colors to show corresponding to each possible palette
     * (if this list is shorter than the number of palettes, then the extra palettes would all use
     * the first color in this list)
     */
    private DotColor(String displayedText, String... colors) {
        super(displayedText, colors[0], null);
        for(int i=0; i<colors.length; i++) {
            this.colors.add(colors[i]);
            this.icons.add(OurUtil.loadIcon("icons/ColorIcons/"+colors[i]+".gif"));
        }
    }

    /** Returns the list of values that the user is allowed to select from. */
    @SuppressWarnings("unchecked")
    public static List<Object> values() { List raw = values; return raw; }

    /** Returns the icon to use, based on the given palette. */
    @Override public Icon getIcon(DotPalette pal) {
        int i=0;
        for(Object choice: DotPalette.values()) {
            if (i>=icons.size()) break;
            if (pal==choice) return icons.get(i);
            i++;
        }
        return icons.get(0);
    }

    /** Returns the color to use for the nodes and edges, based on the given palette. */
    @Override public String getDotText(DotPalette pal) {
        int i=0;
        for(Object choice: DotPalette.values()) {
            if (i>=colors.size()) break;
            if (pal==choice) return colors.get(i);
            i++;
        }
        return colors.get(0);
    }

    /** Returns the color to use for the text label, based on the given palette. */
    public String getLabelColorText(DotPalette pal) {
        return (this==BLACK || (pal==DotPalette.STANDARD && (this==RED || this==BLUE))) ? "white" : "black";
    }

    /** This method is used in parsing the XML value into a valid DotShape; returns null if there is no match. */
    public static DotColor valueOf(String x) {
        if (x!=null) for(Object d:values) if (d.toString().equals(x)) return (DotColor)d;
        return null;
    }

    /** This value is used in writing XML. */
    @Override public String toString() { return getDisplayedText(); }
}
