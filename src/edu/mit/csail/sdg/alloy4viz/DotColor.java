/*
 * Alloy Analyzer 4 -- Copyright (c) 2006-2008, Felix Chang
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

package edu.mit.csail.sdg.alloy4viz;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.swing.Icon;
import edu.mit.csail.sdg.alloy4.OurUtil;

/**
 * Immutable; this defines the set of colors that dot can produce.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
 */

public final class DotColor extends DotAttribute {

    /** The list of values that the user can select from a combobox. */
    private static final List<DotColor> values;

    public static final DotColor MAGIC = new DotColor("Magic", "magic");
    public static final DotColor WHITE = new DotColor("White", "white");
    public static final DotColor GRAY = new DotColor("Gray", "lightgray");
    public static final DotColor BLACK = new DotColor("Black", "black");
    public static final DotColor RED = new DotColor("Red", "palevioletred", "red", "salmon", "magenta");
    public static final DotColor GREEN = new DotColor("Green", "limegreen", "green2","darkolivegreen2","chartreuse2");
    public static final DotColor BLUE = new DotColor("Blue", "cornflowerblue", "blue", "cadetblue", "cyan");
    public static final DotColor YELLOW = new DotColor("Yellow", "gold", "yellow", "lightgoldenrod", "yellow");

    /** This maps each dot color name into the corresponding Java Color object. */
    private static final Map<String,Color> name2color = new HashMap<String,Color>();

    static {
        List<DotColor> list = new ArrayList<DotColor>();
        list.add(MAGIC);
        list.add(YELLOW);
        list.add(GREEN);
        list.add(BLUE);
        list.add(RED);
        list.add(GRAY);
        list.add(WHITE);
        list.add(BLACK);
        values=Collections.unmodifiableList(list);
    }

    /** Convert the Dot color name into its corresponding Java Color object. */
    public static Color name2color(String name) {
        Color ans = name2color.get(name);
        if (ans!=null) return ans;
        else if (name.equals("magic"))           ans=Color.WHITE;
        else if (name.equals("palevioletred"))   ans=new Color(222,113,148);
        else if (name.equals("red"))             ans=new Color(255,0,0);
        else if (name.equals("salmon"))          ans=new Color(255,130,115);
        else if (name.equals("magenta"))         ans=new Color(255,0,255);
        else if (name.equals("limegreen"))       ans=new Color(49,207,49);
        else if (name.equals("green2"))          ans=new Color(0,239,0);
        else if (name.equals("darkolivegreen2")) ans=new Color(189,239,107);
        else if (name.equals("chartreuse2"))     ans=new Color(115,239,0);
        else if (name.equals("gold"))            ans=new Color(255,215,0);
        else if (name.equals("yellow"))          ans=new Color(255,255,0);
        else if (name.equals("lightgoldenrod"))  ans=new Color(239,223,132);
        else if (name.equals("cornflowerblue"))  ans=new Color(99,150,239);
        else if (name.equals("blue"))            ans=new Color(0,0,255);
        else if (name.equals("cadetblue"))       ans=new Color(90,158,165);
        else if (name.equals("cyan"))            ans=new Color(0,255,255);
        else if (name.equals("lightgray"))       ans=new Color(214,214,214);
        else if (name.equals("white"))           ans=Color.WHITE;
        else ans=Color.BLACK;
        name2color.put(name,ans);
        return ans;
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
    public static List<?> values(DotColor... excludes) {
        if (excludes==null || excludes.length==0) { List<?> raw = values; return raw; }
        List<Object> ans = new ArrayList<Object>(values);
        for(int i=0; i<excludes.length; i++) ans.remove(excludes[i]);
        return ans;
    }

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
