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

package edu.mit.csail.sdg.alloy4graph;

import java.util.ArrayList;
import java.util.List;

/**
 * Mutable; this allows you to compute whether a rectangle overlaps with a set of rectangles or not.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
 */

public final class AvailableSpace {

    /** Mutable; represents a rectangle. */
    public static final class Box {
        /** (x,y) is the top-left corner; w is the width; h is the height. */
        int x, y, w, h;
    }

    /** The list of existing rectangles; we ensure every rectangle in here has width>0 and height>0. */
    private List<Box> list = new ArrayList<Box>();

    /** Construct an empty space. */
    public AvailableSpace() { }

    /** Returns true if the given rectangle does not overlap with any existing rectangle in this space. */
    public boolean ok(int x, int y, int w, int h) {
        if (w<=0 || h<=0) return true; // always okay
        for(Box box: list) {
           if ((x >= box.x && x <= box.x+box.w-1) || (x+w >= box.x+1 && x+w <= box.x+box.w))
              if ((y >= box.y && y <= box.y+box.h-1) || (y+h >= box.y+1 && y+h <= box.y+box.h)) return false;
           if ((box.x >= x && box.x <= x+w-1) || (box.x+box.w >= x+1 && box.x+box.w <= x+w))
              if ((box.y >= y && box.y <= y+h-1) || (box.y+box.h >= y+1 && box.y+box.h <= y+h)) return false;
        }
        return true;
    }

    /** Add the given rectangle to the list of rectangles in this space. */
    public void add(int x, int y, int w, int h) {
        if (w<=0 || h<=0) return; // no-op
        Box b = new Box();
        b.x=x; b.y=y; b.w=w; b.h=h;
        list.add(b);
    }

    /** Erases the list of rectangles in this space. */
    public void clear() { list.clear(); }
}
