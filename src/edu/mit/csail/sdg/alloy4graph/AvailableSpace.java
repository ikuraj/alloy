package edu.mit.csail.sdg.alloy4graph;

import java.util.ArrayList;
import java.util.List;

public final class AvailableSpace {

    public static final class Box {
        int x, y, w, h; // width must be at least 1; height must be at least 1
    }

    private List<Box> list = new ArrayList<Box>();

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

    public void add(int x, int y, int w, int h) {
        if (w<=0 || h<=0) return; // no-op
        Box b = new Box();
        b.x=x; b.y=y; b.w=w; b.h=h;
        list.add(b);
    }

    public void clear() { list.clear(); }
}
