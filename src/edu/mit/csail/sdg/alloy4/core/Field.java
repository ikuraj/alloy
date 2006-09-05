package edu.mit.csail.sdg.alloy4.core;

import edu.mit.csail.sdg.alloy4.util.Pos;

public final class Field {
    public final String name;
    public final Pos pos;
    public final String fullname;
    public Type halftype;
    public Type fulltype;
    private final ParaSig parent;
    public ParaSig parent() { return parent; }
    public Field(ParaSig parent, Pos pos, String pathsig, String name, Type fulltype, Type halftype) {
        this.parent=parent;
        this.pos=pos;
        this.name=name;
        this.fullname=pathsig+"."+name;
        this.fulltype=fulltype;
        this.halftype=halftype;
    }
}
