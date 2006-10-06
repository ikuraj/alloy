package edu.mit.csail.sdg.alloy4.node;

import edu.mit.csail.sdg.alloy4.helper.Pos;

public final class Field {
    public final Pos pos;
    public final String name;
    public final String fullname;
    public Type halftype=null;
    public Type fulltype=null;
    public Field(Pos pos, String name, String fullname) {
        this.pos=pos;
        this.name=name;
        this.fullname=fullname;
    }
}
