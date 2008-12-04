package edu.mit.csail.sdg.alloy4compiler.ast;

import edu.mit.csail.sdg.alloy4.Pos;

public final class Attr {

   public enum AttrType {
      ABSTRACT("abstract"), SOME("some"), ONE("one"), LONE("lone"), EXACT("exact"),
      SUBSIG("subsig"), SUBSET("subset"),
      META("meta"), PRIVATE("private"), BUILTIN("builtin"), ENUM("enum");
      private final String label;
      private AttrType(String label) { this.label = label; }
      @Override public final String toString() { return label; }
      public final Attr make(Pos pos) { return new Attr(this, pos); }
      public final Attr makenull(Pos pos) { return pos==null ? null : new Attr(this, pos); }
   }

   public final AttrType type;

   public final Pos pos;

   public static final Attr ABSTRACT = new Attr(AttrType.ABSTRACT, null);
   public static final Attr EXACT    = new Attr(AttrType.EXACT,    null);
   public static final Attr SOME     = new Attr(AttrType.SOME,     null);
   public static final Attr ONE      = new Attr(AttrType.ONE,      null);
   public static final Attr LONE     = new Attr(AttrType.LONE,     null);
   public static final Attr SUBSIG   = new Attr(AttrType.SUBSIG,   null);
   public static final Attr SUBSET   = new Attr(AttrType.SUBSET,   null);
   public static final Attr META     = new Attr(AttrType.META,     null);
   public static final Attr PRIVATE  = new Attr(AttrType.PRIVATE,  null);
   public static final Attr BUILTIN  = new Attr(AttrType.BUILTIN,  null);
   public static final Attr ENUM     = new Attr(AttrType.ENUM,     null);

   private Attr(AttrType type, Pos pos) {
      this.type = type;
      this.pos = (pos==null ? Pos.UNKNOWN : pos);
   }

   @Override public final String toString() { return type.label; }
}
