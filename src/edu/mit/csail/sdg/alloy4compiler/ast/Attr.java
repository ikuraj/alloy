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

package edu.mit.csail.sdg.alloy4compiler.ast;

import edu.mit.csail.sdg.alloy4.Pos;

/** This class represents attributes that can be associated with Signatures and some other AST objects. */

public final class Attr {

   /** This represents the types of attributes. */
   public enum AttrType {

      /** ABSTRACT. If a PrimSig is abstract, it is equal to the union of its subsigs. */
      ABSTRACT("abstract"),

      /** SOME. If a Sig is some, it has at least one atom. */
      SOME("some"),

      /** ONE. If a Sig is one, it has exactly one atom. */
      ONE("one"),

      /** LONE. If a Sig is lone, it has at most one atom. */
      LONE("lone"),

      /** EXACT. If a SubsetSig is exact, it is equal to the union of its parents. */
      EXACT("exact"),

      /** SUBSIG. Every PrimSig (including the builtin sigs) has the SUBSIG attribute set, and the SUBSET attribute unset. */
      SUBSIG("subsig"),

      /** SUBSET. Every SubsetSig has the SUBSET attribute set, and the SUBSIG attribute unset. */
      SUBSET("subset"),

      /** META. If a Sig has the META attribute, it means it is a META atom corresponding to some real signature or field. */
      META("meta"),

      /** PRIVATE. If a Sig has the PRIVATE attribute, it means its label is private within the same module. */
      PRIVATE("private"),

      /** BUILTIN. Every builtin Sig has the BUILTIN attribute, and every non-builtin Sig does not. */
      BUILTIN("builtin"),

      /** ENUM. If a PrimSig has the ENUM attribute, it is toplevel and abstract and has only singleton children. */
      ENUM("enum");

      /** The label for this attribute type. */
      private final String label;

      /** Constructor for this attribute type. */
      private AttrType(String label) { this.label = label; }

      /** {@inheritDoc} */
      @Override public final String toString() { return label; }

      /** Construct an attribute of this type with this position; if pos==null, it is treated as Pos.UNKNOWN. */
      public final Attr make(Pos pos) { return new Attr(this, pos); }

      /** Construct an attribute of this type with this position; if pos==null, this method returns null. */
      public final Attr makenull(Pos pos) { return pos==null ? null : new Attr(this, pos); }
   }

   /** The type of this attribute. */
   public final AttrType type;

   /** The position associated with this attribute. */
   public final Pos pos;

   /** ABSTRACT. If a PrimSig is abstract, it is equal to the union of its subsigs. */
   public static final Attr ABSTRACT = new Attr(AttrType.ABSTRACT, null);

   /** SOME. If a Sig is some, it has at least one atom. */
   public static final Attr SOME = new Attr(AttrType.SOME,     null);

   /** ONE. If a Sig is one, it has exactly one atom. */
   public static final Attr ONE = new Attr(AttrType.ONE,      null);

   /** LONE. If a Sig is lone, it has at most one atom. */
   public static final Attr LONE = new Attr(AttrType.LONE,     null);

   /** EXACT. If a SubsetSig is exact, it is equal to the union of its parents. */
   public static final Attr EXACT = new Attr(AttrType.EXACT,    null);

   /** SUBSIG. Every PrimSig (including the builtin sigs) has the SUBSIG attribute set, and the SUBSET attribute unset. */
   public static final Attr SUBSIG = new Attr(AttrType.SUBSIG,   null);

   /** SUBSET. Every SubsetSig has the SUBSET attribute set, and the SUBSIG attribute unset. */
   public static final Attr SUBSET = new Attr(AttrType.SUBSET,   null);

   /** META. If a Sig has the META attribute, it means it is a META atom corresponding to some real signature or field. */
   public static final Attr META = new Attr(AttrType.META,     null);

   /** PRIVATE. If a Sig has the PRIVATE attribute, it means its label is private within the same module. */
   public static final Attr PRIVATE = new Attr(AttrType.PRIVATE,  null);

   /** BUILTIN. Every builtin Sig has the BUILTIN attribute, and every non-builtin Sig does not. */
   public static final Attr BUILTIN = new Attr(AttrType.BUILTIN,  null);

   /** ENUM. If a PrimSig has the ENUM attribute, it is toplevel and abstract and has only singleton children. */
   public static final Attr ENUM = new Attr(AttrType.ENUM,     null);

   /** Construct an attribute of the given type with the given position; if pos==null, it is treated as Pos.UNKNOWN. */
   private Attr(AttrType type, Pos pos) {
      this.type = type;
      this.pos = (pos==null ? Pos.UNKNOWN : pos);
   }

   /** {@inheritDoc} */
   @Override public final String toString() { return type.label; }
}
