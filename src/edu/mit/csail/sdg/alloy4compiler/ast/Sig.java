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

import java.util.Collection;
import java.util.List;
import edu.mit.csail.sdg.alloy4.ErrorAPI;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorFatal;
import edu.mit.csail.sdg.alloy4.ErrorType;
import edu.mit.csail.sdg.alloy4.ErrorWarning;
import edu.mit.csail.sdg.alloy4.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.Version;
import edu.mit.csail.sdg.alloy4.ConstList.TempList;
import edu.mit.csail.sdg.alloy4.SafeList;
import edu.mit.csail.sdg.alloy4.Util;

/** Mutable; represents a signature. */

public abstract class Sig extends Expr {

    /** The built-in "univ" signature. */
    public static final PrimSig UNIV = new PrimSig("univ", null, false);

    /** The built-in "Int" signature. */
    public static final PrimSig SIGINT = new PrimSig("Int", UNIV, false);

    /** The built-in "seq/Int" signature. */
    public static final PrimSig SEQIDX = new PrimSig("seq/Int", SIGINT, true);

    /** The built-in "String" signature. */
    public static final PrimSig STRING = new PrimSig("fun/String", UNIV, true);

    /** The built-in "none" signature. */
    public static final PrimSig NONE = new PrimSig("none", null, false);

    /** Returns the name for this sig; this name need not be unique. */
    @Override public final String toString() { return label; }

    /** {@inheritDoc} */
    @Override public final void toString(StringBuilder out, int indent) {
        if (indent<0) {
            out.append(label);
        } else {
            for(int i=0; i<indent; i++) { out.append(' '); }
            out.append("sig ").append(label).append(" with type=").append(type).append('\n');
        }
    }

    /** {@inheritDoc} */
    @Override public final Pos span() { return pos; }

    /** {@inheritDoc} */
    @Override public Expr resolve(Type t, Collection<ErrorWarning> warns) { return this; }

    /** {@inheritDoc} */
    @Override final<T> T accept(VisitReturn<T> visitor) throws Err { return visitor.visit(this); }

    /**
     * True if this sig is one of the built-in sig.
     * <p> Note: if builtin==true, then we ensure it is not abstract
     */
    public final boolean builtin;

    /**
     * Nonnull if this sig is abstract.
     * <p> Note: if a sig is abstract, then it cannot and will not be a subset sig.
     */
    public final Pos isAbstract;

    /**
     * Nonnull if this sig is a PrimSig but not a builtin sig and its parent is not UNIV
     */
    public final Pos isSubsig;

    /**
     * Nonnull if this sig is a SubsetSig.
     * <p> Note: if a sig is a subset sig, then it cannot and will not be abstract.
     */
    public final Pos isSubset;

    /**
     * Nonnull if this sig's multiplicity is declared to be lone.
     * <p> Note: at most one of "lone", "one", "some" can be nonnull for each sig.
     */
    public final Pos isLone;

    /**
     * Nonnull if this sig's multiplicity is declared to be one.
     * <p> Note: at most one of "lone", "one", "some" can be nonnull for each sig.
     */
    public final Pos isOne;

    /**
     * Nonnull if this sig's multiplicity is declared to be some.
     * <p> Note: at most one of "lone", "one", "some" can be nonnull for each sig.
     */
    public final Pos isSome;

    /**
     * Nonnull if the user wanted this sig to be private.
     * <p> Note: this value is always null for builtin sigs.
     */
    public final Pos isPrivate;

    /**
     * Nonnull if this sig is a meta sig.
     * <p> Note: this value is always null for builtin sigs.
     */
    public final Pos isMeta;

    /** The label for this sig; this name does not need to be unique. */
    public final String label;

    /** Returns true if this sig is a toplevel sig (meaning: it is UNIV, or it is a non-subset sig with parent==UNIV) */
    public final boolean isTopLevel() {
        return (this!=NONE) && (this instanceof PrimSig) && (this==UNIV || ((PrimSig)this).parent==UNIV);
    }

    /** Returns true if this sig is an enumeration of some parent sig. */
    public final boolean isEnum() {
        if (!(this instanceof PrimSig)) return false;
        PrimSig x = (PrimSig)this;
        if (x.parent!=null) for(x=x.parent; x!=null; x=x.parent) if (x.hint_isLeaf) return true;
        return false;
    }

    /** Returns true if this sig is a leaf sig (and thus all subsigs will be enumerations of this sig) */
    public final boolean isLeaf() {
        if (!(this instanceof PrimSig)) return false;
        PrimSig x = (PrimSig)this;
        if (x.parent!=null) for(x=x.parent; x!=null; x=x.parent) if (x.hint_isLeaf) return false;
        return ((PrimSig)this).hint_isLeaf;
    }

    /** Constructs a new builtin PrimSig. */
    private Sig(String label) {
        super(Pos.UNKNOWN, null);
        this.builtin = true;
        this.isAbstract = null;
        this.isLone = null;
        this.isOne = null;
        this.isSome = null;
        this.label = label;
        this.isSubset = null;
        this.isSubsig = null;
        this.isPrivate = null;
        this.isMeta = null;
    }

    /** Constructs a new PrimSig or SubsetSig. */
    private Sig(Pos pos, Type type, String label, Pos abs, Pos lone, Pos one, Pos some, Pos subsig, Pos subset, Pos isPrivate, Pos isMeta) throws Err {
        super(pos, type);
        if (lone!=null && one!=null)  throw new ErrorSyntax(lone.merge(one),  "You cannot delcare a sig to be both lone and one.");
        if (lone!=null && some!=null) throw new ErrorSyntax(lone.merge(some), "You cannot delcare a sig to be both lone and some.");
        if (one!=null  && some!=null) throw new ErrorSyntax(one.merge(some),  "You cannot delcare a sig to be both one and some.");
        this.builtin = false;
        this.isPrivate = isPrivate;
        this.isMeta = isMeta;
        this.isAbstract = abs;
        this.isLone = lone;
        this.isOne = one;
        this.isSome = some;
        this.label = label;
        this.isSubset = subset;
        this.isSubsig = subsig;
    }

    /** Returns true if we can determine the two expressions are equivalent; may sometimes return false. */
    @Override public boolean isSame(Expr obj) {
        Sig me = this;
        while(obj instanceof ExprUnary && ((ExprUnary)obj).op==ExprUnary.Op.NOOP) obj=((ExprUnary)obj).sub;
        while(obj instanceof SubsetSig && ((SubsetSig)obj).exact && ((SubsetSig)obj).parents.size()==1) obj = ((SubsetSig)obj).parents.get(0);
        while(me instanceof SubsetSig && ((SubsetSig)me).exact && ((SubsetSig)me).parents.size()==1) me = ((SubsetSig)me).parents.get(0);
        return (me == obj);
    }

    /** Returns true iff "this is equal or subtype of that" */
    public abstract boolean isSameOrDescendentOf(Sig that);

    /** {@inheritDoc} */
    public int getDepth() { return 1; }

    //==============================================================================================================//

    /**
     * Mutable; reresents a non-subset signature.
     *
     * <p> Note: except for "children()", the return value of every method is always valid for all time;
     * for example, given sigs A and B, and you call C=A.intersect(B), then the result C will always be
     * the intersection of A and B even if the caller later constructs more sigs or subsigs or subsetsigs...
     */

    public static final class PrimSig extends Sig {

        /**
         * Stores its immediate children sigs (not including NONE)
         * <p> Note: if this==UNIV, then this list will always be empty, since we don't keep track of UNIV's children
         */
        private final SafeList<PrimSig> children = new SafeList<PrimSig>();

        /**
         * Returns its immediate children sigs (not including NONE)
         * <p> Note: if this==UNIV, then this method will throw an exception, since we don't keep track of UNIV's children
         */
        public SafeList<PrimSig> children() throws Err {
            if (this==UNIV) throw new ErrorFatal("Internal error (cannot enumerate the subsigs of UNIV)");
            return children.dup();
        }

        /**
         * Returns its subsigs and their subsigs and their subsigs, etc.
         * <p> Note: if this==UNIV, then this method will throw an exception, since we don't keep track of UNIV's children
         */
        public Iterable<PrimSig> descendents() throws Err {
            if (this==UNIV) throw new ErrorFatal("Internal error (cannot enumerate the subsigs of UNIV)");
            Iterable<PrimSig> answer = children.dup();
            for(PrimSig x:children) answer = Util.fastJoin(answer, x.descendents());
            return answer;
        }

        /** True if all subsigs will get this sig's type. */
        final boolean hint_isLeaf;

        /** If this is UNIV or NONE, then this field is null, else this field is the parent sig. */
        public final PrimSig parent;

        /** Constructs a builtin PrimSig. */
        private PrimSig(String label, PrimSig parent, boolean add) {
            super(label);
            this.hint_isLeaf=false;
            this.parent=parent;
            if (add) this.parent.children.add(this);
        }

        /**
         * Constructs a non-builtin sig.
         *
         * @param pos - the position in the original file where this sig was defined (can be null if unknown)
         * @param parent - the parent (must not be null, and must not be NONE)
         * @param label - the name of this sig (it does not need to be unique)
         * @param isAbstract - nonnull iff this sig should be abstract
         * @param lone - nonnull iff this sig has the "lone" multiplicity
         * @param one - nonnull iff this sig has the "one" multiplicity
         * @param some - nonnull iff this sig has the "some" multiplicity
         * @param isLeaf - true if all its future subsigs shall have the same "type" as this sig
         *
         * @throws ErrorSyntax if the signature has two or more multiplicities
         * @throws ErrorType if you attempt to extend the builtin sigs NONE, SIGINT, SEQIDX, or STRING
         */
        public PrimSig
        (Pos pos, PrimSig parent, String label, Pos isAbstract, Pos lone, Pos one, Pos some, Pos subsig, Pos isPrivate, Pos isMeta, boolean isLeaf)
        throws Err {
            super(pos,
                (parent!=null && parent.hint_isLeaf) ? parent.type : null,
                label, isAbstract, lone, one, some,
                (parent!=null && parent!=UNIV) ? Pos.UNKNOWN.merge(subsig) : null,
                null, isPrivate, isMeta);
            if (parent==SIGINT) throw new ErrorSyntax(pos, "sig "+label+" cannot extend the builtin \"Int\" signature");
            if (parent==SEQIDX) throw new ErrorSyntax(pos, "sig "+label+" cannot extend the builtin \"seq/Int\" signature");
            if (parent==STRING) throw new ErrorSyntax(pos, "sig "+label+" cannot extend the builtin \"fun/String\" signature");
            if (parent==NONE)   throw new ErrorSyntax(pos, "sig "+label+" cannot extend the builtin \"none\" signature");
            if (parent==null) parent=UNIV; else if (parent!=UNIV) parent.children.add(this);
            this.parent = parent;
            this.hint_isLeaf = isLeaf || (parent.hint_isLeaf);
        }

        /**
         * Constructs a non-builtin sig.
         *
         * @param pos - the position in the original file where this sig was defined (can be null if unknown)
         * @param parent - the parent (must not be null, and must not be NONE)
         * @param label - the name of this sig (it does not need to be unique)
         * @param isAbstract - true iff this sig should be abstract
         * @param lone - true iff this sig has the "lone" multiplicity
         * @param one - true iff this sig has the "one" multiplicity
         * @param some - true iff this sig has the "some" multiplicity
         * @param isLeaf - true if all its future subsigs shall have the same "type" as this sig
         *
         * @throws ErrorSyntax if the signature has two or more multiplicities
         * @throws ErrorType if you attempt to extend the builtin sigs NONE, SIGINT, SEQIDX, or STRING
         */
        public PrimSig(Pos pos, PrimSig parent, String label, boolean isAbstract, boolean lone, boolean one, boolean some,
        boolean isLeaf) throws Err {
            this(pos, parent, label, isAbstract?Pos.UNKNOWN:null,
               lone?Pos.UNKNOWN:null, one?Pos.UNKNOWN:null, some?Pos.UNKNOWN:null, null, null, null, isLeaf);
        }

        /**
         * Constructs a non-builtin sig.
         * @param pos - the position in the original file where this sig was defined (can be null if unknown)
         * @param label - the name of this sig (it does not need to be unique)
         */
        public PrimSig(Pos pos, String label) throws Err { this(pos, null, label, null,null,null,null,null,null,null, false); }

        /** {@inheritDoc} */
        @Override public boolean isSameOrDescendentOf(Sig that) {
            if (this==NONE || this==that || that==UNIV) return true;
            if (this==UNIV || that==NONE) return false;
            for(PrimSig me=this; me!=null; me=me.parent) if (me==that) return true;
            return false;
        }

        /** Returns the intersection between this and that (and returns "none" if they do not intersect). */
        public PrimSig intersect(PrimSig that) {
            if (this.isSameOrDescendentOf(that)) return this;
            if (that.isSameOrDescendentOf(this)) return that;
            return NONE;
        }

        /** Returns true iff the intersection between this and that is not "none". */
        public boolean intersects(PrimSig that) {
            if (this.isSameOrDescendentOf(that)) return this!=NONE;
            if (that.isSameOrDescendentOf(this)) return that!=NONE;
            return false;
        }

        /**
         * Returns the most-specific-sig that contains this and that.
         * In particular, if this extends that, then return that.
         */
        public PrimSig leastParent(PrimSig that) {
            if (isSameOrDescendentOf(that)) return that;
            PrimSig me=this;
            while(true) {
                if (that.isSameOrDescendentOf(me)) return me;
                me=me.parent;
                if (me==null) return UNIV;
            }
        }

        /** {@inheritDoc} */
        @Override public String getDescription() { return "<b>sig</b> " + label; }

        /** {@inheritDoc} */
        @Override public List<? extends Browsable> getSubnodes() {
            TempList<Browsable> ans = new TempList<Browsable>();
            if (parent!=null && !parent.builtin) ans.add(make(parent.pos, parent.span(), "<b>extends sig</b> " + parent.label, parent.getSubnodes()));
            for(Field f: super.fields) ans.add(f);
            return ans.makeConst();
        }
    }

    //==============================================================================================================//

    /** Mutable; reresents a subset signature. */

    public static final class SubsetSig extends Sig {

        /** The list of Sig that it is a subset of; this list is never empty. */
        public final ConstList<Sig> parents;

        /** If true, then this sig is EXACTLY equal to the union of its parents. */
        public final boolean exact;

        /** Computes the type for this sig. */
        private static Type getType(Pos pos, String label, Iterable<Sig> parents) throws Err {
            Type ans=null;
            if (parents!=null) for(Sig parent: parents) {
                if (!Version.experimental) {
                  if (parent==SIGINT) throw new ErrorSyntax(pos, "sig "+label+" cannot be a subset of the builtin \"Int\" signature");
                  if (parent==SEQIDX) throw new ErrorSyntax(pos, "sig "+label+" cannot be a subset of the builtin \"seq/Int\" signature");
                  if (parent==STRING) throw new ErrorSyntax(pos, "sig "+label+" cannot be a subset of the builtin \"fun/String\" signature");
                }
                if (parent==UNIV) return UNIV.type;
                if (ans==null) ans=parent.type; else ans=ans.unionWithCommonArity(parent.type);
            }
            return (ans!=null) ? ans : (UNIV.type);
        }

        /**
         * Constructs a subset sig.
         *
         * @param pos - the position in the original file where this sig was defined (can be null if unknown)
         * @param parents - the list of parents (if this list is null or empty, we assume the caller means UNIV)
         * @param label - the name of this sig (it does not need to be unique)
         * @param subsetPosition - if nonnull, it denotes the location of the "subset" token in the input file
         * @param lone - nonnull iff this sig has the "lone" multiplicity
         * @param one - nonnull iff this sig has the "one" multiplicity
         * @param some - nonnull iff this sig has the "some" multiplicity
         *
         * @throws ErrorSyntax if the signature has two or more multiplicities
         * @throws ErrorType if parents only contains NONE
         */
        public SubsetSig(Pos pos, Collection<Sig> parents, String label, Pos subsetPosition, Pos lone, Pos one, Pos some, Pos isPrivate, Pos isMeta) throws Err {
            super(pos, getType(pos,label,parents), label, null, lone, one, some, null, Pos.UNKNOWN.merge(subsetPosition), isPrivate, isMeta);
            this.exact = false;
            TempList<Sig> temp = new TempList<Sig>(parents==null ? 1 : parents.size());
            if (parents==null || parents.size()==0) {
               temp.add(UNIV);
            } else {
               for(Sig parent:parents) {
                 if (parent==Sig.UNIV) {temp.clear(); temp.add(UNIV); break;}
                 else if (parent!=Sig.NONE && !temp.contains(parent)) temp.add(parent);
               }
            }
            if (temp.size()==0) throw new ErrorType(pos, "Sig "+label+" must have at least one non-empty parent.");
            this.parents = temp.makeConst();
        }

        /**
         * Constructs a subset sig.
         *
         * @param pos - the position in the original file where this sig was defined (can be null if unknown)
         * @param parents - the list of parents (if this list is null or empty, we assume the caller means UNIV)
         * @param label - the name of this sig (it does not need to be unique)
         * @param exact - if true, it denotes this subset is exact
         * @param subsetPosition - if nonnull, it denotes the location of the "subset" token in the input file
         * @param lone - nonnull iff this sig has the "lone" multiplicity
         * @param one - nonnull iff this sig has the "one" multiplicity
         * @param some - nonnull iff this sig has the "some" multiplicity
         *
         * @throws ErrorSyntax if the signature has two or more multiplicities
         * @throws ErrorType if parents only contains NONE
         */
        public SubsetSig(Pos pos, Collection<Sig> parents, String label, boolean exact, Pos subsetPosition, Pos lone, Pos one, Pos some, Pos isPrivate, Pos isMeta) throws Err {
            super(pos, getType(pos,label,parents), label, null, lone, one, some, null, Pos.UNKNOWN.merge(subsetPosition), isPrivate, isMeta);
            this.exact = exact;
            TempList<Sig> temp = new TempList<Sig>(parents==null ? 1 : parents.size());
            if (parents==null || parents.size()==0) {
               temp.add(UNIV);
            } else {
               for(Sig parent:parents) {
                 if (parent==Sig.UNIV) {temp.clear(); temp.add(UNIV); break;}
                 else if (parent!=Sig.NONE && !temp.contains(parent)) temp.add(parent);
               }
            }
            if (temp.size()==0) throw new ErrorType(pos, "Sig "+label+" must have at least one non-empty parent.");
            this.parents = temp.makeConst();
        }

        /** {@inheritDoc} */
        @Override public boolean isSameOrDescendentOf(Sig that) {
            if (that==UNIV || that==this) return true;
            if (that==NONE) return false;
            for(Sig p:parents) if (p.isSameOrDescendentOf(that)) return true;
            return false;
        }

        /** {@inheritDoc} */
        @Override public String getDescription() { return "<b>sig</b> " + label; }

        /** {@inheritDoc} */
        @Override public List<? extends Browsable> getSubnodes() {
            TempList<Browsable> ans = new TempList<Browsable>();
            for(Sig p: parents) ans.add(make(p.pos, p.span(), "<b>in sig</b> " + p.label, p.getSubnodes()));
            for(Field f: super.fields) ans.add(f);
            return ans.makeConst();
        }
    }

    //==============================================================================================================//

    /** Mutable; represents a field. */

    public static final class Field extends Expr {

        /** The sig that this field belongs to; never null. */
        public final Sig sig;

        /** Nonnull if the user wanted this field to be private. */
        public final Pos isPrivate;

        /** Nonnull if this field is a meta field. */
        public final Pos isMeta;

        /** The label for this field; this name does not need to be unique. */
        public final String label;

        /** The bounding formula (null if this.definition!=null); if nonnull it is always of the form "all x: one ThisSig | x.ThisField in y" */
        public final Expr boundingFormula;

        /** The definition expression (null if this.boundFormula!=null). */
        public final Expr definition;

        /** Constructs a new Field object. */
        private Field(Pos pos, Pos isPrivate, Pos isMeta, Sig sig, String label, Expr definition) throws Err {
            super(pos, null, false, definition.type, 0, 0, definition.errors);
            if (sig.builtin) throw new ErrorSyntax(pos, "Builtin sig \""+sig+"\" cannot have fields.");
            if (definition.mult!=0 || definition.type.arity()<=1 || definition.ambiguous ||
                (definition.type.hasTuple() && !definition.type.firstColumnOverlaps(sig.type))) {
                throw new ErrorAPI(pos, "This field's definition must be a binary or higher arity expression that intersects this sig.");
            }
            this.isPrivate = (isPrivate!=null ? isPrivate : sig.isPrivate);
            this.isMeta = (isMeta!=null ? isMeta : sig.isMeta);
            this.sig = sig;
            this.label = label;
            this.boundingFormula = null;
            this.definition = definition;
        }

        /** Constructs a new Field object. */
        private Field(Pos pos, Pos isPrivate, Pos isMeta, Sig sig, String label, ExprVar var, Expr bound) throws Err {
            super(pos, null, false, sig.type.product(bound.type), 0, 0, bound.errors);
            if (sig.builtin) throw new ErrorSyntax(pos, "Builtin sig \""+sig+"\" cannot have fields.");
            this.isPrivate = (isPrivate!=null ? isPrivate : sig.isPrivate);
            this.isMeta = (isMeta!=null ? isMeta : sig.isMeta);
            this.sig = sig;
            this.label = label;
            // If the field declaration is unary, and does not have any multiplicity symbol, we assume it's "one of"
            if (bound.mult==0 && bound.type.arity()==1) bound=ExprUnary.Op.ONEOF.make(null, bound);
            if (var==null) var = ExprVar.make(null, "this", sig.oneOf());
            boundingFormula = ExprQuant.Op.ALL.make(pos, null, Util.asList(var), var.join(this).in(bound));
            if (!boundingFormula.errors.isEmpty())
                throw boundingFormula.errors.pick();
            if (boundingFormula.hasCall())
                throw new ErrorSyntax(pos, "Field \""+label+"\" declaration cannot contain a function or predicate call.");
            if (bound.type.arity()>0 && bound.type.hasNoTuple()) throw new ErrorType(pos, "Cannot bind field "+label+" to the empty set or empty relation.");
            this.definition = null;
        }

        /** Returns true if we can determine the two expressions are equivalent; may sometimes return false. */
        @Override public boolean isSame(Expr obj) {
            while(obj instanceof ExprUnary && ((ExprUnary)obj).op==ExprUnary.Op.NOOP) obj=((ExprUnary)obj).sub;
            return (obj==this);
        }

        /** Returns a human-readable description of this field's name. */
        @Override public String toString() {
            if (sig.label.length()==0) return label; else return "field ("+sig+" <: "+label+")";
        }

        /** {@inheritDoc} */
        @Override public void toString(StringBuilder out, int indent) {
            if (indent<0) {
                out.append("(").append(sig.label).append(" <: ").append(label).append(")");
            } else {
                for(int i=0; i<indent; i++) { out.append(' '); }
                out.append("field ").append(sig.label).append(" <: ").append(label).append(" with type=").append(type).append('\n');
            }
        }

        /** {@inheritDoc} */
        @Override public Pos span() { return pos; }

        /** {@inheritDoc} */
        @Override public Expr resolve(Type t, Collection<ErrorWarning> warns) { return this; }

        /** {@inheritDoc} */
        @Override final<T> T accept(VisitReturn<T> visitor) throws Err { return visitor.visit(this); }

        /** {@inheritDoc} */
        public int getDepth() { return 1; }

        /** {@inheritDoc} */
        @Override public String getDescription() { return "<b>field</b> " + label; }

        /** {@inheritDoc} */
        @Override public List<? extends Browsable> getSubnodes() {
            Browsable s = make(sig.pos, sig.span(), "<b>from sig</b> "+sig.label, sig.getSubnodes());
            Browsable b;
            if (boundingFormula!=null) {
                b = ((ExprBinary)(((ExprQuant)boundingFormula).sub)).right;
                b = make(b.pos(), b.span(), "<b>bound</b>", b);
            } else {
                b = definition;
                b = make(b.pos(), b.span(), "<b>definition</b>", b);
            }
            return Util.asList(s, b);
        }
    }

    //==============================================================================================================//

    /** The list of fields. */
    private final SafeList<Field> fields = new SafeList<Field>();

    /** Returns the list of fields (as an unmodifiable list). */
    public final SafeList<Field> getFields() { return fields.dup(); }

    /**
     * Add then return a new field, where "all x: ThisSig | x.F in bound"
     * <p> Note: the bound must be fully-typechecked and have exactly 0 free variable, or have "x" as its sole free variable.
     *
     * @param pos - the position in the original file where this field was defined (can be null if unknown)
     * @param isPrivate - if nonnull, that means the user intended this field to be "private"
     * @param label - the name of this field (it does not need to be unique)
     * @param x - a quantified variable "x: one ThisSig"
     * @param bound - the new field will be bound by "all x: one ThisSig | x.ThisField in bound"
     *
     * @throws ErrorSyntax  if the sig is one of the builtin sig
     * @throws ErrorSyntax  if the bound contains a predicate/function call
     * @throws ErrorType    if the bound is not fully typechecked or is not a set/relation
     */
    public final Field addTrickyField(Pos pos, Pos isPrivate, Pos isMeta, String label, ExprVar x, Expr bound) throws Err {
        bound=bound.typecheck_as_set();
        if (bound.ambiguous) bound=bound.resolve_as_set(null);
        final Field f=new Field(pos, isPrivate, isMeta, this, label, x, bound);
        fields.add(f);
        return f;
    }

    /**
     * Add then return a new field F, where "all x: ThisSig | x.F in bound"
     * <p> Note: the bound must be fully-typechecked and have exactly 0 free variables.
     *
     * @param pos - the position in the original file where this field was defined (can be null if unknown)
     * @param label - the name of this field (it does not need to be unique)
     * @param bound - the new field will be bound by "all x: one ThisSig | x.ThisField in y"
     *
     * @throws ErrorSyntax  if the sig is one of the builtin sig
     * @throws ErrorSyntax  if the bound contains a predicate/function call
     * @throws ErrorType    if the bound is not fully typechecked or is not a set/relation
     */
    public final Field addField(Pos pos, String label, Expr bound) throws Err {
        return addTrickyField(pos, null, null, label, null, bound);
    }

    /**
     * Add then return a new field F, where "all x: ThisSig | x.F in bound"
     * <p> Note: the bound must be fully-typechecked and have exactly 0 free variables.
     *
     * @param pos - the position in the original file where this field was defined (can be null if unknown)
     * @param isPrivate - if nonnull, that means this field should be marked as private
     * @param label - the name of this field (it does not need to be unique)
     * @param bound - the new field will be bound by "all x: one ThisSig | x.ThisField in y"
     *
     * @throws ErrorSyntax  if the sig is one of the builtin sig
     * @throws ErrorSyntax  if the bound contains a predicate/function call
     * @throws ErrorType    if the bound is not fully typechecked or is not a set/relation
     */
    public final Field addField(Pos pos, Pos isPrivate, String label, Expr bound) throws Err {
        return addTrickyField(pos, isPrivate, null, label, null, bound);
    }

    /**
     * Add then return a new field F, where "all x: ThisSig | x.F in bound"
     * <p> Note: the bound must be fully-typechecked and have exactly 0 free variables.
     *
     * @param pos - the position in the original file where this field was defined (can be null if unknown)
     * @param isPrivate - if nonnull, that means this field should be marked as private
     * @param isMeta - if nonnull, that means this field should be marked as meta
     * @param label - the name of this field (it does not need to be unique)
     * @param bound - the new field will be bound by "all x: one ThisSig | x.ThisField in y"
     *
     * @throws ErrorSyntax  if the sig is one of the builtin sig
     * @throws ErrorSyntax  if the bound contains a predicate/function call
     * @throws ErrorType    if the bound is not fully typechecked or is not a set/relation
     */
    public final Field addField(Pos pos, Pos isPrivate, Pos isMeta, String label, Expr bound) throws Err {
        return addTrickyField(pos, isPrivate, isMeta, label, null, bound);
    }

    /**
     * Add then return a new field F where F is bound to an exact "definition" expression.
     * <p> Note: the definition must be fully-typechecked and have exactly 0 free variables.
     *
     * @param pos - the position in the original file where this field was defined (can be null if unknown)
     * @param isPrivate - if nonnull, that means this field should be marked as private
     * @param isMeta - if nonnull, that means this field should be marked as meta
     * @param label - the name of this field (it does not need to be unique)
     * @param definition - the new field will be defined to be exactly equal to this definition
     *
     * @throws ErrorSyntax  if the sig is one of the builtin sig
     * @throws ErrorSyntax  if the bound contains a predicate/function call
     * @throws ErrorType    if the bound is not fully typechecked or is not a set/relation
     */
    public final Field addDefinedField(Pos pos, Pos isPrivate, Pos isMeta, String label, Expr definition) throws Err {
        definition = definition.typecheck_as_set();
        if (definition.ambiguous) definition = definition.resolve_as_set(null);
        final Field f = new Field(pos, isPrivate, isMeta, this, label, definition);
        fields.add(f);
        return f;
    }
}
