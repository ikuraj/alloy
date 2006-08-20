package edu.mit.csail.sdg.alloy4;

import java.util.List;
import java.util.ArrayList;

/**
 * Mutable; reresents a "signature".
 * @author Felix Chang
 */

public final class ParaSig extends Para {

	public static final ParaSig UNIV=new ParaSig("$univ","");
	public static final ParaSig NONE=new ParaSig("$none","");
	public static final ParaSig SIGINT=new ParaSig("$Int","");

	public static final String UNIV_NAME = "$univ";
	public static final String NONE_NAME = "$none";
	public static final String SIGINT_NAME = "$Int";

	// Abstract or not. Note, if a sig is abstract, it cannot be a subset sig (ie. "in" field must be null)
	public final boolean abs;

	// At most 1 can be true
	public final boolean lone,one,some;

	public List<String> aliases=new ArrayList<String>();

	// The list of field declarations (in 2 data structures)
	public List<VarDecl> decls;
	// The list of field objects. fields.size() must equal FieldDecl.count(decls).
	public List<Field> fields;
	// If non-null, it is an "appended facts" paragraph
	public Expr appendedFacts;

	public final String fullname;

	// The following 4 fields are initially empty until we properly initialize them
	// (Though "type" will be set already, for ParaSig.UNIV/NONE/SIGINT)
	public Type type;
	public ParaSig sup;                        // If I'm a SUBSIG, this is the parent. ELSE null.
	public List<ParaSig> sups=new ArrayList<ParaSig>(); // If I'm a SUBSETSIG, this is the list of parent(s).
	public List<ParaSig> subs=new ArrayList<ParaSig>(); // If I'm a TOPSIG/SUBSIG/"Int", sigs who EXTEND me.
	public final boolean subset;
	public String placeholder=null;

	public ParaSig(Pos p, String n) {
		super(p, "?", (n.lastIndexOf('/')>=0 ? n.substring(n.lastIndexOf('/')+1) : n));
		placeholder=n;
		one=false;
		lone=false;
		some=false;
		abs=false;
		fullname="?";
		subset=false;
	}

	public ParaSig(Pos p, String al, String n, boolean fa, boolean fl, boolean fo, boolean fs,
			List<ParaSig> i, ParaSig e, List<VarDecl> d, Expr f) {
		super(p, al, n);
		if (al.length()==0) fullname="/"+n; else fullname="/"+al+"/"+n;
		aliases.add(al);
		abs=fa; lone=fl; one=fo; some=fs;
		if (n==null || d==null) throw this.internalError("NullPointerException in Sig constructor!");
		if (n.length()==0) throw this.syntaxError("A signature must have a name!");
		if (n.indexOf('/')>=0) throw this.syntaxError("Signature name must not contain \'/\'.");
		if ((lone && one) || (lone && some) || (one && some)) throw this.syntaxError("A signature definition can only include at most one of the three keywords: ONE, LONE, and SOME.");

		if (i!=null) {
			if (abs) throw this.syntaxError("A subset signature cannot be abstract!");
			if (e!=null) throw this.syntaxError("A signature cannot both be a subset signature and a subsignature!");
			if (i.size()==0) throw this.syntaxError("To declare a subset signature, you must give the names of its parent signatures!");
			for(int j=0;j<i.size();j++) {
				String k=i.get(j).placeholder;
				if (k==null) sups.add(i.get(j)); else sups.add(new ParaSig(i.get(j).pos, k));
			}
			subset=true;
		} else subset=false;

		if (e!=null) {
			if (e.placeholder!=null) sup=new ParaSig(e.pos, e.placeholder); else sup=e;
		} else sup=null;

		fields=new ArrayList<Field>();
		decls=new ArrayList<VarDecl>(d);
		for(VarDecl dd:decls) {
			List<String> names=new ArrayList<String>();
			for(String dn:dd.names) {
				fields.add(new Field(dd.value.pos, (path.length()==0?"/"+name:"/"+path+"/"+name), dn, null, null));
				names.add(dn);
			}
		}
		String dup=VarDecl.hasDuplicateName(decls);
		if (dup!=null) throw this.syntaxError("This signature cannot have two fields with the same name: \""+dup+"\"");
		appendedFacts=f;
		type=null;
	}

	public boolean isSubtypeOf(ParaSig other) {
		if (subset || other.subset) return false; // Since this method is undefined for SUBSETSIG
		if (this==NONE || this==other || other==UNIV) return true;
		if (other==NONE) return false;
		for(ParaSig me=this; me!=null; me=me.sup) if (me==other) return true;
		return false;
	}

	public ParaSig intersect(ParaSig other) {
		if (subset || other.subset) return NONE; // Since this method is undefined for SUBSETSIG
		if (this.isSubtypeOf(other)) return this;
		if (other.isSubtypeOf(this)) return other;
		return NONE;
	}

	private ParaSig(String n, String al) {
		super(new Pos("$builtin library$",1,1), al, n);
		fullname="/"+al+"/"+n;
		aliases.add(al);
		abs=false; lone=false; one=false; some=false; //in=null; // ext=null;
		decls=new ArrayList<VarDecl>(0);
		appendedFacts=null;
		type=Type.make(this);
		fields=new ArrayList<Field>(0);
		sup=null;
		subset=false;
	}

	public boolean isEmpty() { return this==NONE; }
	public boolean isNonEmpty() { return this!=NONE; }

	public final class Field {
		public final Pos pos;
		public final String name;
		public final String fullname;
		public Type halftype;
		public final String sig;
		public final Full full;
		public ParaSig parent() { return ParaSig.this; }
		public Field(Pos pos, String pathsig, String name, Type halftype, Type fulltype) {
			this.pos=pos;
			this.name=name;
			this.full=new Full(pos, pathsig, name, fulltype);
			this.sig=pathsig;
			this.fullname=this.sig+".."+name;
			this.halftype=halftype;
		}
		public final class Full {
			public final Pos pos;
			public final String fullname;
			public Type fulltype;
			public Full(Pos pos, String pathsig, String name, Type type) {
				this.pos=pos;
				this.fullname=pathsig+"."+name;
				this.fulltype=type;
			}
		}
	}

}
