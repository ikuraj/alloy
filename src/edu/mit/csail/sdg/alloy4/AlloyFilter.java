package edu.mit.csail.sdg.alloy4;

import java.util.LinkedList;
import java.io.Reader;
import java.io.IOException;
import java_cup.runtime.Symbol;
import java_cup.runtime.Scanner;

// There are 3 sets of "special tokens" that the lexer will not output.
// But the Parser expects them.
// So a special Filter class is written that sits between Lexer and Parser.
// The Filter class observes the stream of tokens, and intelligently
// merges or changes some primitive tokens into special tokens.
// For more details, refer to the main documentation.
//
// But, very briefly, here are the 3 groups:
//
// (1) The lexer will generate only ALL, NO, LONE, ONE, SUM, SOME.
// It will not output ALL2, NO2, LONE2, ONE2, SUM2, SOME2.
// (The Filter class will change some ONE into ONE2, etc)
//
// (2) The lexer won't output NOTEQUALS, NOTIN, NOTLT, NOTLTE, NOTGT, NOTGTE.
// Instead it outputs them as separate tokens (eg. "NOT" "EQUALS").
// (The Filter class is used to merge them into a single "NOTEQUALS" token)
//
// (3) The lexer willn't output the 15 special arrows (eg. ONE_ARROW_ONE)
// Instead it outputs them as separate tokens (eg. "ONE", "ARROW", "ONE")
// (The Filter class is used to merge them into a single "ONE_ARROW_ONE" token)

public final class AlloyFilter implements Scanner {

  private final AlloyLexer r;
  public AlloyFilter(String filename,Reader i) {
    r=new AlloyLexer(i);
    r.alloy_filename=filename;
  }

  public final void close() throws IOException { r.yyclose(); }

  private final LinkedList<Symbol> undo=new LinkedList<Symbol>();
  private final Symbol myread() throws IOException {
    if (!undo.isEmpty()) return undo.removeFirst();
    return r.next_token();
  }

  private final Symbol change(Symbol a,int b) { a.sym=b; return a; }
  private Symbol last=null;
  public Symbol next_token() throws IOException {
    Symbol a=myread(),b;
    if (a.sym==AlloySym.ID)
     {
      b=myread();
      if (b.sym==AlloySym.COLON)
       {
        Symbol c; c=myread();
        if (c.sym==AlloySym.RUN || c.sym==AlloySym.CHECK) return last=c;
        undo.add(0,c);
       }
      undo.add(0,b);
     }
    if (a.sym==AlloySym.NOT)
     {
      b=myread();
      if (b.sym==AlloySym.IN)     return last=change(a, AlloySym.NOTIN);
      if (b.sym==AlloySym.EQUALS) return last=change(a, AlloySym.NOTEQUALS);
      if (b.sym==AlloySym.LT)     return last=change(a, AlloySym.NOTLT);
      if (b.sym==AlloySym.LTE)    return last=change(a, AlloySym.NOTLTE);
      if (b.sym==AlloySym.GT)     return last=change(a, AlloySym.NOTGT);
      if (b.sym==AlloySym.GTE)    return last=change(a, AlloySym.NOTGTE);
      undo.add(0,b); return last=a;
     }
    if (a.sym==AlloySym.ARROW) return last=arrow(a,0);
    if (a.sym==AlloySym.SET)
     {
      if ((b=myread()).sym==AlloySym.ARROW) return last=arrow(a,0);
      undo.add(0,b); return last=a;
     }
    if (a.sym==AlloySym.LONE)
     {
      if ((b=myread()).sym==AlloySym.ARROW) return last=arrow(a,1);
      if (last==null || last.sym!=AlloySym.COLON) return last=decl(a,b,AlloySym.LONE2);
      undo.add(0,b);
     }
    if (a.sym==AlloySym.ONE)
     {
      if ((b=myread()).sym==AlloySym.ARROW) return last=arrow(a,2);
      if (last==null || last.sym!=AlloySym.COLON) return last=decl(a,b,AlloySym.ONE2);
      undo.add(0,b);
     }
    if (a.sym==AlloySym.SOME)
     {
      if ((b=myread()).sym==AlloySym.ARROW) return last=arrow(a,3);
      if (last==null || last.sym!=AlloySym.COLON) return last=decl(a,b,AlloySym.SOME2);
      undo.add(0,b);
     }
    if (last==null || last.sym!=AlloySym.COLON)
     {
      if (a.sym==AlloySym.NO) return last=decl(a, myread(), AlloySym.NO2);
      if (a.sym==AlloySym.ALL) return last=decl(a, myread(), AlloySym.ALL2);
      if (a.sym==AlloySym.SUM) return last=decl(a, myread(), AlloySym.SUM2);
     }
    return last=a;
  }

  private final int arrows[] = { // The element order is important
    AlloySym.ANY_ARROW_LONE, AlloySym.ANY_ARROW_ONE, AlloySym.ANY_ARROW_SOME, AlloySym.ARROW,
    AlloySym.LONE_ARROW_LONE, AlloySym.LONE_ARROW_ONE, AlloySym.LONE_ARROW_SOME, AlloySym.LONE_ARROW_ANY,
    AlloySym.ONE_ARROW_LONE, AlloySym.ONE_ARROW_ONE, AlloySym.ONE_ARROW_SOME, AlloySym.ONE_ARROW_ANY,
    AlloySym.SOME_ARROW_LONE, AlloySym.SOME_ARROW_ONE, AlloySym.SOME_ARROW_SOME, AlloySym.SOME_ARROW_ANY
  };

  private final Symbol arrow(Symbol a,int m) throws IOException {
    Symbol b=myread();
    if (b.sym==AlloySym.LONE) return change(a,arrows[m*4  ]);
    if (b.sym==AlloySym.ONE ) return change(a,arrows[m*4+1]);
    if (b.sym==AlloySym.SOME) return change(a,arrows[m*4+2]);
    if (b.sym==AlloySym.SET ) return change(a,arrows[m*4+3]);
    undo.add(0,b);            return change(a,arrows[m*4+3]);
  }

  private final LinkedList<Symbol> temp=new LinkedList<Symbol>();
  private final Symbol decl(Symbol a,Symbol b,int c) throws IOException {
    temp.clear();
    if (b.sym==AlloySym.DISJ || b.sym==AlloySym.PART || b.sym==AlloySym.EXH)
       {undo.add(0,b); return change(a,c);}
    if (b.sym!=AlloySym.ID)
       {undo.add(0,b); return a;}
    temp.add(b);
    while(true) {
      temp.add(b=myread());
      if (b.sym==AlloySym.COLON) {change(a,c);break;}
      if (b.sym!=AlloySym.COMMA) break;
      temp.add(b=myread());
      if (b.sym!=AlloySym.ID) break;
    }
    for(int i=temp.size()-1;i>=0;i--) undo.add(0,temp.get(i));
    temp.clear(); // To allow garbage collection on the objects
    return a;
  }
}
