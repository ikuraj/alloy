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

package edu.mit.csail.sdg.alloy4compiler.parser;

import java.util.LinkedList;
import java.io.Reader;
import java.io.IOException;
import java_cup_11a.runtime.Scanner;
import java_cup_11a.runtime.Symbol;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorAPI;
import static edu.mit.csail.sdg.alloy4compiler.parser.CompSym.*;

/**
 * This class sits between the lexer and the parser.
 *
 * <p>
 * Reason: there are 3 sets of "special tokens" that the lexer will not output.
 * But the Parser expects them. So this filter class observes the stream of
 * tokens, and intelligently merges or changes some primitive tokens into special tokens.
 *
 * <p>
 * For more details, refer to the main documentation.
 * But, very briefly, here are the 3 groups:
 *
 * <p>
 * (1) The lexer will generate only ALL, NO, LONE, ONE, SUM, SOME.
 * It will not output ALL2, NO2, LONE2, ONE2, SUM2, SOME2.
 * (The Filter class will change ONE into ONE2 when appropriate)
 *
 * <p>
 * (2) The lexer won't output NOTEQUALS, NOTIN, NOTLT, NOTLTE, NOTGT, NOTGTE.
 * Instead it outputs them as separate tokens (eg. "NOT" "EQUALS").
 * (The Filter class is used to merge them into a single "NOTEQUALS" token)
 *
 * <p>
 * (3) The lexer willn't output the 15 special arrows (eg. ONE_ARROW_ONE)
 * Instead it outputs them as separate tokens (eg. "ONE", "ARROW", "ONE")
 * (The Filter class is used to merge them into a single "ONE_ARROW_ONE" token)
 */

final class CompFilter implements Scanner {

    // TODO: should double check that the filter rules in this file still matches the current Alloy4 grammar

    //===================== PHASE 1 ==================================================================================

    /** The underlying lexer. */
    private final CompLexer r;

    /** A list of tokens that we prefetched from the underlying lexer. */
    private final LinkedList<Symbol> undo=new LinkedList<Symbol>();

    /** Stores the latest token passed from phase 1 to phase 2. */
    private Symbol last=null;

    private final Symbol change(Symbol a,int b) { a.sym=b; return a; }

    /** Reads a token from the underlying lexer; if the undo list is not empty, we take it from there instead. */
    private Symbol myread() throws IOException, Err {
      if (!undo.isEmpty()) return undo.removeFirst();
      return r.next_token();
    }

    /** Reads one or more tokens from the underlying lexer, transform them, then give it to phase 2. */
    private Symbol myreadtoken() throws IOException, Err {
      Symbol a=myread(),b;
      if (a.sym==ID)
       {
        b=myread();
        if (b.sym==COLON)
         {
          Symbol c; c=myread();
          if (c.sym==RUN || c.sym==CHECK) {
              undo.add(0,a);
              undo.add(0,change(b,DOT));
              return last=c;
          }
          undo.add(0,c);
         }
        undo.add(0,b);
       }
      if (a.sym==NOT)
       {
        b=myread();
        if (b.sym==IN)     return last=change(a, NOTIN);
        if (b.sym==EQUALS) return last=change(a, NOTEQUALS);
        if (b.sym==LT)     return last=change(a, NOTLT);
        if (b.sym==LTE)    return last=change(a, NOTLTE);
        if (b.sym==GT)     return last=change(a, NOTGT);
        if (b.sym==GTE)    return last=change(a, NOTGTE);
        undo.add(0,b); return last=a;
       }
      if (a.sym==ARROW) return last=arrow(a,0);
      if (a.sym==SET)
       {
        if ((b=myread()).sym==ARROW) return last=arrow(a,0);
        undo.add(0,b);
       }
      if (a.sym==LONE)
       {
        if ((b=myread()).sym==ARROW) return last=arrow(a,1);
        if (last==null || last.sym!=COLON) return last=decl(a,b,LONE2);
        undo.add(0,b);
       }
      if (a.sym==ONE)
       {
        if ((b=myread()).sym==ARROW) return last=arrow(a,2);
        if (last==null || last.sym!=COLON) return last=decl(a,b,ONE2);
        undo.add(0,b);
       }
      if (a.sym==SOME)
       {
        if ((b=myread()).sym==ARROW) return last=arrow(a,3);
        if (last==null || last.sym!=COLON) return last=decl(a,b,SOME2);
        undo.add(0,b);
       }
      if (last==null || last.sym!=COLON)
       {
        if (a.sym==NO) return last=decl(a, myread(), NO2);
        if (a.sym==ALL) return last=decl(a, myread(), ALL2);
        if (a.sym==SUM) return last=decl(a, myread(), SUM2);
       }
      return last=a;
    }

    private final int arrows[] = { // The element order is important
      ANY_ARROW_LONE, ANY_ARROW_ONE, ANY_ARROW_SOME, ARROW,
      LONE_ARROW_LONE, LONE_ARROW_ONE, LONE_ARROW_SOME, LONE_ARROW_ANY,
      ONE_ARROW_LONE, ONE_ARROW_ONE, ONE_ARROW_SOME, ONE_ARROW_ANY,
      SOME_ARROW_LONE, SOME_ARROW_ONE, SOME_ARROW_SOME, SOME_ARROW_ANY
    };

    private final Symbol arrow(Symbol a,int m) throws IOException, Err {
      Symbol b=myread();
      if (b.sym==LONE) return change(a,arrows[m*4  ]);
      if (b.sym==ONE ) return change(a,arrows[m*4+1]);
      if (b.sym==SOME) return change(a,arrows[m*4+2]);
      if (b.sym==SET ) return change(a,arrows[m*4+3]);
      undo.add(0,b);   return change(a,arrows[m*4+3]);
    }

    private final Symbol decl(Symbol a,Symbol b,int c) throws IOException, Err {
        LinkedList<Symbol> temp=new LinkedList<Symbol>();
        if (b.sym==DISJ || b.sym==PART || b.sym==EXH) {temp.add(b); b=myread();}
        temp.add(b);
        if (b.sym==ID) {
            while(true) {
                temp.add(b=myread());
                if (b.sym==COLON) {change(a,c);break;}
                if (b.sym!=COMMA) break;
                temp.add(b=myread());
                if (b.sym!=ID) break;
            }
        }
        for(int i=temp.size()-1; i>=0; i--) undo.add(0, temp.get(i));
        return a;
    }

    //===================== PHASE 2 ==================================================================================

    /** This class represents a list of Symbol objects. */
    private static final class PList {
        private final Symbol object;
        private PList prev,next;
        public PList(Symbol object, PList prev, PList next) { this.object=object; this.prev=prev; this.next=next; }
        public PList(Symbol object) { this.object=object; this.prev=null; this.next=null; }
        public int id() { return object.sym; }
        public boolean is(int a) { return object.sym==a; }
        public boolean is(int a,int b,int... c) {
            int id=object.sym;
            if (id==a || id==b) return true;
            for(int n=0; n<c.length; n++) if (c[n]==id) return true;
            return false;
        }
        public Symbol obj() { return object; }
        public PList next() { if (object.sym==EOF) return this; else return next; }
        public PList prepend(Symbol object) {
            PList x=new PList(object, prev, this);
            if (prev!=null) prev.next=x;
            this.prev=x;
            return x;
        }
        public PList append(Symbol object) {
            if (id()==EOF) return prepend(object);
            PList x=new PList(object, this, next);
            if (next!=null) next.prev=x;
            this.next=x;
            return x;
        }
        public PList append(String filename, int id) { return append(new Symbol(id, object.pos, null)); }
        public PList prepend(String filename, int id) { return prepend(new Symbol(id, object.pos, null)); }
    }

    /** Invariant: once constructor is done, this list's will contain exactly one EOF, which will be at its end. */
    private PList list=null;

    /** Reads every token from filename into an internal list. */
    public CompFilter(boolean allowDollar, final String filename, int lineOffset, Reader i) throws Err {
        try {
            PList ptr=null;
            r=new CompLexer(i);
            r.alloy_dollar=allowDollar;
            r.alloy_filename=filename;
            r.alloy_lineoffset=lineOffset;
            while(true) {
                Symbol s=myreadtoken();
                if (s==null) throw new IOException("Null token returned by the lexer");
                if (list==null) ptr=(list=new PList(s)); else ptr=ptr.append(s);
                if (s.sym==EOF) break;
            }
            r.yyclose();
        } catch(IOException ex) {
            throw new ErrorAPI("IO error: "+ex.getMessage());
        }
        PList ptr=list;
        while(true) {
            if (ptr.is(EOF)) break;
            if (ptr.is(OR,IFF,IMPLIES,ELSE,AND,NOT,PLUS,MINUS) && ptr.next().is(SOME2,ALL2,NO2,LONE2,ONE2,SUM2,LET)) {
                ptr=ptr.append(filename, LPAREN);
                PList ptr2=consumeTOPEXPR(ptr.next());
                ptr2.prepend(filename, RPAREN);
            }
            ptr=ptr.next();
        }
    }

    /** Consumes properly nested "[...]"; assumes the first token is "open". */
    private static PList consumePAIR(PList ptr, int open, int close) {
        ptr=ptr.next();
        for(int d=1; d>0 && !ptr.is(EOF); ptr=ptr.next()) {
            if (ptr.is(open)) d++; else if (ptr.is(close)) d--;
        }
        return ptr;
    }

    /** Consumes "ID/THIS (SLASH ID)*" ; assumes the first token is "ID" or "THIS". */
    private static PList consumeNAME(PList ptr) {
        while(true) {
            ptr=ptr.next();
            if (!ptr.is(SLASH) || !ptr.next().is(ID)) return ptr;
            ptr=ptr.next();
        }
    }

    /** Consumes "TOPEXPR".  */
    public static PList consumeTOPEXPR(PList ptr) {
        int step=1;
        while(true) {
            // =================== Step 1a ==============================================================
            // If we see INT[..] or SUM[..] or DISJ[..], then consume them then goto 2.
            if (step==1 && ptr.is(INT,SUM,DISJ) && ptr.next().is(LBRACKET)) {
                ptr=consumePAIR(ptr.next(), LBRACKET, RBRACKET);
                step=2;
            }
            // =================== Step 1b ==============================================================
            // If we see {NUMBER, NONE, IDEN, UNIV, SIGINT, THIS, LPAREN..RPAREN, LBRACE..RBRACE, AT Name, Name}
            // Then: consume it, goto 2.
            if (step==1 && ptr.is(AT) && ptr.next().is(ID,THIS)) { ptr=consumeNAME(ptr.next()); step=2; }
            if (step==1 && ptr.is(ID,THIS)) { ptr=consumeNAME(ptr); step=2; }
            if (step==1 && ptr.is(NUMBER,NONE,IDEN,UNIV,SIGINT)) { ptr=ptr.next(); step=2; }
            if (step==1 && ptr.is(LPAREN)) { ptr=consumePAIR(ptr,LPAREN,RPAREN); step=2; }
            if (step==1 && ptr.is(LBRACE)) { ptr=consumePAIR(ptr,LBRACE,RBRACE); step=2; }
            // =================== Step 1c ==============================================================
            // If we see {NOT NO SOME LONE ONE SET ALL HASH SUM INT TILDE STAR CARET}, consume then go to step 1
            if (step==1 && ptr.is(NOT,NO,SOME,LONE,ONE,SET,ALL,HASH,SUM,INT,TILDE,STAR,CARET)) {ptr=ptr.next(); continue;}
            // =================== Step 1d ==============================================================
            if (step==1 && ptr.is(LET)) {
                ptr=ptr.next();
                while(true) {
                    if (ptr.is(ID,THIS)) ptr=consumeNAME(ptr);
                    if (ptr.is(EQUALS)) ptr=ptr.next();
                    ptr=consumeTOPEXPR(ptr);
                    if (ptr.is(COMMA)) ptr=ptr.next(); else break;
                }
                if (ptr.is(BAR)) ptr=consumeTOPEXPR(ptr.next()); else if (ptr.is(LBRACE)) ptr=consumePAIR(ptr,LBRACE,RBRACE);
                return ptr;
            }
            // =================== Step 1e ==============================================================
            if (step==1 && ptr.is(ALL2,NO2,SOME2,LONE2,ONE2,SUM2)) {
                ptr=ptr.next();
                while(true) {
                    if (ptr.is(PART,DISJ,EXH) && ptr.next().is(ID,THIS)) ptr=ptr.next();
                    while(true) {
                        if (ptr.is(ID,THIS)) ptr=consumeNAME(ptr);
                        if (ptr.is(COMMA)) ptr=ptr.next(); else break;
                    }
                    if (ptr.is(COLON)) ptr=ptr.next();
                    ptr=consumeTOPEXPR(ptr);
                    if (ptr.is(COMMA)) ptr=ptr.next(); else break;
                }
                if (ptr.is(BAR)) ptr=consumeTOPEXPR(ptr.next()); else if (ptr.is(LBRACE)) ptr=consumePAIR(ptr,LBRACE,RBRACE);
                return ptr;
            }
            // =================== Step 2 ==============================================================
            // If next token is DOT INT or DOT SUM or DOT DISJ
            //    Consume both, goto 2.
            // If next token is {DOT DOMAIN RANGE ARROW* AMPERSAND PLUSPLUS PLUS MINUS AND OR IFF
            //                   IMPLIES ELSE NOT?EQUALS NOT?IN NOT?LT NOT?GT NOT?LTE NOT?GTE}
            //    Consume it. Goto 1.
            // If next token is LBRACKET
            //    Consume up to and including the matching RBRACKET. Goto 2.
            // All else, you're done
            if (step==2 && ptr.is(DOT) && ptr.next().is(INT,SUM,DISJ)) { ptr=ptr.next().next(); continue; }
            if (step==2 && ptr.is(DOT,DOMAIN,RANGE,AMPERSAND,PLUSPLUS,PLUS,MINUS,AND,OR,IFF,IMPLIES,ELSE,
                    ARROW, ANY_ARROW_LONE, ANY_ARROW_ONE, ANY_ARROW_SOME,
                    LONE_ARROW_ANY, LONE_ARROW_LONE, LONE_ARROW_ONE, LONE_ARROW_SOME,
                    ONE_ARROW_ANY, ONE_ARROW_LONE, ONE_ARROW_ONE, ONE_ARROW_SOME,
                    SOME_ARROW_ANY, SOME_ARROW_LONE, SOME_ARROW_ONE, SOME_ARROW_SOME,
                    EQUALS, IN, LT, LTE, GT, GTE, NOTEQUALS, NOTIN, NOTLT, NOTLTE, NOTGT, NOTGTE)) {
                ptr=ptr.next(); step=1; continue;
            }
            if (step==2 && ptr.is(LBRACKET)) { ptr=consumePAIR(ptr,LBRACKET,RBRACKET); continue; }
            return ptr;
        }
    }

    public Symbol next_token() {
        Symbol obj=list.object;
        list=list.next();
        return obj;
    }
}
