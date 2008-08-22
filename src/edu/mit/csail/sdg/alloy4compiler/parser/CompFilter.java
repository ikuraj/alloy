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

package edu.mit.csail.sdg.alloy4compiler.parser;

import java.util.LinkedList;
import java.util.List;
import java.io.Reader;
import java.io.IOException;
import java_cup_11a.runtime.Scanner;
import java_cup_11a.runtime.Symbol;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorFatal;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprConstant;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprVar;
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
 * (3) The lexer won't output the 15 special arrows (eg. ONE_ARROW_ONE)
 * Instead it outputs them as separate tokens (eg. "ONE", "ARROW", "ONE")
 * (The Filter class is used to merge them into a single "ONE_ARROW_ONE" token)
 */

final class CompFilter implements Scanner {

    //===================== PHASE 1 ==================================================================================

    /** The underlying lexer. */
    private final CompLexer r;

    /** A list of tokens that we prefetched from the underlying lexer. */
    private final LinkedList<Symbol> undo = new LinkedList<Symbol>();

    /** Stores the latest token passed from phase 1 to phase 2. */
    private Symbol last = null;

    private final Symbol change(Symbol a,int b) { a.sym=b; return a; }

    /** Reads a token from the underlying lexer; if the undo list is not empty, we take it from there instead. */
    private Symbol myread() throws Err {
      if (!undo.isEmpty()) return undo.removeFirst();
      try {
          return r.next_token();
      } catch(IOException ex) {
          throw new ErrorFatal("IO error: "+ex.getMessage(), ex);
      }
    }

    /** The symbols that can be at the end of a UnionDiffExpr. */
    private int[] follow = new int[] {RBRACE, RBRACKET, NUMBER, IDEN, THIS, DISJ, INT, SUM, UNIV, SIGINT, NONE, ID, RPAREN};

    /** Reads one or more tokens from the underlying lexer, transform them, then give it to phase 2. */
    public Symbol next_token() throws Err {
      Symbol a=myread(), b, c;
      for(int i=0; i<=follow.length; i++) {
          if (i==follow.length) {
              if ((b=myread()).sym!=MINUS)  { undo.add(0,b); break; }
              if ((c=myread()).sym!=NUMBER) { undo.add(0,c); undo.add(0,b); break; }
              ExprConstant num = (ExprConstant)(c.value);
              Pos pos = ((Pos)(b.value)).merge(num.span());
              c.value = ExprConstant.Op.NUMBER.make(pos, 0-num.num);
              undo.add(0,c);
              break;
          }
          if (a.sym == follow[i]) break;
      }
      if (a.sym==ID)
       {
        b=myread();
        if (b.sym==COLON)
         {
          c=myread();
          if (c.sym==RUN || c.sym==CHECK) {
              undo.add(0,a);
              undo.add(0,change(b,DOT));
              return last=c;
          }
          undo.add(0,c);
         }
        undo.add(0,b);
       }
      if (a.sym==PRED)
       {
        b=myread();
        if (b.sym==SLASH)
         {
          c=myread();
          if (c.sym==ID && ((ExprVar)(c.value)).label.equals("totalOrder")) { a.pos=a.pos.merge(c.pos); return last=change(a, TOTALORDER); }
          undo.add(0,c);
         }
        undo.add(0,b);
       }
      if (a.sym==FUN)
       {
        b=myread();
        if (b.sym==SLASH)
         {
          c=myread();
          if (c.sym==ID && ((ExprVar)(c.value)).label.equals("add")) { a.pos=a.pos.merge(c.pos); return last=change(a, INTADD); }
          if (c.sym==ID && ((ExprVar)(c.value)).label.equals("sub")) { a.pos=a.pos.merge(c.pos); return last=change(a, INTSUB); }
          if (c.sym==ID && ((ExprVar)(c.value)).label.equals("mul")) { a.pos=a.pos.merge(c.pos); return last=change(a, INTMUL); }
          if (c.sym==ID && ((ExprVar)(c.value)).label.equals("div")) { a.pos=a.pos.merge(c.pos); return last=change(a, INTDIV); }
          if (c.sym==ID && ((ExprVar)(c.value)).label.equals("rem")) { a.pos=a.pos.merge(c.pos); return last=change(a, INTREM); }
          if (c.sym==ID && ((ExprVar)(c.value)).label.equals("min")) { a.pos=a.pos.merge(c.pos); return last=change(a, INTMIN); }
          if (c.sym==ID && ((ExprVar)(c.value)).label.equals("max")) { a.pos=a.pos.merge(c.pos); return last=change(a, INTMAX); }
          if (c.sym==ID && ((ExprVar)(c.value)).label.equals("next")) { a.pos=a.pos.merge(c.pos); return last=change(a, INTNEXT); }
          if (c.sym==ID && ((ExprVar)(c.value)).label.equals("String")) { c.pos=c.pos.merge(a.pos); c.value=ExprVar.make(c.pos, "fun/String"); return last=c; }
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

    private Symbol arrow(Symbol a,int m) throws Err {
      Symbol b=myread();
      if (b.sym==LONE) return change(a,arrows[m*4  ]);
      if (b.sym==ONE ) return change(a,arrows[m*4+1]);
      if (b.sym==SOME) return change(a,arrows[m*4+2]);
      if (b.sym==SET ) return change(a,arrows[m*4+3]);
      undo.add(0,b);   return change(a,arrows[m*4+3]);
    }

    private Symbol decl(Symbol a,Symbol b,int c) throws Err {
        LinkedList<Symbol> temp=new LinkedList<Symbol>();
        if (b.sym==PRIVATE) {temp.add(b); b=myread();}
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

    /** Construct a filter for the tokens from the given file. */
    public CompFilter(Module module, List<Object> seenDollar, String filename, int lineOffset, Reader i) throws Err {
        r = new CompLexer(i);
        r.alloy_module = module;
        r.alloy_filename = filename;
        r.alloy_lineoffset = lineOffset;
        r.alloy_seenDollar = seenDollar;
    }
}
