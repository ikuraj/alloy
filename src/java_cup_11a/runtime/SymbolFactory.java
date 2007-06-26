package java_cup_11a.runtime;

/**
 * Creates the Symbols interface, which CUP uses as default
 *
 * @version last updated 27-03-2006
 * @author Michael Petter
 */

public final class SymbolFactory {
    public Symbol newSymbol(String name, int sym, Object value) { return new Symbol(sym,null,value); }
    public Symbol startSymbol(String name, int sym, int state) { return new Symbol(sym,null,null,state); }
}
