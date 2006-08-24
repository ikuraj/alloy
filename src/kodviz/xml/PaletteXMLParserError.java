package kodviz.xml;

public class PaletteXMLParserError extends RuntimeException {
	
	private static final long serialVersionUID = 1L;

	public PaletteXMLParserError(String msg) {
		super("Internal error in Palette XML Parser: " + msg);
	}
	public PaletteXMLParserError() {
		super("Internal error in Palette XML Parser"); 
	}
}
