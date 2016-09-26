public class Main {
    public static void main(String[] args){
    }

    // parenthesization
    class Paren {
	Paren left;
	Paren right;

	public String toString() {
	    if (left == null)
		return right;
	    if (right == null)
		return left;

	    return "(" + left.toString() + right.toString() + ")";
	}
    }

    class Leaf extends Paren {
	String value;
	public String toString() {
	    return value;
	}
}