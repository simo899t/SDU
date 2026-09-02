package Exercise7;

public class Node<S,A> {
    
    public final S state;
    public final Node<S,A> parent;
    public final A action;
    public final double cost;

    Node(S state, Node<S,A> parent, A action, double cost) {
        this.state = state;
        this.parent = parent;
        this.action = action;
        this.cost = cost;
    }

    Node(S state) {
        this(state, null, null, 0);
    }

    S state() {
        return this.state;
    }

    Node<S,A> parent() {
        return this.parent;
    }

    A action() {
        return this.action;
    }

    double cost() {
        return this.cost;
    }
}
