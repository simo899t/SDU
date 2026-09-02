package Exercise7;

public interface SearchStrategy<S,A> {
    Node<S,A> search(Problem<S,A> problem);
}
