package Exercise7;

public interface Problem<S,A> {

 S initial();
 boolean isState(S state);
 boolean isGoal(S state);
 Iterable<A> actions(S state);
 S result(S state, A action);
 double cost(S state, A action);
}
