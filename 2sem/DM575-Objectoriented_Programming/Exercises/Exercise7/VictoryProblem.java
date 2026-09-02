package Exercise7;

public class VictoryProblem extends Connector {
    
    @Override
    public boolean isGoal(Board state) {
        return (state.won() && isState(state));
    }
}
